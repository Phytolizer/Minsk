#include "all.h"
#include <ctype.h>
#include <stdarg.h>

enum {
	Ksb = 4, /* matches Oarg/Opar/Jret */
	Kub,
	Ksh,
	Kuh,
	Kc,
	K0,

	Ke = -2, /* erroneous mode */
	Km = Kl, /* memory pointer */
};

Op optab[NOp] = {
#define O(op, t, cf) [O##op]={#op, t, cf},
	#include "ops.h"
};

typedef enum {
	PXXX,
	PLbl,
	PPhi,
	PIns,
	PEnd,
} PState;

enum Token {
	Txxx = 0,

	/* aliases */
	Tloadw = NPubOp,
	Tloadl,
	Tloads,
	Tloadd,
	Talloc1,
	Talloc2,

	Tblit,
	Tcall,
	Tenv,
	Tphi,
	Tjmp,
	Tjnz,
	Tret,
	Thlt,
	Texport,
	Tthread,
	Tfunc,
	Ttype,
	Tdata,
	Tsection,
	Talign,
	Tl,
	Tw,
	Tsh,
	Tuh,
	Th,
	Tsb,
	Tub,
	Tb,
	Td,
	Ts,
	Tz,

	Tint,
	Tflts,
	Tfltd,
	Ttmp,
	Tlbl,
	Tglo,
	Ttyp,
	Tstr,

	Tplus,
	Teq,
	Tcomma,
	Tlparen,
	Trparen,
	Tlbrace,
	Trbrace,
	Tnl,
	Tdots,
	Teof,

	Ntok
};

static char *kwmap[Ntok] = {
	[Tloadw] = "loadw",
	[Tloadl] = "loadl",
	[Tloads] = "loads",
	[Tloadd] = "loadd",
	[Talloc1] = "alloc1",
	[Talloc2] = "alloc2",
	[Tblit] = "blit",
	[Tcall] = "call",
	[Tenv] = "env",
	[Tphi] = "phi",
	[Tjmp] = "jmp",
	[Tjnz] = "jnz",
	[Tret] = "ret",
	[Thlt] = "hlt",
	[Texport] = "export",
	[Tthread] = "thread",
	[Tfunc] = "function",
	[Ttype] = "type",
	[Tdata] = "data",
	[Tsection] = "section",
	[Talign] = "align",
	[Tsb] = "sb",
	[Tub] = "ub",
	[Tsh] = "sh",
	[Tuh] = "uh",
	[Tb] = "b",
	[Th] = "h",
	[Tw] = "w",
	[Tl] = "l",
	[Ts] = "s",
	[Td] = "d",
	[Tz] = "z",
	[Tdots] = "...",
};

enum {
	NPred = 63,

	TMask = 16383, /* for temps hash */
	BMask = 8191, /* for blocks hash */

	K = 9583425, /* found using tools/lexh.c */
	M = 23,
};

static uchar lexh[1 << (32-M)];
static FILE *inf;
static char *inpath;
static int thead;
static struct {
	char chr;
	double fltd;
	float flts;
	int64_t num;
	char *str;
} tokval;
static int lnum;

static Fn *curf;
static int tmph[TMask+1];
static Phi **plink;
static Blk *curb;
static Blk **blink;
static Blk *blkh[BMask+1];
static int nblk;
static int rcls;
static uint ntyp;

void
err(char *s, ...)
{
	va_list ap;

	va_start(ap, s);
	fprintf(stderr, "qbe:%s:%d: ", inpath, lnum);
	vfprintf(stderr, s, ap);
	fprintf(stderr, "\n");
	va_end(ap);
	exit(1);
}

static void
lexinit()
{
	static int done;
	int i;
	long h;

	if (done)
		return;
	for (i=0; i<NPubOp; ++i)
		if (optab[i].name)
			kwmap[i] = optab[i].name;
	assert(Ntok <= UCHAR_MAX);
	for (i=0; i<Ntok; ++i)
		if (kwmap[i]) {
			h = hash(kwmap[i])*K >> M;
			assert(lexh[h] == Txxx);
			lexh[h] = i;
		}
	done = 1;
}

static int64_t
getint()
{
	uint64_t n;
	int c, m;

	n = 0;
	c = fgetc(inf);
	m = (c == '-');
	if (m || c == '+')
		c = fgetc(inf);
	do {
		n = 10*n + (c - '0');
		c = fgetc(inf);
	} while ('0' <= c && c <= '9');
	ungetc(c, inf);
	if (m)
		n = 1 + ~n;
	return *(int64_t *)&n;
}

static int
lex()
{
	static char tok[NString];
	int c, i, esc;
	int t;

	do
		c = fgetc(inf);
	while (isblank(c));
	t = Txxx;
	tokval.chr = c;
	switch (c) {
	case EOF:
		return Teof;
	case ',':
		return Tcomma;
	case '(':
		return Tlparen;
	case ')':
		return Trparen;
	case '{':
		return Tlbrace;
	case '}':
		return Trbrace;
	case '=':
		return Teq;
	case '+':
		return Tplus;
	case 's':
		if (fscanf(inf, "_%f", &tokval.flts) != 1)
			break;
		return Tflts;
	case 'd':
		if (fscanf(inf, "_%lf", &tokval.fltd) != 1)
			break;
		return Tfltd;
	case '%':
		t = Ttmp;
		c = fgetc(inf);
		goto Alpha;
	case '@':
		t = Tlbl;
		c = fgetc(inf);
		goto Alpha;
	case '$':
		t = Tglo;
		if ((c = fgetc(inf)) == '"')
			goto Quoted;
		goto Alpha;
	case ':':
		t = Ttyp;
		c = fgetc(inf);
		goto Alpha;
	case '#':
		while ((c=fgetc(inf)) != '\n' && c != EOF)
			;
		/* fall through */
	case '\n':
		lnum++;
		return Tnl;
	}
	if (isdigit(c) || c == '-' || c == '+') {
		ungetc(c, inf);
		tokval.num = getint();
		return Tint;
	}
	if (c == '"') {
		t = Tstr;
	Quoted:
		tokval.str = vnew(2, 1, PFn);
		tokval.str[0] = c;
		esc = 0;
		for (i=1;; i++) {
			c = fgetc(inf);
			if (c == EOF)
				err("unterminated string");
			vgrow(&tokval.str, i+2);
			tokval.str[i] = c;
			if (c == '"' && !esc) {
				tokval.str[i+1] = 0;
				return t;
			}
			esc = (c == '\\' && !esc);
		}
	}
Alpha:
	if (!isalpha(c) && c != '.' && c != '_')
		err("invalid character %c (%d)", c, c);
	i = 0;
	do {
		if (i >= NString-1)
			err("identifier too long");
		tok[i++] = c;
		c = fgetc(inf);
	} while (isalpha(c) || c == '$' || c == '.' || c == '_' || isdigit(c));
	tok[i] = 0;
	ungetc(c, inf);
	tokval.str = tok;
	if (t != Txxx) {
		return t;
	}
	t = lexh[hash(tok)*K >> M];
	if (t == Txxx || strcmp(kwmap[t], tok) != 0) {
		err("unknown keyword %s", tok);
		return Txxx;
	}
	return t;
}

static int
peek()
{
	if (thead == Txxx)
		thead = lex();
	return thead;
}

static int
next()
{
	int t;

	t = peek();
	thead = Txxx;
	return t;
}

static int
nextnl()
{
	int t;

	while ((t = next()) == Tnl)
		;
	return t;
}

static void
expect(int t)
{
	static char *ttoa[] = {
		[Tlbl] = "label",
		[Tcomma] = ",",
		[Teq] = "=",
		[Tnl] = "newline",
		[Tlparen] = "(",
		[Trparen] = ")",
		[Tlbrace] = "{",
		[Trbrace] = "}",
		[Teof] = 0,
	};
	char buf[128], *s1, *s2;
	int t1;

	t1 = next();
	if (t == t1)
		return;
	s1 = ttoa[t] ? ttoa[t] : "??";
	s2 = ttoa[t1] ? ttoa[t1] : "??";
	sprintf(buf, "%s expected, got %s instead", s1, s2);
	err(buf);
}

static Ref
tmpref(char *v)
{
	int t, *h;

	h = &tmph[hash(v) & TMask];
	t = *h;
	if (t) {
		if (strcmp(curf->tmp[t].name, v) == 0)
			return TMP(t);
		for (t=curf->ntmp-1; t>=Tmp0; t--)
			if (strcmp(curf->tmp[t].name, v) == 0)
				return TMP(t);
	}
	t = curf->ntmp;
	*h = t;
	newtmp(0, Kx, curf);
	strcpy(curf->tmp[t].name, v);
	return TMP(t);
}

static Ref
parseref()
{
	Con c;

	memset(&c, 0, sizeof c);
	switch (next()) {
	default:
		return R;
	case Ttmp:
		return tmpref(tokval.str);
	case Tint:
		c.type = CBits;
		c.bits.i = tokval.num;
		break;
	case Tflts:
		c.type = CBits;
		c.bits.s = tokval.flts;
		c.flt = 1;
		break;
	case Tfltd:
		c.type = CBits;
		c.bits.d = tokval.fltd;
		c.flt = 2;
		break;
	case Tthread:
		c.sym.type = SThr;
		expect(Tglo);
		/* fall through */
	case Tglo:
		c.type = CAddr;
		c.sym.id = intern(tokval.str);
		break;
	}
	return newcon(&c, curf);
}

static int
findtyp(int i)
{
	while (--i >= 0)
		if (strcmp(tokval.str, typ[i].name) == 0)
			return i;
	err("undefined type :%s", tokval.str);
}

static int
parsecls(int *tyn)
{
	switch (next()) {
	default:
		err("invalid class specifier");
	case Ttyp:
		*tyn = findtyp(ntyp);
		return Kc;
	case Tsb:
		return Ksb;
	case Tub:
		return Kub;
	case Tsh:
		return Ksh;
	case Tuh:
		return Kuh;
	case Tw:
		return Kw;
	case Tl:
		return Kl;
	case Ts:
		return Ks;
	case Td:
		return Kd;
	}
}

static int
parserefl(int arg)
{
	int k, ty, env, hasenv, vararg;
	Ref r;

	hasenv = 0;
	vararg = 0;
	expect(Tlparen);
	while (peek() != Trparen) {
		if (curi - insb >= NIns)
			err("too many instructions");
		if (!arg && vararg)
			err("no parameters allowed after '...'");
		switch (peek()) {
		case Tdots:
			if (vararg)
				err("only one '...' allowed");
			vararg = 1;
			if (arg) {
				*curi = (Ins){.op = Oargv};
				curi++;
			}
			next();
			goto Next;
		case Tenv:
			if (hasenv)
				err("only one environment allowed");
			hasenv = 1;
			env = 1;
			next();
			k = Kl;
			break;
		default:
			env = 0;
			k = parsecls(&ty);
			break;
		}
		r = parseref();
		if (req(r, R))
			err("invalid argument");
		if (!arg && rtype(r) != RTmp)
			err("invalid function parameter");
		if (env)
			if (arg)
				*curi = (Ins){Oarge, k, R, {r}};
			else
				*curi = (Ins){Opare, k, r, {R}};
		else if (k == Kc)
			if (arg)
				*curi = (Ins){Oargc, Kl, R, {TYPE(ty), r}};
			else
				*curi = (Ins){Oparc, Kl, r, {TYPE(ty)}};
		else if (k >= Ksb)
			if (arg)
				*curi = (Ins){Oargsb+(k-Ksb), Kw, R, {r}};
			else
				*curi = (Ins){Oparsb+(k-Ksb), Kw, r, {R}};
		else
			if (arg)
				*curi = (Ins){Oarg, k, R, {r}};
			else
				*curi = (Ins){Opar, k, r, {R}};
		curi++;
	Next:
		if (peek() == Trparen)
			break;
		expect(Tcomma);
	}
	expect(Trparen);
	return vararg;
}

static Blk *
findblk(char *name)
{
	Blk *b;
	uint32_t h;

	h = hash(name) & BMask;
	for (b=blkh[h]; b; b=b->dlink)
		if (strcmp(b->name, name) == 0)
			return b;
	b = blknew();
	b->id = nblk++;
	strcpy(b->name, name);
	b->dlink = blkh[h];
	blkh[h] = b;
	return b;
}

static void
closeblk()
{
	curb->nins = curi - insb;
	idup(&curb->ins, insb, curb->nins);
	blink = &curb->link;
	curi = insb;
}

static PState
parseline(PState ps)
{
	Ref arg[NPred] = {R};
	Blk *blk[NPred];
	Phi *phi;
	Ref r;
	Blk *b;
	Con *c;
	int t, op, i, k, ty;

	t = nextnl();
	if (ps == PLbl && t != Tlbl && t != Trbrace)
		err("label or } expected");
	switch (t) {
	default:
		if (isstore(t)) {
		case Tblit:
		case Tcall:
		case Ovastart:
			/* operations without result */
			r = R;
			k = Kw;
			op = t;
			goto DoOp;
		}
		err("label, instruction or jump expected");
	case Trbrace:
		return PEnd;
	case Ttmp:
		break;
	case Tlbl:
		b = findblk(tokval.str);
		if (curb && curb->jmp.type == Jxxx) {
			closeblk();
			curb->jmp.type = Jjmp;
			curb->s1 = b;
		}
		if (b->jmp.type != Jxxx)
			err("multiple definitions of block @%s", b->name);
		*blink = b;
		curb = b;
		plink = &curb->phi;
		expect(Tnl);
		return PPhi;
	case Tret:
		curb->jmp.type = Jretw + rcls;
		if (peek() == Tnl)
			curb->jmp.type = Jret0;
		else if (rcls != K0) {
			r = parseref();
			if (req(r, R))
				err("invalid return value");
			curb->jmp.arg = r;
		}
		goto Close;
	case Tjmp:
		curb->jmp.type = Jjmp;
		goto Jump;
	case Tjnz:
		curb->jmp.type = Jjnz;
		r = parseref();
		if (req(r, R))
			err("invalid argument for jnz jump");
		curb->jmp.arg = r;
		expect(Tcomma);
	Jump:
		expect(Tlbl);
		curb->s1 = findblk(tokval.str);
		if (curb->jmp.type != Jjmp) {
			expect(Tcomma);
			expect(Tlbl);
			curb->s2 = findblk(tokval.str);
		}
		if (curb->s1 == curf->start || curb->s2 == curf->start)
			err("invalid jump to the start block");
		goto Close;
	case Thlt:
		curb->jmp.type = Jhlt;
	Close:
		expect(Tnl);
		closeblk();
		return PLbl;
	}
	r = tmpref(tokval.str);
	expect(Teq);
	k = parsecls(&ty);
	op = next();
DoOp:
	if (op == Tcall) {
		arg[0] = parseref();
		parserefl(1);
		op = Ocall;
		expect(Tnl);
		if (k == Kc) {
			k = Kl;
			arg[1] = TYPE(ty);
		} else
			arg[1] = R;
		if (k >= Ksb)
			k = Kw;
		goto Ins;
	}
	if (op == Tloadw)
		op = Oloadsw;
	if (op >= Tloadl && op <= Tloadd)
		op = Oload;
	if (op == Talloc1 || op == Talloc2)
		op = Oalloc;
	if (op == Ovastart && !curf->vararg)
		err("cannot use vastart in non-variadic function");
	if (k >= Ksb)
		err("size class must be w, l, s, or d");
	i = 0;
	if (peek() != Tnl)
		for (;;) {
			if (i == NPred)
				err("too many arguments");
			if (op == Tphi) {
				expect(Tlbl);
				blk[i] = findblk(tokval.str);
			}
			arg[i] = parseref();
			if (req(arg[i], R))
				err("invalid instruction argument");
			i++;
			t = peek();
			if (t == Tnl)
				break;
			if (t != Tcomma)
				err(", or end of line expected");
			next();
		}
	next();
	switch (op) {
	case Tphi:
		if (ps != PPhi || curb == curf->start)
			err("unexpected phi instruction");
		phi = alloc(sizeof *phi);
		phi->to = r;
		phi->cls = k;
		phi->arg = vnew(i, sizeof arg[0], PFn);
		memcpy(phi->arg, arg, i * sizeof arg[0]);
		phi->blk = vnew(i, sizeof blk[0], PFn);
		memcpy(phi->blk, blk, i * sizeof blk[0]);
		phi->narg = i;
		*plink = phi;
		plink = &phi->link;
		return PPhi;
	case Tblit:
		if (curi - insb >= NIns-1)
			err("too many instructions");
		memset(curi, 0, 2 * sizeof(Ins));
		curi->op = Oblit0;
		curi->arg[0] = arg[0];
		curi->arg[1] = arg[1];
		curi++;
		if (rtype(arg[2]) != RCon)
			err("blit size must be constant");
		c = &curf->con[arg[2].val];
		r = INT(c->bits.i);
		if (c->type != CBits
		|| rsval(r) < 0
		|| rsval(r) != c->bits.i)
			err("invalid blit size");
		curi->op = Oblit1;
		curi->arg[0] = r;
		curi++;
		return PIns;
	default:
		if (op >= NPubOp)
			err("invalid instruction");
	Ins:
		if (curi - insb >= NIns)
			err("too many instructions");
		curi->op = op;
		curi->cls = k;
		curi->to = r;
		curi->arg[0] = arg[0];
		curi->arg[1] = arg[1];
		curi++;
		return PIns;
	}
}

static int
usecheck(Ref r, int k, Fn *fn)
{
	return rtype(r) != RTmp || fn->tmp[r.val].cls == k
		|| (fn->tmp[r.val].cls == Kl && k == Kw);
}

static void
typecheck(Fn *fn)
{
	Blk *b;
	Phi *p;
	Ins *i;
	uint n;
	int k;
	Tmp *t;
	Ref r;
	BSet pb[1], ppb[1];

	fillpreds(fn);
	bsinit(pb, fn->nblk);
	bsinit(ppb, fn->nblk);
	for (b=fn->start; b; b=b->link) {
		for (p=b->phi; p; p=p->link)
			fn->tmp[p->to.val].cls = p->cls;
		for (i=b->ins; i<&b->ins[b->nins]; i++)
			if (rtype(i->to) == RTmp) {
				t = &fn->tmp[i->to.val];
				if (clsmerge(&t->cls, i->cls))
					err("temporary %%%s is assigned with"
						" multiple types", t->name);
			}
	}
	for (b=fn->start; b; b=b->link) {
		bszero(pb);
		for (n=0; n<b->npred; n++)
			bsset(pb, b->pred[n]->id);
		for (p=b->phi; p; p=p->link) {
			bszero(ppb);
			t = &fn->tmp[p->to.val];
			for (n=0; n<p->narg; n++) {
				k = t->cls;
				if (bshas(ppb, p->blk[n]->id))
					err("multiple entries for @%s in phi %%%s",
						p->blk[n]->name, t->name);
				if (!usecheck(p->arg[n], k, fn))
					err("invalid type for operand %%%s in phi %%%s",
						fn->tmp[p->arg[n].val].name, t->name);
				bsset(ppb, p->blk[n]->id);
			}
			if (!bsequal(pb, ppb))
				err("predecessors not matched in phi %%%s", t->name);
		}
		for (i=b->ins; i<&b->ins[b->nins]; i++)
			for (n=0; n<2; n++) {
				k = optab[i->op].argcls[n][i->cls];
				r = i->arg[n];
				t = &fn->tmp[r.val];
				if (k == Ke)
					err("invalid instruction type in %s",
						optab[i->op].name);
				if (rtype(r) == RType)
					continue;
				if (rtype(r) != -1 && k == Kx)
					err("no %s operand expected in %s",
						n == 1 ? "second" : "first",
						optab[i->op].name);
				if (rtype(r) == -1 && k != Kx)
					err("missing %s operand in %s",
						n == 1 ? "second" : "first",
						optab[i->op].name);
				if (!usecheck(r, k, fn))
					err("invalid type for %s operand %%%s in %s",
						n == 1 ? "second" : "first",
						t->name, optab[i->op].name);
			}
		r = b->jmp.arg;
		if (isret(b->jmp.type)) {
			if (b->jmp.type == Jretc)
				k = Kl;
			else if (b->jmp.type >= Jretsb)
				k = Kw;
			else
				k = b->jmp.type - Jretw;
			if (!usecheck(r, k, fn))
				goto JErr;
		}
		if (b->jmp.type == Jjnz && !usecheck(r, Kw, fn))
		JErr:
			err("invalid type for jump argument %%%s in block @%s",
				fn->tmp[r.val].name, b->name);
		if (b->s1 && b->s1->jmp.type == Jxxx)
			err("block @%s is used undefined", b->s1->name);
		if (b->s2 && b->s2->jmp.type == Jxxx)
			err("block @%s is used undefined", b->s2->name);
	}
}

static Fn *
parsefn(Lnk *lnk)
{
	Blk *b;
	int i;
	PState ps;

	curb = 0;
	nblk = 0;
	curi = insb;
	curf = alloc(sizeof *curf);
	curf->ntmp = 0;
	curf->ncon = 2;
	curf->tmp = vnew(curf->ntmp, sizeof curf->tmp[0], PFn);
	curf->con = vnew(curf->ncon, sizeof curf->con[0], PFn);
	for (i=0; i<Tmp0; ++i)
		if (T.fpr0 <= i && i < T.fpr0 + T.nfpr)
			newtmp(0, Kd, curf);
		else
			newtmp(0, Kl, curf);
	curf->con[0].type = CBits;
	curf->con[0].bits.i = 0xdeaddead;  /* UNDEF */
	curf->con[1].type = CBits;
	curf->lnk = *lnk;
	blink = &curf->start;
	curf->retty = Kx;
	if (peek() != Tglo)
		rcls = parsecls(&curf->retty);
	else
		rcls = K0;
	if (next() != Tglo)
		err("function name expected");
	strncpy(curf->name, tokval.str, NString-1);
	curf->vararg = parserefl(0);
	if (nextnl() != Tlbrace)
		err("function body must start with {");
	ps = PLbl;
	do
		ps = parseline(ps);
	while (ps != PEnd);
	if (!curb)
		err("empty function");
	if (curb->jmp.type == Jxxx)
		err("last block misses jump");
	curf->mem = vnew(0, sizeof curf->mem[0], PFn);
	curf->nmem = 0;
	curf->nblk = nblk;
	curf->rpo = 0;
	for (b=0; b; b=b->link)
		b->dlink = 0; /* was trashed by findblk() */
	for (i=0; i<BMask+1; ++i)
		blkh[i] = 0;
	memset(tmph, 0, sizeof tmph);
	typecheck(curf);
	return curf;
}

static void
parsefields(Field *fld, Typ *ty, int t)
{
	Typ *ty1;
	int n, c, a, al, type;
	uint64_t sz, s;

	n = 0;
	sz = 0;
	al = ty->align;
	while (t != Trbrace) {
		ty1 = 0;
		switch (t) {
		default: err("invalid type member specifier");
		case Td: type = Fd; s = 8; a = 3; break;
		case Tl: type = Fl; s = 8; a = 3; break;
		case Ts: type = Fs; s = 4; a = 2; break;
		case Tw: type = Fw; s = 4; a = 2; break;
		case Th: type = Fh; s = 2; a = 1; break;
		case Tb: type = Fb; s = 1; a = 0; break;
		case Ttyp:
			type = FTyp;
			ty1 = &typ[findtyp(ntyp-1)];
			s = ty1->size;
			a = ty1->align;
			break;
		}
		if (a > al)
			al = a;
		a = (1 << a) - 1;
		a = ((sz + a) & ~a) - sz;
		if (a) {
			if (n < NField) {
				/* padding */
				fld[n].type = FPad;
				fld[n].len = a;
				n++;
			}
		}
		t = nextnl();
		if (t == Tint) {
			c = tokval.num;
			t = nextnl();
		} else
			c = 1;
		sz += a + c*s;
		if (type == FTyp)
			s = ty1 - typ;
		for (; c>0 && n<NField; c--, n++) {
			fld[n].type = type;
			fld[n].len = s;
		}
		if (t != Tcomma)
			break;
		t = nextnl();
	}
	if (t != Trbrace)
		err(", or } expected");
	fld[n].type = FEnd;
	a = 1 << al;
	if (sz < ty->size)
		sz = ty->size;
	ty->size = (sz + a - 1) & -a;
	ty->align = al;
}

static void
parsetyp()
{
	Typ *ty;
	int t, al;
	uint n;

	/* be careful if extending the syntax
	 * to handle nested types, any pointer
	 * held to typ[] might be invalidated!
	 */
	vgrow(&typ, ntyp+1);
	ty = &typ[ntyp++];
	ty->isdark = 0;
	ty->isunion = 0;
	ty->align = -1;
	ty->size = 0;
	if (nextnl() != Ttyp ||  nextnl() != Teq)
		err("type name and then = expected");
	strcpy(ty->name, tokval.str);
	t = nextnl();
	if (t == Talign) {
		if (nextnl() != Tint)
			err("alignment expected");
		for (al=0; tokval.num /= 2; al++)
			;
		ty->align = al;
		t = nextnl();
	}
	if (t != Tlbrace)
		err("type body must start with {");
	t = nextnl();
	if (t == Tint) {
		ty->isdark = 1;
		ty->size = tokval.num;
		if (ty->align == -1)
			err("dark types need alignment");
		if (nextnl() != Trbrace)
			err("} expected");
		return;
	}
	n = 0;
	ty->fields = vnew(1, sizeof ty->fields[0], PHeap);
	if (t == Tlbrace) {
		ty->isunion = 1;
		do {
			if (t != Tlbrace)
				err("invalid union member");
			vgrow(&ty->fields, n+1);
			parsefields(ty->fields[n++], ty, nextnl());
			t = nextnl();
		} while (t != Trbrace);
	} else
		parsefields(ty->fields[n++], ty, t);
	ty->nunion = n;
}

static void
parsedatref(Dat *d)
{
	int t;

	d->isref = 1;
	d->u.ref.name = tokval.str;
	d->u.ref.off = 0;
	t = peek();
	if (t == Tplus) {
		next();
		if (next() != Tint)
			err("invalid token after offset in ref");
		d->u.ref.off = tokval.num;
	}
}

static void
parsedatstr(Dat *d)
{
	d->isstr = 1;
	d->u.str = tokval.str;
}

static void
parsedat(void cb(Dat *), Lnk *lnk)
{
	char name[NString] = {0};
	int t;
	Dat d;

	if (nextnl() != Tglo || nextnl() != Teq)
		err("data name, then = expected");
	strncpy(name, tokval.str, NString-1);
	t = nextnl();
	lnk->align = 8;
	if (t == Talign) {
		if (nextnl() != Tint)
			err("alignment expected");
		lnk->align = tokval.num;
		t = nextnl();
	}
	d.type = DStart;
	d.name = name;
	d.lnk = lnk;
	cb(&d);

	if (t != Tlbrace)
		err("expected data contents in { .. }");
	for (;;) {
		switch (nextnl()) {
		default: err("invalid size specifier %c in data", tokval.chr);
		case Trbrace: goto Done;
		case Tl: d.type = DL; break;
		case Tw: d.type = DW; break;
		case Th: d.type = DH; break;
		case Tb: d.type = DB; break;
		case Ts: d.type = DW; break;
		case Td: d.type = DL; break;
		case Tz: d.type = DZ; break;
		}
		t = nextnl();
		do {
			d.isstr = 0;
			d.isref = 0;
			memset(&d.u, 0, sizeof d.u);
			if (t == Tflts)
				d.u.flts = tokval.flts;
			else if (t == Tfltd)
				d.u.fltd = tokval.fltd;
			else if (t == Tint)
				d.u.num = tokval.num;
			else if (t == Tglo)
				parsedatref(&d);
			else if (t == Tstr)
				parsedatstr(&d);
			else
				err("constant literal expected");
			cb(&d);
			t = nextnl();
		} while (t == Tint || t == Tflts || t == Tfltd || t == Tstr || t == Tglo);
		if (t == Trbrace)
			break;
		if (t != Tcomma)
			err(", or } expected");
	}
Done:
	d.type = DEnd;
	cb(&d);
}

static int
parselnk(Lnk *lnk)
{
	int t, haslnk;

	for (haslnk=0;; haslnk=1)
		switch ((t=nextnl())) {
		case Texport:
			lnk->export = 1;
			break;
		case Tthread:
			lnk->thread = 1;
			break;
		case Tsection:
			if (lnk->sec)
				err("only one section allowed");
			if (next() != Tstr)
				err("section \"name\" expected");
			lnk->sec = tokval.str;
			if (peek() == Tstr) {
				next();
				lnk->secf = tokval.str;
			}
			break;
		default:
			if (t == Tfunc && lnk->thread)
				err("only data may have thread linkage");
			if (haslnk && t != Tdata && t != Tfunc)
				err("only data and function have linkage");
			return t;
		}
}

void
parse(FILE *f, char *path, void data(Dat *), void func(Fn *))
{
	Lnk lnk;
	uint n;

	lexinit();
	inf = f;
	inpath = path;
	lnum = 1;
	thead = Txxx;
	ntyp = 0;
	typ = vnew(0, sizeof typ[0], PHeap);
	for (;;) {
		lnk = (Lnk){0};
		switch (parselnk(&lnk)) {
		default:
			err("top-level definition expected");
		case Tfunc:
			func(parsefn(&lnk));
			break;
		case Tdata:
			parsedat(data, &lnk);
			break;
		case Ttype:
			parsetyp();
			break;
		case Teof:
			for (n=0; n<ntyp; n++)
				if (typ[n].nunion)
					vfree(typ[n].fields);
			vfree(typ);
			return;
		}
	}
}

static void
printcon(Con *c, FILE *f)
{
	switch (c->type) {
	case CUndef:
		break;
	case CAddr:
		fprintf(f, "$%s", str(c->sym.id));
		if (c->bits.i)
			fprintf(f, "%+"PRIi64, c->bits.i);
		break;
	case CBits:
		if (c->flt == 1)
			fprintf(f, "s_%f", c->bits.s);
		else if (c->flt == 2)
			fprintf(f, "d_%lf", c->bits.d);
		else
			fprintf(f, "%"PRIi64, c->bits.i);
		break;
	}
}

void
printref(Ref r, Fn *fn, FILE *f)
{
	int i;
	Mem *m;

	switch (rtype(r)) {
	case RTmp:
		if (r.val < Tmp0)
			fprintf(f, "R%d", r.val);
		else
			fprintf(f, "%%%s", fn->tmp[r.val].name);
		break;
	case RCon:
		if (req(r, UNDEF))
			fprintf(f, "UNDEF");
		else
			printcon(&fn->con[r.val], f);
		break;
	case RSlot:
		fprintf(f, "S%d", rsval(r));
		break;
	case RCall:
		fprintf(f, "%04x", r.val);
		break;
	case RType:
		fprintf(f, ":%s", typ[r.val].name);
		break;
	case RMem:
		i = 0;
		m = &fn->mem[r.val];
		fputc('[', f);
		if (m->offset.type != CUndef) {
			printcon(&m->offset, f);
			i = 1;
		}
		if (!req(m->base, R)) {
			if (i)
				fprintf(f, " + ");
			printref(m->base, fn, f);
			i = 1;
		}
		if (!req(m->index, R)) {
			if (i)
				fprintf(f, " + ");
			fprintf(f, "%d * ", m->scale);
			printref(m->index, fn, f);
		}
		fputc(']', f);
		break;
	case RInt:
		fprintf(f, "%d", rsval(r));
		break;
	}
}

void
printfn(Fn *fn, FILE *f)
{
	static char ktoc[] = "wlsd";
	static char *jtoa[NJmp] = {
	#define X(j) [J##j] = #j,
		JMPS(X)
	#undef X
	};
	Blk *b;
	Phi *p;
	Ins *i;
	uint n;

	fprintf(f, "function $%s() {\n", fn->name);
	for (b=fn->start; b; b=b->link) {
		fprintf(f, "@%s\n", b->name);
		for (p=b->phi; p; p=p->link) {
			fprintf(f, "\t");
			printref(p->to, fn, f);
			fprintf(f, " =%c phi ", ktoc[p->cls]);
			assert(p->narg);
			for (n=0;; n++) {
				fprintf(f, "@%s ", p->blk[n]->name);
				printref(p->arg[n], fn, f);
				if (n == p->narg-1) {
					fprintf(f, "\n");
					break;
				} else
					fprintf(f, ", ");
			}
		}
		for (i=b->ins; i<&b->ins[b->nins]; i++) {
			fprintf(f, "\t");
			if (!req(i->to, R)) {
				printref(i->to, fn, f);
				fprintf(f, " =%c ", ktoc[i->cls]);
			}
			assert(optab[i->op].name);
			fprintf(f, "%s", optab[i->op].name);
			if (req(i->to, R))
				switch (i->op) {
				case Oarg:
				case Oswap:
				case Oxcmp:
				case Oacmp:
				case Oacmn:
				case Oafcmp:
				case Oxtest:
				case Oxdiv:
				case Oxidiv:
					fputc(ktoc[i->cls], f);
				}
			if (!req(i->arg[0], R)) {
				fprintf(f, " ");
				printref(i->arg[0], fn, f);
			}
			if (!req(i->arg[1], R)) {
				fprintf(f, ", ");
				printref(i->arg[1], fn, f);
			}
			fprintf(f, "\n");
		}
		switch (b->jmp.type) {
		case Jret0:
		case Jretsb:
		case Jretub:
		case Jretsh:
		case Jretuh:
		case Jretw:
		case Jretl:
		case Jrets:
		case Jretd:
		case Jretc:
			fprintf(f, "\t%s", jtoa[b->jmp.type]);
			if (b->jmp.type != Jret0 || !req(b->jmp.arg, R)) {
				fprintf(f, " ");
				printref(b->jmp.arg, fn, f);
			}
			if (b->jmp.type == Jretc)
				fprintf(f, ", :%s", typ[fn->retty].name);
			fprintf(f, "\n");
			break;
		case Jhlt:
			fprintf(f, "\thlt\n");
			break;
		case Jjmp:
			if (b->s1 != b->link)
				fprintf(f, "\tjmp @%s\n", b->s1->name);
			break;
		default:
			fprintf(f, "\t%s ", jtoa[b->jmp.type]);
			if (b->jmp.type == Jjnz) {
				printref(b->jmp.arg, fn, f);
				fprintf(f, ", ");
			}
			assert(b->s1 && b->s2);
			fprintf(f, "@%s, @%s\n", b->s1->name, b->s2->name);
			break;
		}
	}
	fprintf(f, "}\n");
}
