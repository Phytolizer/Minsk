#include "all.h"
#include <limits.h>

/* For x86_64, do the following:
 *
 * - check that constants are used only in
 *   places allowed
 * - ensure immediates always fit in 32b
 * - expose machine register contraints
 *   on instructions like division.
 * - implement fast locals (the streak of
 *   constant allocX in the first basic block)
 * - recognize complex addressing modes
 *
 * Invariant: the use counts that are used
 *            in sel() must be sound.  This
 *            is not so trivial, maybe the
 *            dce should be moved out...
 */

typedef struct ANum ANum;

struct ANum {
	char n, l, r;
	Ins *i;
};

static int amatch(Addr *, Ref, int, ANum *, Fn *);

static int
noimm(Ref r, Fn *fn)
{
	int64_t val;

	if (rtype(r) != RCon)
		return 0;
	switch (fn->con[r.val].type) {
	case CAddr:
		/* we only support the 'small'
		 * code model of the ABI, this
		 * means that we can always
		 * address data with 32bits
		 */
		return 0;
	case CBits:
		val = fn->con[r.val].bits.i;
		return (val < INT32_MIN || val > INT32_MAX);
	default:
		die("invalid constant");
	}
}

static int
rslot(Ref r, Fn *fn)
{
	if (rtype(r) != RTmp)
		return -1;
	return fn->tmp[r.val].slot;
}

static void
fixarg(Ref *r, int k, Ins *i, Fn *fn)
{
	char buf[32];
	Addr a, *m;
	Con cc, *c;
	Ref r0, r1, r2, r3;
	int s, n, op;

	r1 = r0 = *r;
	s = rslot(r0, fn);
	op = i ? i->op : Ocopy;
	if (KBASE(k) == 1 && rtype(r0) == RCon) {
		/* load floating points from memory
		 * slots, they can't be used as
		 * immediates
		 */
		r1 = MEM(fn->nmem);
		vgrow(&fn->mem, ++fn->nmem);
		memset(&a, 0, sizeof a);
		a.offset.type = CAddr;
		n = stashbits(&fn->con[r0.val].bits, KWIDE(k) ? 8 : 4);
		sprintf(buf, "\"%sfp%d\"", T.asloc, n);
		a.offset.sym.id = intern(buf);
		fn->mem[fn->nmem-1] = a;
	}
	else if (op != Ocopy && k == Kl && noimm(r0, fn)) {
		/* load constants that do not fit in
		 * a 32bit signed integer into a
		 * long temporary
		 */
		r1 = newtmp("isel", Kl, fn);
		emit(Ocopy, Kl, r1, r0, R);
	}
	else if (s != -1) {
		/* load fast locals' addresses into
		 * temporaries right before the
		 * instruction
		 */
		r1 = newtmp("isel", Kl, fn);
		emit(Oaddr, Kl, r1, SLOT(s), R);
	}
	else if (!(isstore(op) && r == &i->arg[1])
	&& !isload(op) && op != Ocall && rtype(r0) == RCon
	&& fn->con[r0.val].type == CAddr) {
		/* apple as does not support 32-bit
		 * absolute addressing, use a rip-
		 * relative leaq instead
		 */
		r1 = newtmp("isel", Kl, fn);
		emit(Oaddr, Kl, r1, r0, R);
	}
	else if (rtype(r0) == RMem) {
		/* eliminate memory operands of
		 * the form $foo(%rip, ...)
		 */
		m = &fn->mem[r0.val];
		if (req(m->base, R))
		if (m->offset.type == CAddr) {
			r0 = newtmp("isel", Kl, fn);
			emit(Oaddr, Kl, r0, newcon(&m->offset, fn), R);
			m->offset.type = CUndef;
			m->base = r0;
		}
	} else if (T.apple && rtype(r0) == RCon
	&& (c = &fn->con[r0.val])->type == CAddr
	&& c->sym.type == SThr) {
		r1 = newtmp("isel", Kl, fn);
		if (c->bits.i) {
			r2 = newtmp("isel", Kl, fn);
			cc = (Con){.type = CBits};
			cc.bits.i = c->bits.i;
			r3 = newcon(&cc, fn);
			emit(Oadd, Kl, r1, r2, r3);
		} else
			r2 = r1;
		emit(Ocopy, Kl, r2, TMP(RAX), R);
		r2 = newtmp("isel", Kl, fn);
		r3 = newtmp("isel", Kl, fn);
		emit(Ocall, 0, R, r3, CALL(17));
		emit(Ocopy, Kl, TMP(RDI), r2, R);
		emit(Oload, Kl, r3, r2, R);
		cc = *c;
		cc.bits.i = 0;
		r3 = newcon(&cc, fn);
		emit(Oload, Kl, r2, r3, R);
	}
	*r = r1;
}

static void
seladdr(Ref *r, ANum *an, Fn *fn)
{
	Addr a;
	Ref r0;

	r0 = *r;
	if (rtype(r0) == RTmp) {
		memset(&a, 0, sizeof a);
		if (!amatch(&a, r0, an[r0.val].n, an, fn))
			return;
		if (!req(a.base, R))
		if (a.offset.type == CAddr) {
			/* apple as does not support
			 * $foo(%r0, %r1, M); try to
			 * rewrite it or bail out if
			 * impossible
			 */
			if (!req(a.index, R) || rtype(a.base) != RTmp)
				return;
			else {
				a.index = a.base;
				a.scale = 1;
				a.base = R;
			}
		}
		chuse(r0, -1, fn);
		vgrow(&fn->mem, ++fn->nmem);
		fn->mem[fn->nmem-1] = a;
		chuse(a.base, +1, fn);
		chuse(a.index, +1, fn);
		*r = MEM(fn->nmem-1);
	}
}

static int
cmpswap(Ref arg[2], int op)
{
	switch (op) {
	case NCmpI+Cflt:
	case NCmpI+Cfle:
		return 1;
	case NCmpI+Cfgt:
	case NCmpI+Cfge:
		return 0;
	}
	return rtype(arg[0]) == RCon;
}

static void
selcmp(Ref arg[2], int k, int swap, Fn *fn)
{
	Ref r;
	Ins *icmp;

	if (swap) {
		r = arg[1];
		arg[1] = arg[0];
		arg[0] = r;
	}
	emit(Oxcmp, k, R, arg[1], arg[0]);
	icmp = curi;
	if (rtype(arg[0]) == RCon) {
		assert(k != Kw);
		icmp->arg[1] = newtmp("isel", k, fn);
		emit(Ocopy, k, icmp->arg[1], arg[0], R);
		fixarg(&curi->arg[0], k, curi, fn);
	}
	fixarg(&icmp->arg[0], k, icmp, fn);
	fixarg(&icmp->arg[1], k, icmp, fn);
}

static void
sel(Ins i, ANum *an, Fn *fn)
{
	Ref r0, r1, tmp[7];
	int x, j, k, kc, sh, swap;
	Ins *i0, *i1;

	if (rtype(i.to) == RTmp)
	if (!isreg(i.to) && !isreg(i.arg[0]) && !isreg(i.arg[1]))
	if (fn->tmp[i.to.val].nuse == 0) {
		chuse(i.arg[0], -1, fn);
		chuse(i.arg[1], -1, fn);
		return;
	}
	i0 = curi;
	k = i.cls;
	switch (i.op) {
	case Odiv:
	case Orem:
	case Oudiv:
	case Ourem:
		if (KBASE(k) == 1)
			goto Emit;
		if (i.op == Odiv || i.op == Oudiv)
			r0 = TMP(RAX), r1 = TMP(RDX);
		else
			r0 = TMP(RDX), r1 = TMP(RAX);
		emit(Ocopy, k, i.to, r0, R);
		emit(Ocopy, k, R, r1, R);
		if (rtype(i.arg[1]) == RCon) {
			/* immediates not allowed for
			 * divisions in x86
			 */
			r0 = newtmp("isel", k, fn);
		} else
			r0 = i.arg[1];
		if (fn->tmp[r0.val].slot != -1)
			err("unlikely argument %%%s in %s",
				fn->tmp[r0.val].name, optab[i.op].name);
		if (i.op == Odiv || i.op == Orem) {
			emit(Oxidiv, k, R, r0, R);
			emit(Osign, k, TMP(RDX), TMP(RAX), R);
		} else {
			emit(Oxdiv, k, R, r0, R);
			emit(Ocopy, k, TMP(RDX), CON_Z, R);
		}
		emit(Ocopy, k, TMP(RAX), i.arg[0], R);
		fixarg(&curi->arg[0], k, curi, fn);
		if (rtype(i.arg[1]) == RCon)
			emit(Ocopy, k, r0, i.arg[1], R);
		break;
	case Osar:
	case Oshr:
	case Oshl:
		r0 = i.arg[1];
		if (rtype(r0) == RCon)
			goto Emit;
		if (fn->tmp[r0.val].slot != -1)
			err("unlikely argument %%%s in %s",
				fn->tmp[r0.val].name, optab[i.op].name);
		i.arg[1] = TMP(RCX);
		emit(Ocopy, Kw, R, TMP(RCX), R);
		emiti(i);
		i1 = curi;
		emit(Ocopy, Kw, TMP(RCX), r0, R);
		fixarg(&i1->arg[0], argcls(&i, 0), i1, fn);
		break;
	case Ouwtof:
		r0 = newtmp("utof", Kl, fn);
		emit(Osltof, k, i.to, r0, R);
		emit(Oextuw, Kl, r0, i.arg[0], R);
		fixarg(&curi->arg[0], k, curi, fn);
		break;
	case Oultof:
		/* %mask =l and %arg.0, 1
		   %isbig =l shr %arg.0, 63
		   %divided =l shr %arg.0, %isbig
		   %or =l or %mask, %divided
		   %float =d sltof %or
		   %cast =l cast %float
		   %addend =l shl %isbig, 52
		   %sum =l add %cast, %addend
		   %result =d cast %sum
		*/
		r0 = newtmp("utof", k, fn);
		if (k == Ks)
			kc = Kw, sh = 23;
		else
			kc = Kl, sh = 52;
		for (j=0; j<4; j++)
			tmp[j] = newtmp("utof", Kl, fn);
		for (; j<7; j++)
			tmp[j] = newtmp("utof", kc, fn);
		emit(Ocast, k, i.to, tmp[6], R);
		emit(Oadd, kc, tmp[6], tmp[4], tmp[5]);
		emit(Oshl, kc, tmp[5], tmp[1], getcon(sh, fn));
		emit(Ocast, kc, tmp[4], r0, R);
		emit(Osltof, k, r0, tmp[3], R);
		emit(Oor, Kl, tmp[3], tmp[0], tmp[2]);
		emit(Oshr, Kl, tmp[2], i.arg[0], tmp[1]);
		sel(*curi++, 0, fn);
		emit(Oshr, Kl, tmp[1], i.arg[0], getcon(63, fn));
		fixarg(&curi->arg[0], Kl, curi, fn);
		emit(Oand, Kl, tmp[0], i.arg[0], getcon(1, fn));
		fixarg(&curi->arg[0], Kl, curi, fn);
		break;
	case Ostoui:
		i.op = Ostosi;
		kc = Ks;
		tmp[4] = getcon(0xdf000000, fn);
		goto Oftoui;
	case Odtoui:
		i.op = Odtosi;
		kc = Kd;
		tmp[4] = getcon(0xc3e0000000000000, fn);
	Oftoui:
		if (k == Kw)
			goto Emit;
		r0 = newtmp("ftou", kc, fn);
		for (j=0; j<4; j++)
			tmp[j] = newtmp("ftou", Kl, fn);
		emit(Oor, Kl, i.to, tmp[0], tmp[3]);
		emit(Oand, Kl, tmp[3], tmp[2], tmp[1]);
		emit(i.op, Kl, tmp[2], r0, R);
		emit(Oadd, kc, r0, tmp[4], i.arg[0]);
		i1 = curi; /* fixarg() can change curi */
		fixarg(&i1->arg[0], kc, i1, fn);
		fixarg(&i1->arg[1], kc, i1, fn);
		emit(Osar, Kl, tmp[1], tmp[0], getcon(63, fn));
		emit(i.op, Kl, tmp[0], i.arg[0], R);
		fixarg(&curi->arg[0], Kl, curi, fn);
		break;
	case Onop:
		break;
	case Ostored:
	case Ostores:
	case Ostorel:
	case Ostorew:
	case Ostoreh:
	case Ostoreb:
		if (rtype(i.arg[0]) == RCon) {
			if (i.op == Ostored)
				i.op = Ostorel;
			if (i.op == Ostores)
				i.op = Ostorew;
		}
		seladdr(&i.arg[1], an, fn);
		goto Emit;
	case_Oload:
		seladdr(&i.arg[0], an, fn);
		goto Emit;
	case Ocall:
	case Osalloc:
	case Ocopy:
	case Oadd:
	case Osub:
	case Oneg:
	case Omul:
	case Oand:
	case Oor:
	case Oxor:
	case Oxtest:
	case Ostosi:
	case Odtosi:
	case Oswtof:
	case Osltof:
	case Oexts:
	case Otruncd:
	case Ocast:
	case_OExt:
Emit:
		emiti(i);
		i1 = curi; /* fixarg() can change curi */
		fixarg(&i1->arg[0], argcls(&i, 0), i1, fn);
		fixarg(&i1->arg[1], argcls(&i, 1), i1, fn);
		break;
	case Oalloc4:
	case Oalloc8:
	case Oalloc16:
		salloc(i.to, i.arg[0], fn);
		break;
	default:
		if (isext(i.op))
			goto case_OExt;
		if (isload(i.op))
			goto case_Oload;
		if (iscmp(i.op, &kc, &x)) {
			switch (x) {
			case NCmpI+Cfeq:
				/* zf is set when operands are
				 * unordered, so we may have to
				 * check pf
				 */
				r0 = newtmp("isel", Kw, fn);
				r1 = newtmp("isel", Kw, fn);
				emit(Oand, Kw, i.to, r0, r1);
				emit(Oflagfo, k, r1, R, R);
				i.to = r0;
				break;
			case NCmpI+Cfne:
				r0 = newtmp("isel", Kw, fn);
				r1 = newtmp("isel", Kw, fn);
				emit(Oor, Kw, i.to, r0, r1);
				emit(Oflagfuo, k, r1, R, R);
				i.to = r0;
				break;
			}
			swap = cmpswap(i.arg, x);
			if (swap)
				x = cmpop(x);
			emit(Oflag+x, k, i.to, R, R);
			selcmp(i.arg, kc, swap, fn);
			break;
		}
		die("unknown instruction %s", optab[i.op].name);
	}

	while (i0>curi && --i0) {
		assert(rslot(i0->arg[0], fn) == -1);
		assert(rslot(i0->arg[1], fn) == -1);
	}
}

static Ins *
flagi(Ins *i0, Ins *i)
{
	while (i>i0) {
		i--;
		if (amd64_op[i->op].zflag)
			return i;
		if (amd64_op[i->op].lflag)
			continue;
		return 0;
	}
	return 0;
}

static void
seljmp(Blk *b, Fn *fn)
{
	Ref r;
	int c, k, swap;
	Ins *fi;
	Tmp *t;

	if (b->jmp.type == Jret0
	|| b->jmp.type == Jjmp
	|| b->jmp.type == Jhlt)
		return;
	assert(b->jmp.type == Jjnz);
	r = b->jmp.arg;
	t = &fn->tmp[r.val];
	b->jmp.arg = R;
	assert(rtype(r) == RTmp);
	if (b->s1 == b->s2) {
		chuse(r, -1, fn);
		b->jmp.type = Jjmp;
		b->s2 = 0;
		return;
	}
	fi = flagi(b->ins, &b->ins[b->nins]);
	if (!fi || !req(fi->to, r)) {
		selcmp((Ref[2]){r, CON_Z}, Kw, 0, fn);
		b->jmp.type = Jjf + Cine;
	}
	else if (iscmp(fi->op, &k, &c)
	     && c != NCmpI+Cfeq /* see sel() */
	     && c != NCmpI+Cfne) {
		swap = cmpswap(fi->arg, c);
		if (swap)
			c = cmpop(c);
		if (t->nuse == 1) {
			selcmp(fi->arg, k, swap, fn);
			*fi = (Ins){.op = Onop};
		}
		b->jmp.type = Jjf + c;
	}
	else if (fi->op == Oand && t->nuse == 1
	     && (rtype(fi->arg[0]) == RTmp ||
	         rtype(fi->arg[1]) == RTmp)) {
		fi->op = Oxtest;
		fi->to = R;
		b->jmp.type = Jjf + Cine;
		if (rtype(fi->arg[1]) == RCon) {
			r = fi->arg[1];
			fi->arg[1] = fi->arg[0];
			fi->arg[0] = r;
		}
	}
	else {
		/* since flags are not tracked in liveness,
		 * the result of the flag-setting instruction
		 * has to be marked as live
		 */
		if (t->nuse == 1)
			emit(Ocopy, Kw, R, r, R);
		b->jmp.type = Jjf + Cine;
	}
}

static int
aref(Ref r, ANum *ai)
{
	switch (rtype(r)) {
	case RCon:
		return 2;
	case RTmp:
		return ai[r.val].n;
	default:
		die("constant or temporary expected");
	}
}

static int
ascale(Ref r, Con *con)
{
	int64_t n;

	if (rtype(r) != RCon)
		return 0;
	if (con[r.val].type != CBits)
		return 0;
	n = con[r.val].bits.i;
	return n == 1 || n == 2 || n == 4 || n == 8;
}

static void
anumber(ANum *ai, Blk *b, Con *con)
{
	/* This should be made obsolete by a proper
	 * reassoc pass.
	 *
	 * Rules:
	 *
	 *   RTmp(_) -> 0    tmp
	 *   ( RTmp(_) -> 1    slot )
	 *   RCon(_) -> 2    con
	 *   0 * 2   -> 3    s * i (when constant is 1,2,4,8)
	 */
	static char add[10][10] = {
		[2] [4] = 4, [4] [2] = 4,
		[2] [6] = 6, [6] [2] = 6,
		[2] [7] = 7, [7] [2] = 7,
		[0] [2] = 4, [2] [0] = 4, /* 4: o + b */
		[0] [0] = 5,              /* 5: b + s * i */
		[0] [3] = 5, [3] [0] = 5,
		[2] [3] = 6, [3] [2] = 6, /* 6: o + s * i */
		[2] [5] = 7, [5] [2] = 7, /* 7: o + b + s * i */
		[0] [6] = 7, [6] [0] = 7,
		[4] [3] = 7, [3] [4] = 7,
	};
	int a, a1, a2, n1, n2, t1, t2;
	Ins *i;

	for (i=b->ins; i<&b->ins[b->nins]; i++) {
		if (rtype(i->to) == RTmp)
			ai[i->to.val].i = i;
		if (i->op != Oadd && i->op != Omul)
			continue;
		a1 = aref(i->arg[0], ai);
		a2 = aref(i->arg[1], ai);
		t1 = a1 != 1 && a1 != 2;
		t2 = a2 != 1 && a2 != 2;
		if (i->op == Oadd) {
			a = add[n1 = a1][n2 = a2];
			if (t1 && a < add[0][a2])
				a = add[n1 = 0][n2 = a2];
			if (t2 && a < add[a1][0])
				a = add[n1 = a1][n2 = 0];
			if (t1 && t2 && a < add[0][0])
				a = add[n1 = 0][n2 = 0];
		} else {
			n1 = n2 = a = 0;
			if (ascale(i->arg[0], con) && t2)
				a = 3, n1 = 2, n2 = 0;
			if (t1 && ascale(i->arg[1], con))
				a = 3, n1 = 0, n2 = 2;
		}
		ai[i->to.val].n = a;
		ai[i->to.val].l = n1;
		ai[i->to.val].r = n2;
	}
}

static int
amatch(Addr *a, Ref r, int n, ANum *ai, Fn *fn)
{
	Ins *i;
	int nl, nr, t, s;
	Ref al, ar;

	if (rtype(r) == RCon) {
		if (!addcon(&a->offset, &fn->con[r.val]))
			err("unlikely sum of $%s and $%s",
				str(a->offset.sym.id),
				str(fn->con[r.val].sym.id));
		return 1;
	}
	assert(rtype(r) == RTmp);
	i = ai[r.val].i;
	nl = ai[r.val].l;
	nr = ai[r.val].r;
	if (i) {
		if (nl > nr) {
			al = i->arg[1];
			ar = i->arg[0];
			t = nl, nl = nr, nr = t;
		} else {
			al = i->arg[0];
			ar = i->arg[1];
		}
	}
	switch (n) {
	case 3: /* s * i */
		a->index = al;
		a->scale = fn->con[ar.val].bits.i;
		return 0;
	case 5: /* b + s * i */
		switch (nr) {
		case 0:
			if (fn->tmp[ar.val].slot != -1) {
				al = i->arg[1];
				ar = i->arg[0];
			}
			a->index = ar;
			a->scale = 1;
			break;
		case 3:
			amatch(a, ar, nr, ai, fn);
			break;
		}
		r = al;
		/* fall through */
	case 0:
		s = fn->tmp[r.val].slot;
		if (s != -1)
			r = SLOT(s);
		a->base = r;
		return n || s != -1;
	case 2: /* constants */
	case 4: /* o + b */
	case 6: /* o + s * i */
	case 7: /* o + b + s * i */
		amatch(a, ar, nr, ai, fn);
		amatch(a, al, nl, ai, fn);
		return 1;
	default:
		die("unreachable");
	}
}

/* instruction selection
 * requires use counts (as given by parsing)
 */
void
amd64_isel(Fn *fn)
{
	Blk *b, **sb;
	Ins *i;
	Phi *p;
	uint a;
	int n, al;
	int64_t sz;
	ANum *ainfo;

	/* assign slots to fast allocs */
	b = fn->start;
	/* specific to NAlign == 3 */ /* or change n=4 and sz /= 4 below */
	for (al=Oalloc, n=4; al<=Oalloc1; al++, n*=2)
		for (i=b->ins; i<&b->ins[b->nins]; i++)
			if (i->op == al) {
				if (rtype(i->arg[0]) != RCon)
					break;
				sz = fn->con[i->arg[0].val].bits.i;
				if (sz < 0 || sz >= INT_MAX-15)
					err("invalid alloc size %"PRId64, sz);
				sz = (sz + n-1) & -n;
				sz /= 4;
				if (sz > INT_MAX - fn->slot)
					die("alloc too large");
				fn->tmp[i->to.val].slot = fn->slot;
				fn->slot += sz;
				*i = (Ins){.op = Onop};
			}

	/* process basic blocks */
	n = fn->ntmp;
	ainfo = emalloc(n * sizeof ainfo[0]);
	for (b=fn->start; b; b=b->link) {
		curi = &insb[NIns];
		for (sb=(Blk*[3]){b->s1, b->s2, 0}; *sb; sb++)
			for (p=(*sb)->phi; p; p=p->link) {
				for (a=0; p->blk[a] != b; a++)
					assert(a+1 < p->narg);
				fixarg(&p->arg[a], p->cls, 0, fn);
			}
		memset(ainfo, 0, n * sizeof ainfo[0]);
		anumber(ainfo, b, fn->con);
		seljmp(b, fn);
		for (i=&b->ins[b->nins]; i!=b->ins;)
			sel(*--i, ainfo, fn);
		b->nins = &insb[NIns] - curi;
		idup(&b->ins, curi, b->nins);
	}
	free(ainfo);

	if (debug['I']) {
		fprintf(stderr, "\n> After instruction selection:\n");
		printfn(fn, stderr);
	}
}
