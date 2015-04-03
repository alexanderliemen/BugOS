trigflags			equ		$00
trigdeg				equ		2

doneflags			equ		$00
donePrgm			equ		5

plotflags			equ		$02
plotloc				equ		1
plotdisp			equ		2

grfmodeflags		equ		$02
grffuncm			equ		4
grfpolarm			equ		5
grfparamm			equ		6
grfrecurm			equ		7

graphflags			equ		$03
graphdraw			equ		0
graphcursor			equ		2

grfdbflags			equ		$04
grfdot				equ		0
grfsimul			equ		1
grfgrid				equ		2
grfpolar			equ		3
grfnocoord			equ		4
grfnoaxis			equ		5
grflabel			equ		6

textflags			equ		$05
textEraseBelow		equ		1
textScrolled		equ		2
textInverse			equ		3

parsflag			equ		$06

parsflag2			equ		$07
numop1				equ		0

apdflags			equ		$08
apdable				equ		2		; enable APD counter
apdlock				equ		3		; lock APD
apdwarmstart		equ		4		; APD has expired

onflags				equ		$09
onRunning			equ		3
onInterrupt			equ		4

fmtflags			equ		$0A
fmtExponent			equ		0
fmtEng				equ		1
fmtHex				equ		2
fmtOct				equ		3
fmtBin				equ		4

curflags			equ		$0C
curAble				equ		2
curOn				equ		3
curLock				equ		4

appflags			equ		$0D
appTextSave			equ		1
appAutoScroll		equ		2

indicflags			equ		$12
indicRun			equ		0
indicOnly			equ		2

shiftflags			equ		$12
shift2nd			equ		3
shiftAlpha			equ		4
shiftLwrAlph		equ		5
shiftALock			equ		6

asap_cmd_flag		equ		$16
ex_asap_cmd			equ		7

NewDispf			equ		$17
ProgramExecuting	equ		6

new_grf_flgs		equ		$18
textwrite			equ		6

statflags			equ		$1D
statsvalid			equ		6

anumeditflgs		equ		$1F
ex_asm_module		equ		2

exceptionflg		equ		$23
alt_font			equ		0		; enable user font (5x7)
alt_vfont			equ		1		; enable user font (variable width)
alt_int				equ		2		; enable user interrupt routine
alt_on				equ		3		; enable user on routine
alt_link			equ		4		; enable user link routine
alt_sqrt			equ		5
alt_grphexpr		equ		6		; enable (sqrt)EXPR
alt_off				equ		7		; enable user off routine

exceptionflg2		equ		$24
alt_parse			equ		0		; enable (sqrt)PARSE
alt_form			equ		1		; enable (sqrt)FORM
alt_exec			equ		2		; enable (sqrt)EXEC
alt_home			equ		4		; enable (sqrt)HOME
alt_cmdtok			equ		5		; enable (sqrt)CMDTOK
alt_key				equ		6		; enable (sqrt)KEY
alt_grf				equ		7		; enable (sqrt)GRF

asm_flag1			equ		$28		; unused flags
asm_flag2			equ		$29		; for programmers
asm_flag3			equ		$2A
asm_flag4			equ		$2B
asm_flag5			equ		$2C
asm_flag6			equ		$2D
asm_flag7			equ		$2E

;============ PORTS ============

scrnPort		equ		0			; screen port
kbdPort			equ		1			; keyboard port
intrptPort		equ		3			; interrupt port
iall			equ		11			; interrupts on, lcd on
bPort			equ		7			; link port
D0lD1l			equ		$FC
D0lD1h			equ		$D4
D0hD1l			equ		$E8
D0hD1h			equ		$C0

;========= CALC STATE ==========

sHome			equ		$01			; home screen
sPoly			equ		$02			; polynomial solver
sSimult			equ		$03			; simultaneous equation solver
sCons			equ		$05			; constant editor
sVectr			equ		$06			; vector editor
sMatrx			equ		$07			; matrix editor
sPrgm			equ		$08			; program editor
sInter			equ		$0c			; interpolate/extrapolate editor
sList			equ		$0e			; list/stat editor
sTol			equ		$12			; tolerance
sTable			equ		$19			; table
sTblst			equ		$1A			; table setup
sLink			equ		$1B			; link
sMem			equ		$1D			; reset mem prompt
sDefaults		equ		$1E			; reset defaults prompt
sAll			equ		$1F			; reset all prompt
sRAM			equ		$20			; RAM
sMode			equ		$21			; mode/self test
sDelete			equ		$23			; delete variables
sFunc			equ		$49			; function editor (all modes)
sWind			equ		$4a			; window editor
sGraph			equ		$4c			; graph
sIntCond		equ		$53			; initial conditions prompt
sForm			equ		$62			; graph format
sCatlg			equ		$96			; catlg-vars
sError			equ		$98			; error message

;============ MISC =============

textend			equ		$8E
txasmcmp		equ		$28

lcdmem			equ		$fc00
lcdwide			equ		16
lcdtall			equ		64

;============ GetKy ============

K_NOKEY			equ		00h
K_DOWN			equ		01h
K_LEFT			equ		02h
K_RIGHT			equ		03h
K_UP			equ		04h
K_ENTER			equ		09h
K_PLUS			equ		0Ah
K_MINUS			equ		0Bh
K_STAR			equ		0Ch
K_SLASH			equ		0Dh
K_RAISE			equ		0Eh
K_CLEAR			equ		0Fh
K_SIGN			equ		11h
K_3				equ		12h
K_6				equ		13h
K_9				equ		14h
K_RIGHTPAR		equ		15h
K_TAN			equ		16h
K_CUSTOM		equ		17h
K_DOT			equ		19h
K_2				equ		1Ah
K_5				equ		1Bh
K_8				equ		1Ch
K_LEFTPAR		equ		1Dh
K_COS			equ		1Eh
K_PRGM			equ		1Fh
K_DEL			equ		20h
K_0				equ		21h
K_1				equ		22h
K_4				equ		23h
K_7				equ		24h
K_EE			equ		25h
K_SIN			equ		26h
K_TABLE			equ		27h
K_XVAR			equ		28h
K_ON			equ		29h
K_STO			equ		2Ah
K_COMMA			equ		2Bh
K_SQUARE		equ		2Ch
K_LN			equ		2Dh
K_LOG			equ		2Eh
K_GRAPH			equ		2Fh
K_ALPHA			equ		30h
K_F5			equ		31h
K_F4			equ		32h
K_F3			equ		33h
K_F2			equ		34h
K_F1			equ		35h
K_SECOND		equ		36h
K_EXIT			equ		37h
K_MORE			equ		38h

;============ GetKey ===========

kRight			equ		001h
kLeft			equ		002h
kUp				equ		003h
kDown			equ		004h
kColon			equ		005h
kEnter			equ		006h
kExit			equ		007h
kClear			equ		008h
kDel			equ		009h
kIns			equ		00Ah
kNext			equ		00Bh
kAdd			equ		00Ch
kSub			equ		00Dh
kMul			equ		00Eh
kDiv			equ		00Fh
kExpon			equ		010h
kLParen			equ		011h
kRParen			equ		012h
kLBrack			equ		013h
kRBrack			equ		014h
kEqual			equ		015h
kStore			equ		016h
kRecall			equ		017h
kComma			equ		018h
kAng			equ		019h
kChs			equ		01Ah
kDecPnt			equ		01Bh
k0				equ		01Ch
k1				equ		01Dh
k2				equ		01Eh
k3				equ		01Fh
k4				equ		020h
k5				equ		021h
k6				equ		022h
k7				equ		023h
k8				equ		024h
k9				equ		025h
kEE				equ		026h
kSpace			equ		027h
kCapA			equ		028h
kCapB			equ		029h
kCapC			equ		02Ah
kCapD			equ		02Bh
kCapE			equ		02Ch
kCapF			equ		02Dh
kCapG			equ		02Eh
kCapH			equ		02Fh
kCapI			equ		030h
kCapJ			equ		031h
kCapK			equ		032h
kCapL			equ		033h
kCapM			equ		034h
kCapN			equ		035h
kCapO			equ		036h
kCapP			equ		037h
kCapQ			equ		038h
kCapR			equ		039h
kCapS			equ		03Ah
kCapT			equ		03Bh
kCapU			equ		03Ch
kCapV			equ		03Dh
kCapW			equ		03Eh
kCapX			equ		03Fh
kCapY			equ		040h
kCapZ			equ		041h
ka				equ		042h
kb				equ		043h
kc				equ		044h
kd				equ		045h
ke				equ		046h
kf				equ		047h
kg				equ		048h
kh				equ		049h
ki				equ		04Ah
kj				equ		04Bh
kk				equ		04Ch
kl				equ		04Dh
km				equ		04Eh
kn				equ		04Fh
ko				equ		050h
kp				equ		051h
kq				equ		052h
kr				equ		053h
ks				equ		054h
kt				equ		055h
ku				equ		056h
kv				equ		057h
kw				equ		058h
kx				equ		059h
ky				equ		05Ah
kz				equ		05Bh
kVarx			equ		05Ch
kAns			equ		05Dh
kPi				equ		05Eh
kInv			equ		05Fh
kSin			equ		060h
kASin			equ		061h
kCos			equ		062h
kACos			equ		063h
kTan			equ		064h
kATan			equ		065h
kSquare			equ		066h
kSqrt			equ		067h
kLn				equ		068h
kExp			equ		069h
kLog			equ		06Ah
kALog			equ		06Bh
kMath			equ		06Ch
kCplx			equ		06Dh
kString			equ		06Eh
kTest			equ		06Fh
kConv			equ		070h
kChar			equ		071h
kBase			equ		072h
kCustom			equ		073h
kVars			equ		074h
kCatalog		equ		075h
kQuit			equ		076h
kLastEnt		equ		077h
kLinkIO			equ		078h
kMem			equ		079h
kList			equ		07Ah
kVector			equ		07Bh
kConst			equ		07Ch
kMatrix			equ		07Dh
kPoly			equ		07Eh
kSimult			equ		07Fh
kStat			equ		080h
kGrMenu			equ		081h
kMode			equ		082h
kPrgm			equ		083h
kCalcu			equ		084h
kSolver			equ		085h
kTable			equ		086h
kBOL			equ		087h
kEOL			equ		088h
kCUSTM			equ		093h
kF1				equ		0C2h
kF2				equ		0C3h
kF3				equ		0C4h
kF4				equ		0C5h
kF5				equ		0C6h
kF6				equ		0C7h
kF7				equ		0C8h
kF8				equ		0C9h
kF9				equ		0CAh
kF10			equ		0CBh
kGRAPH			equ		0FFh

;========= Characters =========

Lbinary			equ		001h
Loctal			equ		002h
Ldecimal		equ		003h
Lhexadecimal	equ		004h
Lconvert		equ		005h
LsqUp			equ		006h
LsqDown			equ		007h
Lintegral		equ		008h
Lcross			equ		009h
LhexA			equ		00Ah
LhexB			equ		00Bh
LhexC			equ		00Ch
LhexD			equ		00Dh
LhexE			equ		00Eh
LhexF			equ		00Fh
Lroot			equ		010h
Linverse		equ		011h
Lsquare			equ		012h
Langle			equ		013h
Ldegree			equ		014h
Lradian			equ		015h
Ltranspose		equ		016h
LLE				equ		017h
LNE				equ		018h
LGE				equ		019h
Lneg			equ		01Ah
Lexponent		equ		01Bh
Lstore			equ		01Ch
Lten			equ		01Dh
LupArrow		equ		01Eh
LdownArrow		equ		01Fh
Lspace			equ		020h
Lexclam			equ		021h
Lquote			equ		022h
Lpound			equ		023h
Ldollar			equ		024h
Lpercent		equ		025h
Lampersand		equ		026h
Lapostrophe		equ		027h
LlParen			equ		028h
LrParen			equ		029h
Lasterisk		equ		02Ah
LplusSign		equ		02Bh
Lcomma			equ		02Ch
Ldash			equ		02Dh
Lperiod			equ		02Eh
Lslash			equ		02Fh
L0				equ		030h
L1				equ		031h
L2				equ		032h
L3				equ		033h
L4				equ		034h
L5				equ		035h
L6				equ		036h
L7				equ		037h
L8				equ		038h
L9				equ		039h
Lcolon			equ		03Ah
Lsemicolon		equ		03Bh
LLT				equ		03Ch
LEQ				equ		03Dh
LGT				equ		03Eh
Lquestion		equ		03Fh
LatSign			equ		040h
LcapA			equ		041h
LcapB			equ		042h
LcapC			equ		043h
LcapD			equ		044h
LcapE			equ		045h
LcapF			equ		046h
LcapG			equ		047h
LcapH			equ		048h
LcapI			equ		049h
LcapJ			equ		04Ah
LcapK			equ		04Bh
LcapL			equ		04Ch
LcapM			equ		04Dh
LcapN			equ		04Eh
LcapO			equ		04Fh
LcapP			equ		050h
LcapQ			equ		051h
LcapR			equ		052h
LcapS			equ		053h
LcapT			equ		054h
LcapU			equ		055h
LcapV			equ		056h
LcapW			equ		057h
LcapX			equ		058h
LcapY			equ		059h
LcapZ			equ		05Ah
LlBrack			equ		05Bh
Lbackslash		equ		05Ch
LrBrack			equ		05Dh
Lcaret			equ		05Eh
Lunderscore		equ		05Fh
Lbackquote		equ		060h
La				equ		061h
Lb				equ		062h
Lc				equ		063h
Ld				equ		064h
Le				equ		065h
Lf				equ		066h
Lg				equ		067h
Lh				equ		068h
Li				equ		069h
Lj				equ		06Ah
Lk				equ		06Bh
Ll				equ		06Ch
Lm				equ		06Dh
Ln				equ		06Eh
Lo				equ		06Fh
Lp				equ		070h
Lq				equ		071h
Lr				equ		072h
Ls				equ		073h
Lt				equ		074h
Lu				equ		075h
Lv				equ		076h
Lw				equ		077h
Lx				equ		078h
Ly				equ		079h
Lz				equ		07Ah
LlBrace			equ		07Bh
Lbar			equ		07Ch
LrBrace			equ		07Dh
Ltilde			equ		07Eh
LinvEQ			equ		07Fh
Lsub0			equ		080h
Lsub1			equ		081h
Lsub2			equ		082h
Lsub3			equ		083h
Lsub4			equ		084h
Lsub5			equ		085h
Lsub6			equ		086h
Lsub7			equ		087h
Lsub8			equ		088h
Lsub9			equ		089h
LcapAAcute		equ		08Ah
LcapAGrave		equ		08Bh
LcapACaret		equ		08Ch
LcapADier		equ		08Dh
LaAcute			equ		08Eh
LaGrave			equ		08Fh
LaCaret			equ		090h
LaDier			equ		091h
LcapEAcute		equ		092h
LcapEGrave		equ		093h
LcapECaret		equ		094h
LcapEDier		equ		095h
LeAcute			equ		096h
LeGrave			equ		097h
LeCaret			equ		098h
LeDier			equ		099h
LcapIAcute		equ		09Ah
LcapIGrave		equ		09Bh
LcapICaret		equ		09Ch
LcapIDier		equ		09Dh
LiAcute			equ		09Eh
LiGrave			equ		09Fh
LiCaret			equ		0A0h
LiDier			equ		0A1h
LcapOAcute		equ		0A2h
LcapOGrave		equ		0A3h
LcapOCaret		equ		0A4h
LcapODier		equ		0A5h
LoAcute			equ		0A6h
LoGrave			equ		0A7h
LoCaret			equ		0A8h
LoDier			equ		0A9h
LcapUAcute		equ		0AAh
LcapUGrave		equ		0ABh
LcapUCaret		equ		0ACh
LcapUDier		equ		0ADh
LuAcute			equ		0AEh
LuGrave			equ		0AFh
LuCaret			equ		0B0h
LuDier			equ		0B1h
LcapCCed		equ		0B2h
LcCed			equ		0B3h
LcapNTilde		equ		0B4h
LnTilde			equ		0B5h
Laccent			equ		0B6h
Lgrave			equ		0B7h
Ldieresis		equ		0B8h
LquesDown		equ		0B9h
LexclamDown		equ		0BAh
Lalpha			equ		0BBh
Lbeta			equ		0BCh
Lgamma			equ		0BDh
LcapDelta		equ		0BEh
Ldelta			equ		0BFh
Lepsilon		equ		0C0h
Ltheta			equ		0C1h
Llambda			equ		0C2h
Lmu				equ		0C3h
Lpi				equ		0C4h
Lrho			equ		0C5h
LcapSigma		equ		0C6h
Lsigma			equ		0C7h
Ltau			equ		0C8h
Lphi			equ		0C9h
LcapOmega		equ		0CAh
LxMean			equ		0CBh
LyMean			equ		0CCh
LsupX			equ		0CDh
Lellipsis		equ		0CEh
Lleft			equ		0CFh
Lblock			equ		0D0h
Lper			equ		0D1h
Lhyphen			equ		0D2h
Larea			equ		0D3h
Ltemp			equ		0D4h
Lcube			equ		0D5h
Lenter			equ		0D6h
LblockArrow    	equ		0D7h

.end
