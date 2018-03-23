000100171103     /*CMD ovrdbf file(FLTR902X) tofile(FLTR902L) ovrscope(*calllvl)
000200101130     /*PRM dbgview(*source)
000300171103     /*END dltovr file(FLTR902X) lvl(*)
000400101130     /*END
000500031007      *===============================================================*
000600031009      * FIMS03R * Gestione Piano di Smistamento Picking               *
000700031007      *===============================================================*
000800040621
000900171107      * Per compilare:  OVRDBF FILE(FLTR902X) TOFILE(FLTR902L)?
001000031014
001100031022     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200151104     h alwnull(*inputonly)
001300031007
001400031014      *---------------------------------------------------------------*
001500031014
001600171103     fFLTR902X  if   e           k disk    rename(FLTR9000:FLTR902)
001700040621     f                                     prefix(XR9:3)
001800040623     f                                     usropn
001900040227     fFNFVV03L  if   e           k disk
002000031009     fFNFVV08L  if   e           k disk    rename(FNFVV000:FNFVV08)
002100031009     f                                     prefix(FV8:3)
002200040615     fFNFGV02L  if   e           k disk
002300040621     fFNFGV01L  if   e           k disk    rename(FNFGV000:FNFGV01)
002400120613     fFIDST01L  if   e           k disk
002500031013     fFIAPD01L  if   e           k disk
002600031007     fTNTBE01L  if   e           k disk
002700040302     fTABEL00F  if   e           k disk
002800031007     fAZORG01L  if   e           k disk
002900031007      *
003000171103     fFLTR902L  Uf A e           k disk
003100031007      *
003200031007     fFIMS03D   cf   e             workstn
003300080129     f                                     infds(InfDspF)
003400031007     f                                     sfile(MS03S01:S01nrr)
003500031007     f                                     sfile(MS03S02:S02nrr)
003600031007     f                                     sfile(MS03S03:S01nrr)
003700031007     f                                     sfile(MS03S04:S02nrr)
003800031007
003900031007      *---------------------------------------------------------------*
004000031014
004100031009      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004200031007      *
004300080129     d C_Enter         c                   const(x'F1')
004400080129     d C_F3            c                   const(x'33')
004500080129     d C_F6            c                   const(x'36')
004600080129     d C_F12           c                   const(x'3C')
004700031007     d DigitN          c                   const('0123456789')
004800031023     d C_Default       c                   const('Default:')
004900040625     d C_Postazione    c                   const('Post')
005000031023     d C_Autotr        c                   const('Autotr.')
005100040316     d C_PostazNull    c                   const('XXX')
005200050629     d C_Messaggeria   c                   const('Mes')
005300040309     d C_BaiaNonLetti  c                   const('BAIA  NON  LETTI')
005400060214      * P.O. da gestire come NO VdL
005500060214     d C_PO_a          c                   const(006)
005600040226      * Numero max di record nel subfile
005700040518     d C_MaxRecSf      c                   const(300)
005800040226      * Numero di F.V. nel subfile (deve essere doppio di C_MaxRecSf)
005900040518     d C_MaxRecPf      c                   const(600)
006000040309      * N.B.: la modifica di queste 2 costanti (C_MaxRecSf/C_MaxRecPf)
006100040309      * ¯¯¯¯  comporta l'allineamento nei pgm. richiamati:
006200040309      *       - FNLR02R - per schiera $FV
006300040309      *       - FIMS04R - per schiera $FV
006400040310      *       e negli altri pgm. che elaborano il file FLTR900F:
006500040310      *       - FIMS66R - per schiere $TR9nfv / $TR9rec
006600180110
006700180110       // - ?Attributi di Visualizzazione?
006800180110     d c_DspAtrNorm    c                   const(x'A0')
006900180110     d*//- c_DspAtrRI      c                   const(x'A1')
007000180110     d c_DspAtrRI      c                   const(x'21')
007100180110     d c_DspAtrRED     c                   const(x'A8')
007200180110     d c_DspAtrRI_RED  c                   const(x'A9')
007300180110      *
007400031009      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
007500031007      *
007600031007      * - Messaggi di errore
007700171103     d $Msg            s             78    dim(62) ctdata  perrcd(1)
007800031010      * - P.O. in £1
007900080129     d £1              s              3  0 dim(30)         inz
008000040705     d £1pog           s              3    dim(30)         inz
008100040607      * - P.O. in AZORG
008200080129     d $ORGfil         s              3    dim(500)        inz
008300080129     d $ORGdes         s             20    dim(500)        inz
008400080129     d $ORGntw         s                   dim(500)        inz
008500040611     d                                     like(§OGntw)
008600080129     d $ORGerr         s               n   dim(500)        inz(*off)
008700040607      * - P.O. in AZORG: nr. rec. di utilizzo
008800040607     d $ORGnr          s              3  0 dim(500)        inz
008900031007      * - Parcel Entrance Point ammessi x P.O. in gestione
009000160714     d $MSLpep         s                   dim(80)         inz
009100031111     d                                     like(§MSLpep)
009200040408      * - Tab. "MPT": postazioni "IMA" previste
009300040408     d $MPT            s                   dim(20)         inz
009400040408     d                                     like(TR9bai)
009500040423     d $MPTdes         s                   dim(20)         inz
009600040423     d                                     like(§MPTdes)
009700040408      * - Tab. "MPT": flag obbligatorietà x postazione "IMA"
009800040409     d $MPTobbl        s              1    dim(20)         inz(*off)
009900040409      * - Tab. "MPT": nr. rec. di utilizzo x postazione "IMA"
010000040409     d $MPTnr          s              3  0 dim(20)         inz
010100040409      *
010200031111      * - F.V. immessi a video (da scartare da FNLR02R)
010300031111     d $FV             s                   dim(C_MaxRecPf) inz
010400031111     d                                     like(FVVnfv)
010500040315      *
010600040315      * - SFLrrn di riferimento per le schiere $PEP/$BAU
010700040315     d $CsrRRN         s                   dim(C_MaxRecSf) inz
010800040315     d                                     like(C01csr)
010900031007      *
011000040315      * - Codici Baia (no VdL) immessi nel sfl
011100031111     d $PEP            s                   dim(C_MaxRecSf) inz
011200031111     d                                     like(S1Cpep)
011300031021      *
011400050629      * - Codice (prima) Postazione x Baia (no VdL) immessa nel sfl
011500031111     d $PEPbai         s                   dim(C_MaxRecSf) inz
011600031111     d                                     like(S1Cbai)
011700031007      *
011800040315      * - Codici Postazione (VdL & non) immessi nel sfl
011900031111     d $BAI            s                   dim(C_MaxRecSf) inz
012000031111     d                                     like(S1Cbai)
012100040315      * - Numero record del sfl x Postazione (VdL & non)
012200031111     d $BAInr          s              3  0 dim(C_MaxRecSf) inz
012300050629      * - Codice Baia di Uscita (sì VdL) corrispondente alla Postazione
012400050629     d $BAIbau         s                   dim(C_MaxRecSf) inz
012500050629     d                                     like(S2Cpep)
012600031007      *
012700040315      * - Codici Baia di Uscita (sì VdL) immessi nel sfl
012800031111     d $BAU            s                   dim(C_MaxRecSf) inz
012900031111     d                                     like(S2Cpep)
013000040305      *
013100050629      * - Codice (prima) Postazione x Baia (sì VdL) immessa nel sfl
013200040305     d $BAUbai         s                   dim(C_MaxRecSf) inz
013300040305     d                                     like(S2Cbai)
013400040607      *
013500040607      * - Flag Postazione P.O. (sì VdL) x Baia immessa nel sfl
013600040607      *   (in corrispondenza alla schiera $BAUbai)
013700040607     d $PosPO          s              1    dim(C_MaxRecSf) inz(*off)
013800040628     d $PosPOdpd       s              1    dim(C_MaxRecSf) inz(*off)
013900061011      *
014000061011      * - Memo Baie e Postazioni (sì VdL) immesse nel sfl
014100061011      *   (per ctrl P.O. x baia)
014200061011     d $PObau          s                   dim(C_MaxRecSf) inz
014300061011     d                                     like(S2Cpep)
014400061011     d $POpos          s                   dim(C_MaxRecSf) inz
014500061011     d                                     like(S2Cbai)
014600161212      *
014700161212      * -?Memo Filiali (Postazioni - sì VdL) immesse nel sfl?
014800161212      *  ?(per ctrl Zone, alias Turno)?
014900161212     d sk_Fil          s                   dim(c_MaxRecSf) inz
015000161212     d                                     like(S2Cbai)
015100161212     d sk_Rrn          s                   dim(c_MaxRecSf) inz
015200161212     d                                     like(S02nrr)
015300040615      *
015400040701      * - Memo F.V. x P.O. DPD (sì VdL) x Baia immessa nel sfl
015500040701     d $FVxPOa         s                   dim(C_MaxRecSf) inz
015600040615     d                                     like(TR9bai)
015700040701     d $FVxPOn         s             +2    dim(C_MaxRecSf) inz
015800040615     d                                     like(FGVnfv)
015900040701     d $FVxPOd         s                   dim(C_MaxRecSf) inz
016000040615     d                                     like(FGVdfv)
016100031007      *
016200040315      * - Codici Baia  Non Letti (sì VdL) immessi nel sfl
016300031111     d $BNL            s                   dim(C_MaxRecSf) inz
016400031111     d                                     like(S2Cbnl)
016500040315      * - Numero record del sfl x Baia Non Letti (sì VdL)
016600031111     d $BNLnr          s              3  0 dim(C_MaxRecSf) inz
016700031030      *
016800171107      * - Numeri F.V. immessi con l'altro "Tipo Piano Smistamento"
016900171107      *   (previsto ora SOLO per la fil. 006-PD)
017000171107     d sk_NFV2         s                   dim(C_MaxRecPf) inz
017100171107     d                                     like(TR9nfv)
017200031030      * - Numeri F.V. immessi nel sfl
017300031111     d $NFV            s                   dim(C_MaxRecPf) inz
017400031111     d                                     like(TR9nfv)
017500040628      * - Baia/Postazione nel sfl x F.V.
017600040628     d $NFVbp          s                   dim(C_MaxRecPf) inz
017700040628     d                                     like(ds_BaiaPost)
017800031030      * - Numero record del sfl x F.V
017900031111     d $NFVnr          s              3  0 dim(C_MaxRecPf) inz
018000031009      * - - codici autotrasportatori
018100040519     d $MEMat          s                   dim(C_MaxRecSf) inz
018200031111     d                                     like(TR9pdc)
018300040519      * - - codici baia/postazione x autotrasportatore
018400040519     d $MEMbp          s                   dim(C_MaxRecSf) inz
018500040519     d                                     like(ds_BaiaPost)
018600031009      * - - data/numero ultimi F.V. x autotrasportatore
018700040519     d $MEMfv1         s             15  0 dim(C_MaxRecSf) inz(*loval)
018800040519     d $MEMfv2         s             15  0 dim(C_MaxRecSf) inz(*loval)
018900031022      * - - flag prestazione autotrasportatore (M-mattino P-pomeriggio)
019000040519     d $MEMmp1         s              1    dim(C_MaxRecSf) inz
019100040519     d $MEMmp2         s              1    dim(C_MaxRecSf) inz
019200050701      * - - tipo zona cons. x F.V./autotrasp. (P-Parcel M-Messaggerie)
019300050701     d $MEMpm1         s              1    dim(C_MaxRecSf) inz
019400050701     d $MEMpm2         s              1    dim(C_MaxRecSf) inz
019500031111      * - - numero F.V. x autotrasportatore
019600031111      *     per evidenziare "più F.V. per autotrasportatore"
019700040519     d $MEMxf          s              3  0 dim(C_MaxRecSf) inz
019800040630      *
019900040630      * - Schiera P.O. x F.V. da FNFGV
020000040630     d xFGVffv         s              3    dim(80)         inz
020100031007      *
020200031007      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
020300031007      *
020400031007     d KPJBA         e ds
020500080129      *
020600031007      * - Parametri x Controllo profilo utenti
020700080122     d TIBS34ds      e ds                  inz
020800031007      * - Ds di riferimento al file esterno AZUTE00F
020900080122     d AZUTEds       e ds                  extname(AZUTE00F)
021000031007      * - Ds per dati organigramma
021100080122     d dDatiUte      e ds
021200040623      * - Strutture dati "di comodo"
021300040623     d FLTR9ds       e ds                  extname(FLTR900F) inz
021400040623     d FLXR9ds       e ds                  extname(FLTR900F) inz
021500040623     d                                     prefix(XR9:3)
021600031010      * - Parametri x Caricamento £1
021700080122     d TRUL06ds      e ds                  inz
021800080122     d   Lin                   1     90  0 dim(30)
021900031010      * - Parametri x reperimento terminal di linea
022000040628     d FNLV55ds      e ds                  inz
022100040611      * - Ds descrizione 143 dell'organigramma
022200040611     d OG143         e ds                  inz
022300031007      * - Ds descrizione 150 dell'organigramma
022400031007     d OG150         e ds                  inz
022500050614      * - Ds lettura campo APDFLR di FIAPD00F
022600050614     d dAPDflr       e ds                  inz
022700050614      * - Ds lettura campo FVVSPN di FNFVV00F
022800050614     d dFVVspn       e ds                  inz
022900031015      * - Parametri standard nuovi x SRCFNLS
023000031015     d DSLR01        e ds                  inz
023100031015     d   DLRric      e                     inz('S')
023200031113     d   DLRnpg      e                     inz(4)
023300031015     d   DLRtfv      e                     inz('2')
023400031113     d   DLRgal      e                     inz(31129999)
023500031113     d   DLRaal      e                     inz(99991231)
023600031007      * - Parametri x Interrogazione tabelle
023700080122     d TIBS02ds      e ds                  inz
023800031007     d   T02mod      e                     inz('C')
023900031007     d   T02cod      e                     inz('MFP')
024000031007      * - Tab. "MFP"
024100031007     d dMFP          e ds                  inz
024200031007     d   §MFPap                1     10    dim(10)
024300040408      * - Tab. "MPT"
024400040408     d dMPT          e ds                  inz
024500031007      * - Tab. "MSL"
024600031007     d dMSL          e ds                  inz
024700040302      * - Tab. "7N"
024800040302     d ds7N          e ds                  inz
024900171205      * - ?Tab. "VPO" / "DECO"?
025000171205      *   ?(vedi progetto 898 - Doppio picking in contemporanea)?
025100171205     d dVPOdeco      e ds                  qualified    inz
025200171205     d   §VPO2PkPd   e                     inz(*hival)
025300031007      * - Parametri x interrogazione autotrasportatori
025400031007     d FNLV24ds      e ds                  inz
025500031007     d   D24tip      e                     inz('A')
025600031007     d   D24flg      e                     inz('R')
025700031029      * - Parametri x lista Piano di Smistamento per Picking
025800031029     d FIMS04ds      e ds                  inz
025900031007      *
026000031007     d Status         sds           333
026100031007     d   SDSpgm          *proc
026200080129      *
026300080129     d InfDspF         ds           512
026400080129     d  $Tasto               369    369                                         AID byte
026500031009      *
026600031022     d DS_FV           ds            15    inz
026700031009     d   dsFVdfv                      8s 0 inz
026800031009     d   dsFVnfv                      7s 0 inz
026900040519      *
027000040519     d DS_BaiaPost     ds            13    inz
027100040519     d   dsBPbai                     -4    like(TR9pep) inz
027200040519     d   dsBPpos                           like(TR9bai) inz
027300031007      *
027400031007     d WLBda8          ds                  inz
027500031007     d   G08dat                       8  0 inz
027600031007     d   G08inv                       8  0 inz
027700031106     d   G08err                       1    inz('3')
027800031007     d   G08tgi                       5  0 inz
027900031021      *
028000031021     d DSdata8         ds             8    inz
028100031021     d   DSdt8gg                      2  0 inz
028200031021     d   DSdt8mm                      2  0 inz
028300031021     d   DSdt8aa                      4  0 inz
028400031021     d DSdata6         ds             6    inz
028500031021     d   DSdt6gg                      2  0 inz
028600031021     d   DSdt6mm                      2  0 inz
028700031021     d   DSdt6aa                      2  0 inz
028800040611      *
028900130506     d fnlst3ds        ds                  inz
029000040611     d   DLSfl1                       1    inz('R')
029100040611     d   DLSfl2                       1    inz
029200040611     d   DLSnfv                       5p 0 inz
029300040611     d   DLSdti                       9p 0 inz
029400040611     d   DLSdtf                       9p 0 inz
029500040611     d   DLStfv                       1    inz('2')
029600040611     d   DLSfv                        5    inz      dim(45)
029700040611     d   DLSfgs                       3  0 inz
029800151106
029900151106       // -?Dati estratti via SQL?
030000151106     d ds_NFVxDFV      ds                  inz  qualified
030100151106     d   TR9fgs                            inz  like(TR9fgs)
030200151106     d   TR9npga                           inz  like(TR9npgA)
030300151106     d   TR9nfva                           inz  like(TR9nfvA)
030400151109     d   TR9fl2                            inz  like(TR9fl2)
030500151106     d   FVVdfv                            inz  like(FVVdfv)
030600151106     d   wNFVxDFV                     5  0 inz
030700031007      *
030800031007      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
030900031007      *
031000031007      * - Flags
031100031007     d $Fine           s              1    inz(*off)
031200031007     d $InzD01         s              1    inz(*on)
031300031007     d $InzS01         s              1    inz(*on)
031400031007     d $InzS02         s              1    inz(*on)
031500031023     d $EoF            s              1    inz(*off)
031600031007     d $EndSFL         s              1    inz(*off)
031700031007     d $Default        s              1    inz(*off)
031800031009     d $NewFVa         s              1    inz(*off)
031900040701     d $DltPS          s              1    inz(*off)
032000031007     d $VDL            s              1    inz(*off)
032100040408     d $IMA            s              1    inz(*off)
032200040423     d $IMAdft         s              1    inz(*off)
032300040607     d $PO             s              1    inz(*off)
032400040611     d $POdpd          s              1    inz(*off)
032500080129     d $POerr          s              1    inz(*off)
032600040607     d $POdft          s              1    inz(*off)
032700040423     d $Hidden         s              1    inz(*off)
032800031023     d $Found          s              1    inz(*off)
032900171102     d $First          s               n   inz(*on)
033000050629     d $MsgBAIbau      s              1    inz(*off)
033100031112     d WinKH           s              1    inz(*off)
033200031020      *
033300031007     d $Video          s              1    inz('1')
033400031007      *
033500031007      * - Indici di schiera
033600031007     d XX              s              3  0 inz
033700031107     d YY              s              3  0 inz
033800040607     d XY              s              3  0 inz
033900040630     d WX              s              3  0 inz
034000080129     d WY              s              3  0 inz
034100031007      *
034200031007      * - Variabili riferite al data base o al display file
034300031007     d SAVpog          s                   like(V1Cpog) inz(*loval)
034400031106     d SAVpep          s                   like(S1Cpep) inz(*loval)
034500031107     d SAVbau          s                   like(S2Cpep) inz(*loval)
034600031022     d SAVnfvIMA       s                   like(S1Cnfv) inz(*zeros)
034700031022     d SAVnfv          s                   like(S1Cnfv) inz(*zeros)
034800031022     d SAVdfva         s                   like(FVVdfv) inz
034900031022     d SAVpdr          s                   like(DSTpdr) inz
035000040305     d SAVprg          s                   like(TR9prg) inz
035100031007     d S01nrr          s                   like(C01rcd) inz
035200031007     d S02nrr          s                   like(C02rcd) inz
035300031007     d MAXnrr          s                   like(C01rcd) inz
035400090203     d Free_Rec_Sfl    s                   like(C01rcd) inz
035500031007     d Wpdr            s                   like(DSTpdr) inz
035600040621     d Wdfv            s                   like(FVVdfv) inz
035700031022     d Wnfv            s                   like(S1Cnfv) inz
035800031030     d Wnfv7           s                   like(TR9nfv) inz
035900040423     d Wpost           s                   like(TR9bai) inz
036000031024     d Wast            s                   like(S1Cast) inz
036100040611     d DLSpo           s                   like(TR9fgs) inz
036200040630     d DLSpo2          s                   like(TR9fgs) inz
036300031007      *
036400031007      * - Variabili definite a programma
036500031112     d n08             s              8  0                                      *numerico 8
036600031112     d n14             s             14  0                                      *numerico 14
036700031007     d Wdate           s              8  0                                      *data corrente a/m/g
036800031022     d wNFVseq         s              1  0 inz                                  *sequenza F.V.
036900061011     d wFL3            s              2  0 inz
037000031009     d w0150           s             15  0 inz
037100031007     d Wcampo          s             10    inz
037200031023     d w002a           s              2    inz
037300040705     d w003a           s              3    inz
037400031010     d w014a           s             14    inz
037500151106     d wDate_Eur       s               d   inz   datfmt(*EUR)
037600151106
037700151106       // -?Stringhe SQL da eseguire?
037800151106     d wSQL            s          32740    inz   varying
037900151106
038000151106       //--------------------------------------------------------------
038100171107       //? Definizione prototipi procedure.                            ?
038200151106       //--------------------------------------------------------------
038300151106
038400151106       // -?Parametri API QCAPCMD (Process Commands)?
038500151106     d*// Qcmd            s           2048    inz  varying
038600151106      /copy qSysInc/qRpgleSrc,QCAPCMD
038700151106       // -?API QCAPCMD (Process Commands)?
038800151106      /copy gaitrasrc/srcProtoPR,QCAPCMD
038900151106
039000151106       // -?Parametri gestione errori API.?
039100151106      /copy qsysinc/qrpglesrc,QUSEC
039200171107
039300171107       //--------------------------------------------------------------
039400171107       //? Definizione key-list.                                       ?
039500171107       //--------------------------------------------------------------
039600171107
039700171107       // -?File FLTR902L?
039800171107     d keyFLTR902    e ds                  extname( FLTR902L : *key )
039900171107     d                                     prefix(k_)   inz
040000031007
040100031007      *---------------------------------------------------------------*
040200031007      *  RIEPILOGO INDICATORI                                         *
040300031007      *---------------------------------------------------------------*
040400031010      * 01    - Visualizza "Piano di default"  come sotto-titolo      *
040500031010      * 02    - Visualizza "Piano giornaliero" come sotto-titolo      *
040600040705      * 03    - Visualizza richiesta per "Piano DPD" in D01           *
040700050615      * 04    - Visualizza dati per gestione picking MESSAGGERIA.     *
040800171102      * 05    - Visualizza richiesta per "Tipo Piano Smistamento" (PD)*
040900031007      * 10    - Comodo                                                *
041000031007      * 28    - Emette il messaggio di errore a video                 *
041100031007      * 30    - Ripulisce il subfile                                  *
041200031007      * 31    - NON emette il subfile                                 *
041300031007      * 32    - Record modificato nel subfile (sflnxtchg)             *
041400031007      * 33    - Fine dati nel subfile         (sflend)                *
041500040702      * 39    - Codice P.O. in gestione   protetto     (PR)           *
041600040702      *        ?se?livello P.O. utente = '2'                          *
041700031007      * 41    - P.O. in gestione errato                               *
041800031007      * 42    - Categoria foglio errato                               *
041900031007      * 43    - Numero F.V. errato                                    *
042000040705      * 44    - P.O. non abilitato al piano smistamento DPD           *
042100171102      * 45    - Tipo Piano Smistamento errato                         *
042200031007      * 51    - Codice postazione / baia-uscita  errato               *
042300031022      * 52    - Descrizione mancante (NON usato)                      *
042400031007      * 53    - Codice baia  errato                                   *
042500031022      * 54    - Numero foglio viaggio  errato                         *
042600031007      * 55    - Codice autotrasportatore  errato                      *
042700031022      * 56    - 2° Numero foglio viaggio  errato                      *
042800040304      * 57    - Flag "baia non letti"  errato                         *
042900161212      * 58    - Zona (cioè Turno) per Baia errata                     *
043000040623      * 80    - Tutti i campi I/O del sfl protetti     (PR)           *
043100040623      *         descr. autotrasportatore  sottolineato (UL)           *
043200040309      *         (baia "non letti" in piano giornaliero per VdL)       *
043300040623      * 85    - Codice autotrasportatore  in evidenza  (RI)           *
043400031023      *         (variato in piano giornaliero rispetto al default)    *
043500040628      *         (variata postazione in piano giornaliero rispetto al  *
043600040628      *         default)                                              *
043700040623      * 86    - Descr. autotrasportatore  in evidenza  (HI)           *
043800040628      *         (postazione IMx o P.O.)                               *
043900050629      * 87    - Flag Messaggeria attivo                (RI)           *
044000050629      *         (Autotrasportatore Messaggeria - in default;          *
044100050629      *          1°  F.V.          Messaggeria - in smistamento)      *
044200050614      * 88    - Flag Messaggerie attivo                (RI)           *
044300050629      *         (2°  F.V.          Messaggeria - in smistamento)      *
044400031007      * 90    - Generico di errore                                    *
044500031007      *---------------------------------------------------------------*
044600031007
044700031007     c     *Entry        plist
044800031007     c                   parm                    KPJBA
044900031007      *
045000031007      * Operazioni Iniziali
045100040705     c                   exsr      RoutInz
045200031007      *
045300031007      * Gestione Video
045400031007do  1c                   DOW       $Fine = *off
045500031007      * - Filtro di lancio
045600031007cas 2c     $Video        caseq     '1'           GesD01
045700031007      * - Window con messaggio
045800031007cas 2c     $Video        caseq     'W'           GesW01
045900031106      * - SubFile (Default o Giornaliero, con VDL o senza VDL)
046000031106cas 2c                   cas                     GesSFL
046100031007e   2c                   endcs
046200031007e   1c                   ENDDO
046300031007      *
046400031007      * Fine
046500171103     c                   close     FLTR902X
046600171103     c                   eval      Qcmd = 'DLTOVR  FILE(FLTR902X)'
046700040623     c                   call      'QCMDEXC'
046800040623     c                   parm                    Qcmd
046900040623     c                   parm      50            Qlen
047000040623      *
047100031010     c                   clear                   TRUL06ds
047200031010     c                   eval      D06tla = 'C'
047300031010     c                   movel(p)  TRUL06ds      KPJBU
047400031010     c                   call      'TRUL06R'
047500031010     c                   parm                    KPJBA
047600031010     c                   clear                   FNLV55ds
047700031010     c                   eval      D55tla = 'C'
047800031010     c                   call      'FNLV55R'
047900031010     c                   parm                    FNLV55ds
048000031010     c                   reset                   TIBS02ds
048100031010     c                   eval      T02tla = 'C'
048200031010     c                   call      'TIBS02R'
048300031010     c                   parm                    KPJBA
048400031010     c                   parm                    TIBS02ds
048500031007     c                   movel     *on           *inLR
048600031007      *
048700031007      *---------------------------------------------------------------*
048800031007      * Operazioni Iniziali                                           *
048900031007      *---------------------------------------------------------------*
049000040705     c     RoutInz       BEGSR
049100031007      *
049200031007      * Definizione Key-List
049300171103      * - FLTR902L
049400171103     c     K04FLTR9      klist
049500031009     c                   kfld                    TR9fgs
049600031009     c                   kfld                    TR9npgA
049700031009     c                   kfld                    TR9nfvA
049800171103     c                   kfld                    TR9tps
049900171103     c     K06TR902      klist
050000040623     c                   kfld                    TR9fgs
050100040623     c                   kfld                    XR9npgA
050200040623     c                   kfld                    XR9nfvA
050300171103     c                   kfld                    TR9tps
050400040623     c                   kfld                    TR9pep
050500040621     c                   kfld                    XR9prg
050600171103     c     K07TR902      klist
050700040623     c                   kfld                    TR9fgs
050800040623     c                   kfld                    XR9npgA
050900040623     c                   kfld                    XR9nfvA
051000171103     c                   kfld                    TR9tps
051100040623     c                   kfld                    TR9pep
051200040621     c                   kfld                    XR9prg
051300040621     c                   kfld                    TR9bai
051400031007      * - FIAPD01L
051500031007     c     K02FIAPD      klist
051600031007     c                   kfld                    APDtip
051700031007     c                   kfld                    APDpdr
051800031007     c                   eval      APDtip = 'A'
051900031007      * - FNFVV03L
052000031007     c     K03FNFVV      klist
052100031007     c                   kfld                    FVVfgs
052200031007     c                   kfld                    FVVnpg
052300031007     c                   kfld                    FVVnfv
052400031007     c     K02FNFVV      klist
052500031007     c                   kfld                    FVVfgs
052600031007     c                   kfld                    FVVnpg
052700031009      * - FNFVV08L
052800031009     c     K03FVV08      klist
052900031010     c                   kfld                    FV8fgs
053000031010     c                   kfld                    FV8npg
053100031010     c                   kfld                    FV8dfv
053200031010     c     K02FVV08      klist
053300031010     c                   kfld                    FV8fgs
053400031010     c                   kfld                    FV8npg
053500120613      * - FIDST01L
053600120613     c     K03FIDST      klist
053700120613     c                   kfld                    DSTnpg
053800120613     c                   kfld                    DSTnfv
053900120613     c                   kfld                    DSTfgs
054000040621      * - FNFGV02L
054100040621     c     K03FGV02      klist
054200040621     c                   kfld                    FGVlnp
054300040621     c                   kfld                    FGVdfv
054400040621     c                   kfld                    FGVnfv
054500040621      * - FNFGV01L
054600040621     c     K02FGV01      klist
054700040621     c                   kfld                    FGVnfv
054800040621     c                   kfld                    FGVlnp
054900031007      * - TNTBE01L
055000031007     c     K02TNTBE      klist
055100031007     c                   kfld                    TBEcod
055200031007     c                   kfld                    TBEke1
055300040302      * - TABEL00F
055400040227     c     KTAB          klist
055500040227     c                   kfld                    TBLkut
055600040227     c                   kfld                    TBLcod
055700040227     c                   kfld                    TBLkey
055800040227     c                   eval      TBLkut = 1
055900031007      *
056000031007      * Reperisco dati job
056100031007     c                   exsr      DatiJob
056200031008     c                   movel     SDSpgm        V1Tpgm
056300031007      *
056400031007      * Reperisco la data corrente
056500031007     c                   time                    n14                            *Ora (6)+ Data (8)
056600031007     c                   move      n14           n08                            *Data (8) in g/m/a
056700031106     c                   clear                   WLBda8
056800031007     c                   z-add     n08           G08dat
056900031106     c                   eval      G08err = '0'
057000031007     c                   call      'XSRDA8'
057100031007     c                   parm                    WLBda8
057200031007     c                   z-add     G08inv        Wdate                          *Data corrente a/m/g
057300031010      *
057400031010      * Carico schiera P.O. gestiti (£1)
057500031010     c                   clear                   TRUL06ds
057600031010     c                   movel     '£1'          D06cod
057700040701     c                   movel     DUTpou        D06key
057800031010     c                   movel(p)  TRUL06ds      KPJBU
057900031010     c                   call      'TRUL06R'
058000031010     c                   parm                    KPJBA
058100031010     c                   movel     KPJBU         TRUL06ds
058200040701     c                   movea     Lin           £1
058300040607      *
058400040607      * Carico schiera di tutti i P.O.
058500040607     c                   clear                   xx
058600040607     c                   clear                   $ORGfil
058700040607     c                   clear                   $ORGdes
058800040607     c*** qui non serve: clear                   $ORGnr
058900080129     c*** qui non serve: reset                   $ORGerr
059000040607     c     *loval        setll     AZORG
059100040607     c                   read      AZORG
059200040607do  1c                   dow       NOT %eof(AZORG01L)
059300040607if  2c                   if        ORGfva = *blanks
059400040607     c                   add       1             xx
059500040607     c                   move      ORGfil        $ORGfil(xx)
059600040607     c                   movel     ORGdes        $ORGdes(xx)
059700040611     c                   movel     ORGde3        Og143
059800040611     c                   movel     §OGntw        $ORGntw(xx)
059900040607e   2c                   endif
060000040607     c                   read      AZORG
060100040607e   1c                   enddo
060200171205      *
060300171205      * - ?Reperimento data attivazione del "Doppio Picking" per fil. 006-PD?
060400171205     c                   reset                   dVPOdeco
060500171205     c                   eval      TBEcod = 'VPO'
060600171205     c                   eval      TBEke1 = 'DECO'
060700171205     c     K02TNTBE      chain     TNTBE000
060800171205if  1c                   if        %found(TNTBE01L)
060900171205     c                   eval      dVPOdeco = TBEuni
061000171205e   1c                   endif
061100040623      *
061200040623      * Override
061300171103     c                   eval      Qcmd = 'OVRDBF  FILE(FLTR902X)'
061400171103     c                                  +      ' TOFILE(FLTR902L)'
061500040623     c                   call      'QCMDEXC'
061600040623     c                   parm                    Qcmd             50
061700040623     c                   parm      50            Qlen             15 5
061800171103     c                   open      FLTR902X
061900031007      *
062000031007     c                   ENDSR
062100031007      *
062200031007      *---------------------------------------------------------------*
062300031007      * Reperimento Dati del job (Utente/Operativi)                   *
062400031007      *---------------------------------------------------------------*
062500031007     c     DatiJob       BEGSR
062600031007      *
062700031007     c     *dtaara       define    §azute        azuteds
062800031007     c     *dtaara       define    §datiute      ddatiute
062900031007      *
063000031007     c                   in(E)     *dtaara
063100031007     c                   IF        %ERROR or RSUT = *blanks
063200031007     c                   clear                   Tibs34Ds
063300031007     c                   call      'TIBS34R'
063400031007     c                   parm                    Tibs34Ds
063500031007     c                   in        *dtaara
063600031007     c                   ENDIF
063700031007      *
063800031007     c                   ENDSR
063900031007      *
064000031007      *---------------------------------------------------------------*
064100031007      * Gestione videata D01                                          *
064200031007      *---------------------------------------------------------------*
064300031007     c     GesD01        BEGSR
064400031007      *
064500031007      * Inizializzo la videata
064600031007if  1c                   if        $InzD01  = *on
064700031007     c                   exsr      InzD01
064800031007     c                   movel     *off          $InzD01
064900031007e   1c                   endif
065000031014      * Reimposto il numero F.V. a *blank se *zero (numerico editato)
065100031014if  2c                   if        V1Cnfv =  *zeros
065200031014     c                   eval      V1Cnfv =  *blanks
065300031014e   2c                   endif
065400031007      *
065500031007      * Scrivo la testata e la riga tasti funzionali abilitati
065600170117if  1c*//                if        NOT *in28
065700031007     c                   write     MS03T01
065800031007     c                   write     MS03Z01
065900170117e   1c*//                endif
066000031007      * Emetto la videata
066100031007     c                   exfmt     MS03D01
066200031007     c                   setoff                                       28  90
066300031007     c                   clear                   V1Dmsg
066400031007      *
066500031007sel 1c                   select
066600031007      * F3=Fine
066700031007w   1c                   when      *inKC
066800031007     c                   exsr      F03D01
066900060110     c                   leavesr
067000031007      *
067100031007e   1c                   endsl
067200031007      *
067300031007      * Controllo dati immessi a video
067400031007     c                   exsr      CtrD01
067500060110     c                   if        *in90
067600060110     c                   leavesr
067700060110     c                   endif
067800031009      *
067900031009      * Inizializzazione schiere, flag, variabili, ecc.
068000031009     c                   exsr      InzALL_1
068100031106     c                   eval      $Default = (V1Cnfv = *zeros)
068200040705      *
068300040705      * Testo eventuali sostituzioni tra piano std e piano dpd
068400040705if  1c                   if            $Default = *off
068500040705     c                             and $NewFVa  = *off
068600040705     c                   exsr      Check_PS
068700040705if  2c                   if        *inKC  or  *inKL
068800060110     c                   leavesr
068900040705e   2c                   endif
069000040705e   1c                   endif
069100031009      *
069200031009      * Caricamento dati da tab. MSL
069300031009     c                   exsr      Tab_MSL
069400031007      *
069500031007      * Controllo se P.O. in gestione ha VDL
069600031007     c                   exsr      Ctrl_VDL
069700171123      *
069800171123      * - Caricamento distinte eventualmente già inserite dalla
069900171123      *   fil. 006 per l'altro "Tipo Piano Smistamento"
070000171123     c                   clear                   sk_NFV2
070100171123     c                   clear                   wy
070200171123if  1c                   if        V1Cpog   = c_PO_a  and
070300171123     c                             $Default = *off
070400171123     c                   exsr      sr_CarDist_2
070500171123e   1c                   endif
070600031009      *
070700031110      * Se piano giornaliero:
070800031106      * Intabellamento dati di default   &
070900031106      * Memorizzazione dati per il reperimento dell'ultimo foglio
071000031106      *   consegne per autotrasportatore
071100040309if  1c                   if        $Default = *off
071200031020     c                   exsr      Memo_DFT
071300031009e   1c                   endif
071400031010      *
071500031106      * Passaggio alla videata successiva
071600031010if  1c                   if        $Default = *on
071700031010     c                   movea     '10'          *in(01)
071800031010x   1c                   else
071900031010     c                   movea     '01'          *in(01)
072000031010e   1c                   endif
072100031007      *
072200031007if  1c                   if        $VDL     = *off
072300031007      *
072400031007     c                   eval      $InzS01  = *on
072500031007if  2c                   if        $Default = *on
072600031007     c                   eval      $Video   = '2'
072700031007x   2c                   else
072800031007     c                   eval      $Video   = '4'
072900031007e   2c                   endif
073000031007      *
073100031007x   1c                   else
073200031007      *
073300031007     c                   eval      $InzS02  = *on
073400031007if  2c                   if        $Default = *on
073500031007     c                   eval      $Video   = '3'
073600031007x   2c                   else
073700031007     c                   eval      $Video   = '5'
073800031007e   2c                   endif
073900031007      *
074000031007e   1c                   endif
074100031007      *
074200060110     c                   ENDSR
074300031007      *
074400031007      *---------------------------------------------------------------*
074500031007      * Inizializzazione videata D01                                  *
074600031007      *---------------------------------------------------------------*
074700031007     c     InzD01        BEGSR
074800031007      *
074900031007      * Imposto i dati di default
075000031010if  1c                   if        V1Cpog = *zeros
075100031007     c                   eval      V1Cpog = DUTpou
075200031007     c                   eval      V1Cnpg = 2
075300031010e   1c                   endif
075400031010     c                   clear                   V1Cnfv
075500031014     c                   clear                   V1Dnfv
075600031010     c                   clear                   V1Ddfv
075700031007      *
075800031007     c                   clear                   V1Dmsg
075900031007     c                   setoff                                       28  90
076000031007     c                   movea     *zeros        *in(41)
076100031010      *
076200031010      * Posiziono il cursore sul campo "numero F.V."
076300031010     c                   eval      *in43  = *on
076400040702      *
076500040702      * Proteggo il campo "P.O. in gestione" se P.O. utente di 2° livello
076600040705     c                   eval      *in39  = (DUTlpo   = '2')
076700040705      *
076800040705      * Controllo se P.O. abilitato a piano DPD
076900040705     c                   eval      *in03  = *off
077000040705     c                   eval      xx     = *zeros
077100040705do  1c                   dow       xx     < %elem(£1)
077200040705     c                   eval      xx     = xx + 1
077300040705if  2c                   if        £1(xx) = *zeros
077400040705     c                   leave
077500040705e   2c                   endif
077600040705     c                   eval      yy     = 1
077700040705     c                   eval      *in10  = *off
077800040705     c                   move      £1(xx)        w003a
077900040705     c     w003a         lookup    $ORGfil(yy)                            10
078000040705if  2c                   if            *in10       =  *on
078100040705     c                             and $ORGntw(yy) =  'DPD'
078200040705     c                   eval      *in03  = *on
078300040705     c                   leave
078400040705e   2c                   endif
078500040705e   1c                   enddo
078600070530if  1c                   if            V1Cpog      = 049
078700070530     c                             and *in03       = *on
078800070530     c                   eval      V1Cdpd = 'S'
078900070530e   1c                   endif
079000171102      *
079100171102      * - ?Controllo se Filiale Gestione = 006-PD?
079200171102      *   ?(per abilitazione richiesta "Tipo Piano Smistamento")?
079300171205     c*//                eval      *in05  = (V1Cpog   = c_PO_a)
079400171205     c                   eval      *in05  = (V1Cpog   = c_PO_a  and
079500171205     c                                       wDate   >= dVPOdeco.§VPO2PkPd)
079600171102      *
079700171102      * - ?Impostazione "Tipo Piano Smistamento" di default?
079800171102      *   ?(sempre e comunque - anche se gestito solo per 006-PD)?
079900171103sel 1c                   select
080000171103w   1c                   when      V1Cpog =  SAVpog
080100171103w   1c                   when      V1Cpog =  c_PO_a
080200171103     c                   eval      V1Ctps = 'R'
080300171103x   1c                   other
080400171103     c                   clear                   V1Ctps
080500171103e   1c                   endsl
080600171102     c                   exsr      Ctrl_VDL
080700171102      * - ?Decodifica filiale?
080800171102      *   ?(per evitare inutili riemissioni)?
080900171102     c                   exsr      DecodPOG
081000171102if  1c                   if        *in90   = *off
081100171102     c                   eval      SAVpog  = V1Cpog
081200171102x   1c                   else
081300171102     c                   reset                   SAVpog
081400171102e   1c                   endif
081500031007      *
081600031007     c                   ENDSR
081700031007      *
081800031007      *---------------------------------------------------------------*
081900031007      * Gestione tasto funzionale F03 da videata D01                  *
082000031007      *---------------------------------------------------------------*
082100031007     c     F03D01        BEGSR
082200031007      *
082300031007      * Chiudo il programma
082400031007     c                   eval      $Fine = *on
082500031007      *
082600031007     c                   ENDSR
082700031007      *
082800031007      *---------------------------------------------------------------*
082900031007      * Controllo dati immessi in videata D01                         *
083000031007      *---------------------------------------------------------------*
083100031007     c     CtrD01        BEGSR
083200031007      *
083300031113     c                   reset                   $NewFVa
083400040702     c                   reset                   $DltPS
083500031007     c                   movea     *zeros        *in(41)
083600151109      * -?Spegnimento indicatore di errore SQL?
083700151109     c                   eval      *in34 = *off
083800031007      *
083900031007      * Controllo P.O. in gestione
084000171102     c                   eval      *in05   = *off
084100031007if  1c                   if        V1Cpog <> SAVpog
084200031007     c                   exsr      DecodPOG
084300031009if  2c                   if        *in90   = *off
084400171106if  3c                   if        V1Cpog  = c_PO_a  or
084500171106     c                             SAVpog  = c_PO_a
084600171106     c                   eval      *in90   = *on
084700171106e   3c                   endif
084800031009     c                   eval      SAVpog  = V1Cpog
084900031007x   2c                   else
085000031007     c                   reset                   SAVpog
085100031007e   2c                   endif
085200171103      * - ?Impostazione richiesta "Tipo Piano Smistamento"?
085300171103      *   ?(anche se visualizzata solo per la fil. 006-PD)?
085400171103if  2c                   if        SAVpog  = c_PO_a
085500171103     c                   eval      V1Ctps  = 'R'
085600171103x   2c                   else
085700171103     c                   clear                   V1Ctps
085800171103e   2c                   endif
085900171103     c                   exsr      Ctrl_VDL
086000031007e   1c                   endif
086100171102      * - ?(Ri)Controllo se Filiale Gestione = 006-PD?
086200171102      *   ?(per abilitazione richiesta "Tipo Piano Smistamento")?
086300171205     c*//                eval      *in05   = (V1Cpog = c_PO_a)
086400171205     c                   eval      *in05   = (V1Cpog = c_PO_a  and
086500171205     c                                        wDate >= dVPOdeco.§VPO2PkPd)
086600060110     c                   if        *in90
086700060110     c                   leavesr
086800060110     c                   endif
086900031007      *
087000031007      * Controllo categoria (tipo)
087100031007sel 1c                   select
087200031007w   1c                   when          V1Cnpg =  *zeros
087300031013     c                             and V1Cnfv <> *zeros
087400031013     c                             and V1Cnfv <> *blanks
087500031007     c                   seton                                        422890
087600031007     c                   movel     $Msg(4)       V1Dmsg
087700031007w   1c                   when          V1Cnpg <> *zeros
087800031007     c                             and V1Cnpg <> 2
087900031007     c                   seton                                        422890
088000031007     c                   movel     $Msg(5)       V1Dmsg
088100031007e   1c                   endsl
088200060110     c                   if        *in90
088300060110     c                   leavesr
088400060110     c                   endif
088500040302      *
088600040302      * Controllo categoria da TABEL (se NON default)
088700040302if  1c                   if            V1Cnpg <> *zeros
088800040302     c                             or (V1Cnfv <> *zeros
088900040302     c                             and V1Cnfv <> *blanks)
089000040302      *
089100040227     c                   movel     '7N'          TBLcod
089200040312     c                   movel(p)  V1Cnpg        TBLkey
089300040227     c     KTAB          chain     TABEL
089400040227      * - inesistente
089500040227if  2c                   if        NOT %found(TABEL00F)
089600040227     c                             or  TBLflg <> *blanks
089700040227     c                   seton                                        422890
089800040227     c                   movel     $Msg(5)       V1Dmsg
089900060110     c                   leavesr
090000040227e   2c                   endif
090100040227     c                   movel     TBLuni        ds7N
090200040227      * - non utilizzabile dal P.O. in gestione
090300040227if  2c                   if        §7Nuti = '9'
090400040227     c                   seton                                        422890
090500040227     c                   movel     $Msg(5)       V1Dmsg
090600060110     c                   leavesr
090700040227e   2c                   endif
090800040227      * - utilizzabile solo da P.O. di diverso livello
090900040227if  2c                   if           (§7Nuti =  '1'
091000040227     c                             and V1Cpog <> D55tfp)
091100040227     c                             OR (§7Nuti =  '2'
091200040227     c                             and V1Cpog =  D55tfp)
091300040227     c                   seton                                        422890
091400040227     c                   movel     $Msg(6)       V1Dmsg
091500060110     c                   leavesr
091600040227e   2c                   endif
091700040302      *
091800040302e   1c                   endif
091900040302      *
092000031007      * Controllo numero foglio viaggio
092100031013      * - se *blanks lo considero *zeros (sarebbe numerico)
092200031013if  1c                   if        V1Cnfv =  *blanks
092300031013     c                   eval      V1Cnfv =  *zeros
092400031013e   1c                   endif
092500031013      * - ricerca
092600031013     c     '?'           scan      V1Cnfv                                 10
092700031013if  1c                   if        *in10  =  *on
092800040415      * - - per interrogare il foglio è obbligatorio inserirne il tipo
092900040415      *     => ora fisso a "2"
093000040415if  2c                   if        V1Cnpg =  *zeros
093100040415     c                   seton                                        422890
093200040415     c                   movel     $Msg(5)       V1Dmsg
093300060110     c                   leavesr
093400040415e   2c                   endif
093500031013     c                   eval      V1Cnfv =  *zeros
093600031013     c                   eval      Wcampo =  'V1CNFV    '
093700031013     c                   exsr      srSEARCH
093800040705     c                   eval      *in90  =  *on
093900031013e   1c                   endif
094000031013      * - controlli:
094100031013      * - - numericità
094200031013     c     DigitN        check     V1Cnfv                                 10
094300031013if  1c                   if        *in10  =  *on
094400031013     c                   seton                                        542890
094500031013     c                   movel     $Msg(20)      V1Dmsg
094600060110     c                   leavesr
094700031013e   1c                   endif
094800040415      * - - effettiva immissione
094900040415if  1c                   if            V1Cnpg <> *zeros
095000040415     c                             and V1Cnfv =  *zeros
095100040415     c                   seton                                        432890
095200040415     c                   movel     $Msg(7)       V1Dmsg
095300060110     c                   leavesr
095400040415e   1c                   endif
095500031009      *
095600040701     c*** (già fatto)    reset                   $NewFVa
095700031009if  1c                   IF        V1Cnfv <> *zeros
095800031013      * - - inserimento in FLTR9
095900031009     c                   eval      TR9fgs  = V1Cpog
096000031009     c                   eval      TR9npga = V1Cnpg
096100031013     c                   move      V1Cnfv        TR9nfva
096200171103     c                   eval      TR9tps  = V1Ctps
096300171103     c     K04FLTR9      setll     FLTR9000
096400171103if  2c                   if        NOT %equal(FLTR902L)
096500031009     c                   eval      $NewFVa = *on
096600031013      * - - esistenza in FNFVV
096700031009     c                   eval      FVVfgs  = V1Cpog
096800031009     c                   eval      FVVnpg  = V1Cnpg
096900031013     c                   move      V1Cnfv        FVVnfv
097000031009     c     K03FNFVV      chain     FNFVV000
097100031009if  3c                   if        NOT %found(FNFVV03L)
097200031009     c*** (già da vl)              or  FVVatb <> *blanks
097300031009     c*** (già da vl)              or  FVVfcf <> *blanks
097400151106     c                   seton                                        432890
097500031009     c                   movel     $Msg(8)       V1Dmsg
097600060110     c                   leavesr
097700031010x   3c                   else
097800031106     c                   reset                   WLBda8
097900040705     c                   eval      G08inv  = FVVdfv
098000031010     c                   call      'XSRDA8'
098100031010     c                   parm                    WLBda8
098200040705     c                   eval      V1Ddfv  = G08dat
098300040705     c                   eval      SAVdfva = FVVdfv
098400031009e   3c                   endif
098500031013x   2c                   else
098600031013      * - - già esistente in FLTR9 (=> reperisco la data del F.V.)
098700040705     c                   eval      FVVfgs  = V1Cpog
098800040705     c                   eval      FVVnpg  = V1Cnpg
098900031013     c                   move      V1Cnfv        FVVnfv
099000031013     c     K03FNFVV      chain     FNFVV000
099100031013if  3c                   if            %found(FNFVV03L)
099200031013     c*** (già da vl)              and FVVatb <> *blanks
099300031013     c*** (già da vl)              and FVVfcf <> *blanks
099400031106     c                   reset                   WLBda8
099500040705     c                   eval      G08inv  = FVVdfv
099600031013     c                   call      'XSRDA8'
099700031013     c                   parm                    WLBda8
099800040705     c                   eval      V1Ddfv  = G08dat
099900040705     c                   eval      SAVdfva = FVVdfv
100000031013e   3c                   endif
100100031009e   2c                   endif
100200151106      /free
100300151106
100400151106           // -?Verifica esistenza altri F.V. per la stessa data?
100500151109           //  ?(NON se Piano di Default)?
100600151109           //If  $NewFVA = *on;
100700151109           If  V1Cnpg <> *zero;
100800151106
100900151106             exsr  sr_OpenCursor;
101000151106
101100151106             If  Not *in34;
101200151106
101300151106               exsr  sr_ReadCursor;
101400151106               exsr  sr_CloseCursor;
101500151106
101600151109               //if  ds_NFVxDFV.TR9nfva > *zeros;
101700151109               if  ds_NFVxDFV.TR9nfva > *zero
101800151109                   and        $NewFVA = *on;
101900151106
102000151109                 wDate_Eur = %date( SavDFVa : *ISO );
102100151106                 *in43  = *on;
102200151106                 *in28  = *on;
102300151106                 *in90  = *on;
102400151106                 V1Dmsg = $Msg(55);
102500151109                 V1Dmsg = %replace( %trim( %editc(
102600151109                                    ds_NFVxDFV.TR9nfva : '2' ) )
102700151106                                  : V1Dmsg
102800151109                                  : %scan( '_      ' : V1Dmsg ) );
102900151109                 V1Dmsg = %replace( %editc( %dec( wDate_Eur ) : 'Y' )
103000151106                                  : V1Dmsg
103100151106                                  : %scan( '__/__/____' : V1Dmsg ) );
103200151106                 leavesr;
103300151106
103400151106               endif;
103500151106
103600151106             Else;
103700151106
103800151106               *in28  = *on;
103900151106               *in90  = *on;
104000151106               V1Dmsg = $Msg(56);
104100151106               leavesr;
104200151106
104300151106             EndIf;
104400151106
104500151109           EndIf;
104600151106
104700151106      /end-free
104800031009e   1c                   ENDIF
104900040705      * Anche se ho solo eseguito una ricerca ("?") del F.V.
105000040705      *   riemetto questa videata prima di proseguire con i
105100040705      *   controlli relativi al flag S/N DPD.
105200040705if  1c                   if        *in90   = *on
105300040705     c                   eval      *in44   =*on
105400060110     c                   leavesr
105500040705e   1c                   endif
105600040705      *
105700040705      * Se richiesto un piano di default, ma impostato piano DPD:
105800040705      *   vince il piano di default
105900040705if  1c                   if            V1Cnpg =  *zeros
106000040705     c                             and V1Cnfv =  *zeros
106100040705     c                   clear                   V1Cdpd
106200040705e   1c                   endif
106300040705      *
106400040705      * Controllo se P.O. abilitato a piano DPD
106500040705if  1c                   if        V1Cdpd  = 'S'
106600040705     c                   eval      *in90   = *on
106700040705     c                   eval      xx      = *zeros
106800040705do  2c                   dow       xx      < %elem(£1pog)
106900040705     c                   eval      xx      = xx + 1
107000040705if  3c                   if        £1pog(xx) <= *zeros
107100040705     c                   leave
107200040705e   3c                   endif
107300040705     c                   eval      yy      = 1
107400040705     c                   eval      *in10   = *off
107500040705     c     £1pog(xx)     lookup    $ORGfil(yy)                            10
107600040705if  3c                   if            *in10       =  *on
107700040705     c                             and $ORGntw(yy) =  'DPD'
107800040705     c                   eval      *in90   = *off
107900040705     c                   leave
108000040705e   3c                   endif
108100040705e   2c                   enddo
108200040705if  2c                   if        *in90   = *on
108300040705     c                   seton                                        442890
108400040705     c                   movel     $Msg(12)      V1Dmsg
108500060110     c                   leavesr
108600040705e   2c                   endif
108700040705e   1c                   endif
108800171103      *
108900171103      * - Controllo se richiesto il piano DPD ma con tipo Rulliera
109000171103if  1c                   if             V1Cdpd  = 'S'
109100171103     c                             and  V1Ctps  = 'R'
109200171103     c                   seton                                        452890
109300171103     c                   movel     $Msg(62)      V1Dmsg
109400171103     c                   leavesr
109500171103e   1c                   endif
109600040705      *
109700040705      * Controllo se esistono P.O. DPD nel piano di default
109800040705if  1c                   if        V1Cdpd  = 'S'
109900040705     c                   eval      *in90   = *on
110000040705     c                   eval      TR9fgs  = V1Cpog
110100040705     c                   clear                   TR9npga
110200040705     c                   clear                   TR9nfva
110300171103     c                   eval      TR9tps  = V1Ctps
110400171103     c     K04FLTR9      setll     FLTR9000
110500171103     c     K04FLTR9      reade(n)  FLTR9000
110600171103do  2c                   dow       NOT %eof(FLTR902L)
110700040705     c                   eval      xx      = 1
110800050615     c                   eval      *in10   = *off
110900040705     c     TR9bai        lookup    $ORGfil(xx)                            10
111000040705if  3c                   if            *in10       = *on
111100040705     c                             and $ORGntw(xx) = 'DPD'
111200040705     c                   eval      *in90   = *off
111300040705     c                   leave
111400040705e   3c                   endif
111500171103     c     K04FLTR9      reade(n)  FLTR9000
111600040705e   2c                   enddo
111700040705      * - Se non ne trovo: emetto l'apposito messaggio (xx) in window
111800040705if  2c                   if        *in90   = *on
111900040705     c                   eval      $Video  = 'W'
112000040705     c                   eval      xx      = 48
112100060110     c                   leavesr
112200040705e   2c                   endif
112300040705e   1c                   endif
112400031014      *
112500031014      * imposto numero F.V. in campo numerico editato a video
112600031014     c                   move      V1Cnfv        V1Dnfv
112700031007      *
112800060110     c                   ENDSR
112900171107      *
113000171107      /free
113100171107
113200171107       //--------------------------------------------------------------
113300171107       // ?Caricamento distinte già inserite dalla filiale 006-PD per?
113400171107       // ?l'altro "Tipo Piano Smistamento".?
113500171107       //--------------------------------------------------------------
113600171107       BEGSR  sr_CarDist_2;
113700171107
113800171107         clear  keyFLTR902;
113900171107         k_TR9fgs   = V1Cpog;            // ?(006)?
114000171107         k_TR9npga  = V1Cnpg;            // ?(2)?
114100171107         k_TR9nfva  = %int( V1Cnfv );
114200171107         if  V1Ctps = 'V';
114300171107           k_TR9tps = 'R';
114400171107         else;
114500171107           k_TR9tps = 'V';
114600171107         endif;
114700171107
114800171107         setll  %kds( keyFLTR902 ) FLTR9000;
114900171107         reade(N)  %kds( keyFLTR902 : 4 ) FLTR9000;
115000171107
115100171107         DoW  Not %eof( FLTR902L );
115200171107
115300171107           select;
115400171107             // - ?Distinta?
115500171107             when  TR9npg  = 4;
115600171107               wy += 1;
115700171107               sk_NFV2( wy ) = TR9nfv;
115800171107             // - ?$IMA?
115900171107             //*·when  TR9npg  = 3;
116000171107             // - ?$PO (solo piano VDL)?
116100171107             //*·when  TR9npg  = 1;
116200171107           endsl;
116300171107
116400171107           reade(N)  %kds( keyFLTR902 : 4 ) FLTR9000;
116500171107
116600171107         EndDo;
116700171107
116800171107       ENDSR;
116900171107
117000171107      /free
117100031010      *
117200031010      *---------------------------------------------------------------*
117300031010      * Decodifica P.O. in gestione                                   *
117400031010      *---------------------------------------------------------------*
117500031010     c     DecodPOG      BegSr
117600031010      *
117700031010if  1c                   if        V1Cpog =  *zeros
117800031010     c                   clear                   V1Dpog
117900031010     c                   clear                   OG150
118000031010     c                   seton                                        412890
118100031010     c                   movel     $Msg(1)       V1Dmsg
118200060110     c                   leavesr
118300031010e   1c                   endif
118400031010      *
118500031010     c     V1Cpog        chain     AZORG
118600031010      *
118700031010if  1c                   if        not %found(AZORG01L)
118800031010     c                             or  ORGfva = '*'
118900031010      *
119000031010     c                   movel     *all'? '      V1Dpog
119100031010     c                   clear                   OG150
119200031010     c                   seton                                        412890
119300031010     c                   movel     $Msg(2)       V1Dmsg
119400060110     c                   leavesr
119500031010      *
119600031010x   1c                   else
119700031010      *
119800031010     c                   movel     ORGdes        V1Dpog
119900031010      *
120000031010     c                   eval      *in10  =  *off
120100040701     c     V1Cpog        lookup    £1                                     10
120200031010if  2c                   if        NOT *in10
120300031010     c                   seton                                        412890
120400031010     c                   movel     $Msg(2)       V1Dmsg
120500060110     c                   leavesr
120600031010e   2c                   endif
120700031010      *
120800031010     c                   movel     ORGdf0        OG150
120900031013if  2c                   if        §OGpck <> 'S'
121000040427     c                             and    %subst(KNMUS:1:3) <> 'EDP'
121100031010     c                   seton                                        412890
121200031010     c                   movel     $Msg(3)       V1Dmsg
121300060110     c                   leavesr
121400031010e   2c                   endif
121500031010      *
121600031010      * Se tutto ok reperisco il terminal per verificarne il livello
121700031010     c                   clear                   FNLV55ds
121800031010     c                   eval      D55lin = V1Cpog
121900031010     c                   eval      D55drf = Wdate
122000031010     c                   call      'FNLV55R'
122100031010     c                   parm                    FNLV55ds
122200040705      *
122300040705     c                   clear                   TRUL06ds
122400040705     c                   movel     '£1'          D06cod
122500040705     c                   movel     V1Cpog        D06key
122600040705     c                   movel(p)  TRUL06ds      KPJBU
122700040705     c                   call      'TRUL06R'
122800040705     c                   parm                    KPJBA
122900040705     c                   movel     KPJBU         TRUL06ds
123000040705     c                   movea     D06lia        £1pog
123100031010      *
123200031010e   1c                   endif
123300031010      *
123400060110     c                   ENDSR
123500031009      *
123600031009      *---------------------------------------------------------------*
123700031009      * Inizializzazione schiere, flag, variabili, ecc                *
123800031009      *---------------------------------------------------------------*
123900031009     c     InzALL_1      BEGSR
124000031009      *
124100031009     c                   reset                   $Fine
124200031009     c                   reset                   $InzS01
124300031009     c                   reset                   $InzS02
124400031023     c                   reset                   $EoF
124500031009     c                   reset                   $EndSFL
124600031009     c                   reset                   $Default
124700040701     c*** già impostato: reset                   $NewFVa
124800040701     c*** già impostato: reset                   $DltPS
124900031009     c                   reset                   $VDL
125000040409     c                   reset                   $IMA
125100040423     c                   reset                   $IMAdft
125200040607     c                   reset                   $PO
125300040611     c                   reset                   $POdpd
125400080129     c*** non qui:       reset                   $POerr
125500040607     c                   reset                   $POdft
125600040423     c                   reset                   $Hidden
125700031023     c                   reset                   $Found
125800171102     c                   reset                   $First
125900050629     c                   reset                   $MsgBAIbau
126000031009      *
126100031009     c                   reset                   $MEMat
126200040519     c*** non qui:       reset                   $MEMbp
126300031111     c                   reset                   $MEMfv1
126400031111     c                   reset                   $MEMmp1
126500050701     c                   reset                   $MEMpm1
126600031111     c                   reset                   $MEMfv2
126700031111     c                   reset                   $MEMmp2
126800050701     c                   reset                   $MEMpm2
126900031009     c                   reset                   $MEMxf
127000040615     c                   reset                   $FVxPOa
127100040615     c                   reset                   $FVxPOn
127200040615     c                   reset                   $FVxPOd
127300031009      *
127400031009     c                   reset                   S01nrr
127500031009     c                   reset                   S02nrr
127600031009     c                   reset                   MAXnrr
127700031030     c                   reset                   SAVpep
127800031107     c                   reset                   SAVbau
127900040309      *
128000040423     c                   reset                   Wpost
128100031009      *
128200031009     c                   ENDSR
128300031009      *
128400031021      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
128500031021      *
128600031009     c     InzALL_2      BEGSR
128700031112      *
128800031111     c                   reset                   $FV
128900040315     c                   reset                   $CsrRRN
129000031009     c                   reset                   $PEP
129100031030     c                   reset                   $PEPbai
129200031009     c                   reset                   $BAI
129300031009     c                   reset                   $BAInr
129400050629     c                   reset                   $BAIbau
129500031009     c                   reset                   $BAU
129600040305     c                   reset                   $BAUbai
129700031009     c                   reset                   $BNL
129800031009     c                   reset                   $BNLnr
129900031030     c                   reset                   $NFV
130000040628     c                   reset                   $NFVbp
130100031030     c                   reset                   $NFVnr
130200040519     c                   reset                   $MEMbp
130300040409     c                   reset                   $MPTnr
130400040607     c                   reset                   $ORGnr
130500040607     c                   reset                   $PosPO
130600040628     c                   reset                   $PosPOdpd
130700031009      *
130800031009     c                   reset                   XX
130900031022     c                   reset                   SAVnfvIMA
131000031030     c                   reset                   SAVnfv
131100031022     c                   reset                   SAVpdr
131200040305     c                   reset                   SAVprg
131300031009     c                   reset                   Wpdr
131400031022     c                   reset                   Wnfv
131500031111     c                   reset                   Wnfv7
131600040423     c                   reset                   Wpost
131700031024     c                   reset                   Wast
131800031009      *
131900031009     c                   ENDSR
132000031009      *
132100031009      *---------------------------------------------------------------*
132200031009      * Caricamento dati da tab. MSL                                  *
132300031009      *---------------------------------------------------------------*
132400031009     c     Tab_MSL       BEGSR
132500031009      *
132600031009      * Carico schiera tab. MSL
132700031009     c                   clear                   XX
132800031009     c                   clear                   $MSLpep
132900031009     c                   movel     'MSL'         TBEcod
133000031009     c                   movel(p)  V1Cpog        TBEke1
133100031009     c     K02TNTBE      setll     TNTBE000
133200031009     c     K02TNTBE      reade     TNTBE000
133300031009do  1c                   dow       NOT %eof(TNTBE01L)
133400031009if  2c                   if        TBEatb = *blanks
133500031009     c                   add       1             xx
133600031009     c                   movel     TBEuni        dMSL
133700031009     c                   movel     §MSLpep       $MSLpep(xx)
133800031009e   2c                   endif
133900031009     c     K02TNTBE      reade     TNTBE000
134000031009e   1c                   enddo
134100031009      *
134200031009     c                   ENDSR
134300040408      *
134400040408      *---------------------------------------------------------------*
134500040408      * Caricamento tab. MPT                                          *
134600040408      *---------------------------------------------------------------*
134700040408     c     Tab_MPT       BEGSR
134800040408      *
134900040408      * Carico schiera tab. MPT
135000040408     c                   clear                   XX
135100040408     c                   clear                   $MPT
135200040423     c                   clear                   $MPTdes
135300040409     c                   reset                   $MPTobbl
135400040423     c*** qui non serve: clear                   $MPTnr
135500040408     c                   movel     'MPT'         TBEcod
135600040408     c     TBEcod        setll     TNTBE000
135700040408     c     TBEcod        reade     TNTBE000
135800040408do  1c                   dow       NOT %eof(TNTBE01L)
135900040408if  2c                   if        TBEatb  = *blanks
136000040408     c                   movel     TBEuni        dMPT
136100040409if  3c                   if            $VDL    =  *on
136200040409     c                             and §MPTsiv <> 'N'
136300040409     c                             OR  $VDL    =  *off
136400040409     c                             and §MPTnov <> 'N'
136500040408     c                   add       1             xx
136600040408     c                   movel     TBEke1        $MPT(xx)
136700040423     c                   movel     §MPTdes       $MPTdes(xx)
136800040409if  4c                   if            $VDL    =  *on
136900040409     c                             and §MPTsiv =  'S'
137000040409     c                             OR  $VDL    =  *off
137100040409     c                             and §MPTnov =  'S'
137200040409     c                   movel     *on           $MPTobbl(xx)
137300040409e   4c                   endif
137400040408e   3c                   endif
137500040408e   2c                   endif
137600040408     c     TBEcod        reade     TNTBE000
137700040408e   1c                   enddo
137800040408      *
137900040408      * La postazione "IMA" (generica) la considero sempre valida
138000040408      *   ma dò per scontato che sia inserita in tab. "MPT" !!!
138100040408if  1c***                if        xx      = *zeros
138200040408     c***                eval      $MPT(1) = 'IMA'
138300040408e   1c***                endif
138400040408      *
138500040408     c                   ENDSR
138600031009      *
138700031009      *---------------------------------------------------------------*
138800031009      * Controllo se P.O. in gestione con VDL                         *
138900031009      *---------------------------------------------------------------*
139000031009     c     Ctrl_VDL      BEGSR
139100031009      *
139200031009     c                   reset                   $VDL
139300031009     c                   clear                   dMFP
139400031009      *
139500031009     c                   reset                   TIBS02ds
139600031009     c                   movel     KNSIF         T02sif
139700031009     c                   movel     V1Cpog        T02ke1
139800031009     c                   call      'TIBS02R'
139900031009     c                   parm                    KPJBA
140000031009     c                   parm                    TIBS02ds
140100080121if  1c                   if        T02err  = *blanks
140200031009     c                   movel     T02uni        dMFP
140300031009e   1c                   endif
140400031009      *
140500040630      * Test se presente VdL o Cml per piano
140600171102      *//* - NON per i P.O. "elencati" in C_PO_...
140700161110      * => CML non è più usato!
140800171102      * => ?ORA anche 006-PD può gestire il Piano Smistamento VDL?
140900171102      *    ?(anche SE in maniera "particolare")?
141000171102if  1c*//                if        V1Cpog <> C_PO_a
141100060214      *
141200171102     c*//  'V'           lookup    §MFPap                                 10
141300161110     ***c  n10'C'           lookup    §MFPap                                 10
141400171102     c*//                movel     *in10         $VDL
141500060214      *
141600171102e   1c*//                endif
141700171102      *
141800171103sel 1c                   select
141900171103w   1c                   when      V1Ctps <> *blanks
142000171103w   1c                   when      %lookup( 'V' : §MFPap ) > *zero
142100171103     c                   eval      V1Ctps = 'V'
142200171103x   1c                   other
142300171103     c                   eval      V1Ctps = 'R'
142400171103e   1c                   endsl
142500171102     c                   eval      $VDL = ( V1Ctps = 'V' )
142600040408      *
142700040408      * Caricamento postazioni "IMA" da tab. MPT
142800040408     c                   exsr      Tab_MPT
142900031009      *
143000031009     c                   ENDSR
143100031009      *
143200031009      *---------------------------------------------------------------*
143300031020      * Intabellamento dati dal default                               *
143400031009      *---------------------------------------------------------------*
143500031020     c     Memo_DFT      BEGSR
143600031009      *
143700031009     c                   clear                   TR9npga
143800031009     c                   clear                   TR9nfva
143900171103     c     K04FLTR9      setll     FLTR9000
144000171103     c     K04FLTR9      reade(n)  FLTR9000
144100031009      *
144200171103do  1c                   DOW       NOT %eof(FLTR902L)
144300031020      *
144400031023      * Intabellamento autotrasportatori di default:
144500040701      * serve a "dare priorità" ad essi quando (dopo, qui sotto), si
144600040701      * memorizzeranno i F.V. più recenti di tutti gli autotrasportato-
144700040701      * ri (memorizzati e non): potrebbe andare "buca" all'ultimo auto-
144800040701      * trasportatore del default - se sono già stati memorizzati tanti
144900040701      * autotrasportatori quanti sono gli elementi della schiera $MEMat...
145000040315if  2c                   IF        TR9bnl <> 'S'
145100031023      *
145200031009      * - controllo se cod. autotrasportatore già memorizzato
145300040315if  3c                   if        TR9pdc <> *zeros
145400040615     c                   eval      xx      = 1
145500040615     c                   eval      *in10   = *off
145600031009     c     TR9pdc        lookup    $MEMat(xx)                             10
145700040615if  4c                   if        *in10   = *off
145800031009      * - - memorizzo nuovo cod. autotrasportatore
145900040615     c                   eval      xx      = 1
146000031009     c     *zeros        lookup    $MEMat(xx)                             10
146100040615if  5c                   if        *in10   = *on
146200040615     c                   eval      $MEMat(xx)  = TR9pdc
146300040315x   5c                   else
146400031009      * - - - se già memorizzati il n° massimo di autotrasportatori
146500031009      *       esco dal ciclo di intabellamento
146600031009     c                   leave
146700040315e   5c                   endif
146800040315e   4c                   endif
146900040315e   3c                   endif
147000040309      *
147100040315e   2c                   ENDIF
147200031020      *
147300171103     c     K04FLTR9      reade(n)  FLTR9000
147400040315e   1c                   ENDDO
147500040315      *
147600031023      *
147700031009      * Intabellamento F.V. più recente per ogni autotrasportatore
147800031110      *   prima intabellato (dal default) e NON (basta che abbia un
147900031110      *   f.v. valido per la data del f.v.arrivi impostato)
148000031023      *
148100040615     c                   eval      FV8fgs  = V1Cpog
148200040615     c                   eval      FV8npg  = 4
148300040615     c                   eval      FV8dfv  = FVVdfv
148400031009     c     K03FVV08      setll     FNFVV08
148500031010     c     K02FVV08      reade     FNFVV08
148600031009      *
148700031009do  1c                   DOW       NOT %eof(FNFVV08L)
148800040615if  2c                   if            FV8atb  = *blanks
148900040615     c                             and FV8fcf  = *blanks
149000120613     c                   eval      DSTnpg  = FV8npg
149100120613     c                   eval      DSTnfv  = FV8nfv
149200120613     c                   eval      DSTfgs  = V1Cpog
149300120613     c     K03FIDST      chain     FIDST000
149400120613if  3c                   if        %found(FIDST01L)
149500031009     c                   clear                   DS_FV
149600040615     c                   eval      dsFVdfv     = FV8dfv
149700040615     c                   eval      dsFVnfv     = FV8nfv
149800031022     c                   movel     DS_FV         w0150
149900171107      * - controllo per la fil. 006 se distinta già inserita
150000171107      *   nell'altro piano smistamento (VDL se questo è x Rulliera,
150100171107      *   Rulliera se questo è x VDL)
150200171107if 3Bc                   If        V1Cpog <> c_PO_a   or
150300171107     c                             %lookup( dsFVnfv : sk_NFV2 )
150400171107     c                                     = *zero
150500031009      * - controllo se cod. autotrasportatore già memorizzato
150600040615     c                   eval      xx      = 1
150700040615     c                   eval      *in10   = *off
150800031009     c     DSTpdr        lookup    $MEMat(xx)                             10
150900040615if  4c                   if        *in10   = *off
151000031110     c     *zeros        lookup    $MEMat(xx)                             10
151100040615if  5c                   if        *in10   = *on
151200040615     c                   eval      $MEMat(xx)  = DSTpdr
151300031110e   5c                   endif
151400031110e   4c                   endif
151500031111      * - - memorizzo data e numero distinte più recenti
151600040615if  4c                   if        *in10   = *on
151700050701     c                   movel     FV8spn        dFVVspn
151800031111     c                   add       1             $MEMxf(xx)
151900031111sel 5c                   select
152000031111      * - - - 1° f.v.
152100040615w   5c                   when      $MEMfv1(xx) < w0150
152200040615if  6c                   if        $MEMfv1(xx) > $MEMfv2(xx)
152300040615     c                   eval      $MEMfv2(xx) = $MEMfv1(xx)
152400040615     c                   eval      $MEMmp2(xx) = $MEMmp1(xx)
152500050701     c                   eval      $MEMpm2(xx) = $MEMpm1(xx)
152600031111e   6c                   endif
152700040615     c                   eval      $MEMfv1(xx) = w0150
152800040615     c                   eval      $MEMmp1(xx) = FV8fpp
152900050701if  2c                   eval      $MEMpm1(xx) = §FVVtpm
153000031111      * - - - 2° f.v.
153100040615w   5c                   when      $MEMfv2(xx) < w0150
153200040615     c                   eval      $MEMfv2(xx) = w0150
153300040615     c                   eval      $MEMmp2(xx) = FV8fpp
153400050701if  2c                   eval      $MEMpm2(xx) = §FVVtpm
153500031111e   5c                   endsl
153600031009e   4c                   endif
153700171107e  3Bc                   EndIf
153800031009e   3c                   endif
153900031009e   2c                   endif
154000031010     c     K02FVV08      reade     FNFVV08
154100031009e   1c                   ENDDO
154200040615      *
154300040615      *
154400040615      * Intabellamento F.V. più recente per ogni P.O. DPD
154500040615      *   prima intabellato (dal default) e NON (basta che abbia un
154600040615      *   f.v. valido per la data del f.v.arrivi impostato)
154700040615      *
154800040615     c                   clear                   xx
154900040615     c                   eval      FGVlnp  = V1Cpog
155000040615     c                   eval      FGVdfv  = SAVdfva
155100040615     c                   eval      FGVnfv  = *loval
155200040621     c     K03FGV02      setll     FNFGV000
155300040615     c     FGVlnp        reade     FNFGV000
155400040615      *
155500040615do  1c                   DOW       NOT %eof(FNFGV02L)
155600040615      *
155700040615if  2c                   IF            FGVatb  = *blanks
155800040615     c                             and FGVfcf  = *blanks
155900040705      *
156000040705     c                   clear                   Wnfv7
156100040705      * - 1° controllo con P.O. arrivo
156200040615     c                   eval      xy      = 1
156300040615     c                   eval      *in10   = *off
156400040615     c                   move      FGVlna        Wpost
156500040615     c     Wpost         lookup    $ORGfil(xy)                            10
156600040705      * - - imposto numero distinta
156700070530if  3c                   IF            *in10       = *on
156800040705     c                             and $ORGntw(xy) = 'DPD'
156900040705     c                   eval      Wnfv7   = FGVnfv
157000070530     c                   exsr      Memo_NFV
157100070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
157200070530      *     memorizzabile
157300080121if  4c                   if        xx      = %elem($FVxPOa)
157400070530     c                   leavesr
157500070530e   4c                   endif
157600070530e   3c                   ENDIF
157700040705      *
157800040705      * - 2° controllo con altri P.O. (1) del F.V.
157900040630     c                   clear                   xFGVffv
158000070530if  3c                   IF        FGVttr <> 'L'
158100040630     c                   movea     FGVffv        xFGVffv
158200040630     c                   clear                   wx
158300070530do  4c                   DoW       wx      < %elem(xFGVffv)
158400040630     c                   add       1             wx
158500040630if  5c                   if        xFGVffv(wx)     =  *blanks
158600040630     c                   leave
158700040630e   5c                   endif
158800040630     c                   eval      xy      = 1
158900040630     c                   eval      *in10   = *off
159000040630     c     xFGVffv(wx)   lookup    $ORGfil(xy)                            10
159100040705      * - - imposto numero distinta - negativo
159200040705      *     per memorizzare che il p.o. DPD è tra "altri p.o." del f.v.
159300040705      *     (e non è il p.o. finale di arrivo)
159400040630if  5c                   if            *in10       =  *on
159500040630     c                             and $ORGntw(xy) =  'DPD'
159600040630     c                   move      xFGVffv(wx)   Wpost
159700040705     c                   eval      Wnfv7   = FGVnfv * (-1)
159800070521     c                   exsr      Memo_NFV
159900070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
160000070530      *     memorizzabile
160100080121if  6c                   if        xx      = %elem($FVxPOa)
160200070530     c                   leavesr
160300070530e   6c                   endif
160400040630e   5c                   endif
160500070530e   4c                   EndDo
160600070530e   3c                   ENDIF
160700040705      *
160800040705      * - 3° controllo con altri P.O. (2) del F.V.
160900070530if  3c                   IF        FGVttr <> 'L'
161000040630     c                   clear                   xFGVffv
161100040630     c                   movea     FGVff2        xFGVffv
161200040630     c                   clear                   wx
161300070530do  4c                   DoW       wx      < %elem(xFGVffv)
161400040630     c                   add       1             wx
161500040630if  5c                   if        xFGVffv(wx)     =  *blanks
161600040630     c                   leave
161700040630e   5c                   endif
161800040630     c                   eval      xy      = 1
161900040630     c                   eval      *in10   = *off
162000040630     c     xFGVffv(wx)   lookup    $ORGfil(xy)                            10
162100040705      * - - imposto numero distinta - negativo
162200040705      *     per memorizzare che il p.o. DPD è tra "altri p.o." del f.v.
162300040705      *     (e non è il p.o. finale di arrivo)
162400040630if  5c                   if            *in10       =  *on
162500040630     c                             and $ORGntw(xy) =  'DPD'
162600040630     c                   move      xFGVffv(wx)   Wpost
162700040705     c                   eval      Wnfv7   = FGVnfv * (-1)
162800070521     c                   exsr      Memo_NFV
162900070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
163000070530      *     memorizzabile
163100080121if  6c                   if        xx      = %elem($FVxPOa)
163200070530     c                   leavesr
163300070530e   6c                   endif
163400040630e   5c                   endif
163500070530e   4c                   EndDo
163600070530e   3c                   ENDIF
163700040705      *
163800040705e   2c                   ENDIF
163900040615      *
164000040615     c     FGVlnp        reade     FNFGV000
164100040615      *
164200040615e   1c                   ENDDO
164300031009      *
164400031009     c                   ENDSR
164500070521      *
164600070521      *---------------------------------------------------------------*
164700070521      * Intabellamento F.V. disponibili x filiale                     *
164800070521      *---------------------------------------------------------------*
164900070521     c     Memo_NFV      BEGSR
165000070521      *
165100070521      * - Memorizzo data e numero distinte più recenti
165200070521     c                   eval      xx          = xx + 1
165300070521     c                   eval      $FVxPOa(xx) = Wpost
165400070521     c                   eval      $FVxPOn(xx) = Wnfv7
165500070521     c                   eval      $FVxPOd(xx) = FGVdfv
165600070521      *
165700070521     c                   ENDSR
165800031007      *
165900031007      *---------------------------------------------------------------*
166000031007      * Gestione videata W01                                          *
166100031007      *---------------------------------------------------------------*
166200031007     c     GesW01        BEGSR
166300031007      *
166400031007      * Preparazione videata con messaggio
166500031007     c                   clear                   MS03W01
166600040705     c                   movel     $Msg(xx)      W1Dms2
166700031009     c                   eval      W1Dtfa  = 'F12=Ritorno'
166800031007      *
166900031007      * Emissione messaggio
167000031009do  2c                   dou       *inKL   = *on
167100031007     c                   exfmt     MS03W01
167200031007e   2c                   enddo
167300031007      *
167400031007      * Ritorno alla videata iniziale
167500031007     c                   reset                   $Video
167600040705     c                   eval      *in90   = *off
167700031007      *
167800031007     c                   ENDSR
167900031007      *
168000031007      *---------------------------------------------------------------*
168100031106      * Gestione videata C01/S01 & C03/S03 o C02/S02 & C04/S04        *
168200031007      *---------------------------------------------------------------*
168300031106     c     GesSFL        BEGSR
168400031106      *
168500031106      * Inizializzo la videata
168600031106if  1c                   if            $VDL    = *off
168700031106     c                             and $InzS01 = *on
168800031106     c                             OR  $VDL    = *on
168900031106     c                             and $InzS02 = *on
169000031106     c                   exsr      InzSFL
169100031106     c                   eval      $InzS01     = *off
169200031106     c                   eval      $InzS02     = *off
169300031106e   1c                   endif
169400031106      *
169500031106      * Se non sono stati trovati record NEANCHE con il piano di
169600031106      *   default emetto un messaggio e torno alla 1ª videata
169700031106if  1c                   if        MAXnrr  = *zeros
169800031106     c                   eval      $Video  = 'W'
169900040705     c                   eval      xx      = 9
170000060110     c                   leavesr
170100031106x   1c                   else
170200031106     c                   eval      *in30   = *off
170300031106e   1c                   endif
170400031106      *
170500031106      * Posiziono il cursore
170600031106sel 1c                   select
170700031106w   1c                   when          $VDL   = *off
170800031106     c                             and C01csr > *zeros
170900031106     c                   z-add     C01csr        C01rcd
171000031106w   1c                   when          $VDL   = *on
171100031106     c                             and C02csr > *zeros
171200031106     c                   z-add     C02csr        C02rcd
171300031106w   1c                   when          $VDL   = *off
171400031106     c                             and C01csr = *zeros
171500031106     c                   z-add     1             C01rcd
171600031106w   1c                   when          $VDL   = *on
171700031106     c                             and C02csr = *zeros
171800031106     c                   z-add     1             C02rcd
171900031106e   1c                   endsl
172000031106      *
172100031106      * Scrivo la testata e la riga tasti funzionali abilitati
172200031106if  1c                   if        NOT *in90
172300031106     c                   write     MS03T01
172400031106if  2c                   if        $Default = *on
172500031106     c                   write     MS03Z02
172600031106x   2c                   else
172700031106     c                   write     MS03Z03
172800031106e   2c                   endif
172900031106e   1c                   endif
173000151109      *
173100151109      *?Altro Piano Smistamento già esistente per lo stesso giorno?
173200151109     c                   if        ds_NFVxDFV.TR9nfva > *zero
173300151109     c                             and        $NewFVA = *off
173400151109     c                             and        V1Cnpg <> *zero
173500151109     c                   eval      wDate_Eur = %date( SavDFVa : *ISO )
173600151109     c                   eval      *in28  = *on
173700151109     c                   eval      *in90  = *on
173800151109     c                   eval      V1Dmsg = $Msg(55)
173900151109     c                   eval      V1Dmsg = %replace( %trim( %editc(
174000151109     c                                        ds_NFVxDFV.TR9nfva : '2' ) )
174100151109     c                                      : V1Dmsg
174200151109     c                                      : %scan( '_      ' : V1Dmsg ) )
174300151109     c                   eval      V1Dmsg = %replace(
174400151109     c                                      %editc( %dec( wDate_Eur ) : 'Y' )
174500151109     c                                      : V1Dmsg
174600151109     c                                      : %scan( '__/__/____' : V1Dmsg ) )
174700151109     c                   endif
174800031106      *
174900031106      * Emetto la videata
175000060110     c     §OGpkm        comp      'S'                                    04
175100031106sel 1c                   select
175200031106w   1c                   when      $Video = '2'
175300031106     c                   exfmt     MS03C01
175400031106w   1c                   when      $Video = '3'
175500031106     c                   exfmt     MS03C02
175600031106w   1c                   when      $Video = '4'
175700031106     c                   exfmt     MS03C03
175800031106w   1c                   when      $Video = '5'
175900031106     c                   exfmt     MS03C04
176000031106e   1c                   endsl
176100031106      *
176200031106     c                   setoff                                       28  90
176300031106     c                   clear                   V1Dmsg
176400031106      *
176500031106sel 1c                   select
176600031106      * F3=Fine
176700031106w   1c                   when      *inKC
176800031106     c                   exsr      F03D01
176900060110     c                   leavesr
177000031106      * F12=Ritorno
177100031106w   1c                   when      *inKL
177200031106     c                   exsr      F12SFL
177300060110     c                   leavesr
177400040315      * F16=Cancellazione piano
177500040315w   1c                   when      *inKQ
177600040315     c                   exsr      F16SFL
177700060110     c                   leavesr
177800031106      *
177900031106e   1c                   endsl
178000031106      *
178100151109      *?Altro Piano Smistamento già esistente per lo stesso giorno?
178200151109     c                   if        ds_NFVxDFV.TR9nfva > *zero
178300151109     c                             and        $NewFVA = *off
178400151109     c                   leavesr
178500151109     c                   endif
178600151109      *
178700031106     c                   exsr      InzALL_2
178800031106      *
178900031106      * Controllo dati immessi a video
179000031106if  1c                   if        $VDL = *off
179100031106     c                   exsr      CtrS01
179200031106x   1c                   else
179300031106     c                   exsr      CtrS02
179400031106e   1c                   endif
179500060110     c                   if        *in90
179600060110     c                   leavesr
179700060110     c                   endif
179800031111      *
179900031111      * F7=Interrogazione F.V. non inseriti
180000031111if  1c                   if        *inKG
180100031111     c                   clear                   Wcampo
180200031111     c                   exsr      srSEARCH
180300060110     c                   leavesr
180400031111e   1c                   endif
180500031112      *
180600031112      * Alla conferma (F6 o F8): segnalo se esistono distinte aperte
180700031112      * ------------------------ NON inserite nel piano giornaliero
180800080129      *   e/o rimembra delle filiali DPD lasciate SENZA F.V.
180900040311     c                   reset                   WinKH
181000031112if  1c                   if        (*inKF  or  *inKH)
181100031112     c                             and $Default = *off
181200080129     c                   exsr      CtrlFVdpd
181300080129if  2c                   if        *inKL
181400080129     c                   leavesr
181500080129e   2c                   endif
181600031112     c                   exsr      CtrlDistAp
181700031112e   1c                   endif
181800031106      *
181900031106      * F6=Aggiornamento
182000031106      * F8=Aggiornamento & Stampa
182100031112if  1c                   if        *inKF   or  *inKH
182200031112     c                                     or  WinKH = *on
182300031106     c                   exsr      Upd_FLTR9
182400031112if  2c                   if        *inKH   or  WinKH = *on
182500031106     c                   exsr      F08SFL
182600031106e   2c                   endif
182700031106     c                   reset                   $Video
182800031106     c                   reset                   $InzD01
182900031106e   1c                   endif
183000031106      *
183100060110     c                   ENDSR
183200031007      *
183300031007      *---------------------------------------------------------------*
183400031106      * Inizializzazione videata S01 & S03 o S02 & S04                *
183500031007      *---------------------------------------------------------------*
183600031106     c     InzSFL        BEGSR
183700031007      *
183800031007      * Pulizia subfile
183900031007     c                   seton                                        3031
184000031106sel 1c                   select
184100031106w   1c                   when      $Video = '2'
184200031007     c                   write     MS03C01
184300031106w   1c                   when      $Video = '3'
184400031106     c                   write     MS03C02
184500031106w   1c                   when      $Video = '4'
184600031007     c                   write     MS03C03
184700031106w   1c                   when      $Video = '5'
184800031106     c                   write     MS03C04
184900031106e   1c                   endsl
185000031009     c                   setoff                                         31
185100031007      *
185200031009     c                   clear                   C01rcd
185300031009     c                   clear                   C01csr
185400031009     c                   clear                   S01nrr
185500031106     c                   clear                   C02rcd
185600031106     c                   clear                   C02csr
185700031106     c                   clear                   S02nrr
185800031007     c                   clear                   V1Dmsg
185900031007     c                   setoff                                       28  90
186000031007     c                   movea     *zeros        *in(41)
186100080129      *
186200080129     c                   reset                   $ORGerr
186300031007      *
186400031009      * Caricamento dei dati da presentare nel sfl (max 90 rec)
186500031023      * se è piano di DEFAULT leggo con key *zeros
186600031023      * se è NUOVO piano GIORNALIERO propongo quelli di default
186700040315      * se è VECCHIO piano GIORNALIERO rifaccio la lista per baie /
186800040315      *   postazioni dal piano di default e vi abbino i dati già memo-
186900040315      *   rizzati (questo per mantenere l'elenco delle baie/postazioni
187000040315      *   comprensivo di quelle eventualmente non registrate, visua-
187100040315      *   lizzate con "XXX").
187200031106     c                   eval      TR9fgs  = V1Cpog
187300031106     c                   clear                   TR9npga
187400031106     c                   clear                   TR9nfva
187500040623     c                   eval      XR9npga = V1Cnpg
187600040623     c                   move      V1Cnfv        XR9nfva
187700171103     c                   eval      TR9tps  = V1Ctps
187800031106      *
187900031007     c                   reset                   $EndSFL
188000031007do  1c                   DOW       $EndSFL = *off
188100031007     c                   exsr      Carica_SFL
188200031007e   1c                   ENDDO
188300031007      *
188400031106if  1c                   if        $VDL    = *off
188500031007     c                   eval      MAXnrr  = S01nrr
188600031106x   1c                   else
188700031106     c                   eval      MAXnrr  = S02nrr
188800031106e   1c                   endif
188900031009     c                   eval      *in33   = ($EndSFL = *on)
189000031007      *
189100031007     c                   ENDSR
189200151104      *
189300151106      /free
189400151106
189500151106       //--------------------------------------------------------------
189600151106       //?Preparaz. stringa SQL per estrazione altri piani smistamento ?
189700151106       //?  picking per lo stesso giorno.                              ?
189800151106       //?Preparazione cursore.                                        ?
189900151106       //?Dichiarazione cursore.                                       ?
190000151106       //?Apertura cursore.                                            ?
190100151106       //--------------------------------------------------------------
190200151106       BEGSR  sr_OpenCursor;
190300151109
190400151106         // -?Impostazione opzioni per SQL?
190500151106         exec sql   set  option  DynUsrPrf = *Owner,
190600151106                                 CloSqlCsr = *EndMod;
190700151106
190800151106         // -?Preparazione stringa SQL per estrarre i dati?
190900151106         wSQL = 'with NFV_x_DFV as +
191000151109                              ( select TR9fgs, TR9npga, TR9nfva, TR9fl2, +
191100151109                                       Fvvdfv +
191200151106                                  from FLTR900F inner join FNFVV00F +
191300151106                                    on TR9fgs  = FVVfgs +
191400151106                                   and TR9npga = FVVnpg +
191500151106                                   and TR9nfva = FVVnfv +
191600151106                                 where FVVfcf  = '' '' +
191700151109                              group by TR9fgs, TR9npga, TR9nfva, TR9fl2, +
191800151109                                       FVVdfv +
191900151109                              order by TR9fgs, TR9npga, TR9nfva, TR9fl2, +
192000151109                                       FVVdfv ), +
192100151106
192200151106                      CountNFV  as +
192300151109                              ( select TR9fgs, TR9npga, TR9fl2, FVVdfv, +
192400151106                                       count(*) as wNFVxDFV +
192500151106                                  from NFV_x_DFV +
192600151109                              group by TR9fgs, TR9npga, TR9fl2, FVVdfv ) +
192700151106
192800151106                 select NFV_x_DFV.*, CountNFV.wNFVxDFV +
192900151106
193000151106                   from NFV_x_DFV inner join CountNFV +
193100151106                     on NFV_x_DFV.TR9fgs  = CountNFV.TR9fgs +
193200151106                    and NFV_x_DFV.TR9npgA = CountNFV.TR9npgA +
193300151109                    and NFV_x_DFV.TR9fl2  = CountNFV.TR9fl2 +
193400151106                    and NFV_x_DFV.FVVdfv  = CountNFV.FVVdfv +
193500151106
193600151109                 where NFV_x_DFV.TR9fgs  = ' +
193700151109                       %trim( %editc( V1Cpog : '3' ) ) +
193800151109                 ' and NFV_x_DFV.TR9nfvA <> ' +
193900151109                       %trim( %editc( TR9nfva : '3' ) ) +
194000151109                 ' and NFV_x_DFV.FVVdfv  = ' +
194100151106                       %trim( %editc( SavDFVa : '3' ) );
194200151106
194300151109         if  V1Cdpd = 'S';
194400151109           wSQL +=
194500151109                 ' and NFV_x_DFV.TR9fl2  = ''D''';
194600151109         else;
194700151109           wSQL +=
194800151109                 ' and NFV_x_DFV.TR9fl2  = '' ''';
194900151109         endif;
195000151109
195100151106         // -?Dichiarazione cursore?
195200151106         exec sql   prepare S1   from :wSQL;
195300151106         exec sql   declare C1   cursor for S1;
195400151106
195500151106         if  SQLcode < *zero;
195600151106           *in34 = *on;
195700151109           exsr  sr_CloseCursor;
195800151106           exsr  sr_PrintERR;
195900151106           leavesr;
196000151106         endif;
196100151106
196200151106         // -?Apertura del cursore?
196300151106         exec sql   open C1;
196400151106
196500151106         if  SQLcode < *zero;
196600151106           *in34 = *on;
196700151106           exsr  sr_CloseCursor;
196800151106           exsr  sr_PrintERR;
196900151106           leavesr;
197000151106         endif;
197100151106
197200151106       ENDSR;
197300151106
197400151106       //--------------------------------------------------------------
197500151106       //?Lettura cursore.                                             ?
197600151106       //--------------------------------------------------------------
197700151106       BEGSR  sr_ReadCursor;
197800151106
197900151106         clear  ds_NFVxDFV;
198000151106
198100151106         exec sql   fetch next   from C1   into :ds_NFVxDFV;
198200151106
198300151106         select;
198400151106           when  SQLcode = 100;
198500151106             *in34 = *on;
198600151106             exsr  sr_CloseCursor;
198700151106           when  SQLcode < *zero;
198800151106             *in34 = *on;
198900151106             exsr  sr_CloseCursor;
199000151106             exsr  sr_PrintERR;
199100151106         endsl;
199200151106
199300151106       ENDSR;
199400151106
199500151106       //--------------------------------------------------------------
199600151106       //?Chiusura cursore.                                            ?
199700151106       //--------------------------------------------------------------
199800151106       BEGSR  sr_CloseCursor;
199900151106
200000151106         // -?Chiusura del cursore?
200100151109         exec sql   close C1;
200200151106
200300151106       ENDSR;
200400151106
200500151106       //--------------------------------------------------------------
200600151106       //?Stampa segnalazione dell'errore rilevato via SQL             ?
200700151106       //--------------------------------------------------------------
200800151106       BEGSR  sr_PrintERR;
200900151106
201000151106         // -?Stampa del Dump?
201100151106         Dump(A);
201200151106
201300151106         // -?Stampa del Job-Log?
201400151106         Qcmd = 'DSPJOBLOG job(*) output(*print)';
201500151106         exsr  sr_ExecCmd;
201600151106
201700151106       ENDSR;
201800151106
201900151106       //--------------------------------------------------------------
202000151106       //?Esecuzione del comando (già impostato).                      ?
202100151106       //--------------------------------------------------------------
202200151106       BEGSR  sr_ExecCmd;
202300151106
202400151106         clear Qcap0100;
202500151106         Qcabcsdh = *off;
202600151106         Qcapa    = *off;
202700151106         Qcacmdss = *off;
202800151106         Qcaerved = *allX'00';
202900151106
203000151106         clear Qusec;
203100151106         Qusbprv  = %size(Qusec);
203200151106
203300151106         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
203400151106                           %size(Qcap0100) : 'CPOP0100' : *omit :
203500151106                           0 : 0 : Qusec);
203600151106
203700151106         //if  Qusei <> *blank;
203800151106         //  ...;
203900151106         //endif;
204000151106
204100151106       ENDSR;
204200151106
204300151106      /end-free
204400031029      *
204500031029      *---------------------------------------------------------------*
204600031029      * Gestione tasto funzionale F8  da videata S01/S02/S03/S04      *
204700031029      *---------------------------------------------------------------*
204800031106     c     F08SFL        BEGSR
204900031029      *
205000031029      * Richiamo pgm. di stampa per piano di smistamento in esame
205100031029     c                   clear                   FIMS04ds
205200031029     c                   z-add     V1Cpog        D04pog
205300040408if  1c                   if        V1Cnfv > *zeros
205400031029     c                   z-add     V1Cnpg        D04npga
205500040408e   1c                   endif
205600031029     c                   move      V1Cnfv        D04nfva
205700171102     c                   movel     V1Ctps        D04tps
205800031029      *
205900031029     c                   movel(p)  FIMS04ds      KPJBU
206000031029     c                   call      'FIMS04R'
206100031029     c                   parm                    KPJBA
206200031029      *
206300031029     c                   ENDSR
206400031007      *
206500031007      *---------------------------------------------------------------*
206600031007      * Gestione tasto funzionale F12 da videata S01/S02/S03/S04      *
206700031007      *---------------------------------------------------------------*
206800031106     c     F12SFL        BEGSR
206900031007      *
207000031007      * Torno alla 1ª videata D01
207100031007     c                   reset                   $Video
207200031010     c                   movea     '00'          *in(01)
207300031007      *
207400031007     c                   ENDSR
207500040315      *
207600040315      *---------------------------------------------------------------*
207700040315      * Gestione tasto funzionale F16 da videata S03/S04              *
207800040315      *---------------------------------------------------------------*
207900040315     c     F16SFL        BEGSR
208000040315      *
208100040315      * Cancella record esistenti (se?NON?immissione)
208200040702if  1c                   if            $NewFVa = *off
208300040702     c                             and $DltPS  = *off
208400040315      *
208500040315      * - Richiesta conferma
208600040315     c                   clear                   MS03W01
208700040315     c                   movel     $Msg(19)      W1Dms2
208800040315     c                   eval      W1Dtfa = 'F6=Conferma   '
208900040315     c                                    + 'F12=Ritorno'
209000040315do  2c                   dou       *inKF   or   *inKL
209100040315     c                   exfmt     MS03W01
209200040315e   2c                   enddo
209300060110      * F12-Ritorno
209400060110     c                   if        *inKL
209500060110     c                   leavesr
209600060110     c                   endif
209700060110      * F6-Conferma
209800040315     c                   exsr      Dlt_FLTR9
209900040315      *
210000040315e   1c                   endif
210100040315      *
210200040315      * Torno alla 1ª videata D01
210300040315     c                   reset                   $Video
210400040315     c                   movea     '00'          *in(01)
210500040315      *
210600060110     c                   ENDSR
210700040701      *
210800040701      *---------------------------------------------------------------*
210900040701      * Controllo esistenza piano smistamento STD se richiesto DPD    *
211000040701      *---------------------------------------------------------------*
211100040701     c     Check_PS      BEGSR
211200040701      *
211300040701     c*** (già fatto)    reset                   $DltPS
211400040705     c                   eval      TR9fgs  = V1Cpog
211500040705     c                   eval      TR9npga = V1Cnpg
211600040705     c                   move      V1Cnfv        TR9nfva
211700171103     c                   eval      TR9tps  = V1Ctps
211800040701      *
211900171103     c     K04FLTR9      setll     FLTR9000
212000171103     c     K04FLTR9      reade(n)  FLTR9000
212100040701      *
212200040701sel 1c                   select
212300040701      **** - nuovo piano (IMPOSSIBILE qui)
212400171103w   1c***                when      %eof(FLTR902L)
212500040701     c***                eval      $NewFVa = *on
212600060110     c***                leavesr
212700040701      * - richiesto piano std, esiste piano std: modificabile
212800040701w   1c                   when          V1Cdpd =  *blanks
212900040701     c                             and TR9fl2 =  *blanks
213000060110     c                   leavesr
213100040701      * - richiesto piano std, esiste piano dpd: da sostituire
213200040701w   1c                   when          V1Cdpd =  *blanks
213300040701     c                             and TR9fl2 =  'D'
213400040701     c                   eval      $DltPS  = *on
213500060110     c                   leavesr
213600040701      * - richiesto piano dpd, esiste piano dpd: modificabile
213700040701w   1c                   when          V1Cdpd =  'S'
213800040701     c                             and TR9fl2 =  'D'
213900060110     c                   leavesr
214000040701      **** - richiesto piano dpd, esiste piano std: da sostituire
214100040701w   1c***                when          V1Cdpd =  'S'
214200040701     c***                          and TR9fl2 =  *blanks
214300040701     c*** (sotto)        ...
214400040701e   1c                   endsl
214500040701      *
214600040701      * Richiesto piano dpd, esiste piano std: da sostituire
214700040701      * Richiesta conferma per annullamento piano smistamento picking
214800040701     c                   clear                   MS03W01
214900040701     c                   movel     $Msg(45)      W1Dms1
215000040701     c                   movel     $Msg(46)      W1Dms2
215100040701     c                   movel     $Msg(47)      W1Dms3
215200040701     c                   eval      W1Dtfa  = 'F6=Conferma'
215300040701     c                                     + ' (nuovo piano DPD)   '
215400040701     c                                     + 'F12=Ritorno'
215500040701do  1c                   dou       *inKF   or   *inKL
215600040701     c                   exfmt     MS03W01
215700040701e   1c                   enddo
215800040701      *
215900040701sel 1c                   select
216000040701      * - F6 = cancella
216100040701w   1c                   when      *inKF   = *on
216200040701     c*** non adesso:    exsr      Dlt_FLTR9
216300040701     c                   eval      $DltPS  = *on
216400040701      * - F12 = ritorno
216500040701w   1c                   when      *inKL   = *on
216600040701     c                   reset                   $Video
216700040701     c                   movea     '00'          *in(01)
216800040701e   1c                   endsl
216900040701      *
217000060110     c                   ENDSR
217100031106      *
217200031106      *---------------------------------------------------------------*
217300031106      * Caricamento subfile da file FLTR900F                          *
217400031106      *---------------------------------------------------------------*
217500040705     c     Carica_SFL    BEGSR
217600031106      *
217700031106      * Solo la prima volta:
217800031106      *   posizionamento iniziale + lettura 1° record
217900031106if  1c                   if            S01nrr =  *zeros
218000031106     c                             and S02nrr =  *zeros
218100171102     c                             and $First =  *on
218200171102     c                   eval      $First = *off
218300040705     c                   clear                   TR9npga
218400040705     c                   clear                   TR9nfva
218500171103     c     K04FLTR9      setll     FLTR9000
218600171103     c     K04FLTR9      reade(n)  FLTR9000
218700171103if  2c                   if        %eof(FLTR902L)
218800031106     c                   eval      $EOF   = *on
218900031106e   2c                   endif
219000031106e   1c                   endif
219100031106      *
219200031106      * Pulizia iniziale del record nel sfl in esame
219300031106sel 1c                   select
219400031106w   1c                   when      $Video = '2'
219500031106     c                   clear                   MS03S01
219600031106w   1c                   when      $Video = '3'
219700031106     c                   clear                   MS03S02
219800031106e   1c                   when      $Video = '4'
219900031106     c                   clear                   MS03S03
220000031106w   1c                   when      $Video = '5'
220100031106     c                   clear                   MS03S04
220200031106e   1c                   endsl
220300031106      *
220400031106      * Se NON già rilevato %EOF:
220500031106      *   impostazione campi sfl con quelli dell'ultimo rec. letto
220600031106      *   e lettura singolo record successivo
220700031106if  1c                   IF        $EOF   = *off
220800031106      *
220900040309     c                   reset                   $Found
221000040629     c                   reset                   $IMA
221100040629     c                   reset                   $IMAdft
221200040629     c                   reset                   $PO
221300040629     c                   reset                   $POdpd
221400080129     c                   reset                   $POerr
221500040629     c                   reset                   $POdft
221600171102      *
221700171102      * - ?Selezione per "Tipo Piano Smistamento"?
221800171102if  2c                   if        %subst( TR9des : 28 : 1 )
221900171102     c                                         <> V1Ctps
222000171102     c                   goto      Read_Next
222100171102e   2c                   endif
222200040309      *
222300040702      * - Per PIANO GIORNALIERO già immesso:
222400031106      *   controllo la corrispondenza dei dati di default (che sto
222500031106      *   leggendo) con quelli effettivamente immessi
222600040315if  2c                   if            $Default = *off
222700040315     c                             and $NewFVa  = *off
222800040702     c                             and $DltPS   = *off
222900031106     c                   exsr      RepFVAeff
223000040702      * - Per PIANO DI DEFAULT  &
223100040702      *   Per PIANO GIORNALIERO nuovo:
223200040623      *   imposto i campi del file di comodo (per il piano giornaliero)
223300040623      *   come quelli del file in aggiornamento (per i dati di default)
223400040623x   2c                   else
223500040623     c                   movel     FLTR9ds       FLXR9ds
223600040623     c                   eval      $Found   = *on
223700031106e   2c                   endif
223800040701      *
223900040701      * - Controllo se postazione IMA o P.O.
224000080118if  2c                   if            $Default = *off
224100040702     c                             and TR9bnl  <> 'S'
224200040702     c                   eval      $Hidden  = *on
224300040702     c                   eval      Wpost    = TR9bai
224400040702     c                   exsr      Ctrl_IMA
224500040702     c                   exsr      Ctrl_PO
224600040705e   2c                   endif
224700040705if  2c                   if        $Found   = *on
224800040701     c                   eval      $Hidden  = *off
224900040701     c                   eval      Wpost    = XR9bai
225000040701     c                   exsr      Ctrl_IMA
225100040701     c                   exsr      Ctrl_PO
225200040705e   2c                   endif
225300040701      *
225400040702      * - Selezione per piano giornaliero DPD:
225500040705if  2c                   if            $Default = *off
225600040702     c*** non solo new:            and $NewFVa  = *off
225700040702     c                             and V1Cdpd   = 'S'
225800040702      *   - baia non letti
225900040702     c                             and TR9bnl  <> 'S'
226000040705      *   - postazioni IMx
226100040705     c                             and $IMA     = *off
226200040702      *   - postazioni PO-dpd
226300040702     c                             and $POdpd   = *off
226400040702     c                   goto      Read_Next
226500040705e   2c                   endif
226600040705      *
226700040705      *   - postazioni IMx obbligatorie
226800040705if  2c                   if            $Default = *off
226900040705     c*** non solo new:            and $NewFVa  = *off
227000040705     c                             and V1Cdpd   = 'S'
227100040705     c                             and $IMA     = *on
227200040705     c                   eval      *in10    = *off
227300040705     c                   eval      xx       = 1
227400040705     c     Wpost         lookup    $MPT(xx)                               10
227500040705     c   10$MPTobbl(xx)  cabeq     *off          Read_Next
227600040705e   2c                   endif
227700031106      *
227800031106      * - Default (comuni)
227900040309      * > > P.O. senza VDL
228000040701if  2c                   if        $VDL     = *off
228100040701     c                   exsr      CarNoVDL_1
228200040309      * > > P.O. con VDL
228300040701x   2c                   else
228400040701     c                   exsr      CarSiVDL_1
228500040701e   2c                   endif
228600031106      *
228700031106      * - Impostazione dei F.V. in piano giornaliero
228800031106if  2c                   IF        $Default = *off
228900031106      *
229000031106      * - - da default
229100040702if  3c                   IF            $NewFVa  = *on
229200040702     c                             or  $DltPS   = *on
229300031106      *
229400040309sel 4c                   select
229500040309w   4c                   when      TR9bnl = 'S'
229600040615w   4c                   when      $PO    = *on
229700040705if  5c                   if        $POdpd = *on
229800040705     c                   exsr      PostPOdpd
229900040705e   5c                   endif
230000040309w   4c                   when      $IMA   = *on
230100040615     c                   exsr      PostIMA
230200040309x   4c                   other
230300040615     c                   exsr      PostNoIMA
230400040309e   4c                   endsl
230500031106      *
230600031106      * - - da record già immessi
230700031106x   3c                   ELSE
230800031106      *
230900040621if  4c                   if        $Found   = *on
231000031106     c                   reset                   WLBda8
231100031106     c                   reset                   DSdata8
231200031106     c                   reset                   DSdata6
231300040621if  5c                   if        XR9dfv   > *zeros
231400040623     c                   eval      G08inv   = XR9dfv
231500031106     c                   call      'XSRDA8'
231600031106     c                   parm                    WLBda8
231700031106     c                   move      G08dat        DSdata8
231800031106     c                   z-add     DSdt8gg       DSdt6gg
231900031106     c                   z-add     DSdt8mm       DSdt6mm
232000031106     c                   z-add     DSdt8aa       DSdt6aa
232100040621e   5c                   endif
232200040621e   4c                   endif
232300040309      * > > > P.O. senza VDL
232400031106if  4c                   if        $VDL     = *off
232500040701     c                   exsr      CarNoVDL_2
232600040309      * > > > P.O. con VDL
232700031106x   4c                   else
232800040701     c                   exsr      CarSiVDL_2
232900031106e   4c                   endif
233000031106      *
233100031106e   3c                   ENDIF
233200040705      *
233300040705      * Reimposto il valore assoluto tra i F.V. memorizzati x P.O. DPD
233400040705     c                   eval      $FVxPOn  =  %abs($FVxPOn)
233500031106      *
233600031106      * Per piano giornaliero imposto "*" se più F.V. per il padroncino
233700040615      *   o se più F.V. per la postazione = P.O. DPD (solo VdL)
233800040701     c                   clear                   Wast
233900031106     c                   clear                   Wpdr
234000040615     c                   clear                   Wpost
234100040701sel 3c                   select
234200040701w   3c                   when      XR9pdc   > *zeros
234300040621     c                   eval      Wpdr     = XR9pdc
234400031106     c                   exsr      CtrlAutotr3
234500040701w   3c                   when      $POdpd   = *on
234600040621     c                   eval      Wpost    = XR9bai
234700040615     c                   exsr      AsterFVxPO
234800040701e   3c                   endsl
234900040701if  3c                   if        $VDL     = *off
235000040701     c                   eval      S1Cast   = Wast
235100040701x   3c                   else
235200040701     c                   eval      S2Cast   = Wast
235300040701e   3c                   endIf
235400031106      *
235500031106e   2c                   ENDIF
235600031106      *
235700031106      * Se *zeros li reimposto a *blanks (numerici "editati")
235800031106if  2c                   if        S1Cnfv   = *zeros
235900031106     c                   clear                   S1Cnfv
236000031106e   2c                   endif
236100031106if  2c                   if        S3Cnfv   = *zeros
236200031106     c                   clear                   S1Cnfv
236300031106e   2c                   endif
236400031106if  2c                   if        S2Cnfv   = *zeros
236500031106     c                   clear                   S2Cnfv
236600031106e   2c                   endif
236700040304if  2c                   if        S4Cnfv   = *zeros
236800040304     c                   clear                   S4Cnfv
236900040304e   2c                   endif
237000031106      *
237100090203x   1c***                ELSE
237200031106      *
237300031106      * Se già rilevato %EOF:
237400031106      * - impostazione campi vuoti nei restanti record del sfl
237500031106      *   (se "default")
237600031106      *
237700031106e   1c                   ENDIF
237800040701      *
237900040701      * Scrivo il singolo record nel subfile
238000040701     c                   exsr      WrtSflRec
238100040701      *
238200040701     c     Read_Next     tag
238300031106      *
238400031106      * Leggo il record successivo da caricare
238500031106if  1c                   if        $EOF     = *off
238600171103     c     K04FLTR9      reade(n)  FLTR9000
238700171103if  2c                   if        %eof(FLTR902L)
238800031106     c                   eval      $EoF     = *on
238900031106e   2c                   endif
239000031106e   1c                   endif
239100031106      *
239200031106      * Imposto l'uscita SE:
239300031106      * - ho già scritto il numero massimo di rec. previsti nel sfl
239400031106if  1c                   if            S01nrr  >= C_MaxRecSf
239500031106     c                             or  S02nrr  >= C_MaxRecSf
239600031106      * - è stato rilevato EOF in "Piano Giornaliero" (NON di default)
239700040702      *   non emetto records vuoti (come per il default)
239800031106     c                             or ($Default = *off
239900040702     c                             and $EoF     = *on)
240000031106     c                   eval      $EndSFL  = *on
240100031106e   1c                   endif
240200031106      *
240300031106     c                   ENDSR
240400040701      *
240500040701      *---------------------------------------------------------------*
240600050614      * Scrittura singolo record nel subfile                          *
240700040701      *---------------------------------------------------------------*
240800040701     c     WrtSflRec     BEGSR
240900040701      *
241000040701      * - reimposto indicatori x DSPATR condizionati
241100040701if  1c                   IF        $VDL = *off
241200060110     c                   exsr      AtrS01S03
241300040701x   1c                   ELSE
241400060110     c                   exsr      AtrS02S04
241500040701e   1c                   ENDIF
241600040701      *
241700040701      * Scrittura del singolo record nel sfl in esame
241800040702if  1c                   if            $Default = *on
241900040702     c                             OR  $Default = *off
242000040702     c                             and $EoF     = *off
242100040701     c                   eval      *in32    = *on
242200040701sel 2c                   select
242300040701w   2c                   when      $Video   = '2'
242400040701     c                   add       1             S01nrr
242500040701     c                   write     MS03S01
242600040701w   2c                   when      $Video   = '3'
242700040701     c                   add       1             S02nrr
242800040701     c                   write     MS03S02
242900040701e   2c                   when      $Video   = '4'
243000060110     c                   eval      *in54    = (*in80  = *off)
243100040701     c                   add       1             S01nrr
243200040701     c                   write     MS03S03
243300040701w   2c                   when      $Video   = '5'
243400060110     c                   eval      *in54    = (*in80  = *off)
243500040701     c                   add       1             S02nrr
243600040701     c                   write     MS03S04
243700040701e   2c                   endsl
243800040701e   1c                   endif
243900040701      *
244000040701     c                   ENDSR
244100031106      *
244200031106      *---------------------------------------------------------------*
244300031106      * Reperimento dei dati effettivamente immessi per postazione    *
244400031106      *---------------------------------------------------------------*
244500031106     c     RepFVAeff     BEGSR
244600031106      *
244700031106      * Ctrl se esiste il rec. corrispondente nel piano giornaliero
244800031106      *   già immesso
244900040623     c                   eval      XR9prg = (TR9prg * 2) - 1
245000171103     c     K06TR902      chain     FLTR902
245100171103     c                   eval      $Found = (%found(FLTR902X))
245200040621      *
245300040621      * Se NON lo trovo: segnalo il rec. di dft come omesso ("XXX")
245400040623if  1c                   if        $Found  = *off
245500040623     c*** troppo:        clear                   FLXR9ds
245600040623     c                   clear                   XR9bai
245700161213     c                   clear                   XR9des
245800040623     c                   clear                   XR9pdc
245900040623     c                   clear                   XR9npg
246000040623     c                   clear                   XR9nfv
246100040623     c                   clear                   XR9dfv
246200040623     c                   clear                   XR9fl1
246300040623     c                   clear                   XR9fl2
246400040623     c                   clear                   XR9fl3
246500040623     c                   eval      XR9bai  = C_PostazNull
246600040621e   1c                   endif
246700031106      *
246800040315     c                   ENDSR
246900050614      *
247000050614      *---------------------------------------------------------------*
247100050614      * Verifica se:                                                  *
247200050614      * - autotrasportatore MESSAGGERIA per piano di default          *
247300050614      * - distinta MESSAGGERIA per piano di smistamento               *
247400050614      *---------------------------------------------------------------*
247500050614     c     X_Messaggeria BEGSR
247600050614      *
247700050614if  1c                   if        $Default = *on
247800050614      *
247900050614     c                   clear                   S2Dmes
248000050614      * in piano di default: i dati dell'autotrasportatore sono appena
248100050614      *   stati reperiti
248200050614     c*** (già fatto)    movel     APDflr        dAPDflr
248300050614if  2c                   if        §APDtpm  = 'M'
248400180110if  3c                   if        $VDL     = *off
248500180110     c                   eval      H1Dmes   = c_Messaggeria
248600180110     c                   eval      %subst( S1Dpdc : %len(S1Dpdc) - 3 )
248700180110     c                                      = c_DspAtrRI + H1Dmes
248800180110x   3c                   else
248900180110     c                   eval      S2Dmes   = C_Messaggeria
249000180110e   3c                   endif
249100180110e   2c                   endif
249200050614      *
249300050614x   1c                   else
249400050614      *
249500050614if  2c                   if        wNFVseq  = 1
249600050614     c                   clear                   S2Dmes
249700050614x   2c                   else
249800050614     c                   clear                   S4Dmes
249900050614e   2c                   endif
250000050614      * in piano di smistamento: i dati del F.V. sono appena
250100050614      *   stati reperiti da FNFVV
250200050701     c*** (già fatto)    movel     FVVspn        dFVVspn
250300050614if  2c                   if        §FVVtpm  = 'M'
250400180110sel 3c                   select
250500180110w   3c                   when      wNFVseq  = 1  and  $VDL = *off
250600180110     c                   eval      H1Dmes   = c_Messaggeria
250700180110w   3c                   when      wNFVseq  > 1  and  $VDL = *off
250800180110     c                   eval      H3Dmes   = c_Messaggeria
250900180110w   3c                   when      wNFVseq  = 1  and  $VDL = *on
251000050629     c                   eval      S2Dmes   = C_Messaggeria
251100180110w   3c                   when      wNFVseq  > 1  and  $VDL = *on
251200050629     c                   eval      S4Dmes   = C_Messaggeria
251300180110e   3c                   endsl
251400050614e   2c                   endif
251500050614      *
251600050614e   1c                   endif
251700050614      *
251800050614     c                   ENDSR
251900040701      *
252000040701      *---------------------------------------------------------------*
252100040701      * Caricamento dati di default in F.V. per P.O. senza VDL        *
252200040701      *---------------------------------------------------------------*
252300040701     c     CarNoVDL_1    BEGSR
252400040701      *
252500040701     c                   move      TR9pep        S1Cpep
252600040701     c                   eval      S1Cdes   = TR9des
252700040701     c                   eval      S1Cbai   = XR9bai
252800040701if  1c                   if        $Found   = *on
252900040701sel 2c                   select
253000040701w   2c                   when      $IMA     = *on
253100040701     c                   movel     $MPTdes(yy)   S1Dpdc
253200040701     c                   movel     $MPTdes(yy)   S3Dpdc
253300040701w   2c*** solo x VDL:    when      $PO      = *on
253400040701w   2c                   when      XR9pdc  <> *zeros
253500040701     c                   move      XR9pdc        S1Cpdc
253600040701     c                   eval      APDpdr   = XR9pdc
253700040701     c                   exsr      CtrlAutotr2
253800040701     c                   movel     APDrsc        S1Dpdc
253900040701     c                   movel     APDrsc        S3Dpdc
254000180110      * - imposto il flag per autotrasportatore Messaggeria
254100180110if  3c                   if            $Default = *on
254200180110     c                             and §OGpkm   = 'S'
254300180110     c                             and $VDL     = *off
254400180110     c                             and $IMA     = *off
254500180110     c                             and $PO      = *off
254600180110     c                   eval      wNFVseq  = 1
254700180110     c                   movel     APDflr        dAPDflr
254800180110     c                   exsr      X_Messaggeria
254900180110e   3c                   endif
255000180110     c                   setoff                                       552890
255100040701     c                   clear                   V1Dmsg
255200040701e   2c                   endsl
255300040701e   1c                   endif
255400040701      * - - - Campi hidden
255500040701if  1c                   if        $Default = *off
255600060113if  2c                   if            $NewFVa = *off
255700060113     c                             and $DltPS  = *off
255800060113     c                             and $Found  = *on
255900060113     c                   eval      H1Cprg   = XR9prg
256000060113x   2c                   else
256100060113     c                   eval      H1Cprg   = (TR9prg * 2) - 1
256200060113e   2c                   endif
256300040701     c                   eval      H1Cbai   = TR9bai
256400040701     c                   eval      H1Cpdc   = TR9pdc
256500040701if  2c                   if            $IMAdft = *off
256600040701     c                             and H1Cpdc <> APDpdr
256700040701     c                   eval      APDpdr   = H1Cpdc
256800040701     c                   exsr      CtrlAutotr2
256900040701     c                   setoff                                       552890
257000040701     c                   clear                   V1Dmsg
257100040701e   2c                   endif
257200040701if  2c                   if        $IMAdft  = *off
257300040701     c                   eval      H1Dpdc   = APDrsc
257400040701x   2c                   else
257500040701     c                   eval      H1Dpdc   = $MPTdes(yy)
257600040701e   2c                   endif
257700040701e   1c                   endif
257800040701      *
257900040701     c                   ENDSR
258000040701      *
258100040701      *---------------------------------------------------------------*
258200040701      * Impostazione dei F.V. in piano giornaliero per P.O. senza VDL *
258300040701      *---------------------------------------------------------------*
258400040701     c     CarNoVDL_2    BEGSR
258500040701      *
258600040701if  1c                   if        $Found   = *off
258700040701      *
258800040701     c                   eval      S1Cbai   = C_PostazNull
258900040701     c                   eval      S1Kdft   = C_Default
259000040701     c                   eval      S1Kbad   = C_Postazione
259100040701     c                   eval      S1Cbad   = H1Cbai
259200040701if  2c                   if        H1Cpdc  <> *zeros
259300040701     c                   eval      S1Katd   = C_Autotr
259400040701     c                   move      H1Cpdc        S1Catd
259500040701e   2c                   endif
259600040701     c                   eval      S1Datd   = H1Dpdc
259700040701      *
259800040701x   1c                   else
259900040701      *
260000040701     c                   move      XR9nfv        S1Cnfv
260100040701     c                   move      DSdata6       S1Ddfv
260200040701     c                   eval      S1Dfpp   = XR9fl1
260300180110      * - reperisco il flag per autotrasportatore Messaggeria
260400180110if  2c                   if            $Default = *off
260500180110     c                             and §OGpkm   = 'S'
260600180110     c*//                          and $VDL     = *off
260700180110     c                             and $IMA     = *off
260800180110     c                             and $PO      = *off
260900180110     c                   eval      FVVfgs   = V1Cpog
261000180110     c                   eval      FVVnpg   = 4
261100180110     c                   move      XR9nfv        FVVnfv
261200180110     c     K03FNFVV      chain     FNFVV000
261300180110if  3c                   if            %found(FNFVV03L)
261400180110     c                   eval      wNFVseq  = 1
261500180110     c                   movel     FVVspn        dFVVspn
261600180110     c                   exsr      X_Messaggeria
261700180110e   3c                   endif
261800180110e   2c                   endif
261900180110if  2c                   if        $IMA     = *off
262000040702     c                             and $PO  = *off
262100040701     c                   exsr      CercaIIfv
262200180110      * - reperisco il flag per autotrasportatore Messaggeria
262300180110if  3c                   if            $Default = *off
262400180110     c                             and §OGpkm   = 'S'
262500180110     c*//                          and $VDL     = *off
262600180110     c                             and $IMA     = *off
262700180110     c                             and $PO      = *off
262800180110     c                             and %found(FLTR902X)
262900180110     c                             and XR9nfv   > *zeros
263000180110     c                   eval      FVVfgs   = V1Cpog
263100180110     c                   eval      FVVnpg   = 4
263200180110     c                   move      XR9nfv        FVVnfv
263300180110     c     K03FNFVV      chain     FNFVV000
263400180110if  4c                   if            %found(FNFVV03L)
263500180110     c                   eval      wNFVseq  = 2
263600180110     c                   movel     FVVspn        dFVVspn
263700180110     c                   exsr      X_Messaggeria
263800180110e   4c                   endif
263900180110e   3c                   endif
264000180110e   2c                   endif
264100040701      *
264200040701e   1c                   endif
264300040701      *
264400040701     c                   ENDSR
264500040701      *
264600040701      *---------------------------------------------------------------*
264700040701      * Caricamento dati di default in F.V. per P.O.  con  VDL        *
264800040701      *---------------------------------------------------------------*
264900040701     c     CarSiVDL_1    BEGSR
265000040701      *
265100040701     c                   movel     TR9pep        S2Cpep
265200040701     c                   eval      S2Cdes   = TR9des
265300040701     c                   eval      S2Cbnl   = TR9bnl
265400040701     c                   eval      S2Cbai   = XR9bai
265500161213     c                   if        $PO      = *on
265600161215     c                   eval      S2Cznc   = %subst( XR9des : 29 )
265700161213     c                   endif
265800040701      *
265900040701if  1c                   IF        $Found   = *on
266000040701      *
266100040701sel 2c                   SELECT
266200040701      *
266300040701w   2c                   WHEN      $IMA     = *on
266400040701     c                   movel     $MPTdes(yy)   S2Dpdc
266500040701     c                   movel     $MPTdes(yy)   S4Dpdc
266600040701      *
266700040701w   2c                   WHEN      $PO      = *on
266800040701     c                   movel     $ORGdes(xy)   S2Dpdc
266900040701     c                   movel     $ORGdes(xy)   S4Dpdc
267000040701      *
267100040701w   2c                   WHEN      XR9pdc  <> *zeros
267200040701     c                   move      XR9pdc        S2Cpdc
267300040701     c                   eval      APDpdr   = XR9pdc
267400040701     c                   exsr      CtrlAutotr2
267500040701     c                   movel     APDrsc        S2Dpdc
267600040701     c                   movel     APDrsc        S4Dpdc
267700050629      * - imposto il flag per autotrasportatore Messaggeria
267800050614if  3c                   if            $Default = *on
267900050614     c                             and §OGpkm   = 'S'
268000050629     c                             and $VDL     = *on
268100050614     c                             and $IMA     = *off
268200050614     c                             and $PO      = *off
268300050701     c                   eval      wNFVseq  = 1
268400050614     c                   movel     APDflr        dAPDflr
268500050629     c                   exsr      X_Messaggeria
268600050614e   3c                   endif
268700040701     c                   setoff                                       552890
268800040701     c                   clear                   V1Dmsg
268900040701      *
269000040701w   2c                   WHEN      TR9bnl   = 'S'
269100040701     c                   eval      S4Dpdc   = C_BaiaNonLetti
269200040701      *
269300040701e   2c                   ENDSL
269400040701      *
269500040701e   1c                   ENDIF
269600040701      *
269700040701      * - - - Campi hidden
269800060113if  1c                   if        $Default = *off
269900060113if  2c                   if            $NewFVa = *off
270000060113     c                             and $DltPS  = *off
270100060113     c                             and $Found  = *on
270200060113     c                   eval      H2Cprg   = XR9prg
270300060113x   2c                   else
270400060113     c                   eval      H2Cprg   = (TR9prg * 2) - 1
270500060113e   2c                   endif
270600060113e   1c                   endif
270700040701if  1c                   IF            $Default = *off
270800040701     c                             and TR9bnl  <> 'S'
270900040701      *
271000040701     c                   eval      H2Cbai   = TR9bai
271100161213if  2c                   if        $POdft   = *on
271200161215     c                   eval      H2Cznc   = %subst( TR9des : 29 )
271300161213e   2c                   endif
271400040701     c                   eval      H2Cpdc   = TR9pdc
271500040701      *
271600040701if  2c                   if            $IMAdft = *off
271700040701     c                             and $POdft  = *off
271800040701     c                             and H2Cpdc <> APDpdr
271900040701     c                   eval      APDpdr   = H2Cpdc
272000040701     c                   exsr      CtrlAutotr2
272100040701     c                   setoff                                       552890
272200040701     c                   clear                   V1Dmsg
272300040701e   2c                   endif
272400040701      *
272500040701sel 2c                   Select
272600040701w   2c                   When      $IMAdft  = *off   and
272700040701     c                             $POdft   = *off
272800040701     c                   eval      H2Dpdc   = APDrsc
272900040701w   2c                   When      $IMAdft  = *on
273000040701     c                   eval      H2Dpdc   = $MPTdes(yy)
273100040701w   2c                   When      $POdft   = *on
273200040701     c                   eval      H2Dpdc   = $ORGdes(xy)
273300040701e   2c                   EndSl
273400040701      *
273500040701e   1c                   ENDIF
273600040701      *
273700040701     c                   ENDSR
273800040701      *
273900040701      *---------------------------------------------------------------*
274000040701      * Impostazione dei F.V. in piano giornaliero per P.O.  con  VDL *
274100040701      *---------------------------------------------------------------*
274200040701     c     CarSiVDL_2    BEGSR
274300040701      *
274400040701sel 1c                   Select
274500040701      *
274600040701w   1c                   When      TR9bnl   = 'S'
274700040701      *
274800040701w   1c                   When          $Found =  *off
274900040701     c                             and TR9bnl <> 'S'
275000040701     c                             and TR9bai <> C_PostazNull
275100040701     c                   eval      S2Cbai   = C_PostazNull
275200040701     c                   eval      S2Kdft   = C_Default
275300040701     c                   eval      S2Kbad   = C_Postazione
275400040701     c                   eval      S2Cbad   = H2Cbai
275500040701if  2c                   if        H2Cpdc  <> *zeros
275600040701     c                   eval      S2Katd   = C_Autotr
275700040701     c                   move      H2Cpdc        S2Catd
275800040701e   2c                   endif
275900040701     c                   eval      S2Datd   = H2Dpdc
276000040701      *
276100040701w   1c                   When          $Found =  *on
276200040701     c                             and XR9nfv <> *zeros
276300040701     c                   move      XR9nfv        S2Cnfv
276400040701     c                   move      DSdata6       S2Ddfv
276500040701     c                   eval      S2Dfpp   = XR9fl1
276600050629      * - reperisco il flag per autotrasportatore Messaggeria
276700050629if  2c                   if            $Default = *off
276800050629     c                             and §OGpkm   = 'S'
276900050629     c                             and $VDL     = *on
277000050629     c                             and $IMA     = *off
277100050629     c                             and $PO      = *off
277200050701     c                   eval      FVVfgs   = V1Cpog
277300050701     c                   eval      FVVnpg   = 4
277400050629     c                   move      XR9nfv        FVVnfv
277500050629     c     K03FNFVV      chain     FNFVV000
277600050629if  3c                   if            %found(FNFVV03L)
277700050701     c                   eval      wNFVseq  = 1
277800050629     c                   movel     FVVspn        dFVVspn
277900050629     c                   exsr      X_Messaggeria
278000050629e   3c                   endif
278100050629e   2c                   endif
278200040701if  2c                   if        $IMA     = *off
278300040702     c                             and $PO  = *off
278400040701     c                   exsr      CercaIIfv
278500050629      * - reperisco il flag per autotrasportatore Messaggeria
278600050629if  3c                   if            $Default = *off
278700050629     c                             and §OGpkm   = 'S'
278800050629     c                             and $VDL     = *on
278900050629     c                             and $IMA     = *off
279000050629     c                             and $PO      = *off
279100171103     c                             and %found(FLTR902X)
279200050629     c                             and XR9nfv   > *zeros
279300050701     c                   eval      FVVfgs   = V1Cpog
279400050701     c                   eval      FVVnpg   = 4
279500050629     c                   move      XR9nfv        FVVnfv
279600050629     c     K03FNFVV      chain     FNFVV000
279700050629if  4c                   if            %found(FNFVV03L)
279800050701     c                   eval      wNFVseq  = 2
279900050629     c                   movel     FVVspn        dFVVspn
280000050629     c                   exsr      X_Messaggeria
280100050629e   4c                   endif
280200050629e   3c                   endif
280300040701e   2c                   endif
280400040701      *
280500040701e   1c                   EndSl
280600040701      *
280700040701     c                   ENDSR
280800040607      *
280900040607      *---------------------------------------------------------------*
281000040615      * Verifica se Postazione = P.O. (...magari DPD) !!!             *
281100040607      *---------------------------------------------------------------*
281200040607     c     Ctrl_PO       BEGSR
281300040607      *
281400040607if  1c                   if        $Hidden  = *off
281500040607     c                   reset                   $PO
281600040621     c                   reset                   $POdpd
281700080129     c                   reset                   $POerr
281800040607x   1c                   else
281900040607     c                   reset                   $POdft
282000040607e   1c                   endif
282100040607      *
282200040607if  1c                   if            Wpost  <> *blanks
282300040607     c                             and Wpost  <> C_PostazNull
282400040607     c                             and $IMA   =  *off
282500040607     c                             and %subst(Wpost:1:1) >= '0'
282600040607     c                             and %subst(Wpost:1:1) <= '9'
282700040607      *
282800040607     c                   eval      xy       = 1
282900040607     c                   eval      *in10    = *off
283000040607     c     Wpost         lookup    $ORGfil(xy)                            10
283100040607if  2c                   if        $Hidden  = *off
283200040607     c                   eval      $PO      = *in10
283300040607x   2c                   else
283400040607     c                   eval      $POdft   = *in10
283500040607e   2c                   endif
283600040607      *
283700040607if  2c                   if        $Hidden  = *off
283800040607     c                             and $PO  = *on
283900040611     c                   eval      $POdpd   = ($ORGntw(xy) = 'DPD')
284000080129     c                   eval      $POerr   = ($ORGerr(xy) = *on)
284100040607     c                   add       1             $ORGnr(xy)
284200040607e   2c                   endif
284300040607      *
284400040607e   1c                   endif
284500040607      *
284600040607     c                   ENDSR
284700040408      *
284800040408      *---------------------------------------------------------------*
284900040408      * Verifica se postazione "IMA"                                  *
285000040408      *---------------------------------------------------------------*
285100040408     c     Ctrl_IMA      BEGSR
285200040408      *
285300040423if  1c                   if        $Hidden  = *off
285400040408     c                   reset                   $IMA
285500040423x   1c                   else
285600040423     c                   reset                   $IMAdft
285700040423e   1c                   endif
285800040408      *
285900040423if  1c                   if            Wpost  <> *blanks
286000040423     c                             and Wpost  <> C_PostazNull
286100040408      *
286200040423     c                   eval      yy       = 1
286300040408     c                   eval      *in10    = *off
286400040423     c     Wpost         lookup    $MPT(yy)                               10
286500040423if  2c                   if        $Hidden  = *off
286600040408     c                   eval      $IMA     = *in10
286700040423x   2c                   else
286800040423     c                   eval      $IMAdft  = *in10
286900040423e   2c                   endif
287000040423      *
287100040408e   1c                   endif
287200040408      *
287300040408     c                   ENDSR
287400040705      *
287500040705      *---------------------------------------------------------------*
287600040705      * Impostazione F.V. da default per postazione = P.O. DPD        *
287700040705      *---------------------------------------------------------------*
287800040705     c     PostPOdpd     BEGSR
287900040705      *
288000040705     c                   eval      xx     = 1
288100040705     c                   eval      *in10  = *off
288200040705     c                   reset                   Wpost
288300040705if  1c                   if        $VDL   = *off
288400040705     c                   move      S1Cbai        Wpost
288500040705x   1c                   else
288600040705     c                   move      S2Cbai        Wpost
288700040705e   1c                   endif
288800040705      *
288900040705do  1c                   DOW       xx    <= %elem($FVxPOa)
289000040705      *
289100040705     c     Wpost         lookup    $FVxPOa(xx)                            10
289200040705      *
289300040705      * Nessun f.v. reperito per il DPD in esame
289400040705if  2c                   if        NOT *in10
289500040705     c                   leave
289600040705e   2c                   endif
289700040705      *
289800040705      * Scarto i f.v. con p.o. DPD tra "altri P.O." del f.v.
289900040705if  2c                   if            *in10       = *on
290000040705     c                             and $FVxPOn(xx) < *zeros
290100040705     c                   eval      xx     = xx + 1
290200040705     c                   iter
290300040705e   2c                   endif
290400040705      *
290500040705      * Propongo il 1° f.v. con LNA = p.o. DPD
290600040705if  2c                   IF        *in10  = *on
290700040705      *
290800040705     c                   reset                   WLBda8
290900040705     c                   reset                   DSdata8
291000040705     c                   reset                   DSdata6
291100040705     c                   eval      G08inv = $FVxPOd(xx)
291200040705     c                   call      'XSRDA8'
291300040705     c                   parm                    WLBda8
291400040705     c                   move      G08dat        DSdata8
291500040705     c                   z-add     DSdt8gg       DSdt6gg
291600040705     c                   z-add     DSdt8mm       DSdt6mm
291700040705     c                   z-add     DSdt8aa       DSdt6aa
291800040705if  3c                   if        $VDL   = *off
291900040705     c                   move      $FVxPOn(xx)   S1Cnfv
292000040705     c                   move      DSdata6       S1Ddfv
292100040705x   3c                   else
292200040705     c                   move      $FVxPOn(xx)   S2Cnfv
292300040705     c                   move      DSdata6       S2Ddfv
292400040705e   3c                   endif
292500040705     c                   leave
292600040705      *
292700040705e   2c                   ENDIF
292800040705      *
292900040705e   1c                   ENDDO
293000040705      *
293100040705     c                   ENDSR
293200031106      *
293300031106      *---------------------------------------------------------------*
293400040615      * Impostazione F.V. da default per postazione IMA               *
293500031106      *---------------------------------------------------------------*
293600040615     c     PostIMA       BEGSR
293700031106      *
293800031106     c                   eval      FVVfgs = V1Cpog
293900031106     c                   eval      FVVnpg = 3
294000031106     c                   eval      FVVnfv = *hival
294100031106     c     K03FNFVV      setgt     FNFVV000
294200031106     c     K02FNFVV      readpe    FNFVV000
294300031106      *
294400031106do  1c                   DOW       NOT %eof(FNFVV03L)
294500031106      *
294600031106if  2c                   IF            FVVatb = *blanks
294700031106     c                             and FVVfcf = *blanks
294800031106     c                             and FVVspg = 'A'
294900031106     c                   reset                   WLBda8
295000031106     c                   reset                   DSdata8
295100031106     c                   reset                   DSdata6
295200031106     c                   eval      G08inv = FVVdfv
295300031106     c                   call      'XSRDA8'
295400031106     c                   parm                    WLBda8
295500031106     c                   move      G08dat        DSdata8
295600031106     c                   z-add     DSdt8gg       DSdt6gg
295700031106     c                   z-add     DSdt8mm       DSdt6mm
295800031106     c                   z-add     DSdt8aa       DSdt6aa
295900031106if  3c                   if        $VDL   = *off
296000031106     c                   move      *zeros        S1Cnfv
296100031106     c                   move      FVVnfv        S1Cnfv
296200031106     c                   move      DSdata6       S1Ddfv
296300031106     c                   eval      S1Dfpp = FVVfpp
296400031106x   3c                   else
296500031106     c                   move      *zeros        S2Cnfv
296600031106     c                   move      FVVnfv        S2Cnfv
296700031106     c                   move      DSdata6       S2Ddfv
296800031106     c                   eval      S2Dfpp = FVVfpp
296900031106e   3c                   endif
297000031106     c                   leave
297100031106e   2c                   ENDIF
297200031106      *
297300031106     c     K02FNFVV      readpe    FNFVV000
297400031106e   1c                   ENDDO
297500031106      *
297600031106     c                   ENDSR
297700031106      *
297800031106      *---------------------------------------------------------------*
297900040615      * Impostazione F.V. da default per postazione non IMA           *
298000031106      *---------------------------------------------------------------*
298100040615     c     PostNoIMA     BEGSR
298200031106      *
298300031106     c                   eval      xx     = 1
298400031106     c                   eval      *in10  = *off
298500031106     c                   movel     V1Cpog        Wpdr
298600031106if  1c                   if        $VDL   = *off
298700031106     c                   move      S1Cpdc        Wpdr
298800031106x   1c                   else
298900031106     c                   move      S2Cpdc        Wpdr
299000031106e   1c                   endif
299100031106     c     Wpdr          lookup    $MEMat(xx)                             10
299200050701      *
299300031111if  1c                   IF        *in10  = *on
299400050701      *
299500050701     c                   clear                   dFVVspn
299600031111      * - 1° f.v.
299700031111if  2c                   IF        $MEMfv1(xx) > *zeros
299800031106      *
299900031111     c                   move      $MEMfv1(xx)   DS_FV
300000031106     c                   reset                   WLBda8
300100031106     c                   reset                   DSdata8
300200031106     c                   reset                   DSdata6
300300031106     c                   eval      G08inv = dsFVdfv
300400031106     c                   call      'XSRDA8'
300500031106     c                   parm                    WLBda8
300600031106     c                   move      G08dat        DSdata8
300700031106     c                   z-add     DSdt8gg       DSdt6gg
300800031106     c                   z-add     DSdt8mm       DSdt6mm
300900031106     c                   z-add     DSdt8aa       DSdt6aa
301000031111if  3c                   if        $VDL   = *off
301100031106     c                   move      dsFVnfv       S1Cnfv
301200031106     c                   move      DSdata6       S1Ddfv
301300031111     c                   eval      S1Dfpp = $MEMmp1(xx)
301400031111x   3c                   else
301500031106     c                   move      dsFVnfv       S2Cnfv
301600031106     c                   move      DSdata6       S2Ddfv
301700031111     c                   eval      S2Dfpp = $MEMmp1(xx)
301800050629      *   - imposto il flag per autotrasportatore Messaggeria
301900050614if  4c                   if            $Default = *off
302000050614     c                             and §OGpkm   = 'S'
302100180110     c*// (anche per Rulliera)     and $VDL     = *on
302200050614     c                             and $IMA     = *off
302300050614     c                             and $PO      = *off
302400050701     c                   eval      wNFVseq = 1
302500050701     c                   eval      §FVVtpm = $MEMpm1(xx)
302600050614     c                   exsr      X_Messaggeria
302700050614e   4c                   endif
302800031111e   3c                   endif
302900031111      *
303000031111e   2c                   ENDIF
303100031111      * - 2° f.v.
303200031111if  2c                   IF        $MEMfv2(xx) > *zeros
303300031111      *
303400031111     c                   move      $MEMfv2(xx)   DS_FV
303500031111     c                   reset                   WLBda8
303600031111     c                   reset                   DSdata8
303700031111     c                   reset                   DSdata6
303800031111     c                   eval      G08inv = dsFVdfv
303900031111     c                   call      'XSRDA8'
304000031111     c                   parm                    WLBda8
304100031111     c                   move      G08dat        DSdata8
304200031111     c                   z-add     DSdt8gg       DSdt6gg
304300031111     c                   z-add     DSdt8mm       DSdt6mm
304400031111     c                   z-add     DSdt8aa       DSdt6aa
304500031111if  3c                   if        $VDL   = *off
304600031111     c                   move      dsFVnfv       S3Cnfv
304700031111     c                   move      DSdata6       S3Ddfv
304800031111     c                   eval      S3Dfpp = $MEMmp2(xx)
304900031111x   3c                   else
305000031111     c                   move      dsFVnfv       S4Cnfv
305100031111     c                   move      DSdata6       S4Ddfv
305200031111     c                   eval      S4Dfpp = $MEMmp2(xx)
305300050629      *   - imposto il flag per autotrasportatore Messaggeria
305400050614if  4c                   if            $Default = *off
305500050614     c                             and §OGpkm   = 'S'
305600180110     c*// (anche per Rulliera)     and $VDL     = *on
305700050614     c                             and $IMA     = *off
305800050614     c                             and $PO      = *off
305900050701     c                   eval      wNFVseq = 2
306000050701     c                   eval      §FVVtpm = $MEMpm2(xx)
306100050614     c                   exsr      X_Messaggeria
306200050614e   4c                   endif
306300031111e   3c                   endif
306400031111      *
306500031111e   2c                   ENDIF
306600031106      *
306700031106e   1c                   ENDIF
306800031106      *
306900031106     c                   ENDSR
307000031106      *
307100031106      *---------------------------------------------------------------*
307200031106      * Reperimento eventuale secondo F.V. per postazione/baia        *
307300031106      *---------------------------------------------------------------*
307400031106     c     CercaIIfv     BegSr
307500031106      *
307600040621      * (Se modifica del "piano giornaliero" per P.O. con/senza VDL)
307700040621      *
307800040621     c                   eval      XR9prg = XR9prg + 1
307900171103     c     K07TR902      chain     FLTR902
308000031106      *
308100040621      * Se esiste: ne visualizzo i dati del f.v.
308200171103if  1c                   if        %found(FLTR902X)
308300040309     c                   reset                   WLBda8
308400040309     c                   reset                   DSdata8
308500040309     c                   reset                   DSdata6
308600040621if  2c                   if        XR9dfv   > *zeros
308700040621     c                   eval      G08inv   = XR9dfv
308800040309     c                   call      'XSRDA8'
308900040309     c                   parm                    WLBda8
309000040309     c                   move      G08dat        DSdata8
309100040309     c                   z-add     DSdt8gg       DSdt6gg
309200040309     c                   z-add     DSdt8mm       DSdt6mm
309300040309     c                   z-add     DSdt8aa       DSdt6aa
309400040309e   2c                   endif
309500040315if  2c                   if        $VDL   = *off
309600040621     c                   move      XR9nfv        S3Cnfv
309700040621     c                   eval      S3Dfpp   = XR9fl1
309800040621if  3c                   if        XR9dfv   > *zeros
309900031106     c                   move      DSdata6       S3Ddfv
310000040309e   3c                   endif
310100040315x   2c                   else
310200040621     c                   move      XR9nfv        S4Cnfv
310300040621     c                   eval      S4Dfpp   = XR9fl1
310400040621if  3c                   if        XR9dfv   > *zeros
310500040315     c                   move      DSdata6       S4Ddfv
310600040315e   3c                   endif
310700040315e   2c                   endif
310800031106e   1c                   endif
310900031106      *
311000031106     c                   ENDSR
311100031007      *
311200031007      *---------------------------------------------------------------*
311300031007      * Controllo dati immessi in videata S01 & S03                   *
311400031007      *---------------------------------------------------------------*
311500031007     c     CtrS01        BEGSR
311600031007      *
311700031007      * Ciclo di lettura del sfl
311800031007if  1c                   if        $Default = *on
311900031007     c                   readc     MS03S01
312000031007x   1c                   else
312100031007     c                   readc     MS03S03
312200031007e   1c                   endif
312300031020      *
312400031007do  1c                   DOW       NOT %eof(FIMS03D)
312500031007      *
312600031007     c                   movea     *zeros        *in(41)
312700031007     c                   z-add     S01nrr        C01rcd
312800031007      * - ctrl su singola riga
312900031014if  2c                   if            S1Cpep  <> *blanks
313000031014     c                             or  S1Cdes  <> *blanks
313100031014     c                             or  S1Cbai  <> *blanks
313200031014     c                             or (S1Cpdc  <> *zeros
313300031014     c                             and S1Cpdc  <> *blanks)
313400031014     c                             or (S1Cnfv  <> *blanks
313500031014     c                             and S1Cnfv  <> *zeros)
313600031007     c                   exsr      CtrS01rec
313700031020if  3c                   if            $Default = *off
313800031007     c                             and *in90    = *off
313900040518     c                             and S1Cbai  <> C_PostazNull
314000031007     c                   exsr      CtrS03rec
314100031007e   3c                   endif
314200031007x   2c                   else
314300031007     c                   clear                   S1Ddfv
314400031021     c                   clear                   S1Dfpp
314500031022     c                   clear                   S3Ddfv
314600031022     c                   clear                   S3Dfpp
314700031023     c                   clear                   S1Cast
314800031007     c                   clear                   S1Cpdc
314900031007     c                   clear                   S1Dpdc
315000031007     c                   clear                   S3Dpdc
315100031023     c                   clear                   S1Cdes
315200031023     c                   clear                   S1Kdft
315300031023     c                   clear                   S1Kbad
315400031023     c                   clear                   S1Cbad
315500031023     c                   clear                   S1Katd
315600031023     c                   clear                   S1Catd
315700031023     c                   clear                   S1Datd
315800031007e   2c                   endif
315900031010      *
316000031010      * - reimposto indicatori x DSPATR condizionati
316100040702      *   se NON rilevati errori
316200040702if  2c                   if        NOT *in90
316300040702     c                             and $Default = *on
316400040702     c                             or  $Default = *off
316500060110     c                   exsr      AtrS01S03
316600040702e   2c                   endif
316700031007      *
316800031007      * - riattivo SFLNXTCHG (non solo se rilevato errore)
316900031007     c                   eval      *in32    = *on
317000031007      * - aggiorno il SFL
317100031007if  2c                   if        $Default = *on
317200031007     c                   update    MS03S01
317300031007x   2c                   else
317400031007     c                   update    MS03S03
317500031007e   2c                   endif
317600031007     c                   eval      *in32    = *off
317700031007      * - se rilevato errore esco dal ciclo di controllo
317800031007if  2c                   if        *in28    = *on
317900031104     c                   z-add     C01rcd        C01csr
318000031007     c                   leave
318100031007e   2c                   endif
318200031007      * - leggo il record successivo
318300031007if  2c                   if        $Default = *on
318400031007     c                   readc     MS03S01
318500031007x   2c                   else
318600031007     c                   readc     MS03S03
318700031007e   2c                   endif
318800031007      *
318900031007e   1c                   ENDDO
319000031020      *
319100040423      * Controlli finali "generali"
319200031021      *   (se NON ho già rilevato altri errori da segnalare)
319300040423if  1c                   if        *in28    = *off
319400031021     c                   exsr      CtrBAIxPEP
319500031020e   1c                   endif
319600031007      *
319700031007     c                   ENDSR
319800031007      *
319900031007      *---------------------------------------------------------------*
320000031007      * Controllo singolo record del subfile S01 & S03                *
320100031007      *---------------------------------------------------------------*
320200031007     c     CtrS01rec     BEGSR
320300031023      *
320400031023if  1c                   if        $Default = *off
320500031023     c                   clear                   S1Kdft
320600031023     c                   clear                   S1Kbad
320700031023     c                   clear                   S1Cbad
320800031023     c                   clear                   S1Katd
320900031023     c                   clear                   S1Catd
321000031023     c                   clear                   S1Datd
321100031023e   1c                   endif
321200040423      *
321300040423     c                   reset                   $IMA
321400040423     c                   reset                   $IMAdft
321500031007      *
321600040305      * Controllo codice baia
321700031007if  1c                   if        S1Cpep = *blanks
321800031007     c                   seton                                        512890
321900031009     c                   movel     $Msg(10)      V1Dmsg
322000031007x   1c                   else
322100031010     c                   move(p)   S1Cpep        w014a
322200031010     c     w014a         lookup    $MSLpep                                10
322300031007if  2c                   if        NOT *in10
322400031007     c                   seton                                        512890
322500031009     c                   movel     $Msg(11)      V1Dmsg
322600060110     c                   leavesr
322700031007e   2c                   endif
322800040226     c                   exsr      ContaPEP
322900031007e   1c                   endif
323000060110     c                   if        *in90
323100060110     c                   leavesr
323200060110     c                   endif
323300031007      *
323400031007      * Controllo descrizione
323500031014      * (non obbligatoria)
323600031007      *
323700040305      * Controllo codice postazione
323800040423      * - ricerca
323900040423     c     '?'           scan      S1Cbai                                 10
324000040423if  1c                   if        *in10  =  *on
324100040423     c                   eval      S1Cbai =  *blanks
324200040423     c                   eval      Wcampo =  'S1CBAI    '
324300040423     c                   exsr      srSEARCH
324400040423e   1c                   endif
324500040423      * - controllo
324600040423if  1c                   if        $Default = *off
324700040423     c                   eval      $Hidden  = *on
324800040423     c                   eval      Wpost    = H1Cbai
324900040423     c                   exsr      Ctrl_IMA
325000040423if  2c                   if        $IMAdft  = *on
325100040423     c                   eval      H1Dpdc   = $MPTdes(yy)
325200040423e   2c                   endif
325300040423e   1c                   endif
325400040423     c                   eval      $Hidden  = *off
325500040423     c                   eval      Wpost    = S1Cbai
325600040408     c                   exsr      Ctrl_IMA
325700040423if  1c                   if        $IMA     = *on
325800040423     c                   eval      S1Dpdc   = $MPTdes(yy)
325900040423     c                   eval      S3Dpdc   = $MPTdes(yy)
326000040423e   1c                   endif
326100040423if  1c                   if            $Default = *off
326200040423     c                             and $IMAdft  = *on
326300040423     c                             and H1Cbai  <> S1Cbai
326400040423     c                   eval      S1Datd   = H1Dpdc
326500040423e   2c                   endif
326600031007sel 1c                   select
326700031020w   1c                   when          $Default = *off
326800040316     c                             and S1Cbai   = C_PostazNull
326900031020     c                   clear                   S1Cnfv
327000031020     c                   clear                   S1Ddfv
327100031021     c                   clear                   S1Dfpp
327200031211     c                   clear                   S3Cnfv
327300031211     c                   clear                   S3Ddfv
327400031211     c                   clear                   S3Dfpp
327500031020     c                   clear                   S3Dpdc
327600040408     c                   clear                   S1Cpdc
327700031007w   1c                   when      S1Cbai   =  *blanks
327800031007     c                   seton                                        532890
327900031009     c                   movel     $Msg(13)      V1Dmsg
328000070411w   1c                   when         (%subst(S1Cbai:1:1) >= 'A'
328100070411     c                             and %subst(S1Cbai:1:1) <= 'Z')
328200031010     c                             and %subst(S1Cbai:2:2) >= '01'
328300031010     c                             and %subst(S1Cbai:2:2) <= '99'
328400031007     c                   move      S1Cbai        w002a
328500031007     c     DigitN        check     w002a                                  10
328600031009if  2c                   if        *in10  =  *on
328700031007     c                   seton                                        532890
328800031009     c                   movel     $Msg(14)      V1Dmsg
328900031007x   2c                   else
329000040423     c                   eval      Wpost  =  S1Cbai
329100031007     c                   exsr      ContaBAI
329200031007if  3c                   if        $BAInr(xx) >  1
329300031007     c                   seton                                        532890
329400031009     c                   movel     $Msg(15)      V1Dmsg
329500031007e   3c                   endif
329600031007e   2c                   endif
329700040408w   1c                   when      $IMA   =  *off
329800031007     c                   seton                                        532890
329900031009     c                   movel     $Msg(14)      V1Dmsg
330000031007e   1c                   endsl
330100060110     c                   if        *in90
330200060110     c                   leavesr
330300060110     c                   endif
330400040316      *
330500040316      * Controllo la postazione sia "compatibile" con le altre già
330600040316      *   immesse nella stessa baia
330700161110      * -?Su richiesta di Nicola:?tolto controllo postaz. IMA/non-IMA
330800161110      *   (ora IMA e AUT possono stare nella stessa baia;
330900161110      *   sono le FILIALI che continuano a richiedere una loro baia -
331000161110      *   - MA SONO INSERIBILI SOLO NEI PIANI VDL, non in questi)
331100040316      *
331200040305      * ...e ne memorizzo gli utilizzi per baia
331300040316if  1c                   if        S1Cbai <> C_PostazNull
331400031021     c                   exsr      MemBAIxPEP
331500031020e   1c                   endif
331600040315      *
331700040305      * Visualizzo la postazione di default se variata in piano
331800040305      *   giornaliero
331900031023if  1c                   if            $Default = *off
332000031023     c                             and S1Cbai  <> H1Cbai
332100040423     c                   eval      S1Kdft  = C_Default
332200040625     c                   eval      S1Kbad  = C_Postazione
332300040423     c                   eval      S1Cbad  = H1Cbai
332400040423if  2c                   if        $IMAdft = *on
332500040423     c                   eval      S1Datd  = H1Dpdc
332600040423e   2c                   endif
332700031023e   1c                   endif
332800031007      *
332900031007      * Controllo codice autotrasportatore
333000031007      *   (in S03 e S04 è campo protetto)
333100031015      * Per il piano giornaliero farò appositi controlli nella subr.
333200031015      *   CTRS03REC
333300031007if  1c                   if        $Default = *on
333400040423if  2c                   if        $IMA     = *off
333500031007     c                   clear                   S1Dpdc
333600031009     c                   clear                   S3Dpdc
333700040423e   2c                   endif
333800031007      * - se *blanks lo considero *zeros (sarebbe numerico)
333900031007if  2c                   if        S1Cpdc =  *blanks
334000031007     c                   eval      S1Cpdc =  *zeros
334100031007e   2c                   endif
334200031007      * - ricerca
334300031007     c     '?'           scan      S1Cpdc                                 10
334400031007if  2c                   if        *in10  =  *on
334500031007     c                   eval      S1Cpdc =  *zeros
334600031007     c                   eval      Wcampo =  'S1CPDC    '
334700031007     c                   exsr      srSEARCH
334800031007e   2c                   endif
334900031007      * - controllo
335000031007sel 2c                   select
335100040409w   2c                   when         ($IMA   =  *on
335200040409     c                              or S1Cbai =  C_PostazNull)
335300031010     c                             and S1Cpdc =  *zeros
335400040409w   2c                   when          $IMA   =  *on
335500031007     c                             and S1Cpdc <> *zeros
335600031007     c                   seton                                        552890
335700031009     c                   movel     $Msg(16)      V1Dmsg
335800031015x   2c                   other
335900031021     c                   movel     V1Cpog        APDpdr
336000031021     c                   move      S1Cpdc        APDpdr
336100031014     c                   exsr      CtrlAutotr2
336200031007     c                   movel     APDrsc        S1Dpdc
336300031009     c                   movel     APDrsc        S3Dpdc
336400031007e   2c                   endsl
336500060110     c                   if        *in90
336600060110     c                   leavesr
336700060110     c                   endif
336800180110      * - imposto il flag per autotrasportoatore Messaggeria
336900180110if  3c                   if            $Default = *on
337000180110     c                             and §OGpkm   = 'S'
337100180110     c*//                          and $VDL     = *off
337200180110     c                             and $IMA     = *off
337300180110     c                             and $PO      = *off
337400180110     c                             and S1Cbai  <> C_PostazNull
337500180110     c                             and S1Cbai  <> *blanks
337600180110     c                   eval      wNFVseq = 1
337700180110     c                   movel     APDflr        dAPDflr
337800180110     c                   exsr      X_Messaggeria
337900180110e   3c                   endif
338000031007      * - se *zeros lo reimposto a *blanks (numerico "editato")
338100031007if  2c                   if        S1Cpdc =  *zeros
338200031007     c                   eval      S1Cpdc =  *blanks
338300031007e   2c                   endif
338400031007e   1c                   endif
338500031027      *
338600040305      * In caso di piano giornaliero controllo se è stata tolta la
338700040305      *   postazione ("XXX"), perchè in tal caso non verrà eseguita
338800040305      *   la subr. per i controlli specifici, pertanto non verrebbe
338900040409      *   evidenziato l'autotrasportatore pre-impostato nel default
339000031027if  1c                   if            $Default = *off
339100040316     c                             and S1Cbai   = C_PostazNull
339200031027     c                   clear                   S1Cpdc
339300031027     c                   clear                   S3Dpdc
339400031027     c                   exsr      DecS03pdr
339500031027e   1c                   endif
339600031007      *
339700060110     c                   ENDSR
339800031022      *
339900031022      *---------------------------------------------------------------*
340000031022      * Controlli aggiuntivi per singolo record del subfile S03       *
340100031022      *---------------------------------------------------------------*
340200031022     c     CtrS03rec     BEGSR
340300031022      *
340400031022     c                   reset                   SAVnfv
340500031022      *
340600031022      * Se impostato solo il 2° numero F.V. lo sposto al 1°
340700031022if  1c                   if            (S1Cnfv  =  *blanks
340800031022     c                             or   S1Cnfv  =  *zeros)
340900031022     c                             and (S3Cnfv  <> *blanks
341000031022     c                             and  S3Cnfv  <> *zeros)
341100031022     c                   eval      S1Cnfv  =  S3Cnfv
341200031022     c                   clear                   S3Cnfv
341300031022     c                   clear                   S3Ddfv
341400031022     c                   clear                   S3Dfpp
341500031022e   1c                   endif
341600031022      *
341700031022      * Controllo 1° numero foglio viaggio
341800031022     c                   clear                   S1Ddfv
341900031022     c                   clear                   S1Dfpp
342000031022     c                   eval      wNFVseq =  1
342100031022     c                   eval      Wnfv    =  S1Cnfv
342200031106     c                   exsr      Ctrl_1FV
342300031022     c                   eval      S1Cnfv  =  Wnfv
342400060110     c                   if        *in90
342500060110     c                   leavesr
342600060110     c                   endif
342700031024      * Se tutto bene: ne imposto i dati a video
342800040305if  1c                   if        Wnfv    <> *blanks
342900031022     c                   eval      S1Dfpp  =  FVVfpp
343000031022     c                   move      DSdata6       S1Ddfv
343100040305e   1c                   endif
343200031024      *
343300031024      * Imposto flag e decodifico variazioni relative all'autotrasp.
343400031024      *   (che deve essere comune ad entrambi i F.V.)
343500040312if  1c                   if            S1Cnfv  <> *blanks
343600040408     c                             and $IMA    =  *off
343700031024     c                   move      APDpdr        S1Cpdc
343800031024     c                   movel     APDrsc        S3Dpdc
343900031024x   1c                   else
344000031024     c                   clear                   S1Cpdc
344100040423if  2c                   if        $IMA     = *off
344200031024     c                   clear                   S3Dpdc
344300040423e   2c                   endif
344400031024e   1c                   endif
344500031024     c                   exsr      DecS03pdr
344600031022      *
344700040312      * Controllo 2° numero foglio viaggio (se immesso)
344800031022     c                   clear                   S3Ddfv
344900031022     c                   clear                   S3Dfpp
345000040312if  1c                   if            S3Cnfv <> *blanks
345100040312     c                             and S3Cnfv <> *zeros
345200031022     c                   eval      wNFVseq =  2
345300031022     c                   eval      Wnfv    =  S3Cnfv
345400031106     c                   exsr      Ctrl_1FV
345500031022     c                   eval      S3Cnfv  =  Wnfv
345600060110     c                   if        *in90
345700060110     c                   leavesr
345800060110     c                   endif
345900031024      * Se tutto bene: ne imposto i dati a video
346000031024if  2c                   if        Wnfv    <> *blanks
346100031022     c                   eval      S3Dfpp  =  FVVfpp
346200031022     c                   move      DSdata6       S3Ddfv
346300031024e   2c                   endif
346400031111      * e rieseguo il controllo sul numero di F.V. x autotrasportatore
346500031111      *   (se ne ho selezionato uno da ricerca)
346600031111if  2c                   if        S1Cast  =  '?'
346700031111     c                   clear                   S1Cast
346800031111     c                   exsr      CtrlAutotr3
346900031111     c                   eval      S1Cast =  Wast
347000031111e   2c                   endif
347100040312x   1c                   else
347200040312     c                   clear                   S3Cnfv
347300040312e   1c                   endif
347400031022      *
347500060110     c                   ENDSR
347600031007      *
347700031007      *---------------------------------------------------------------*
347800031106      * Controlli per singolo F.V. del subfile S03 o S04              *
347900031007      *---------------------------------------------------------------*
348000031106     c     Ctrl_1FV      BEGSR
348100040621      *
348200040621     c                   reset                   Wdfv
348300031007      *
348400031007      * Controllo numero foglio viaggio
348500031007      * - se *blanks lo considero *zeros (sarebbe numerico)
348600031106if  1c                   if        Wnfv    =  *blanks
348700031106     c                   eval      Wnfv    =  *zeros
348800031007e   1c                   endif
348900031007      * - ricerca
349000031022     c     '?'           scan      Wnfv                                   10
349100031106if  1c                   if        *in10   =  *on
349200031106     c                   eval      Wnfv    =  *zeros
349300031106sel 2c                   select
349400031106w   2c                   when          $VDL    =  *off
349500031106     c                             and wNFVseq =  1
349600031106     c                   eval      Wcampo  =  'S1CNFV    '
349700031106w   2c                   when          $VDL    =  *off
349800031106     c                             and wNFVseq =  2
349900031106     c                   eval      Wcampo  =  'S3CNFV    '
350000031106w   2c                   when          $VDL    =  *on
350100031106     c                             and wNFVseq =  1
350200031106     c                   eval      Wcampo  =  'S2CNFV    '
350300031106w   2c                   when          $VDL    =  *on
350400031106     c                             and wNFVseq =  2
350500031106     c                   eval      Wcampo  =  'S4CNFV    '
350600031106e   2c                   endsl
350700031007     c                   exsr      srSEARCH
350800031022      * - - se non selezionato nulla sul 2° F.V. -> esco
350900031022if  2c                   if            wNFVseq =  2
351000031022     c                             and Wnfv    =  *zeros
351100031106     c                   eval      Wnfv    =  *blanks
351200060110     c                   leavesr
351300031022e   2c                   endif
351400031111      * - - se selezionato: memorizzo che dovrò ri-controllare se
351500031111      *     padroncino con altri f.v. (non visualizzati)
351600031111     c                   eval      S1Cast =  '?'
351700031007e   1c                   endif
351800031007      * - controlli:
351900031007      * - - numericità
352000031022     c     DigitN        check     Wnfv                                   10
352100031106if  1c                   if        *in10   =  *on
352200031106if  2c                   if        wNFVseq =  2
352300040309     c                   eval      *in56   =  *on
352400031022x   2c                   else
352500040309     c                   eval      *in54   =  *on
352600031022e   2c                   endif
352700031022     c                   seton                                          2890
352800040309     c                   eval      V1Dmsg  =  $Msg(20)
352900060110     c                   leavesr
353000031007e   1c                   endif
353100031007      *
353200031010      * - - effettiva immissione
353300031106if  1c                   if        Wnfv    =  *zeros
353400031106      *
353500031106     c                   clear                   FVVdfv
353600031106     c                   clear                   FVVfpp
353700031106      *
353800080129sel 2c                   select
353900080129      * - - - F.V.?non?più obbligatorio per fil. DPD
354000080129      *       (visualizzato solo un msg di avvertimento)
354100080129w   2c                   when          $VDL    =  *on
354200080129     c                             and $POdpd  =  *on
354300080129     c                             and $POerr  =  *off
354400080129     c                   eval      $POerr  = *on
354500080129     c                   seton                                        542890
354600080129     c                   eval      V1Dmsg  =  $Msg(50)
354700080129      * - - - F.V. obbligatorio per postazione regolare
354800080129w   2c                   when      $POdpd  =  *off
354900080129if  3c                   if        wNFVseq =  2
355000040309     c                   eval      *in56   =  *on
355100080129x   3c                   else
355200040309     c                   eval      *in54   =  *on
355300080129e   3c                   endif
355400031022     c                   seton                                          2890
355500080129if  3c                   if        $IMA    =  *off
355600040312     c                   eval      V1Dmsg  =  $Msg(7)
355700080129x   3c                   else
355800040309     c                   eval      V1Dmsg  =  $Msg(30)
355900080129e   3c                   endif
356000080129e   2c                   endsl
356100080129     c                   if        *in90
356200060110     c                   leavesr
356300080129     c                   endif
356400031106      *
356500031010x   1c                   else
356600031106      *
356700040611      * - - univocità per postazione
356800031030sel 2c                   select
356900031106w   2c                   when      wNFVseq =  1
357000031106     c                   eval      SAVnfv  =  Wnfv
357100040610      * - - - ammesso un solo F.V. per postazione "IMA"
357200031106w   2c                   when          wNFVseq =  2
357300040408     c                             and $IMA    =  *on
357400031106     c                   seton                                        562890
357500040309     c                   eval      V1Dmsg  =  $Msg(37)
357600040611      * - - - ammesso un solo F.V. per postazione P.O. DPD
357700040610w   2c                   when          wNFVseq =  2
357800040611     c                             and $POdpd  =  *on
357900040610     c                   seton                                        562890
358000040610     c                   eval      V1Dmsg  =  $Msg(42)
358100040610      * - - - non ammessi 2 F.V. identici
358200031106w   2c                   when          wNFVseq =  2
358300031106     c                             and SAVnfv  =  Wnfv
358400040408     c                             and $IMA    =  *off
358500031022     c                   seton                                        562890
358600040309     c                   eval      V1Dmsg  =  $Msg(35)
358700031106e   2c                   endsl
358800060110     c                   if        *in90
358900060110     c                   leavesr
359000060110     c                   endif
359100031106      *
359200171107      * - - univocità per foglio arrivi
359300171107      *     (per la fil. 006-PD: sia nel piano VDL che Rulliera)
359400171107if  2c                   if             V1Cpog  =  c_PO_a
359500171107     c                             and  $IMA    =  *off
359600171107     c                             and  $PO     =  *off
359700171107     c                             and  %lookup( %int( wNFV ) :
359800171107     c                                           sk_NFV2 ) > *zero
359900171107if  3c                   if        wNFVseq     =  2
360000171107     c                   eval      *in56   =  *on
360100171107x   3c                   else
360200171107     c                   eval      *in54   =  *on
360300171107e   3c                   endif
360400171107     c                   seton                                          2890
360500171107     c                   if        V1Ctps  = 'V'
360600171107     c                   eval      V1Dmsg  =  %trimR( $Msg(40) ) +
360700171107     c                                       ' Rulliera'
360800171107     c                   else
360900171107     c                   eval      V1Dmsg  =  %trimR( $Msg(40) ) +
361000171107     c                                       ' VDL'
361100171107     c                   endif
361200171107     c                   leavesr
361300171107e   2c                   endif
361400031030      * - - univocità per foglio arrivi
361500171107      *     (in questo piano smistamento)
361600040607if  2c                   if            $IMA    =  *off
361700040607     c                             and $PO     =  *off
361800031030     c                   exsr      ContaNFV
361900031106if  3c                   if        $NFVnr(xx)  >  1
362000031106if  4c                   if        wNFVseq     =  2
362100040309     c                   eval      *in56   =  *on
362200031030x   4c                   else
362300040309     c                   eval      *in54   =  *on
362400031030e   4c                   endif
362500031030     c                   seton                                          2890
362600040518     c                   eval      V1Dmsg  =  %replace(dsBPpos:
362700031106     c                                        $Msg(39):
362800040312     c                                        %scan('&PS':$Msg(39)))
362900060110     c                   leavesr
363000031030e   3c                   endif
363100031030e   2c                   endif
363200031007      * - - esistenza in FNFVV
363300040615if  2c                   if        $PO     =  *off
363400031106     c                   eval      FVVfgs  =  V1Cpog
363500040615if  3c                   if        $IMA    =  *on
363600031106     c                   eval      FVVnpg  =  3
363700040615x   3c                   else
363800031106     c                   eval      FVVnpg  =  4
363900040615e   3c                   endif
364000031022     c                   move      Wnfv          FVVnfv
364100031007     c     K03FNFVV      chain     FNFVV000
364200040615if  3c                   if        NOT %found(FNFVV03L)
364300031106     c*** (già da vl)              or  FVVatb  <> *blanks
364400031106     c*** (già da vl)              or  FVVfcf  <> *blanks
364500040414     c                             or ($IMA    =  *on
364600040408     c                             and FVVspg  <> 'A')
364700040615if  4c                   if        wNFVseq =  2
364800040309     c                   eval      *in56   =  *on
364900040615x   4c                   else
365000040309     c                   eval      *in54   =  *on
365100040615e   4c                   endif
365200031022     c                   seton                                          2890
365300040309     c                   eval      V1Dmsg  =  $Msg(21)
365400060110     c                   leavesr
365500040615e   3c                   endif
365600040621     c                   eval      Wdfv    =  FVVdfv
365700050614      * - - - distinta Messaggeria per piano di smistamento
365800050701if  3c                   if            §OGpkm  =  'S'
365900180110     c*// (anche per Rulliera)     and $VDL    =  *on
366000050701     c                             and $IMA    =  *off
366100050701     c                             and $PO     =  *off
366200050701     c                             and S2Cbai  <> *blanks
366300050614     c                             and S2Cbai  <> C_PostazNull
366400050629     c                   movel     FVVspn        dFVVspn
366500050614     c                   exsr      X_Messaggeria
366600050614e   3c                   endif
366700040615      * - - esistenza in FNFGV
366800040615x   2c                   else
366900040615     c                   exsr      CtrlFVxPO
367000060110     c                   if        *in90
367100060110     c                   leavesr
367200060110     c                   endif
367300040615e   2c                   endif
367400031007      * - - univocità F.V. IMA nello stesso piano
367500040408if  2c                   if        $IMA    =  *on
367600031106if  3c                   if        SAVnfvIMA   =  *zeros
367700031022     c                   move      Wnfv          SAVnfvIMA
367800031007e   3c                   endif
367900031106if  3c                   if        Wnfv    <> SAVnfvIMA
368000040615if  4c                   if        wNFVseq =  2
368100040309     c                   eval      *in56   =  *on
368200031022x   4c                   else
368300040309     c                   eval      *in54   =  *on
368400031022e   4c                   endif
368500031022     c                   seton                                          2890
368600040518     c                   eval      V1Dmsg  =  %replace(SAVnfvIMA:
368700031106     c                                        $Msg(22):
368800040408     c                                        %scan('&FIMA':$Msg(22)))
368900060110     c                   leavesr
369000031007e   3c                   endif
369100040408      * - - - pulisco l'eventuale cod. autotrasportatore se F.V. IMA
369200040423      *       e ne imposto la decodifica da tab. "MPT"
369300040408if  3c                   if        $VDL    =  *off
369400040408     c                   clear                   S1Cpdc
369500040423     c*** già decodif.:  clear                   S3Dpdc
369600040408x   3c                   else
369700040408     c                   clear                   S2Cpdc
369800040423     c*** già decodif.:  clear                   S4Dpdc
369900040408e   3c                   endif
370000031007e   2c                   endif
370100040615      * - - controllo F.V. non IMA né per P.O. DPD
370200040408if  2c                   if        $IMA    =  *off
370300040615     c                             and $PO =  *off
370400031022      * - - - F.V. di data antecedente a quella del F.V. arrivi
370500031106if  3c                   if        FVVdfv  <  SAVdfva
370600031106if  4c                   if        wNFVseq =  2
370700040309     c                   eval      *in56   =  *on
370800031022x   4c                   else
370900040309     c                   eval      *in54   =  *on
371000031022e   4c                   endif
371100031022     c                   seton                                          2890
371200040309     c                   eval      V1Dmsg  =  $Msg(38)
371300060110     c                   leavesr
371400031022e   3c                   endif
371500031022      * - - - F.V. non IMA assegnato ad altro autotrasportatore
371600040519      *       o ad autotrasportatore già impegnato in altra baia/post.
371700031014     c                   exsr      CtrlAutotr1
371800060110     c                   if        *in90
371900060110     c                   leavesr
372000060110     c                   endif
372100040519      * - - - Controllo/decodifica codice autotrasportatore
372200031106if  3c                   if        wNFVseq <  2
372300031106     c                   eval      APDpdr  =  DSTpdr
372400031020     c                   exsr      CtrlAutotr2
372500060110     c                   if        *in90
372600060110     c                   leavesr
372700060110     c                   endif
372800031022e   3c                   endif
372900031024e   2c                   endif
373000031024e   1c                   endif
373100031007      *
373200031007      * Se OK imposto la data
373300031106     c                   reset                   WLBda8
373400031022     c                   reset                   DSdata8
373500031022     c                   reset                   DSdata6
373600031106if  1c                   if        Wnfv    <> *zeros
373700040621     c                   eval      G08inv  =  Wdfv
373800031007     c                   call      'XSRDA8'
373900031007     c                   parm                    WLBda8
374000031021     c                   move      G08dat        DSdata8
374100031021     c                   z-add     DSdt8gg       DSdt6gg
374200031021     c                   z-add     DSdt8mm       DSdt6mm
374300031021     c                   z-add     DSdt8aa       DSdt6aa
374400031106e   1c                   endif
374500031014      * Se *zeros lo reimposto a *blanks (numerico "editato")
374600031106if  1c                   if        Wnfv    =  *zeros
374700031106     c                   eval      Wnfv    =  *blanks
374800031014e   1c                   endif
374900031111      * memorizzo il F.V. tra quelli immessi nel SFL
375000031111if  1c                   if        Wnfv    >  *zeros
375100031111     c                   eval      xx      =  1
375200031111     c                   eval      *in10   =  *off
375300031111     c     *zeros        lookup    $FV(xx)                                10
375400031111if  2c                   if        *in10   =  *on
375500031111     c                   move      Wnfv          $FV(xx)
375600031111e   2c                   endif
375700031111e   1c                   endif
375800031007      *
375900060110     c                   ENDSR
376000040615      *
376100040615      *---------------------------------------------------------------*
376200040621      * Controlli per singolo foglio viaggio per punto operativo      *
376300040615      *---------------------------------------------------------------*
376400040615     c     CtrlFVxPO     BEGSR
376500040615      *
376600040621      * - Verifica esistenza tra i f.v. già memorizzati
376700040621      *   (con data >= a quella del f.v. V1CNFV)
376800040621     c                   clear                   Wnfv7
376900040621     c                   move      Wnfv          Wnfv7
377000040615     c                   eval      xx      =  1
377100040615     c                   eval      *in10   =  *off
377200040621     c     Wnfv7         lookup    $FVxPOn(xx)                            10
377300070521      * - verifica se F.V. assegnabile a più filiali di "passaggio"
377400070521if  1c                   dow           *in10       =  *on
377500070521     c                             and $FVxPOa(xx) <> Wpost
377600070521     c                             and xx          <  %elem($FVxPOn)
377700070521     c                   eval      xx      =  xx + 1
377800070521     c                   eval      *in10   =  *off
377900070521     c     Wnfv7         lookup    $FVxPOn(xx)                            10
378000070521e   1c                   enddo
378100040621sel 1c                   select
378200040621      * - - ok
378300040621w   1c                   when          *in10       =  *on
378400040621     c                             and $FVxPOa(xx) =  Wpost
378500040621     c                   eval      Wdfv    =  $FVxPOd(xx)
378600060110     c                   leavesr
378700040621      * - - assegnato a P.O. diverso da quello indicato come postazione
378800070521w   1c                   when          *in10       =  *on
378900070521     c                             and $FVxPOa(xx) <> Wpost
379000070521if  2c                   if        wNFVseq =  2
379100070521     c                   eval      *in56   =  *on
379200070521x   2c                   else
379300070521     c                   eval      *in54   =  *on
379400070521e   2c                   endif
379500070521     c                   seton                                          2890
379600070521     c                   eval      V1Dmsg  =  %replace($FVxPOa(xx):
379700070521     c                                        $Msg(43):
379800070521     c                                        %scan('&PS':$Msg(43)))
379900070521     c                   leavesr
380000040621e   1c                   endsl
380100040621      *
380200040621      * - Verifica esistenza tra i f.v. NON memorizzati
380300040621     c                   move      Wnfv          FGVnfv
380400040621     c                   move      S2Cbai        FGVlnp
380500040621     c     K02FGV01      chain     FNFGV01
380600040621sel 1c                   select
380700040621w   1c                   when      NOT %found(FNFGV01L)
380800040615     c                             or  %found(FNFGV01L)
380900040615     c                             and (FGVatb <> *blanks
381000040615     c                              or  FGVfcf <> *blanks)
381100040621if  2c                   if        wNFVseq =  2
381200040615     c                   eval      *in56   =  *on
381300040621x   2c                   else
381400040615     c                   eval      *in54   =  *on
381500040621e   2c                   endif
381600040615     c                   seton                                          2890
381700040615     c                   eval      V1Dmsg  =  %replace(S2Cbai:
381800040615     c                                        $Msg(23):
381900040615     c                                        %scan('&PS':$Msg(23)))
382000060110     c                   leavesr
382100040621w   1c                   when      FGVdfv  <  SAVdfvA
382200040621if  2c                   if        wNFVseq =  2
382300040621     c                   eval      *in56   =  *on
382400040621x   2c                   else
382500040621     c                   eval      *in54   =  *on
382600040621e   2c                   endif
382700040621     c                   seton                                          2890
382800040621     c                   eval      V1Dmsg  =  $Msg(38)
382900040621e   1c                   endsl
383000060110     c                   if        *in90
383100060110     c                   leavesr
383200060110     c                   endif
383300040621     c                   eval      Wdfv    =  FGVdfv
383400040615      *
383500060110     c                   ENDSR
383600031024      *
383700031024      *---------------------------------------------------------------*
383800031024      * Impostazione campi descrittivi per l'autotrasportatore        *
383900031024      *---------------------------------------------------------------*
384000031024     c     DecS03pdr     BEGSR
384100031024      *
384200031024     c                   clear                   Wpdr
384300031024if  1c                   if        S1Cpdc >  *zeros
384400031024     c                   move      S1Cpdc        Wpdr
384500031024e   1c                   endif
384600031024      *
384700031024      * Visualizzazione "*" se più F.V. per il padroncino
384800031024     c                   clear                   S1Cast
384900031024if  1c                   if        S1Cpdc >  *zeros
385000031024     c                   exsr      CtrlAutotr3
385100031024     c                   eval      S1Cast =  Wast
385200031024e   1c                   endif
385300031024      *
385400031024      * Visualizzazione padroncino di default se variato
385500031024if  1c                   if        Wpdr   =  H1Cpdc
385600031024     c                   movel     H1Dpdc        S3Dpdc
385700031024x   1c                   else
385800040408if  2c                   if            $IMA   =  *off
385900040316     c                             and S1Cbai <> C_PostazNull
386000040423     c                             and S1Cpdc >  *zeros
386100031024     c                   move      APDpdr        S1Cpdc
386200031024     c                   movel     APDrsc        S3Dpdc
386300031024e   2c                   endif
386400031024     c                   eval      S1Kdft =  C_Default
386500031024if  2c                   if        H1Cpdc <> *zeros
386600040315     c                   eval      S1Katd =  C_Autotr
386700031024     c                   move      H1Cpdc        S1Catd
386800031024e   2c                   endif
386900031024     c                   eval      S1Datd =  H1Dpdc
387000031024e   1c                   endif
387100031024      *
387200031024     c                   ENDSR
387300031007      *
387400031007      *---------------------------------------------------------------*
387500031007      * Controllo dati immessi in videata S02 & S04                   *
387600031007      *---------------------------------------------------------------*
387700031010     c     CtrS02        BEGSR
387800061011      *
387900061011     c                   clear                   $PObau
388000061011     c                   clear                   $POpos
388100161212     c                   clear                   sk_Fil
388200161212     c                   clear                   sk_Rrn
388300090203     c                   clear                   Free_Rec_Sfl
388400031007      *
388500031007      * Ciclo di lettura del sfl
388600031007if  1c                   if        $Default = *on
388700031007     c                   readc     MS03S02
388800031007x   1c                   else
388900031007     c                   readc     MS03S04
389000031007e   1c                   endif
389100031020      *
389200031007do  1c                   DOW       NOT %eof(FIMS03D)
389300031007      *
389400031007     c                   movea     *zeros        *in(41)
389500031007     c                   z-add     S02nrr        C02rcd
389600031007      * - ctrl su singola riga
389700031014if  2c                   if            S2Cpep  <> *zeros
389800031014     c                             or  S2Cdes  <> *blanks
389900031014     c                             or  S2Cbnl  <> *blanks
390000040304     c                             or  S2Cbai  <> *blanks
390100161215     c                             or  S2Cznc  <> *blanks
390200031014     c                             or (S2Cpdc  <> *zeros
390300031014     c                             and S2Cpdc  <> *blanks)
390400031014     c                             or (S2Cnfv  <> *blanks
390500031014     c                             and S2Cnfv  <> *zeros)
390600031007     c                   exsr      CtrS02rec
390700031007if  3c                   if            $Default = *off
390800031007     c                             and *in90    = *off
390900040611     c                             and ($PO     = *off
391000040611     c                              or  $POdpd  = *on)
391100040518     c                             and S2Cbai  <> C_PostazNull
391200040309     c                             and S2Cbnl  <> 'S'
391300031007     c                   exsr      CtrS04rec
391400031007e   3c                   endif
391500031007x   2c                   else
391600040304     c                   clear                   S2Cbai
391700161213     c                   clear                   S2Cznc
391800031023     c                   clear                   S2Ddfv
391900031106     c                   clear                   S2Dfpp
392000050628     c                   clear                   S2Dmes
392100031106     c                   clear                   S4Ddfv
392200031106     c                   clear                   S4Dfpp
392300050628     c                   clear                   S4Dmes
392400031106     c                   clear                   S2Cast
392500031007     c                   clear                   S2Cpdc
392600031007     c                   clear                   S2Dpdc
392700031007     c                   clear                   S4Dpdc
392800031023     c                   clear                   S2Cdes
392900031023     c                   clear                   S2Kdft
393000040305     c                   clear                   S2Kbad
393100040305     c                   clear                   S2Cbad
393200031023     c                   clear                   S2Katd
393300031023     c                   clear                   S2Catd
393400031023     c                   clear                   S2Datd
393500090203      * - Memorizzazione primo rec. vuoto nel sfl
393600090203if  3c                   if        Free_Rec_Sfl = *zero
393700090203     c                   eval      Free_Rec_Sfl = S02nrr
393800090203e   3c                   endif
393900031007e   2c                   endif
394000031024      *
394100031024      * - reimposto indicatori x DSPATR condizionati
394200040702      *   se NON rilevati errori
394300040702if  2c                   if        NOT *in90
394400040702     c                             and $Default = *on
394500040702     c                             or  $Default = *off
394600060110     c                   exsr      AtrS02S04
394700040702e   2c                   endif
394800031007      *
394900031007      * - riattivo SFLNXTCHG (non solo se rilevato errore)
395000031007     c                   eval      *in32    = *on
395100031007      * - aggiorno il SFL
395200031007if  2c                   if        $Default = *on
395300031007     c                   update    MS03S02
395400031007x   2c                   else
395500031007     c                   update    MS03S04
395600031007e   2c                   endif
395700031007     c                   eval      *in32    = *off
395800031007      * - se rilevato errore esco dal ciclo di controllo
395900031007if  2c                   if        *in28    = *on
396000031106     c                   z-add     C02rcd        C02csr
396100090203     c                   leavesr
396200031007e   2c                   endif
396300031007      * - leggo il record successivo
396400031007if  2c                   if        $Default = *on
396500031007     c                   readc     MS03S02
396600031007x   2c                   else
396700031007     c                   readc     MS03S04
396800031007e   2c                   endif
396900031007      *
397000031007e   1c                   ENDDO
397100040312      *
397200040423      * Controlli finali "generali"
397300040312      *   (se NON ho già rilevato altri errori da segnalare)
397400161212      *
397500161212      /free
397600161212
397700161212         // -?Controllo inserimento Zona (cioè Turno) 99 - OBBLIGATORIA) -?
397800161212         //  ?per ogni Baia = Filiale?
397900161212         For  xx = 1  To  %elem(sk_Fil);
398000161212
398100161213           if  sk_Fil(xx) <= *zero;
398200161212             leave;
398300161212           endif;
398400161212
398500161212           If  sk_Rrn(xx) < *zero;
398600161212
398700161212             S02nrr = sk_Rrn(xx) * (-1);
398800161212             C02csr = S02nrr;
398900161212             if  $Video = '3';
399000161213               chain  S02nrr  MS03S02;
399100161212             else;
399200161213               chain  S02nrr  MS03S04;
399300161212             endif;
399400161212             *in58  = *on;
399500161212             *in28  = *on;
399600161212             *in90  = *on;
399700161213             V1Dmsg = %replace( sk_Fil(xx) : $Msg(58)
399800161212                              : %scan( '___' : $MSG(58) ) );
399900161212             if  %found(FIMS03D);
400000161213               *in32 = *on;
400100161212               if   $Video = '3';
400200161212                 update  MS03S02;
400300161212               else;
400400161212                 update  MS03S04;
400500161212               endif;
400600161212             endif;
400700161212             leavesr;
400800161212
400900161212           EndIf;
401000161212
401100161212         EndFor;
401200161212
401300161212      /end-free
401400090203      *
401500090203      * - Baia Non Letti obbligatoria
401600090203      *   (dovrebbe riscontrarsi solo nel piano di default)
401700090203if  1c                   if        %xfoot($BNLnr) = *zero
401800090203     c                   eval      S02nrr = Free_Rec_Sfl
401900090203     c                   eval      C02csr = S02nrr
402000090203if  2c                   if        $Video = '3'
402100090203     c     S02nrr        chain     MS03S02
402200090203x   2c                   else
402300090203     c     S02nrr        chain     MS03S04
402400090203e   2c                   endif
402500090203     c                   seton                                        572890
402600090203     c                   movel     $Msg(54)      V1Dmsg
402700090203if  2c                   if        %found(FIMS03D)
402800090203if  3c                   if        $Video = '3'
402900090203     c                   update    MS03S02
403000090203x   3c                   else
403100090203     c                   update    MS03S04
403200090203e   3c                   endif
403300090203e   2c                   endif
403400090203     c                   leavesr
403500090203e   1c                   endif
403600090203      *
403700090203      * - Controllo postazioni per baia
403800040312     c                   exsr      CtrBAIxPEP
403900031007      *
404000031007     c                   ENDSR
404100031007      *
404200031007      *---------------------------------------------------------------*
404300031007      * Controllo singolo record del subfile S02 & S04                *
404400031007      *---------------------------------------------------------------*
404500031007     c     CtrS02rec     BEGSR
404600031027      *
404700031027if  1c                   if        $Default = *off
404800031027     c                   clear                   S2Kdft
404900040304     c                   clear                   S2Kbad
405000040304     c                   clear                   S2Cbad
405100031027     c                   clear                   S2Katd
405200031027     c                   clear                   S2Catd
405300031027     c                   clear                   S2Datd
405400031027e   1c                   endif
405500040423      *
405600040423     c                   reset                   $IMA
405700040423     c                   reset                   $IMAdft
405800040607     c                   reset                   $PO
405900040611     c                   reset                   $POdpd
406000080129     c                   reset                   $POerr
406100040607     c                   reset                   $POdft
406200031007      *
406300031014      * Controllo baia di uscita
406400031007sel 1c                   select
406500031007w   1c                   when      S2Cpep = *zeros
406600031007     c                   seton                                        512890
406700031009     c                   movel     $Msg(24)      V1Dmsg
406800031007w   1c                   when      S2Cpep < 01  or  S2Cpep > 98
406900031007     c                   seton                                        512890
407000031009     c                   movel     $Msg(25)      V1Dmsg
407100031007x   1c                   other
407200031007     c                   exsr      ContaBAU
407300031007e   1c                   endsl
407400060110     c                   if        *in90
407500060110     c                   leavesr
407600060110     c                   endif
407700031007      *
407800031007      * Controllo descrizione
407900031014      * (non obbligatoria)
408000031007      *
408100031007      * Controllo flag baia non letti
408200031007if  1c                   if            S2Cbnl <> *blanks
408300031007     c                             and S2Cbnl <> 'S'
408400040304     c                   seton                                        572890
408500031009     c                   movel     $Msg(27)      V1Dmsg
408600031007x   1c                   else
408700040309if  2c                   if        S2Cbnl     =  'S'
408800031007     c                   exsr      ContaBNL
408900031007if  3c                   if        $BNLnr(xx) >  1
409000040304     c                   seton                                        572890
409100031009     c                   movel     $Msg(28)      V1Dmsg
409200031007e   3c                   endif
409300031007e   2c                   endif
409400031007e   1c                   endif
409500060110     c                   if        *in90
409600060110     c                   leavesr
409700060110     c                   endif
409800040305      *
409900040305      * Controllo codice postazione
410000040423      * - ricerca
410100040423     c     '?'           scan      S2Cbai                                 10
410200040423if  1c                   if        *in10  =  *on
410300040423     c                   eval      S2Cbai =  *blanks
410400040423     c                   eval      Wcampo =  'S2CBAI    '
410500040423     c                   exsr      srSEARCH
410600040423e   1c                   endif
410700040423      * - controllo
410800040423if  1c                   if        $Default = *off
410900040423     c                   eval      $Hidden  = *on
411000040423     c                   eval      Wpost    = H2Cbai
411100040423     c                   exsr      Ctrl_IMA
411200040423if  2c                   if        $IMAdft  = *on
411300040423     c                   eval      H2Dpdc   = $MPTdes(yy)
411400040423e   2c                   endif
411500040625     c                   exsr      Ctrl_PO
411600040625if  2c                   if        $POdft   = *on
411700040625     c                   eval      H2Dpdc   = $ORGdes(xy)
411800040625e   2c                   endif
411900040423e   1c                   endif
412000040423     c                   eval      $Hidden  = *off
412100040423     c                   eval      Wpost    = S2Cbai
412200040408     c                   exsr      Ctrl_IMA
412300040423if  1c                   if        $IMA     = *on
412400040423     c                   eval      S2Dpdc   = $MPTdes(yy)
412500040423     c                   eval      S4Dpdc   = $MPTdes(yy)
412600040423e   1c                   endif
412700040607     c                   exsr      Ctrl_PO
412800040607if  1c                   if        $PO      = *on
412900040611if  2c                   if        $POdpd   = *off
413000040611     c                   clear                   S2Cnfv
413100040611     c                   clear                   S2Dfpp
413200040611     c                   clear                   S2Ddfv
413300040611     c                   clear                   S4Cnfv
413400040611     c                   clear                   S4Dfpp
413500040611     c                   clear                   S4Ddfv
413600040611     c                   clear                   S2Cast
413700040611e   2c                   endif
413800061011     c                   clear                   S2Cpdc
413900040607     c                   eval      S2Dpdc   = $ORGdes(xy)
414000040607     c                   eval      S4Dpdc   = $ORGdes(xy)
414100040607e   1c                   endif
414200040423if  1c                   if            $Default = *off
414300040423     c                             and $IMAdft  = *on
414400040423     c                             and H2Cbai  <> S2Cbai
414500040423     c                   eval      S2Datd   = H2Dpdc
414600040423e   2c                   endif
414700080902if  1c                   if            $Default = *on
414800080902     c                             and S2Cbnl   = 'S'
414900080902     c                             and S2Cbai  <> *blanks
415000080902     c                   seton                                        532890
415100080902     c                   movel     $Msg(26)      V1Dmsg
415200080902     c                   leavesr
415300080902e   1c                   endif
415400040409if  1c                   if            $Default = *on
415500040409     c                             and S2Cbnl  <> 'S'
415600040409     c                             and S2Cbai   = *blanks
415700040409     c                   eval      S2Cbai   = C_PostazNull
415800040409e   1c                   endif
415900040305sel 1c                   select
416000040305w   1c                   when          $Default = *on
416100040305     c                             and S2Cbnl   = 'S'
416200040305     c                             and S2Cbai   = *blanks
416300040409w   1c                   when      S2Cbai   = C_PostazNull
416400040409if  2c                   if        $Default = *off
416500040305     c                   clear                   S2Cnfv
416600040305     c                   clear                   S2Ddfv
416700040305     c                   clear                   S2Dfpp
416800040305     c                   clear                   S4Cnfv
416900040305     c                   clear                   S4Ddfv
417000040305     c                   clear                   S4Dfpp
417100040305     c                   clear                   S4Dpdc
417200040409e   2c                   endif
417300161216     c                   clear                   S2Cznc
417400040409     c                   clear                   S2Cpdc
417500040305w   1c                   when          S2Cbai =  *blanks
417600040305     c                             and S2Cbnl <> 'S'
417700040305     c                   seton                                        532890
417800040305     c                   movel     $Msg(13)      V1Dmsg
417900040305w   1c                   when          S2Cbai <> *blanks
418000040305     c                             and S2Cbnl =  'S'
418100040305     c                   seton                                        532890
418200040305     c                   movel     $Msg(26)      V1Dmsg
418300040305w   1c                   when         (%subst(S2Cbai:1:1) >= 'A'
418400040305     c                             and %subst(S2Cbai:1:1) <= 'Z')
418500040305     c                             and %subst(S2Cbai:2:2) >= '01'
418600040305     c                             and %subst(S2Cbai:2:2) <= '99'
418700040305     c                   move      S2Cbai        w002a
418800040305     c     DigitN        check     w002a                                  10
418900040305if  2c                   if        *in10  =  *on
419000040305     c                   seton                                        532890
419100040305     c                   movel     $Msg(14)      V1Dmsg
419200040305x   2c                   else
419300040423     c                   eval      Wpost  =  S2Cbai
419400040305     c                   exsr      ContaBAI
419500040305if  3c                   if        $BAInr(xx) >  1
419600040305     c                   seton                                        532890
419700040305     c                   movel     $Msg(15)      V1Dmsg
419800040305e   3c                   endif
419900040305e   2c                   endif
420000040607w   1c                   when          S2Cbnl =  *blanks
420100040607     c                             and $IMA   =  *off
420200040607     c                             and $PO    =  *on
420300061010     c                             and $POdpd =  *on
420400040607if  3c                   if        $ORGnr(xy) >  1
420500040607     c                   seton                                        532890
420600040607     c                   movel     $Msg(33)      V1Dmsg
420700040607e   3c                   endif
420800040305w   1c                   when          S2Cbnl =  *blanks
420900040408     c                             and $IMA   =  *off
421000040607     c                             and $PO    =  *off
421100040305     c                             OR  S2Cbnl =  'S'
421200040305     c                             and S2Cbai <> *blanks
421300040305     c                   seton                                        532890
421400040305     c                   movel     $Msg(14)      V1Dmsg
421500040305e   1c                   endsl
421600060110     c                   if        *in90
421700060110     c                   leavesr
421800060110     c                   endif
421900040316      *
422000040316      * Controllo la postazione sia "compatibile" con le altre già
422100040316      *   immesse nella stessa baia
422200040316if  1c                   if        S2Cbai <> C_PostazNull
422300040409     c                             and
422400040409     c                             S2Cbai <> *blanks
422500040316     c                   eval      xx     =  1
422600040316     c                   eval      *in10  =  *off
422700040316     c     S2Cpep        lookup    $BAU(xx)                               10
422800040628sel 2c                   select
422900040628      * - Unica (o prima) postazione della baia in esame
423000040628w   2c                   when      NOT  *in10
423100040628      * - Postazione IMA/non-IMA o P.O./non-P.O. nella stessa baia
423200161110      *  ?Su richiesta di Nicola:?tolto controllo postaz. IMA/non-IMA
423300161110      *   (ora IMA e AUT possono stare nella stessa baia;
423400161110      *   sono le FILIALI che continuano a richiedere una loro baia)
423500161110      * - Postazione P.O./non-P.O. nella stessa baia
423600161110w   2c                   when           $BAUbai(xx) <> *blanks
423700161110     c                             and  $PosPO(xx)  <> $PO
423800040316     c                   seton                                        532890
423900161110     c                   eval      V1Dmsg =  $Msg(57)
424000060110     c                   leavesr
424100040628      * - Postazione P.O. DPD non unico nella baia
424200040628w   2c                   when           $BAUbai(xx)    <> *blanks
424300040628     c                             and  $BAUbai(xx)    <> S2Cbai
424400040628     c                             and  $PosPOdpd(xx)  <> $POdpd
424500040628     c                   seton                                        532890
424600040628     c                   eval      V1Dmsg =  $Msg(44)
424700060110     c                   leavesr
424800040607w   2c***                when           *in10       =  *on
424900040607     c***                          and  $PosIMA(xx) =  $IMA
425000061011     c***                          and  $PosPO(xx)  =  $PO
425100040316      ***  Ok. Vediamo il prossimo...
425200040628e   2c                   endsl
425300040316e   1c                   endif
425400061011      *
425500061011      * Controllo l'eventuale utilizzo dei P.O. per baia
425600061011if  1c                   if             S2Cbnl  = *blanks
425700061011     c                             and  $IMA    = *off
425800061011     c                             and  $PO     = *on
425900061011     c                             and  $POdpd  = *off
426000061011      *
426100061011     c                   eval      WX     =  1
426200061011     c     S2Cbai        lookup    $POpos(wx)                             10
426300061011do  2c                   dow       *in10  =  *on
426400061011sel 3c                   select
426500061011w   3c                   when      S2Cpep =  $PObau(wx)
426600061011     c                   seton                                        532890
426700161212     c                   eval      V1Dmsg =  %trimR($Msg(33))
426800061012     c                                    +  ' nella stessa baia'
426900061011     c                   leavesr
427000061011w   3c                   when      wx     >= %elem($POpos)
427100061011     c                   leave
427200061011x   3c                   other
427300061011     c                   add       1             wx
427400061013     c     S2Cbai        lookup    $POpos(wx)                             10
427500061011e   3c                   endsl
427600061011e   2c                   enddo
427700061011      * Controllo progressivo per postazione = P.O. (FL3)
427800061011     c                   clear                   wFL3
427900061011do  2c                   do        S02nrr        wx
428000061012if  3c                   if        S2Cbai =  $POpos(wx)
428100061011     c                   add       1             wFL3
428200061012if  4c                   if        wFL3   >= 9
428300061011     c                   seton                                        532890
428400161212     c                   eval      V1Dmsg =  %trimR($Msg(33))
428500061012     c                                    +  ' in oltre 9 baie'
428600061011     c                   leavesr
428700061011e   4c                   endif
428800061011e   3c                   endif
428900061011e   2c                   enddo
429000061011e   1c                   endif
429100161212      *
429200161215      /free
429300161215
429400161215         // -?Controllo inserimento Zona (cioè Turno) solo se Baia = Filiale?
429500161215         //  ?(99 = generica)?
429600161215         Select;
429700161215           // - -?Non richiesta ma inserita?
429800161215           When  ( S2Cbnl <> *blank  or
429900161215                   $IMA    = *on     or
430000161215                   $PO     = *off )  and
430100161215                 S2Cznc   <> *blank;
430200161215             //*·clear  S2Cznc;
430300161215             *in58  =  *on;
430400161215             *in28  =  *on;
430500161215             *in90  =  *on;
430600161215             V1Dmsg =  $Msg(59);
430700161215             leavesr;
430800161215           // - -?Richiesta ma non inserita?
430900161215           When  S2Cbnl   =  *blank  and
431000161215                 $IMA     =  *off    and
431100161215                 $PO      =  *on     and
431200161215                 S2Cznc   =  *blank;
431300161215             //*·S2Cznc = 99;
431400161215             *in58  =  *on;
431500161215             *in28  =  *on;
431600161215             *in90  =  *on;
431700170117             //*·V1Dmsg =  %replace( sk_Fil(xx) : $Msg(60)
431800170117             V1Dmsg =  %replace( S2Cbai : $Msg(60)
431900161215                               : %scan( '___' : $Msg(60) ) );
432000161215             leavesr;
432100161215         EndSl;
432200161215         // - -?Numericità (se inserita)?
432300161215         If  S2Cznc <> *blank  and
432400161215             %check( DigitN : S2Cznc ) > *zero;
432500161215           *in58  =  *on;
432600161215           *in28  =  *on;
432700161215           *in90  =  *on;
432800161215           V1Dmsg =  $Msg(61);
432900161215           leavesr;
433000161215         EndIf;
433100161215
433200161215      /end-free
433300040316      *
433400040305      * ...e ne memorizzo gli utilizzi per baia
433500040409      *   (ammesse postazioni *blank per la "baia non letti")
433600040316if  1c                   if        S2Cbai <> C_PostazNull
433700040305     c                   exsr      MemBAIxPEP
433800040305e   1c                   endif
433900040312      *
434000040305      * Visualizzo la postazione di default se variata in piano
434100040305      *   giornaliero
434200161213if  1c                   if            $Default =  *off
434300161213     c                             and (S2Cbai  <> H2Cbai
434400161213     c                              or  S2Cznc  <> H2Cznc)
434500040423     c                   eval      S2Kdft  = C_Default
434600161213if  2c                   if        S2Cbai <> H2Cbai
434700040625     c                   eval      S2Kbad  = C_Postazione
434800040423     c                   eval      S2Cbad  = H2Cbai
434900161213if  3c                   if           $IMAdft = *on
435000040625     c                             or $POdft  = *on
435100040423     c                   eval      S2Datd  = H2Dpdc
435200161213e   3c                   endif
435300161213x   2c                   else
435400161213     c                   eval      S2Kbad  = 'Zona'
435500161215     c                   eval      S2Cbad  = H2Cznc
435600161213e   2c                   endif
435700040305e   1c                   endif
435800031007      *
435900031007      * Controllo codice autotrasportatore
436000031007      *   (in S03 e S04 è campo protetto)
436100031015      * Per il piano giornaliero farò appositi controlli nella subr.
436200031015      *   CTRS04REC
436300050614     c                   clear                   S2Dmes
436400050614     c                   clear                   S4Dmes
436500031007if  1c                   if        $Default = *on
436600040607if  2c                   if            $IMA = *off
436700040607     c                             and $PO  = *off
436800031007     c                   clear                   S2Dpdc
436900031009     c                   clear                   S4Dpdc
437000040423e   2c                   endif
437100031007      * - se *blanks lo considero *zeros (sarebbe numerico)
437200031007if  2c                   if        S2Cpdc =  *blanks
437300031007     c                   eval      S2Cpdc =  *zeros
437400031007e   2c                   endif
437500031007      * - ricerca
437600031007     c     '?'           scan      S2Cpdc                                 10
437700031007if  2c                   if        *in10  =  *on
437800031007     c                   eval      S2Cpdc =  *zeros
437900031007     c                   eval      Wcampo =  'S2CPDC    '
438000031007     c                   exsr      srSEARCH
438100031007e   2c                   endif
438200031007      * - controllo
438300031007sel 2c                   select
438400040305w   2c                   when          S2Cbnl =  *blanks
438500040409     c                             and ($IMA  =  *on
438600040607     c                              or $PO    =  *on
438700040409     c                              or S2Cbai =  C_PostazNull)
438800040305     c                             and S2Cpdc =  *zeros
438900031007w   2c                   when          S2Cbnl =  'S'
439000031007     c                             and S2Cpdc <> *zeros
439100031007     c                   seton                                        552890
439200031009     c                   movel     $Msg(29)      V1Dmsg
439300040408w   2c                   when          $IMA   =  *on
439400040305     c                             and S2Cpdc <> *zeros
439500040305     c                   seton                                        552890
439600040305     c                   movel     $Msg(16)      V1Dmsg
439700040607w   2c                   when          $PO    =  *on
439800040607     c                             and S2Cpdc <> *zeros
439900040607     c                   seton                                        552890
440000040607     c                   movel     $Msg(18)      V1Dmsg
440100040305w   2c                   when          S2Cbnl =  *blanks
440200040408     c                             and $IMA   =  *off
440300040607     c                             and $PO    =  *off
440400031021     c                   movel     V1Cpog        APDpdr
440500031021     c                   move      S2Cpdc        APDpdr
440600031014     c                   exsr      CtrlAutotr2
440700031007     c                   movel     APDrsc        S2Dpdc
440800040519if  3c                   if        NOT *in90
440900040519     c                   eval      Wpdr   =  APDpdr
441000040519     c                   exsr      CtrlAutotr4
441100040519e   3c                   endif
441200031007e   2c                   endsl
441300060110     c                   if        *in90
441400060110     c                   leavesr
441500060110     c                   endif
441600050629      * - imposto il flag per autotrasportoatore Messaggeria
441700050614if  3c                   if            $Default = *on
441800050614     c                             and §OGpkm   = 'S'
441900050629     c                             and $VDL     = *on
442000050614     c                             and $IMA     = *off
442100050614     c                             and $PO      = *off
442200050614     c                             and S2Cbai  <> C_PostazNull
442300050614     c                             and S2Cbai  <> *blanks
442400050701     c                   eval      wNFVseq = 1
442500050629     c                   movel     APDflr        dAPDflr
442600050614     c                   exsr      X_Messaggeria
442700050614e   3c                   endif
442800031007      * - se *zeros lo reimposto a *blanks (numerico "editato")
442900031014if  2c                   if        S2Cpdc =  *zeros
443000031014     c                   eval      S2Cpdc =  *blanks
443100031007e   2c                   endif
443200031007e   1c                   endif
443300040305      *
443400040305      * In caso di piano giornaliero controllo se è stata tolta la
443500040305      *   postazione ("XXX"), perchè in tal caso non verrà eseguita
443600040305      *   la subr. per i controlli specifici, pertanto non verrebbe
443700040305      *   evidenziato l'autotrasportatore pre-impostato
443800061011if  1c                   if             $Default = *off
443900061011     c                             and (S2Cbai   = C_PostazNull
444000061011     c                             or   $PO      = *on)
444100040305     c                   clear                   S2Cpdc
444200061011if  2c                   if        $PO    =  *off
444300040305     c                   clear                   S4Dpdc
444400061011e   2c                   endif
444500040305     c                   exsr      DecS04pdr
444600040305e   1c                   endif
444700031007      *
444800060110     c                   ENDSR
444900031007      *
445000031007      *---------------------------------------------------------------*
445100031007      * Controlli aggiuntivi per singolo record del subfile S04       *
445200031007      *---------------------------------------------------------------*
445300031007     c     CtrS04rec     BEGSR
445400031023      *
445500031106     c                   reset                   SAVnfv
445600031106      *
445700031106      * Se impostato solo il 2° numero F.V. lo sposto al 1°
445800031106if  1c                   if            (S2Cnfv  =  *blanks
445900031106     c                             or   S2Cnfv  =  *zeros)
446000031106     c                             and (S4Cnfv  <> *blanks
446100031106     c                             and  S4Cnfv  <> *zeros)
446200031106     c                   eval      S2Cnfv  =  S4Cnfv
446300031106     c                   clear                   S4Cnfv
446400031106     c                   clear                   S4Ddfv
446500031106     c                   clear                   S4Dfpp
446600050628     c                   clear                   S4Dmes
446700031106e   1c                   endif
446800031106      *
446900031106      * Controllo 1° numero foglio viaggio
447000031007     c                   clear                   S2Ddfv
447100031027     c                   clear                   S2Dfpp
447200050628     c                   clear                   S2Dmes
447300031106     c                   eval      wNFVseq =  1
447400031106     c                   eval      Wnfv    =  S2Cnfv
447500031106     c                   exsr      Ctrl_1FV
447600031106     c                   eval      S2Cnfv  =  Wnfv
447700080129if  1c                   if            $PO     =  *on
447800080129     c                             and $POdpd  =  *on
447900080130     c                   eval      $ORGerr(xy) =  (Wnfv = *blanks   or
448000080130     c                                             Wnfv = *zeros)
448100080129e   1c                   endif
448200060110     c                   if        *in90
448300060110     c                   leavesr
448400060110     c                   endif
448500031106      * Se tutto bene: ne imposto i dati a video
448600031106if  1c                   if        Wnfv    <> *blanks
448700040621if  2c                   if        $PO     =  *off
448800031106     c                   eval      S2Dfpp  =  FVVfpp
448900031106     c                   move      DSdata6       S2Ddfv
449000040621x   2c                   else
449100040621     c***                clear                   S2Dfpp
449200040621     c                   move      DSdata6       S2Ddfv
449300040621e   2c                   endif
449400031106e   1c                   endif
449500031106      *
449600031106      * Imposto flag e decodifico variazioni relative all'autotrasp.
449700031106      *   (che deve essere comune ad entrambi i F.V.)
449800040610if  1c                   if            $IMA    =  *off
449900040610     c                             and $PO     =  *off
450000040610if  2c                   if        S2Cnfv  <> *blanks
450100031106     c                   move      APDpdr        S2Cpdc
450200031106     c                   movel     APDrsc        S4Dpdc
450300040610x   2c                   else
450400040305     c                   clear                   S2Cpdc
450500040305     c                   clear                   S4Dpdc
450600040423e   2c                   endif
450700031106e   1c                   endif
450800031106     c                   exsr      DecS04pdr
450900040615      *
451000040615      * Imposto flag relativo alla postazione = P.O. DPD con più F.V.
451100040615if  1c                   if        $POdpd   =  *on
451200040621     c*** decod. P.O.    clear                   S2Cpdc
451300040621     c*** decod. P.O.    clear                   S4Dpdc
451400040621     c                   eval      Wpost    = S2Cbai
451500040615     c                   exsr      AsterFVxPO
451600040615     c                   eval      S2Cast   = Wast
451700040615e   1c                   endif
451800031106      *
451900031106      * Controllo 2° numero foglio viaggio (se immesso)
452000031106     c                   clear                   S4Ddfv
452100031106     c                   clear                   S4Dfpp
452200050628     c                   clear                   S4Dmes
452300031106if  1c                   if            S4Cnfv <> *blanks
452400031106     c                             and S4Cnfv <> *zeros
452500031106     c                   eval      wNFVseq =  2
452600031106     c                   eval      Wnfv    =  S4Cnfv
452700031106     c                   exsr      Ctrl_1FV
452800031106     c                   eval      S4Cnfv  =  Wnfv
452900060110     c                   if        *in90
453000060110     c                   leavesr
453100060110     c                   endif
453200031106      * Se tutto bene: ne imposto i dati a video
453300031106if  2c                   if        Wnfv    <> *blanks
453400031106     c                   eval      S4Dfpp  =  FVVfpp
453500031106     c                   move      DSdata6       S4Ddfv
453600031106e   2c                   endif
453700040305      * e rieseguo il controllo sul numero di F.V. x autotrasportatore
453800040305      *   (se ne ho selezionato uno da ricerca)
453900040305if  2c                   if        S2Cast  =  '?'
454000040305     c                   clear                   S2Cast
454100040305     c                   exsr      CtrlAutotr3
454200040305     c                   eval      S2Cast =  Wast
454300040305e   2c                   endif
454400031106x   1c                   else
454500031106     c                   clear                   S4Cnfv
454600031106e   1c                   endif
454700031106      *
454800060110     c                   ENDSR
454900031024      *
455000031024      *---------------------------------------------------------------*
455100031024      * Impostazione campi descrittivi per l'autotrasportatore        *
455200031024      *---------------------------------------------------------------*
455300031024     c     DecS04pdr     BEGSR
455400031024      *
455500031024     c                   clear                   Wpdr
455600031024if  1c                   if        S2Cpdc >  *zeros
455700031024     c                   move      S2Cpdc        Wpdr
455800031024e   1c                   endif
455900031024      *
456000031024      * Visualizzazione "*" se più F.V. per il padroncino
456100031024     c                   clear                   S2Cast
456200031024if  1c                   if        S2Cpdc >  *zeros
456300031024     c                   exsr      CtrlAutotr3
456400031024     c                   eval      S2Cast =  Wast
456500031024e   1c                   endif
456600031024      *
456700031024      * Visualizzazione padroncino di default se variato
456800031024if  1c                   if        Wpdr   =  H2Cpdc
456900040625if  2c                   if            $IMA   =  *off
457000040625     c                             and $PO    =  *off
457100040625     c                             and S2Cbai <> C_PostazNull
457200031024     c                   movel     H2Dpdc        S4Dpdc
457300040423e   2c                   endif
457400031024x   1c                   else
457500040305if  2c                   if            S2Cbnl =  *blanks
457600040408     c                             and $IMA   =  *off
457700040625     c                             and $PO    =  *off
457800040316     c                             and S2Cbai <> C_PostazNull
457900040305if  3c                   if        S2Cpdc >  *zeros
458000031024     c                   move      APDpdr        S2Cpdc
458100031024     c                   movel     APDrsc        S4Dpdc
458200040305e   3c                   endif
458300031024e   2c                   endif
458400031024     c                   eval      S2Kdft =  C_Default
458500031024if  2c                   if        H2Cpdc <> *zeros
458600040315     c                   eval      S2Katd =  C_Autotr
458700031024     c                   move      H2Cpdc        S2Catd
458800031024e   2c                   endif
458900031024     c                   eval      S2Datd =  H2Dpdc
459000031024e   1c                   endif
459100031024      *
459200031024     c                   ENDSR
459300060110      *
459400060110      *---------------------------------------------------------------*
459500060110      * Impostazione settaggi attributi di visualizzazione x S01/S03  *
459600060110      *---------------------------------------------------------------*
459700060110     c     AtrS01S03     BEGSR
459800060110      *
459900060110      * Autotrasportatore Messaggeria
460000060110     c                   eval      *in04 = (§OGpkm = 'S')
460100060110      * Baia non letti
460200060110     c                   eval      *in80 = (S2Cbnl = 'S')
460300060110      * Padroncino e/o Postazione variati
460400060110     c                   clear                   Wpdr
460500060110if  1c                   if            S1Cpdc <> *blanks
460600060110     c                             and S1Cpdc <> *zeros
460700060110     c                   movel     V1Cpog        Wpdr
460800060110     c                   move      S1Cpdc        Wpdr
460900060110e   1c                   endif
461000060110     c                   eval      *in85 = (Wpdr   <> H1Cpdc   or
461100060110     c                                      S1Cbai <> H1Cbai)
461200060110      * Postazione = "IMx" o P.O.
461300060110     c                   eval      *in86 = ($IMA   =  *on      or
461400060110     c                                      $PO    =  *on)
461500180110      * Autotrasportatore Messaggeria
461600180110      * - 1° F.V. o Default Messaggeria
461700180110     c                   eval      *in87 = (H1Dmes <> *blanks)
461800180110      * - 2° F.V. Messaggeria
461900180110     c                   if        $Default    = *off
462000180110     c                   eval      *in88 = (H3Dmes <> *blanks)
462100180110     c                   endif
462200060110      *
462300060110     c                   ENDSR
462400060110      *
462500060110      *---------------------------------------------------------------*
462600060110      * Impostazione settaggi attributi di visualizzazione x S02/S04  *
462700060110      *---------------------------------------------------------------*
462800060110     c     AtrS02S04     BEGSR
462900060110      *
463000060110      * Autotrasportatore Messaggeria
463100060110     c                   eval      *in04 = (§OGpkm = 'S')
463200060110      * Baia non letti
463300060110     c                   eval      *in80 = (S2Cbnl = 'S')
463400060110      * Padroncino e/o Postazione variati
463500060110     c                   clear                   Wpdr
463600060110if  1c                   if            S2Cpdc <> *blanks
463700060110     c                             and S2Cpdc <> *zeros
463800060110     c                   movel     V1Cpog        Wpdr
463900060110     c                   move      S2Cpdc        Wpdr
464000060110e   1c                   endif
464100060110     c                   eval      *in85 = (Wpdr   <> H2Cpdc   or
464200161213     c                                      S2Cbai <> H2Cbai   or
464300161213     c                                      S2Cznc <> H2Cznc)
464400060110      * Postazione = "IMx" o P.O.
464500060110     c                   eval      *in86 = ($IMA   =  *on      or
464600060110     c                                      $PO    =  *on)
464700060110      * Autotrasportatore Messaggeria
464800060110      * - 1° F.V. o Default Messaggeria
464900060110     c                   eval      *in87 = (S2Dmes <> *blanks)
465000060110      * - 2° F.V. Messaggeria
465100060110     c                   if        $Default    = *off
465200060110     c                   eval      *in88 = (S4Dmes <> *blanks)
465300060110     c                   endif
465400060110      *
465500060110     c                   ENDSR
465600031014      *
465700031014      *---------------------------------------------------------------*
465800031030      * Controllo codice autotrasportatore per singolo F.V. / baia    *
465900031014      *---------------------------------------------------------------*
466000031014     c     CtrlAutotr1   BEGSR
466100031014      *
466200031014      * Reperisco il padroncino memorizzato nel F.V. di default
466300031014     c                   clear                   DS_FV
466400031014     c                   eval      dsFVdfv    =  FVVdfv
466500031014     c                   eval      dsFVnfv    =  FVVnfv
466600031022     c                   move      DS_FV         w0150
466700031014     c                   eval      xx    = 1
466800031014     c                   eval      *in10 = *off
466900031111     c     w0150         lookup    $MEMfv1(xx)                            10
467000031111if  1c                   if        *in10 = *off
467100031111     c     w0150         lookup    $MEMfv2(xx)                            10
467200031111e   1c                   endif
467300031014if  1c                   if        *in10 = *on
467400031014     c                   eval      DSTpdr     =  $MEMat(xx)
467500031014x   1c                   else
467600031111      * - Se non reperito il F.V. tra quelli automaticamente proposti
467700031014      *   (per es. durante la variazione di un piano giornaliero)
467800031014      *   aggancio il file Fogli Distinte Consegne & memorizzo il F.V.
467900120613     c                   eval      DSTnpg     =  FVVnpg
468000120613     c                   eval      DSTnfv     =  FVVnfv
468100120613     c                   eval      DSTfgs     =  FVVfgs
468200120613     c     K03FIDST      chain     FIDST000
468300120613if  2c                   if        NOT %found(FIDST01L)
468400031022if  3c                   if        wNFVseq    =  2
468500031022     c                   movel     *on           *in56
468600031022x   3c                   else
468700031022     c                   movel     *on           *in54
468800031022e   3c                   endif
468900031022     c                   seton                                          2890
469000031014     c                   eval      V1Dmsg = $Msg(21)
469100060110     c                   leavesr
469200031014x   2c                   else
469300031111      * - - controllo se padroncino già memorizzato
469400031111      *     (se non lo è ancora  : lo memorizzo ora;
469500031111      *        se non ho più spazio: esco dal ciclo senza memorizzare)
469600031014     c                   eval      xx    = 1
469700031014     c                   eval      *in10 = *off
469800031111do  3c                   DOW       *in10 = *off
469900031111     c     DSTpdr        lookup    $MEMat(xx)                             10
470000031111if  4c                   if        *in10 = *off
470100031111     c     *zeros        lookup    $MEMat(xx)                             10
470200031111if  5c                   if        *in10 = *on
470300031111     c                   eval      $MEMat(xx)  =  DSTpdr
470400031111x   5c                   else
470500031111     c                   leave
470600031111e   5c                   endif
470700031111e   4c                   endif
470800031111      * - - per il padroncino cerco il primo elemento libero tra le
470900031111      *     schiere $MEMfv1 o $MEMfv2;
471000031111      *     se non lo trovo: torno sopra per ri-memorizzare il codice
471100031111      *       padroncino e questo nuovo f.v.
471200050701     c                   movel     FVVspn        dFVVspn
471300031111sel 4c                   select
471400031111w   4c                   when      *in10       = *on    and
471500031111     c                             $MEMfv1(xx) = *loval
471600031111     c                   eval      dsFVdfv     =  FVVdfv
471700031111     c                   eval      dsFVnfv     =  FVVnfv
471800031111     c                   move      DS_FV         w0150
471900031111     c                   eval      $MEMfv1(xx) =  w0150
472000031111     c                   eval      $MEMmp1(xx) =  FVVfpp
472100050701     c                   eval      $MEMpm1(xx) =  §FVVtpm
472200031111w   4c                   when      *in10       = *on    and
472300031111     c                             $MEMfv2(xx) = *loval
472400031111     c                   eval      dsFVdfv     =  FVVdfv
472500031111     c                   eval      dsFVnfv     =  FVVnfv
472600031111     c                   move      DS_FV         w0150
472700031111     c                   eval      $MEMfv2(xx) =  w0150
472800031111     c                   eval      $MEMmp2(xx) =  FVVfpp
472900050701     c                   eval      $MEMpm2(xx) =  §FVVtpm
473000031111x   4c                   other
473100031111if  5c                   if        xx    < %elem($MEMat)
473200031111     c                   eval      xx    = xx + 1
473300031111     c                   eval      *in10 = *off
473400031111x   5c                   else
473500031111     c                   leave
473600031111e   5c                   endif
473700031111e   4c                   endsl
473800031111e   3c                   enddo
473900031014e   2c                   endif
474000031014e   1c                   endif
474100031022      *
474200031106      * Se piano giornaliero (per P.O. con o senza VDL):
474300031022if  1c                   if        $Default   =  *off
474400040519      *?Solo per VdL (almeno per ora):?
474500040519      * Controllo che il codice autotrasportatore NON sia già impostato
474600040519      *   per altra baia/postazione
474700040519      *   (se variato - perchè, altrimenti, risulta GIA' controllato
474800040519      *   nella subr. "CtrS01rec")
474900040519if  2c                   if            $VDL    =  *on
475000040519     c                             and wNFVseq =  1
475100040519     c                   eval      Wpdr   =  DSTpdr
475200040519     c                   exsr      CtrlAutotr4
475300060110     c                   if        *in90
475400060110     c                   leavesr
475500060110     c                   endif
475600040519e   2c                   endif
475700040519      * Confronto autotrasportatori dei 2 F.V. x baia
475800031022sel 2c                   select
475900040519w   2c                   when      wNFVseq     =  1
476000040519      * - memorizzo l'autotrasportatore del 1° F.V. per confrontarlo
476100031022      *   con quello del 2° F.V. (per baia)
476200040519     c                   eval      SAVpdr      =  DSTpdr
476300040519w   2c                   when          wNFVseq =  2
476400040519     c                             and DSTpdr  <> SAVpdr
476500031022      * - l'autotrasportatore del 2° F.V. deve essere uguale a quello
476600031022      *   del 1° F.V. inserito (per baia)
476700031022     c                   seton                                        562890
476800031022     c                   eval      V1Dmsg = %replace(
476900031022     c                                      %editw(DSTpdr:'     0 '):
477000031022     c                                      $Msg(36):
477100031022     c                                      %scan('&PADR  ':$Msg(36)))
477200060110     c                   leavesr
477300031022e   2c                   endsl
477400031022e   1c                   endif
477500031014      *
477600060110     c                   ENDSR
477700031014      *
477800031014      *---------------------------------------------------------------*
477900031014      * Controllo codice autotrasportatore                            *
478000031014      *---------------------------------------------------------------*
478100031022     c     CtrlAutotr2   BEGSR
478200031014      *
478300031014     c     K02FIAPD      chain     FIAPD000
478400031014      *
478500031014if  1c                   if        NOT %found(FIAPD01L)
478600031014     c                             or  APDatb <> *blanks
478700031014     c                   clear                   APDrsc
478800031014     c                   seton                                        552890
478900031014     c                   movel     $Msg(17)      V1Dmsg
479000031014e   1c                   endif
479100031014      *
479200031014     c                   ENDSR
479300031024      *
479400031024      *---------------------------------------------------------------*
479500031024      * Evidenzia autotrasportatore con più F.V.                      *
479600031024      *---------------------------------------------------------------*
479700031024     c     CtrlAutotr3   BEGSR
479800031024      *
479900031024     c                   clear                   Wast
480000031024      * Default o giornaliero, evidenzio più F.V. per il padroncino
480100031024     c                   movel     V1Cpog        Wpdr
480200031024     c                   eval      xx     =  1
480300031024     c                   eval      *in10  =  *off
480400031024     c     Wpdr          lookup    $MEMat(xx)                             10
480500031114if  1c                   if            *in10      = *on
480600031114     c                             and $MEMxf(xx) > 1
480700031024     c                   eval      Wast   =  '*'
480800031024e   1c                   endif
480900031024      *
481000031024     c                   ENDSR
481100040519      *
481200040519      *---------------------------------------------------------------*
481300040519      * Controllo codice autotrasportatore per singola baia/postaz.   *
481400040519      *---------------------------------------------------------------*
481500040519     c     CtrlAutotr4   BEGSR
481600040519      *
481700040519     c                   clear                   ds_BaiaPost
481800040519if  1c                   if        $VDL   =  *off
481900040519     c                   movel     S1Cpep        dsBPbai
482000040519     c                   movel     S1Cbai        dsBPpos
482100040519x   1c                   else
482200040519     c                   movel     S2Cpep        dsBPbai
482300040519     c                   movel     S2Cbai        dsBPpos
482400040519e   1c                   endif
482500040519      *
482600040519     c                   eval      xx     = 1
482700040519     c                   eval      *in10  = *off
482800040519     c     Wpdr          lookup    $MEMat(xx)                             10
482900040519      *
483000040519sel 1c                   select
483100040519      * autotrasportatore non ancora memorizzato
483200040519w   1c                   when      NOT *in10
483300040519     c     *zeros        lookup    $MEMat(xx)                             10
483400040519if  2c                   if        *in10
483500040519     c                   eval      $MEMat(xx) =  Wpdr
483600040519     c                   eval      $MEMbp(xx) =  ds_BaiaPost
483700040519e   2c                   endif
483800040519      * 1° utilizzo dell'autotrasportatore: ne memorizzo baia/postaz.
483900040519w   1c                   when      $MEMbp(xx) =  *blanks
484000040519     c                   movel     ds_BaiaPost   $MEMbp(xx)
484100040519      * autotrasportatore già utilizzato in altre baie/postazioni
484200040519w   1c                   when      $MEMbp(xx) <> ds_BaiaPost
484300040519     c                   movel     $MEMbp(xx)    ds_BaiaPost
484400040519if  2c                   if        $Default   =  *on
484500040519     c                   eval      *in55  =  *on
484600040519x   2c                   else
484700040519     c                   eval      *in54  =  *on
484800040519e   2c                   endif
484900040519     c                   seton                                          2890
485000040519     c                   eval      V1Dmsg = %replace(
485100040519     c                                      %editw(Wpdr:'     0 '):
485200040519     c                                      $Msg(34):
485300040519     c                                      %scan('&PADR  ':$Msg(34)))
485400040519     c                   eval      V1Dmsg = %replace(dsBPpos:V1Dmsg:
485500040519     c                                      %scan('&PS':V1Dmsg))
485600040519     c                   eval      V1Dmsg = %replace(dsBPbai:V1Dmsg:
485700040519     c                                      %scan('&B        ':V1Dmsg))
485800060110     c                   leavesr
485900040519      * ... "beccata" baia/postazione in esame
486000040519w   1c*** ok:            when      $MEMbp(xx) =  ds_BaiaPost
486100040519e   1c                   endsl
486200040519      *
486300060110     c                   ENDSR
486400040615      *
486500040615      *---------------------------------------------------------------*
486600040615      * Evidenzia P.O. con più F.V.                                   *
486700040615      *---------------------------------------------------------------*
486800040615     c     AsterFVxPO    BEGSR
486900040615      *
487000040615     c                   clear                   Wast
487100040615      *
487200040615     c                   eval      xx     =  1
487300040615     c                   eval      *in10  =  *off
487400040615     c     Wpost         lookup    $FVxPOa(xx)                            10
487500040615      *
487600040615if  1c                   if        *in10  =  *on
487700040615      *
487800040615     c                   eval      xx     =  xx + 1
487900040615     c                   eval      *in10  =  *off
488000040615     c     Wpost         lookup    $FVxPOa(xx)                            10
488100040615      *
488200040615if  2c                   if        *in10  =  *on
488300040615     c                   eval      Wast   =  '*'
488400040615e   2c                   endif
488500040615      *
488600040615e   1c                   endif
488700040615      *
488800040615     c                   ENDSR
488900031021      *
489000031021      *---------------------------------------------------------------*
489100040408      * Memorizzo (l'ultima) postazione immessa per ogni codice baia  *
489200031021      *---------------------------------------------------------------*
489300031021     c     MemBAIxPEP    BEGSR
489400031021      *
489500031021     c                   eval      xx     = 1
489600031021     c                   eval      *in10  = *off
489700040305      *
489800040305if  1c                   if        $VDL   = *off
489900040305      *
490000040305     c     S1Cpep        lookup    $PEP(xx)                               10
490100040305      * ...lo trovo sicuro, perchè li ho già memorizzati nelle subr.
490200040305      *   "ContaPEP"
490300040305if  2c                   if            *in10       =  *on
490400040409     c                             and $PEPbai(xx) =  *blanks
490500040305     c                   eval      $PEPbai(xx) = S1Cbai
490600040305e   2c                   endif
490700040305      *
490800040305x   1c                   else
490900040305      *
491000040305     c     S2Cpep        lookup    $BAU(xx)                               10
491100040305      * ...lo trovo sicuro, perchè li ho già memorizzati nelle subr.
491200040305      *   "ContaBAU"
491300040305if  2c                   if            *in10       =  *on
491400040409     c                             and $BAUbai(xx) =  *blanks
491500040309if  3c                   if        S2Cbnl      = 'S'
491600040309     c                   eval      $BAUbai(xx) = S2Cbnl
491700040309x   3c                   else
491800040305     c                   eval      $BAUbai(xx) = S2Cbai
491900040309e   3c                   endif
492000040628     c                   eval      $PosPO(xx)      =  $PO
492100040628     c                   eval      $PosPOdpd(xx)   =  $POdpd
492200040305e   2c                   endif
492300040305      *
492400040305e   1c                   endif
492500040409      *
492600040409      * Memorizzo le postazioni "IMA" immesse per controllare la
492700040409      *  effettiva immissione di quelle obbligatorie
492800161115      *?N.B.?-?Ora le postazioni IMA possono essere nella stessa?
492900161115      *       ?baia con altre postazioni NON IMA.?
493000040409if  1c                   if            *in10       =  *on
493100161115     c                             and $IMA        =  *on
493200040409     c                   eval      xx     = 1
493300040409     c                   eval      *in10  = *off
493400040409if  2c                   if        $VDL   = *off
493500040409     c     S1Cbai        lookup    $MPT(xx)                               10
493600040409x   2c                   else
493700040409     c     S2Cbai        lookup    $MPT(xx)                               10
493800040409e   2c                   endif
493900040409      * - la trova sicuramente !
494000040409if  2c                   if        *in10  = *on
494100040409     c                   add       1             $MPTnr(xx)
494200040409e   2c                   endif
494300040409e   1c                   endif
494400061011      *
494500061011      * Aggiorno anche le postazioni utilizzate per baia
494600061011      *   (per ora utile solo al controllo dell'utilizzo dei P.O.)
494700061011if  1c                   if        $VDL   = *on
494800061011     c                   eval      $PObau(S02nrr)  =  S2Cpep
494900061011     c                   eval      $POpos(S02nrr)  =  S2Cbai
495000061011e   1c                   endif
495100161212      *
495200161212      /free
495300161212
495400161212         // -?Memorizzazione SflRrn per Postazione = Filiale per il?
495500161212         //  ?controllo della Zona (cioè del Turno)?
495600161212         If    $VDL   =  *on    and
495700161212             ( $PO    =  *on  or
495800161212               $POdpd =  *on );
495900161212
496000161212           clear  xx;
496100161212           xx = %lookup ( S2Cbai : sk_Fil );
496200161212
496300161212           if  xx = *zero;
496400161213             xx = %lookup( *blank : sk_Fil );
496500161212             if  xx > *zero;
496600161212               sk_Fil(xx) = S2Cbai;
496700161212             endif;
496800161212           endif;
496900161212
497000161212           if  xx > *zero;
497100161213             select;
497200161213               // -?Memorizzazione zona generica (99)?
497300161215               when  S2Cznc  = '99';
497400161213                 sk_Rrn(xx)  = S02nrr;
497500161213               // -?Memorizzazione zona specifica SE non già?
497600161213               //  ?memorizzata una zona generica (99)?
497700161213               when  sk_Rrn(xx) <= *zero;
497800161213                 sk_Rrn(xx)  = S02nrr * (-1);
497900161213             endsl;
498000161212           endif;
498100161212
498200161212         EndIf;
498300161212
498400161212      /end-free
498500031021      *
498600031021     c                   ENDSR
498700031021      *
498800031021      *---------------------------------------------------------------*
498900040312      * Controllo delle postazioni immesse (per baia)                 *
499000031021      *---------------------------------------------------------------*
499100031021     c     CtrBAIxPEP    BEGSR
499200031021      *
499300040408      * Controllo se immessa ALMENO una postazione "IMA"
499400040408      *                   ed ALMENO una postazione "IM0"
499500040408     c                   clear                   xx
499600040408do  1c                   dou       xx   >= %elem($MPTobbl)
499700040408     c                   eval      xx    = xx + 1
499800040409if  2c                   if            $MPTobbl(xx) = *on
499900040409     c                             and $MPTnr(xx)   = *zeros
500000040408     c                   seton                                          2890
500100040519     c                   eval      V1Dmsg  =  %replace($MPT(xx):
500200040409     c                                        $Msg(31):
500300040409     c                                        %scan('IMA':$Msg(31)))
500400060110     c                   leavesr
500500040408e   2c                   endif
500600040408e   1c                   enddo
500700040312      *
500800040309      * Controllo se immessa almeno una postazione per ogni baia
500900040409      *   (ora SOLO per NO VdL)
501000060110if  1c                   if        $VDL   =  *on
501100060110     c                   leavesr
501200060110e   1c                   endif
501300060110      *
501400031021do  1c                   DO        C_MaxRecSf    xx
501500031021      *
501600040305if  2c                   if            $VDL     =  *off
501700040305     c                             and $PEP(xx) =  *blanks
501800040305     c                             OR  $VDL     =  *on
501900040305     c                             and $BAU(xx) =  *zeros
502000031021     c                   leave
502100031021e   2c                   endif
502200031021      *
502300040305if  2c                   if            $VDL        =  *off
502400040305     c                             and $PEPbai(xx) =  *blanks
502500040305     c                             OR  $VDL        =  *on
502600040305     c                             and $BAUbai(xx) =  *blanks
502700031021     c                   seton                                          2890
502800040315if  2c                   if        $VDL   =  *off
502900040315     c                   eval      C01csr = $CsrRRN(xx)
503000040519     c                   eval      V1Dmsg = %replace($PEP(xx):
503100031021     c                                      $Msg(32):
503200031021     c                                      %scan('&PEP':$Msg(32)))
503300040315x   2c                   else
503400040315     c                   eval      C02csr = $CsrRRN(xx)
503500040315     c                   eval      V1Dmsg = %replace(
503600040315     c                                      %editw($BAU(xx):'0  ')+' ':
503700040315     c                                      $Msg(32):
503800040315     c                                      %scan('&PEP':$Msg(32)))
503900040315e   2c                   endif
504000031021     c                   leave
504100040305e   2c                   endif
504200031021      *
504300031021e   1c                   ENDDO
504400031021      *
504500060110     c                   ENDSR
504600031007      *
504700031007      *---------------------------------------------------------------*
504800040628      * Controllo immissione codice baia                              *
504900031007      *---------------------------------------------------------------*
505000031007     c     ContaPEP      BegSr
505100031007      *
505200031007     c                   eval      xx       =  1
505300031007     c                   eval      *in10    =  *off
505400031007      * Controllo se già immesso
505500031010     c     S1Cpep        lookup    $PEP(xx)                               10
505600031007      * Se NON già immesso: memorizzo
505700031007if  1c                   if        *in10    =  *off
505800031007     c     *blanks       lookup    $PEP(xx)                               10
505900031007if  2c                   if        *in10
506000031007     c                   eval      $PEP(xx) =  S1Cpep
506100031007x   2c                   else
506200031007     c                   eval      xx       =  %elem($PEP)
506300031007     c                   eval      $PEP(xx) =  *hival
506400031007e   2c                   endif
506500040315     c                   eval      $CsrRRN(xx) = S01nrr
506600031007e   1c                   endif
506700031007      *
506800031007     c                   ENDSR
506900031007      *
507000031007      *---------------------------------------------------------------*
507100031007      * Controllo immissione codice baia                              *
507200031007      *---------------------------------------------------------------*
507300031007     c     ContaBAI      BegSr
507400031007      *
507500031007     c                   eval      xx       =  1
507600031007     c                   eval      *in10    =  *off
507700031007      * Controllo se già immesso
507800040423     c     Wpost         lookup    $BAI(xx)                               10
507900031007      * Se NON già immesso: memorizzo
508000031007if  1c                   if        *in10    =  *off
508100031007     c     *blanks       lookup    $BAI(xx)                               10
508200031007if  2c                   if        *in10
508300040423     c                   eval      $BAI(xx) =  Wpost
508400040628x   2c                   else
508500040628     c                   eval      xx       =  %elem($BAI)
508600040628     c                   eval      $BAI(xx) =  *hival
508700031007e   2c                   endif
508800031007e   1c                   endif
508900031007      * Aggiorno conteggio x codice
509000031007     c                   add       1             $BAInr(xx)
509100050629      *
509200050629      *?Se picking su VdL x Messaggeria:?
509300050629      * consento l'ulizzo della stessa postazione più volte,
509400050629      * ma solo nella stessa baia
509500050629if  1c                   if             §OGpkm      =  'S'
509600050629     c                             and  $VDL        =  *on
509700050629     c                             and  $IMA        =  *off
509800050629     c                             and  $PO         =  *off
509900050629      *
510000050629if  2c                   if        $BAIbau(xx) =  *zeros
510100050629     c                   eval      $BAIbau(xx) =  S2Cpep
510200050629e   2c                   endif
510300050629      * - impostazione messaggio di avvertimento
510400050629if  2c                   if             $BAInr(xx)  >  1
510500050629     c                             and  $BAIbau(xx) =  S2Cpep
510600050629     c                   eval      $BAInr(xx)  =  1
510700050629if  3c                   if        $MsgBAIbau  =  *off
510800050629     c                   seton                                        532890
510900050629     c                   eval      V1Dmsg      =  $Msg(49)
511000050629     c                   eval      $MsgBAIbau  =  *on
511100050629     c***                leavesr
511200050629e   3c                   endif
511300050629e   2c                   endif
511400050629      *
511500050629e   1c                   endif
511600031007      *
511700031007     c                   ENDSR
511800031007      *
511900031007      *---------------------------------------------------------------*
512000031007      * Controllo immissione codice baia di uscita                    *
512100031007      *---------------------------------------------------------------*
512200031007     c     ContaBAU      BegSr
512300031007      *
512400031007     c                   eval      xx       =  1
512500031007     c                   eval      *in10    =  *off
512600031007      * Controllo se già immesso
512700031007     c     S2Cpep        lookup    $BAU(xx)                               10
512800031007      * Se NON già immesso: memorizzo
512900031007if  1c                   if        *in10    =  *off
513000031007     c     *zeros        lookup    $BAU(xx)                               10
513100031007if  2c                   if        *in10
513200031007     c                   eval      $BAU(xx) =  S2Cpep
513300040628x   2c                   else
513400040628     c                   eval      xx       =  %elem($BAU)
513500040628     c                   eval      $BAU(xx) =  *hival
513600031007e   2c                   endif
513700040315     c                   eval      $CsrRRN(xx) = S02nrr
513800031007e   1c                   endif
513900031007      *
514000031007     c                   ENDSR
514100031030      *
514200031030      *---------------------------------------------------------------*
514300031030      * Controllo immissione numero foglio viaggio                    *
514400031030      *---------------------------------------------------------------*
514500031030     c     ContaNFV      BegSr
514600031030      *
514700031030     c                   eval      xx       =  1
514800031030     c                   eval      *in10    =  *off
514900031030      * Controllo se già immesso
515000031030     c                   clear                   Wnfv7
515100031030     c                   move      Wnfv          Wnfv7
515200031030     c     Wnfv7         lookup    $NFV(xx)                               10
515300031030      * Se NON già immesso: memorizzo
515400031030if  1c                   if        *in10    =  *off
515500031030     c     *zeros        lookup    $NFV(xx)                               10
515600031030if  2c                   if        *in10
515700031030     c                   eval      $NFV(xx) =  Wnfv7
515800031030x   2c                   else
515900031030     c                   eval      xx       =  %elem($NFV)
516000031030     c                   eval      $NFV(xx) =  *hival
516100031030e   2c                   endif
516200031030      * Se NON già immesso: memorizzo postazione/baia
516300040519     c                   clear                   ds_BaiaPost
516400031030if  2c                   if        $VDL     =  *off
516500040519     c                   move      S1Cpep        dsBPbai
516600040519     c                   movel     S1Cbai        dsBPpos
516700031030x   2c                   else
516800040519     c                   movel     S2Cpep        dsBPbai
516900040519     c                   movel     S2Cbai        dsBPpos
517000031030e   2c                   endif
517100040628     c                   movel     ds_BaiaPost   $NFVbp(xx)
517200040628      * Se già immesso: ne reperisco baia/postazione precedente
517300040628x   1c                   else
517400040628     c                   movel     $NFVbp(xx)    ds_BaiaPost
517500031030e   1c                   endif
517600031030      * Aggiorno conteggio x codice
517700031030     c                   add       1             $NFVnr(xx)
517800031030      *
517900031030     c                   ENDSR
518000031007      *
518100031007      *---------------------------------------------------------------*
518200031007      * Controllo immissione flag baia non letti                      *
518300031007      *---------------------------------------------------------------*
518400031007     c     ContaBNL      BegSr
518500031007      *
518600031007     c                   eval      xx       =  1
518700031007     c                   eval      *in10    =  *off
518800031007      * Controllo se già immesso
518900031007     c     S2Cbnl        lookup    $BNL(xx)                               10
519000031007      * Se NON già immesso: memorizzo
519100031007if  1c                   if        *in10    =  *off
519200031007     c     *blanks       lookup    $BNL(xx)                               10
519300031007if  2c                   if        *in10
519400031007     c                   eval      $BNL(xx) =  S2Cbnl
519500031007x   2c                   else
519600031007     c                   eval      xx       =  %elem($BNL)
519700031007     c                   eval      $BNL(xx) =  *hival
519800031007e   2c                   endif
519900031007e   1c                   endif
520000031007      * Aggiorno conteggio x codice
520100031007     c                   add       1             $BNLnr(xx)
520200031007      *
520300031007     c                   ENDSR
520400031007      *
520500031007      *---------------------------------------------------------------*
520600031007      * Ricerche                                                      *
520700031007      *---------------------------------------------------------------*
520800031007     c     srSEARCH      BEGSR
520900031007      *
521000031007sel 1c                   select
521100040423      *
521200040423      * Postazione (tab. MPT)
521300040423w   1c                   when          Wcampo   = 'S1CBAI    '
521400040423     c                             or  Wcampo   = 'S2CBAI    '
521500040423     c                   clear                   TIBS02ds
521600040423     c                   eval      T02mod = 'R'
521700040423     c                   eval      T02cod = 'MPT'
521800040423     c                   movel     KNSIF         T02sif
521900040423     c                   call      'TIBS02R'
522000040423     c                   parm                    KPJBA
522100040423     c                   parm                    TIBS02ds
522200040423if  1c                   if        T02err = *blanks
522300040423if  2c                   if        Wcampo = 'S1CBAI    '
522400040423     c                   movel     T02ke1        S1Cbai
522500040423x   2c                   else
522600040423     c                   movel     T02ke1        S2Cbai
522700040423e   2c                   endif
522800040423     c                   movel     T02uni        dMFP
522900040423e   1c                   endif
523000031007      *
523100031007      * Numero foglio viaggio
523200031013w   1c                   when          Wcampo   = 'V1CNFV    '
523300031013     c                             or (Wcampo   = 'S1CNFV    '
523400031022     c                              or Wcampo   = 'S2CNFV    '
523500031106     c                              or Wcampo   = 'S3CNFV    '
523600031106     c                              or Wcampo   = 'S4CNFV    ')
523700031007     c                             and $Default = *off
523800040611     c                             and $PO      = *off
523900031111     c                             or  *inKG    = *on
524000031015     c                   reset                   DSLR01
524100031013sel 2c                   select
524200031013w   2c                   when      Wcampo = 'V1CNFV    '
524300031013     c                   eval      DLRnpg = V1Cnpg                              Categoria foglio
524400040408w   2c                   when         (Wcampo = 'S1CNFV    '
524500040408     c                              or Wcampo = 'S3CNFV    '
524600040408     c                              or Wcampo = 'S2CNFV    '
524700040408     c                              or Wcampo = 'S4CNFV    ')
524800040408     c                             and $IMA   = *on
524900031013     c                   eval      DLRnpg = 3                                   Categoria foglio
525000031013e   2c                   endsl
525100031111if  2c                   if        Wcampo <> 'V1CNFV    '
525200031111     c                   z-add     V1Ddfv        DLRgda                         Dal gg/mm/aaaa
525300031111     c                   z-add     SAVdfva       DLRada                         Dal aaaa/mm/gg
525400031111e   2c                   endif
525500031007     c                   z-add     V1Cpog        DLRfgs                         P.O. gestione
525600031007     c                   movel     SDSpgm        DLRpgm                         Nome programma
525700031015     c                   movel(p)  DSLR01        KPJBU
525800031111if  2c                   if        *inKG  = *off
525900031007     c                   call      'FNLR02R'
526000031007     c                   parm                    KPJBA
526100031111x   2c                   else
526200031111     c                   call      'FNLR02R'
526300031111     c                   parm                    KPJBA
526400031111     c                   parm                    $FV
526500031111e   2c                   endif
526600031015     c                   movel     KPJBU         DSLR01
526700031013sel 2c                   select
526800031013w   2c                   when      Wcampo = 'V1CNFV    '
526900031013     c                   move      DLRnfv        V1Cnfv
527000031013w   2c                   when      Wcampo = 'S1CNFV    '
527100031007     c                   move      DLRnfv        S1Cnfv
527200031106     c                   move      DLRnfv        Wnfv
527300031106w   2c                   when      Wcampo = 'S2CNFV    '
527400031106     c                   move      DLRnfv        S2Cnfv
527500031106     c                   move      DLRnfv        Wnfv
527600031022w   2c                   when      Wcampo = 'S3CNFV    '
527700031022     c                   move      DLRnfv        S3Cnfv
527800031022     c                   move      DLRnfv        Wnfv
527900031106w   2c                   when      Wcampo = 'S4CNFV    '
528000031106     c                   move      DLRnfv        S4Cnfv
528100031106     c                   move      DLRnfv        Wnfv
528200031013e   2c                   endsl
528300040611      *
528400040611      * Numero foglio viaggio x P.O. DPD
528500040630      * (Con p.o. 195 si interrogano ANCHE f.v. per P.O. 330)
528600080118      * (...Anche con fil. 196 o 197)
528700040611w   1c                   when         (Wcampo   = 'S2CNFV    '
528800040611     c                              or Wcampo   = 'S4CNFV    ')
528900040611     c                             and $Default = *off
529000040611     c                             and $POdpd   = *on
529100130506     c                   reset                   fnlst3ds
529200040611     c                   eval      DLSdti =  V1Ddfv
529300040621     c                   eval      DLSdtf =  31129999
529400130506     c                   movel(p)  fnlst3ds      KPJBU
529500040611     c                   move      S2Cbai        DLSpo
529600040630     c                   clear                   DLSpo2
529700070521if  2c                   if            S2Cbai   = '195'
529800080118     c                             or  S2Cbai   = '196'
529900070521     c                             or  S2Cbai   = '197'
530000040630     c                   eval      DLSpo2 =  330
530100040630e   2c                   endif
530200130506     c                   call      'FNLST3R'
530300040611     c                   parm                    KPJBA
530400040611     c                   parm                    DLSpo
530500040630     c                   parm                    DLSpo2
530600130506     c                   movel     KPJBU         fnlst3ds
530700040611sel 2c                   select
530800040611      * - non selezionato alcun f.v.
530900040611w   2c                   when      DLSnfv = *zeros
531000040611      * - selezionato il 1° f.v.
531100040621w   2c                   when      Wcampo = 'S2CNFV    '
531200040611     c                   move      DLSnfv        S2Cnfv
531300040611     c                   move      DLSnfv        Wnfv
531400040611      * - selezionato il 2° f.v. (sarebbe proibito!!!)
531500040611w   2c                   when      Wcampo = 'S4CNFV    '
531600040611     c                   move      DLSnfv        S4Cnfv
531700040611     c                   move      DLSnfv        Wnfv
531800040611e   2c                   endsl
531900031007      *
532000031007      * Autotrasportatore
532100031007w   1c                   when          Wcampo   = 'S1CPDC    '
532200031007     c                             or  Wcampo   = 'S2CPDC    '
532300031007     c                   reset                   FNLV24ds
532400031007     c                   z-add     V1Cpog        D24fil
532500031007     c                   movel(p)  FNLV24ds      KPJBU
532600031007     c                   call      'FNLV24R'
532700031007     c                   parm                    KPJBA
532800031007     c                   movel     KPJBU         FNLV24ds
532900031007if  2c                   if        D24pdr <> *zeros
533000031007sel 3c                   select
533100031007w   3c                   when          Wcampo   = 'S1CPDC    '
533200031007     c                             and $Default = *on
533300031007     c                   move      D24pdr        S1Cpdc
533400031007     c                   movel     D24rsc        S1Dpdc
533500031007w   3c                   when          Wcampo   = 'S2CPDC    '
533600031007     c                             and $Default = *on
533700031007     c                   move      D24pdr        S2Cpdc
533800031007     c                   movel     D24rsc        S2Dpdc
533900031007w   3c                   when          Wcampo   = 'S1CPDC    '
534000031007     c                             and $Default = *off
534100031007     c                   move      D24pdr        S1Cpdc
534200031007     c                   movel     D24rsc        S3Dpdc
534300031007w   3c                   when          Wcampo   = 'S2CPDC    '
534400031007     c                             and $Default = *off
534500031007     c                   move      D24pdr        S2Cpdc
534600031007     c                   movel     D24rsc        S4Dpdc
534700031007e   3c                   endsl
534800031007e   2c                   endif
534900031007      *
535000031007e   1c                   endsl
535100031007      *
535200031007     c                   ENDSR
535300031112      *
535400031112      *---------------------------------------------------------------*
535500031112      * Controllo esistenza di distinte aperte non inserite nel piano *
535600031112      *---------------------------------------------------------------*
535700031112     c     CtrlDistAp    BEGSR
535800031112      *
535900031112      * Memorizzo il tasto funzionale F7=Salvataggio e Stampa
536000031112     c                   movel     *inKH         WinKH
536100031112      *
536200031112      * Richiamo pgm. FNLR02R per fare il controllo
536300031112     c                   reset                   DSLR01
536400031112     c                   movel     '3'           DLRtfv
536500031112     c                   z-add     V1Ddfv        DLRgda                         Dal gg/mm/aaaa
536600031112     c                   z-add     SAVdfva       DLRada                         Dal aaaa/mm/gg
536700031112     c                   z-add     V1Cpog        DLRfgs                         P.O. gestione
536800031112     c                   movel     SDSpgm        DLRpgm                         Nome programma
536900031112     c                   movel(p)  DSLR01        KPJBU
537000031112     c                   call      'FNLR02R'
537100031112     c                   parm                    KPJBA
537200031112     c                   parm                    $FV
537300031112     c                   movel     KPJBU         DSLR01
537400031112      *
537500031112      * Emetto (eventualmente) segnalazione a video
537600031112if  1c                   if        DLRfs1 = *on
537700031112     c                   clear                   MS03W01
537800031112     c                   movel     $Msg(41)      W1Dms2
537900031112     c                   eval      W1Dtfa = 'F6=Conferma   '
538000031112     c                                    + 'F12=Ritorno'
538100031112do  2c                   dou       *inKF   or   *inKL
538200031112     c                   exfmt     MS03W01
538300031112e   2c                   enddo
538400031112if  2c                   if        *inKL
538500031112     c                   movel     *off          WinKH
538600031112e   2c                   endif
538700031112e   1c                   endif
538800031112      *
538900031112     c                   ENDSR
539000080129      *
539100080129      *---------------------------------------------------------------*
539200080129      * Controllo mancaato inserimento di F.V. per filiali DPD        *
539300080129      *---------------------------------------------------------------*
539400080129     c     CtrlFVdpd     BEGSR
539500080129      *
539600080129      * Memorizzo il tasto funzionale F7=Salvataggio e Stampa
539700080129     c                   movel     *inKH         WinKH
539800080129      *
539900080129      * Conteggio delle segnalazioni di filiali DPD senza F.V.
540000080129     c                   clear                   wx
540100080129     c                   clear                   wy
540200080129do  1c                   DOW       wx < %elem($ORGerr)
540300080129     c                   add       1             wx
540400080129if  2c                   if        $ORGerr(wx) =  *on
540500080129     c                   eval      wy = wy + 1
540600080129e   2c                   endif
540700080129e   1c                   ENDDO
540800080129if  1c                   if        wy = *zeros
540900080129     c                   leavesr
541000080129e   1c                   endif
541100080129      *
541200080129      * Avviso del totale di tali segnalazioni già effettuate
541300080129     c                   clear                   MS03W01
541400080129     c                   movel     $Msg(51)      W1Dms1
541500080129     c                   eval      W1Dms1  =  %replace(
541600080129     c                                          %editc(wy:'Z'):
541700080129     c                                          $Msg(51):
541800080129     c                                          %scan( '&FV':$Msg(51) ) )
541900080129     c                   eval      W1Dms2  = $Msg(52)
542000080129     c                   eval      W1Dms3  = $Msg(53)
542100080129     c                   eval      W1Dtfa  = 'Enter=Conferma'
542200080129     c                                     + '   '
542300080129     c                                     + 'F12=Ritorno'
542400080129do  1c                   dou           $Tasto = C_Enter
542500080129     c                             or  $Tasto = C_F12
542600080129     c                   exfmt     MS03W01
542700080129e   1c                   enddo
542800080129      *
542900080129sel 1c                   select
543000080129w   1c                   when      $Tasto  = C_F12
543100080129     c                   eval      WinKH   = *off
543200080129w   1c                   when      WinKH   = *off
543300080129     c                   eval      *inKF   = *on
543400080211x   1c                   other
543500080211     c                   eval      *inKH   = WinKH
543600080129e   1c                   endsl
543700080129      *
543800080129     c                   ENDSR
543900031007      *
544000031007      *---------------------------------------------------------------*
544100031007      * Aggiornamento dati nel file FLTR900F                          *
544200031007      *---------------------------------------------------------------*
544300031007     c     Upd_FLTR9     BEGSR
544400031007      *
544500031007      * Cancellazione dei record esistenti
544600031007     c                   exsr      Dlt_FLTR9
544700031007      *
544800031007      * Aggiunta dei record impostati a video nel sfl
544900031107     c                   reset                   SAVpep
545000031107     c                   reset                   SAVbau
545100031107     c                   reset                   SAVpdr
545200031007     c                   clear                   S01nrr
545300031007     c                   clear                   S02nrr
545400031107      *
545500031009do  1c                   DO        MAXnrr
545600031007      *
545700031007sel 2c                   select
545800031007w   2c                   when      $Video = '2'
545900031007     c                   add       1             S01nrr
546000031007     c     S01nrr        chain     MS03S01
546100031007w   2c                   when      $Video = '3'
546200031007     c                   add       1             S02nrr
546300031007     c     S02nrr        chain     MS03S02
546400031007w   2c                   when      $Video = '4'
546500031007     c                   add       1             S01nrr
546600031007     c     S01nrr        chain     MS03S03
546700031007w   2c                   when      $Video = '5'
546800031007     c                   add       1             S02nrr
546900031007     c     S02nrr        chain     MS03S04
547000031007e   2c                   endsl
547100031007      *
547200031007if  2c                   if        NOT %found(FIMS03D)
547300031007     c                   leave
547400031007e   2c                   endif
547500031007      *
547600040409if  2c                   if             $VDL     = *off
547700040409     c                             and (S1Cpep   = *blanks
547800040409     c                             and  S1Cdes   = *blanks
547900040409     c                             and  S1Cbai   = *blanks
548000040409     c                             and (S1Cnfv   = *blanks
548100040409     c                              or  S1Cnfv   = *zeros)
548200040409     c                             and (S1Cpdc   = *blanks
548300040409     c                              or  S1Cpdc   = *zeros))
548400040409     c                             OR   $VDL     = *on
548500040409     c                             and (S2Cpep   = *zeros
548600040409     c                             and  S2Cdes   = *blanks
548700040409     c                             and  S2Cbnl   = *blanks
548800040409     c                             and  S2Cbai   = *blanks
548900161215     c                             and  S2Cznc   = *blanks
549000040409     c                             and (S2Cnfv   = *blanks
549100040409     c                              or  S2Cnfv   = *zeros)
549200040409     c                             and (S2Cpdc   = *blanks
549300040409     c                              or  S2Cpdc   = *zeros))
549400031007     c                   iter
549500031007e   2c                   endif
549600031007      *
549700031007     c                   exsr      Wrt_FLTR9
549800031007      *
549900031007e   1c                   ENDDO
550000031007      *
550100031007     c                   ENDSR
550200031007      *
550300031007      *---------------------------------------------------------------*
550400031007      * Delete da FLTR900F dei dati esistenti                         *
550500031007      *---------------------------------------------------------------*
550600031007     c     Dlt_FLTR9     BEGSR
550700031007      *
550800031007     c                   eval      TR9fgs   = V1Cpog
550900040415if  1c                   if        V1Cnfv   = *zeros
551000040415     c                   clear                   TR9npga
551100040415     c                   clear                   TR9nfva
551200040415     c                   else
551300040415     c                   move      V1Cnpg        TR9npga
551400031013     c                   move      V1Cnfv        TR9nfva
551500040415e   1c                   endif
551600171103     c                   eval      TR9tps   = V1Ctps
551700171103     c     K04FLTR9      setll     FLTR9000
551800031007      *
551900171103     c                   dou       NOT  %found(FLTR902L)
552000171103     c     K04FLTR9      delete    FLTR9000
552100031007     c                   enddo
552200031007      *
552300031007     c                   ENDSR
552400031007      *
552500031007      *---------------------------------------------------------------*
552600031007      * Write  in FLTR900F del singolo record impostato nel subfile   *
552700031007      *---------------------------------------------------------------*
552800031007     c     Wrt_FLTR9     BEGSR
552900040305      *
553000060113      * Calcolo del progressivo per baia in caso di immissione
553100060113      *   postazione in default
553200060113      *   (nel piano giornaliero NON è possibile inserire altre baie o
553300060113      *   postazioni...)
553400040305sel 1c                   select
553500060111      *
553600060113w   1c                   when           $Default = *on
553700060113     c                             and  $VDL     = *off
553800060113     c                             and  S1Cpep  <> SAVpep
553900060111     c                   eval      *in10    = *off
554000060111     c                   eval      xx       = 1
554100060111     c     S1Cpep        lookup    $PEP(xx)                               10
554200060111if  2c                   if        NOT *in10
554300060111     c     *blanks       lookup    $PEP(xx)                               10
554400060111if  3c                   if        *in10
554500060113     c                   eval      $PEP(xx) =  S1Cpep
554600060111e   3c                   endif
554700060111e   2c                   endif
554800060113     c                   eval      SAVpep   = S1Cpep
554900060111      *
555000060113w   1c                   when           $Default = *on
555100060113     c                             and  $VDL     = *on
555200060113     c                             and  S2Cpep  <> SAVbau
555300060111     c                   eval      *in10    = *off
555400060111     c                   eval      xx       = 1
555500060111     c     S2Cpep        lookup    $BAU(xx)                               10
555600060111if  2c                   if        NOT *in10
555700060111     c     *zeros        lookup    $BAU(xx)                               10
555800060111if  3c                   if        *in10
555900060111     c                   eval      $BAU(xx) = S2Cpep
556000060111e   3c                   endif
556100060111e   2c                   endif
556200040305     c                   eval      SAVbau   = S2Cpep
556300060111      *
556400040305e   1c                   endsl
556500060111      *
556600040621      * incremento progressivo per baia (anche per "XXX")
556700060113sel 1c                   select
556800060113      * - Baia "XXX" in piano giornaliero
556900060113if  1c                   when           $VDL     = *off
557000040621     c                             and  $Default = *off
557100040621     c                             and  S1Cbai   = C_PostazNull
557200040621     c                             OR   $VDL     = *on
557300040621     c                             and  $Default = *off
557400040621     c                             and  S2Cbai   = C_PostazNull
557500060113     c                   add       2             SAVprg
557600040621     c                   leavesr
557700060113      * - Piano giornaliero VDL
557800060113w   1c                   when           $VDL     = *on
557900060113     c                             and  $Default = *off
558000060113     c                   eval      SAVprg   = H2Cprg
558100161110      * - Piano giornaliero NON VDL
558200060113w   1c                   when           $VDL     = *off
558300060113     c                             and  $Default = *off
558400060113     c                   eval      SAVprg   = H1Cprg
558500060113      * - Piano di default
558600060113w   1c                   when           $Default = *on
558700060113     c                   add       1             SAVprg
558800060113e   1c                   endsl
558900061011      *
559000061011      * Incremento progressivo per postazione = P.O.
559100061011     c                   clear                   wFL3
559200061011if  1c                   if        $VDL     = *on
559300061011do  2c                   do        S02nrr        wx
559400061011if  3c                   if        S2Cbai   = $POpos(wx)
559500061011     c                   add       1             wFL3
559600061011e   3c                   endif
559700061011e   2c                   enddo
559800061011      * (almeno il record in esame è già stato conteggiato
559900061011      *  SE NON piando di default)
560000061011if  2c                   if        wFL3     = *zeros
560100061011     c                   eval      wFL3     = 1
560200061011e   2c                   endif
560300061011e   1c                   endif
560400031007      *
560500031007     c                   clear                   FLTR9000
560600031007      *
560700031007     c                   eval      TR9fgs   = V1Cpog
560800040408if  1c                   if        V1Cnfv   > *zeros
560900031007     c                   eval      TR9npga  = V1Cnpg
561000031013     c                   move      V1Cnfv        TR9nfva
561100040415e   1c                   endif
561200171103     c*//                eval      TR9tps   = V1Ctps
561300040305     c                   move      SAVprg        TR9prg
561400031007      *
561500031007      * campi x NO VDL
561600031007if  1c                   if        $VDL     = *off
561700031007      *
561800031010     c                   move      S1Cpep        TR9pep
561900031007     c                   eval      TR9des   = S1Cdes
562000171102if  2c                   eval      %subst( TR9des : 28 : 1 ) = V1Ctps
562100031030     c*** (solo VDL)     eval      TR9bnl   = *blanks
562200031007     c                   eval      TR9bai   = S1Cbai
562300031013if  2c                   if            S1Cpdc <> *blanks
562400031013     c                             and S1Cpdc <> *zeros
562500031021     c                   movel     V1Cpog        TR9pdc
562600031007     c                   move      S1Cpdc        TR9pdc
562700031010e   2c                   endif
562800031007if  2c                   if        $Default = *off
562900040423     c                   eval      $Hidden  = *off
563000040423     c                   eval      Wpost    = S1Cbai
563100040408     c                   exsr      Ctrl_IMA
563200040408if  3c                   if        $IMA     = *on
563300031007     c                   eval      TR9npg   = 3
563400031007x   3c                   else
563500031007     c                   eval      TR9npg   = 4
563600031007e   3c                   endif
563700031007     c                   move      S1Cnfv        TR9nfv
563800031007     c                   clear                   WLBda8
563900031007     c                   eval      G08dat   = S1Ddfv
564000031007     c                   eval      G08err   = '0'
564100031007     c                   call      'XSRDA8'
564200031007     c                   parm                    WLBda8
564300031021     c                   eval      TR9dfv   = G08inv
564400031022     c                   eval      TR9fl1   = S1Dfpp
564500031007e   2c                   endif
564600031007      *
564700031007      * campi x SÌ VDL
564800031007x   1c                   else
564900031007      *
565000031007     c                   movel(p)  S2Cpep        TR9pep
565100031007     c                   eval      TR9des   = S2Cdes
565200171102if  2c                   eval      %subst( TR9des : 28 : 1 ) = V1Ctps
565300170214if  2c*//?NON impostato:?if        $PO      = *off         ?($PO non è ancora impostato)?
565400170214if  2c                   if        S2Cznc  <> *blank
565500161215     c                   eval      %subst( TR9des : 29 ) = S2Cznc
565600161213e   2c                   endif
565700031007     c                   eval      TR9bnl   = S2Cbnl
565800040305     c                   eval      TR9bai   = S2Cbai
565900031013if  2c                   if            S2Cpdc <> *blanks
566000031013     c                             and S2Cpdc <> *zeros
566100031021     c                   movel     V1Cpog        TR9pdc
566200031007     c                   move      S2Cpdc        TR9pdc
566300031010e   2c                   endif
566400031007if  2c                   if        $Default = *off
566500031007if  3c                   if        S2Cnfv   > *zeros
566600040423     c                   eval      $Hidden  = *off
566700040423     c                   eval      Wpost    = S2Cbai
566800040408     c                   exsr      Ctrl_IMA
566900040615     c                   exsr      Ctrl_PO
567000040615sel 4c                   select
567100161213w   4c                   when      $PO      = *on
567200040615     c                   eval      TR9npg   = 1
567300040615w   4c                   when      $IMA     = *on
567400040309     c                   eval      TR9npg   = 3
567500040615x   4c                   other
567600040309     c                   eval      TR9npg   = 4
567700040615e   4c                   endsl
567800040309e   3c                   endif
567900031007     c                   move      S2Cnfv        TR9nfv
568000031007     c                   clear                   WLBda8
568100031007     c                   eval      G08dat   = S2Ddfv
568200031007     c                   eval      G08err   = '0'
568300031007     c                   call      'XSRDA8'
568400031007     c                   parm                    WLBda8
568500031021     c                   eval      TR9dfv   = G08inv
568600031029     c                   eval      TR9fl1   = S2Dfpp
568700031007e   2c                   endif
568800061011     c                   move      wFL3          TR9fl3
568900031007      *
569000031007e   1c                   endif
569100040702      *
569200040702      * campi "comuni"
569300040702if  1c                   if        V1Cdpd   = 'S'
569400040702     c                   move      'D'           TR9fl2
569500040702e   1c                   endif
569600040702      *                  __________________
569700031007     c                   WRITE     FLTR9000
569800040702      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
569900031022      *
570000031022      * EVENTUALE 2° record per 2° F.V.
570100031022      *
570200040621if  1c                   if           $Default = *off
570300040621     c                   add       1             SAVprg
570400040621e   1c                   endif
570500040415if  1c                   if           $Default = *off
570600040415     c                             and $VDL    = *off
570700031022     c                             and S3Cnfv <> *blanks
570800031022     c                             and S3Cnfv <> *zeros
570900040415     c                             OR $Default = *off
571000040415     c                             and $VDL    = *on
571100031106     c                             and S4Cnfv <> *blanks
571200031106     c                             and S4Cnfv <> *zeros
571300031022      *
571400040621     c*** già sopra:     add       1             SAVprg
571500040305     c                   eval      TR9prg   = SAVprg
571600031106     c                   clear                   WLBda8
571700031106     c                   eval      G08err   = '0'
571800031106if  2c                   if        $VDL     = *off
571900031022     c                   move      S3Cnfv        TR9nfv
572000031022     c                   eval      G08dat   = S3Ddfv
572100031022     c                   call      'XSRDA8'
572200031022     c                   parm                    WLBda8
572300031022     c                   eval      TR9dfv   = G08inv
572400031022     c                   eval      TR9fl1   = S3Dfpp
572500031106x   2c                   else
572600031106     c                   move      S4Cnfv        TR9nfv
572700031106     c                   eval      G08dat   = S4Ddfv
572800031106     c                   call      'XSRDA8'
572900031106     c                   parm                    WLBda8
573000031106     c                   eval      TR9dfv   = G08inv
573100031106     c                   eval      TR9fl1   = S4Dfpp
573200031106e   2c                   endif
573300040702      *                  __________________
573400031022     c                   WRITE     FLTR9000
573500040702      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
573600031022e   1c                   endif
573700031007      *
573800031007     c                   ENDSR
573900031007
574000031007      *---------------------------------------------------------------*
574100031007      * SCHIERE A TEMPO DI COMPILAZIONE                               *
574200031007      *---------------------------------------------------------------*
574300151106** -- $MSG ------------------------------------------------------------------*
574400070530Immettere la filiale gestione                                                     1
574500070530Filiale gestione errata                                                           2
574600070530Filiale non abilitata al picking diretto                                          3
574700031007Categoria foglio obbligatoria                                                     4
574800031007Categoria foglio errata                                                           5
574900070530Filiale gestione non abilitata all'utilizzo della categoria foglio immessa        6
575000040312Immettere il numero Foglio Viaggio                                                7
575100031010Immettere un numero foglio arrivi esistente e non annullato                       8
575200031009             Mancano ANCHE i dati di default...                                   9
575300031211Immettere il codice baia                                                         10
575400031211Codice baia errato                                                               11
575500070530Filiale non abilitata al piano DPD                                               12
575600031211Immettere il codice postazione                                                   13
575700031211Codice postazione errato                                                         14
575800031211Codice postazione già inserito                                                   15
575900040629Impossibile specificare codice AUT per codice postazione "IMA"                   16
576000031009Codice autotrasportatore errato                                                  17
576100070530Impossibile specificare codice AUT per codice postazione "Filiale"               18
576200040315         CANCELLARE il piano giornaliero in esame ?                              19
576300031009Numero Foglio Viaggio errato                                                     20
576400040702Immettere un numero Foglio Viaggio valido                                        21
576500040408F.V. IMA diverso da quello già impostato in questo piano (&FIMA)                 22
576600070530Immettere un numero F.V. valido e con filiale arrivo &PS                         23
576700031009Immettere la baia di uscita                                                      24
576800031009Baia di uscita errata (deve essere compresa tra 01 e 98)                         25
576900040305Impossibile specificare codice postazione per "baia non letti"                   26
577000031009Flag "Baia Non Letti" errato: 'S' = sì / ' ' = no                                27
577100031009Flag "Baia Non Letti" già specificato per un'altra baia di uscita                28
577200031009Impossibile specificare codice autotrasportatore per "baia non letti"            29
577300031009Immettere il numero foglio IMA                                                   30
577400031211Immettere almeno una postazione "IMA"                                            31
577500031211Immettere almeno una postazione valida per la baia &PEP                          32
577600070530Filiale già specificata come postazione                                          33
577700040519Autotrasportatore &PADR   già impegnato in postazione &PS di baia &B             34
577800031022Immettere un 2° F.V. diverso dal 1° F.V.                                         35
5779000310222° F.V. assegnato ad autotrasportatore (&PADR  ) diverso da quello del 1° F.V.   36
578000040629Immettere SOLO un F.V. per postazione "IMA"                                      37
578100031022F.V. di data antecedente a quella del F.V. arrivi                                38
578200040312Foglio Viaggio già inserito per la postazione &PS                                39
578300171107Foglio Viaggio già inserito nel piano smistamento per                            40
578400031112ATTENZIONE: esistono distinte aperte non inserite nel piano.                     41
578500070530Immettere SOLO un F.V. per postazione "Filiale DPD"                              42
578600070530Foglio Viaggio con filiale di arrivo &PS                                         43
578700070530La postazione "Filiale DPD"  deve essere univoca per baia di uscita              44
578800060110?         Esiste un piano smistamento per picking.          |                   ?45
578900060110   Premere F6 per cancellarlo e proseguire con piano DPD,   |                    46
579000060110   altrimenti premere F12.                                  |                    47
579100070530     Mancano postazioni "Filiale DPD" nel piano di default.                      48
579200050629Esistono postazioni impostate più volte nella stessa baia - ENTER per forzare    49
579300080130Filiale DPD senza F.V. - ENTER per forzare                                       50
579400080129?           Lasciate &FV filiali DPD senza F.V.             |                   ?51
579500080130   Premere Invio per confermare; altrimenti premere F12.    |                    52
579600080130                                                            |                    53
579700090203Immettere una "Baia Non Letti" (obbligatoria)                                    54
579800151109Esiste già il Piano Smistamento Picking _         per il giorno __/__/____       55
579900151106Rilevato errore nell'estrazione dei dati del F.V. arrivi: avvisare CED           56
580000161110La stessa baia può contenere SOLO postaz. IMA/per autotrasp. o SOLO "Filiali"    57
580100161212Per la linea di defluenza ___ è obbligatoria la zona 99 in almeno una baia       58
580200161214La ZONA è inseribile solo per linee di defluenza                                 59
580300161214Immettere una Zona per la linea di defluenza ___                                 60
580400161215Zona errata                                                                      61
580500171103Il piano smistamento DPD può essere SOLO di tipo VDL                             62
