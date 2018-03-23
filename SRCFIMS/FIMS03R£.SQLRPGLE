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
006600031007      *
006700031009      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
006800031007      *
006900031007      * - Messaggi di errore
007000171103     d $Msg            s             78    dim(62) ctdata  perrcd(1)
007100031010      * - P.O. in £1
007200080129     d £1              s              3  0 dim(30)         inz
007300040705     d £1pog           s              3    dim(30)         inz
007400040607      * - P.O. in AZORG
007500080129     d $ORGfil         s              3    dim(500)        inz
007600080129     d $ORGdes         s             20    dim(500)        inz
007700080129     d $ORGntw         s                   dim(500)        inz
007800040611     d                                     like(§OGntw)
007900080129     d $ORGerr         s               n   dim(500)        inz(*off)
008000040607      * - P.O. in AZORG: nr. rec. di utilizzo
008100040607     d $ORGnr          s              3  0 dim(500)        inz
008200031007      * - Parcel Entrance Point ammessi x P.O. in gestione
008300160714     d $MSLpep         s                   dim(80)         inz
008400031111     d                                     like(§MSLpep)
008500040408      * - Tab. "MPT": postazioni "IMA" previste
008600040408     d $MPT            s                   dim(20)         inz
008700040408     d                                     like(TR9bai)
008800040423     d $MPTdes         s                   dim(20)         inz
008900040423     d                                     like(§MPTdes)
009000040408      * - Tab. "MPT": flag obbligatorietà x postazione "IMA"
009100040409     d $MPTobbl        s              1    dim(20)         inz(*off)
009200040409      * - Tab. "MPT": nr. rec. di utilizzo x postazione "IMA"
009300040409     d $MPTnr          s              3  0 dim(20)         inz
009400040409      *
009500031111      * - F.V. immessi a video (da scartare da FNLR02R)
009600031111     d $FV             s                   dim(C_MaxRecPf) inz
009700031111     d                                     like(FVVnfv)
009800040315      *
009900040315      * - SFLrrn di riferimento per le schiere $PEP/$BAU
010000040315     d $CsrRRN         s                   dim(C_MaxRecSf) inz
010100040315     d                                     like(C01csr)
010200031007      *
010300040315      * - Codici Baia (no VdL) immessi nel sfl
010400031111     d $PEP            s                   dim(C_MaxRecSf) inz
010500031111     d                                     like(S1Cpep)
010600031021      *
010700050629      * - Codice (prima) Postazione x Baia (no VdL) immessa nel sfl
010800031111     d $PEPbai         s                   dim(C_MaxRecSf) inz
010900031111     d                                     like(S1Cbai)
011000031007      *
011100040315      * - Codici Postazione (VdL & non) immessi nel sfl
011200031111     d $BAI            s                   dim(C_MaxRecSf) inz
011300031111     d                                     like(S1Cbai)
011400040315      * - Numero record del sfl x Postazione (VdL & non)
011500031111     d $BAInr          s              3  0 dim(C_MaxRecSf) inz
011600050629      * - Codice Baia di Uscita (sì VdL) corrispondente alla Postazione
011700050629     d $BAIbau         s                   dim(C_MaxRecSf) inz
011800050629     d                                     like(S2Cpep)
011900031007      *
012000040315      * - Codici Baia di Uscita (sì VdL) immessi nel sfl
012100031111     d $BAU            s                   dim(C_MaxRecSf) inz
012200031111     d                                     like(S2Cpep)
012300040305      *
012400050629      * - Codice (prima) Postazione x Baia (sì VdL) immessa nel sfl
012500040305     d $BAUbai         s                   dim(C_MaxRecSf) inz
012600040305     d                                     like(S2Cbai)
012700040607      *
012800040607      * - Flag Postazione P.O. (sì VdL) x Baia immessa nel sfl
012900040607      *   (in corrispondenza alla schiera $BAUbai)
013000040607     d $PosPO          s              1    dim(C_MaxRecSf) inz(*off)
013100040628     d $PosPOdpd       s              1    dim(C_MaxRecSf) inz(*off)
013200061011      *
013300061011      * - Memo Baie e Postazioni (sì VdL) immesse nel sfl
013400061011      *   (per ctrl P.O. x baia)
013500061011     d $PObau          s                   dim(C_MaxRecSf) inz
013600061011     d                                     like(S2Cpep)
013700061011     d $POpos          s                   dim(C_MaxRecSf) inz
013800061011     d                                     like(S2Cbai)
013900161212      *
014000161212      * -?Memo Filiali (Postazioni - sì VdL) immesse nel sfl?
014100161212      *  ?(per ctrl Zone, alias Turno)?
014200161212     d sk_Fil          s                   dim(c_MaxRecSf) inz
014300161212     d                                     like(S2Cbai)
014400161212     d sk_Rrn          s                   dim(c_MaxRecSf) inz
014500161212     d                                     like(S02nrr)
014600040615      *
014700040701      * - Memo F.V. x P.O. DPD (sì VdL) x Baia immessa nel sfl
014800040701     d $FVxPOa         s                   dim(C_MaxRecSf) inz
014900040615     d                                     like(TR9bai)
015000040701     d $FVxPOn         s             +2    dim(C_MaxRecSf) inz
015100040615     d                                     like(FGVnfv)
015200040701     d $FVxPOd         s                   dim(C_MaxRecSf) inz
015300040615     d                                     like(FGVdfv)
015400031007      *
015500040315      * - Codici Baia  Non Letti (sì VdL) immessi nel sfl
015600031111     d $BNL            s                   dim(C_MaxRecSf) inz
015700031111     d                                     like(S2Cbnl)
015800040315      * - Numero record del sfl x Baia Non Letti (sì VdL)
015900031111     d $BNLnr          s              3  0 dim(C_MaxRecSf) inz
016000031030      *
016100171107      * - Numeri F.V. immessi con l'altro "Tipo Piano Smistamento"
016200171107      *   (previsto ora SOLO per la fil. 006-PD)
016300171107     d sk_NFV2         s                   dim(C_MaxRecPf) inz
016400171107     d                                     like(TR9nfv)
016500031030      * - Numeri F.V. immessi nel sfl
016600031111     d $NFV            s                   dim(C_MaxRecPf) inz
016700031111     d                                     like(TR9nfv)
016800040628      * - Baia/Postazione nel sfl x F.V.
016900040628     d $NFVbp          s                   dim(C_MaxRecPf) inz
017000040628     d                                     like(ds_BaiaPost)
017100031030      * - Numero record del sfl x F.V
017200031111     d $NFVnr          s              3  0 dim(C_MaxRecPf) inz
017300031009      * - - codici autotrasportatori
017400040519     d $MEMat          s                   dim(C_MaxRecSf) inz
017500031111     d                                     like(TR9pdc)
017600040519      * - - codici baia/postazione x autotrasportatore
017700040519     d $MEMbp          s                   dim(C_MaxRecSf) inz
017800040519     d                                     like(ds_BaiaPost)
017900031009      * - - data/numero ultimi F.V. x autotrasportatore
018000040519     d $MEMfv1         s             15  0 dim(C_MaxRecSf) inz(*loval)
018100040519     d $MEMfv2         s             15  0 dim(C_MaxRecSf) inz(*loval)
018200031022      * - - flag prestazione autotrasportatore (M-mattino P-pomeriggio)
018300040519     d $MEMmp1         s              1    dim(C_MaxRecSf) inz
018400040519     d $MEMmp2         s              1    dim(C_MaxRecSf) inz
018500050701      * - - tipo zona cons. x F.V./autotrasp. (P-Parcel M-Messaggerie)
018600050701     d $MEMpm1         s              1    dim(C_MaxRecSf) inz
018700050701     d $MEMpm2         s              1    dim(C_MaxRecSf) inz
018800031111      * - - numero F.V. x autotrasportatore
018900031111      *     per evidenziare "più F.V. per autotrasportatore"
019000040519     d $MEMxf          s              3  0 dim(C_MaxRecSf) inz
019100040630      *
019200040630      * - Schiera P.O. x F.V. da FNFGV
019300040630     d xFGVffv         s              3    dim(80)         inz
019400031007      *
019500031007      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
019600031007      *
019700031007     d KPJBA         e ds
019800080129      *
019900031007      * - Parametri x Controllo profilo utenti
020000080122     d TIBS34ds      e ds                  inz
020100031007      * - Ds di riferimento al file esterno AZUTE00F
020200080122     d AZUTEds       e ds                  extname(AZUTE00F)
020300031007      * - Ds per dati organigramma
020400080122     d dDatiUte      e ds
020500040623      * - Strutture dati "di comodo"
020600040623     d FLTR9ds       e ds                  extname(FLTR900F) inz
020700040623     d FLXR9ds       e ds                  extname(FLTR900F) inz
020800040623     d                                     prefix(XR9:3)
020900031010      * - Parametri x Caricamento £1
021000080122     d TRUL06ds      e ds                  inz
021100080122     d   Lin                   1     90  0 dim(30)
021200031010      * - Parametri x reperimento terminal di linea
021300040628     d FNLV55ds      e ds                  inz
021400040611      * - Ds descrizione 143 dell'organigramma
021500040611     d OG143         e ds                  inz
021600031007      * - Ds descrizione 150 dell'organigramma
021700031007     d OG150         e ds                  inz
021800050614      * - Ds lettura campo APDFLR di FIAPD00F
021900050614     d dAPDflr       e ds                  inz
022000050614      * - Ds lettura campo FVVSPN di FNFVV00F
022100050614     d dFVVspn       e ds                  inz
022200031015      * - Parametri standard nuovi x SRCFNLS
022300031015     d DSLR01        e ds                  inz
022400031015     d   DLRric      e                     inz('S')
022500031113     d   DLRnpg      e                     inz(4)
022600031015     d   DLRtfv      e                     inz('2')
022700031113     d   DLRgal      e                     inz(31129999)
022800031113     d   DLRaal      e                     inz(99991231)
022900031007      * - Parametri x Interrogazione tabelle
023000080122     d TIBS02ds      e ds                  inz
023100031007     d   T02mod      e                     inz('C')
023200031007     d   T02cod      e                     inz('MFP')
023300031007      * - Tab. "MFP"
023400031007     d dMFP          e ds                  inz
023500031007     d   §MFPap                1     10    dim(10)
023600040408      * - Tab. "MPT"
023700040408     d dMPT          e ds                  inz
023800031007      * - Tab. "MSL"
023900031007     d dMSL          e ds                  inz
024000040302      * - Tab. "7N"
024100040302     d ds7N          e ds                  inz
024200171205      * - ?Tab. "VPO" / "DECO"?
024300171205      *   ?(vedi progetto 898 - Doppio picking in contemporanea)?
024400171205     d dVPOdeco      e ds                  qualified    inz
024500171205     d   §VPO2PkPd   e                     inz(*hival)
024600031007      * - Parametri x interrogazione autotrasportatori
024700031007     d FNLV24ds      e ds                  inz
024800031007     d   D24tip      e                     inz('A')
024900031007     d   D24flg      e                     inz('R')
025000031029      * - Parametri x lista Piano di Smistamento per Picking
025100031029     d FIMS04ds      e ds                  inz
025200031007      *
025300031007     d Status         sds           333
025400031007     d   SDSpgm          *proc
025500080129      *
025600080129     d InfDspF         ds           512
025700080129     d  $Tasto               369    369                                         AID byte
025800031009      *
025900031022     d DS_FV           ds            15    inz
026000031009     d   dsFVdfv                      8s 0 inz
026100031009     d   dsFVnfv                      7s 0 inz
026200040519      *
026300040519     d DS_BaiaPost     ds            13    inz
026400040519     d   dsBPbai                     -4    like(TR9pep) inz
026500040519     d   dsBPpos                           like(TR9bai) inz
026600031007      *
026700031007     d WLBda8          ds                  inz
026800031007     d   G08dat                       8  0 inz
026900031007     d   G08inv                       8  0 inz
027000031106     d   G08err                       1    inz('3')
027100031007     d   G08tgi                       5  0 inz
027200031021      *
027300031021     d DSdata8         ds             8    inz
027400031021     d   DSdt8gg                      2  0 inz
027500031021     d   DSdt8mm                      2  0 inz
027600031021     d   DSdt8aa                      4  0 inz
027700031021     d DSdata6         ds             6    inz
027800031021     d   DSdt6gg                      2  0 inz
027900031021     d   DSdt6mm                      2  0 inz
028000031021     d   DSdt6aa                      2  0 inz
028100040611      *
028200130506     d fnlst3ds        ds                  inz
028300040611     d   DLSfl1                       1    inz('R')
028400040611     d   DLSfl2                       1    inz
028500040611     d   DLSnfv                       5p 0 inz
028600040611     d   DLSdti                       9p 0 inz
028700040611     d   DLSdtf                       9p 0 inz
028800040611     d   DLStfv                       1    inz('2')
028900040611     d   DLSfv                        5    inz      dim(45)
029000040611     d   DLSfgs                       3  0 inz
029100151106
029200151106       // -?Dati estratti via SQL?
029300151106     d ds_NFVxDFV      ds                  inz  qualified
029400151106     d   TR9fgs                            inz  like(TR9fgs)
029500151106     d   TR9npga                           inz  like(TR9npgA)
029600151106     d   TR9nfva                           inz  like(TR9nfvA)
029700151109     d   TR9fl2                            inz  like(TR9fl2)
029800151106     d   FVVdfv                            inz  like(FVVdfv)
029900151106     d   wNFVxDFV                     5  0 inz
030000031007      *
030100031007      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
030200031007      *
030300031007      * - Flags
030400031007     d $Fine           s              1    inz(*off)
030500031007     d $InzD01         s              1    inz(*on)
030600031007     d $InzS01         s              1    inz(*on)
030700031007     d $InzS02         s              1    inz(*on)
030800031023     d $EoF            s              1    inz(*off)
030900031007     d $EndSFL         s              1    inz(*off)
031000031007     d $Default        s              1    inz(*off)
031100031009     d $NewFVa         s              1    inz(*off)
031200040701     d $DltPS          s              1    inz(*off)
031300031007     d $VDL            s              1    inz(*off)
031400040408     d $IMA            s              1    inz(*off)
031500040423     d $IMAdft         s              1    inz(*off)
031600040607     d $PO             s              1    inz(*off)
031700040611     d $POdpd          s              1    inz(*off)
031800080129     d $POerr          s              1    inz(*off)
031900040607     d $POdft          s              1    inz(*off)
032000040423     d $Hidden         s              1    inz(*off)
032100031023     d $Found          s              1    inz(*off)
032200171102     d $First          s               n   inz(*on)
032300050629     d $MsgBAIbau      s              1    inz(*off)
032400031112     d WinKH           s              1    inz(*off)
032500031020      *
032600031007     d $Video          s              1    inz('1')
032700031007      *
032800031007      * - Indici di schiera
032900031007     d XX              s              3  0 inz
033000031107     d YY              s              3  0 inz
033100040607     d XY              s              3  0 inz
033200040630     d WX              s              3  0 inz
033300080129     d WY              s              3  0 inz
033400031007      *
033500031007      * - Variabili riferite al data base o al display file
033600031007     d SAVpog          s                   like(V1Cpog) inz(*loval)
033700031106     d SAVpep          s                   like(S1Cpep) inz(*loval)
033800031107     d SAVbau          s                   like(S2Cpep) inz(*loval)
033900031022     d SAVnfvIMA       s                   like(S1Cnfv) inz(*zeros)
034000031022     d SAVnfv          s                   like(S1Cnfv) inz(*zeros)
034100031022     d SAVdfva         s                   like(FVVdfv) inz
034200031022     d SAVpdr          s                   like(DSTpdr) inz
034300040305     d SAVprg          s                   like(TR9prg) inz
034400031007     d S01nrr          s                   like(C01rcd) inz
034500031007     d S02nrr          s                   like(C02rcd) inz
034600031007     d MAXnrr          s                   like(C01rcd) inz
034700090203     d Free_Rec_Sfl    s                   like(C01rcd) inz
034800031007     d Wpdr            s                   like(DSTpdr) inz
034900040621     d Wdfv            s                   like(FVVdfv) inz
035000031022     d Wnfv            s                   like(S1Cnfv) inz
035100031030     d Wnfv7           s                   like(TR9nfv) inz
035200040423     d Wpost           s                   like(TR9bai) inz
035300031024     d Wast            s                   like(S1Cast) inz
035400040611     d DLSpo           s                   like(TR9fgs) inz
035500040630     d DLSpo2          s                   like(TR9fgs) inz
035600031007      *
035700031007      * - Variabili definite a programma
035800031112     d n08             s              8  0                                      *numerico 8
035900031112     d n14             s             14  0                                      *numerico 14
036000031007     d Wdate           s              8  0                                      *data corrente a/m/g
036100031022     d wNFVseq         s              1  0 inz                                  *sequenza F.V.
036200061011     d wFL3            s              2  0 inz
036300031009     d w0150           s             15  0 inz
036400031007     d Wcampo          s             10    inz
036500031023     d w002a           s              2    inz
036600040705     d w003a           s              3    inz
036700031010     d w014a           s             14    inz
036800151106     d wDate_Eur       s               d   inz   datfmt(*EUR)
036900151106
037000151106       // -?Stringhe SQL da eseguire?
037100151106     d wSQL            s          32740    inz   varying
037200151106
037300151106       //--------------------------------------------------------------
037400171107       //? Definizione prototipi procedure.                            ?
037500151106       //--------------------------------------------------------------
037600151106
037700151106       // -?Parametri API QCAPCMD (Process Commands)?
037800151106     d*// Qcmd            s           2048    inz  varying
037900151106      /copy qSysInc/qRpgleSrc,QCAPCMD
038000151106       // -?API QCAPCMD (Process Commands)?
038100151106      /copy gaitrasrc/srcProtoPR,QCAPCMD
038200151106
038300151106       // -?Parametri gestione errori API.?
038400151106      /copy qsysinc/qrpglesrc,QUSEC
038500171107
038600171107       //--------------------------------------------------------------
038700171107       //? Definizione key-list.                                       ?
038800171107       //--------------------------------------------------------------
038900171107
039000171107       // -?File FLTR902L?
039100171107     d keyFLTR902    e ds                  extname( FLTR902L : *key )
039200171107     d                                     prefix(k_)   inz
039300031007
039400031007      *---------------------------------------------------------------*
039500031007      *  RIEPILOGO INDICATORI                                         *
039600031007      *---------------------------------------------------------------*
039700031010      * 01    - Visualizza "Piano di default"  come sotto-titolo      *
039800031010      * 02    - Visualizza "Piano giornaliero" come sotto-titolo      *
039900040705      * 03    - Visualizza richiesta per "Piano DPD" in D01           *
040000050615      * 04    - Visualizza dati per gestione picking MESSAGGERIA.     *
040100171102      * 05    - Visualizza richiesta per "Tipo Piano Smistamento" (PD)*
040200031007      * 10    - Comodo                                                *
040300031007      * 28    - Emette il messaggio di errore a video                 *
040400031007      * 30    - Ripulisce il subfile                                  *
040500031007      * 31    - NON emette il subfile                                 *
040600031007      * 32    - Record modificato nel subfile (sflnxtchg)             *
040700031007      * 33    - Fine dati nel subfile         (sflend)                *
040800040702      * 39    - Codice P.O. in gestione   protetto     (PR)           *
040900040702      *        ?se?livello P.O. utente = '2'                          *
041000031007      * 41    - P.O. in gestione errato                               *
041100031007      * 42    - Categoria foglio errato                               *
041200031007      * 43    - Numero F.V. errato                                    *
041300040705      * 44    - P.O. non abilitato al piano smistamento DPD           *
041400171102      * 45    - Tipo Piano Smistamento errato                         *
041500031007      * 51    - Codice postazione / baia-uscita  errato               *
041600031022      * 52    - Descrizione mancante (NON usato)                      *
041700031007      * 53    - Codice baia  errato                                   *
041800031022      * 54    - Numero foglio viaggio  errato                         *
041900031007      * 55    - Codice autotrasportatore  errato                      *
042000031022      * 56    - 2° Numero foglio viaggio  errato                      *
042100040304      * 57    - Flag "baia non letti"  errato                         *
042200161212      * 58    - Zona (cioè Turno) per Baia errata                     *
042300040623      * 80    - Tutti i campi I/O del sfl protetti     (PR)           *
042400040623      *         descr. autotrasportatore  sottolineato (UL)           *
042500040309      *         (baia "non letti" in piano giornaliero per VdL)       *
042600040623      * 85    - Codice autotrasportatore  in evidenza  (RI)           *
042700031023      *         (variato in piano giornaliero rispetto al default)    *
042800040628      *         (variata postazione in piano giornaliero rispetto al  *
042900040628      *         default)                                              *
043000040623      * 86    - Descr. autotrasportatore  in evidenza  (HI)           *
043100040628      *         (postazione IMx o P.O.)                               *
043200050629      * 87    - Flag Messaggeria attivo                (RI)           *
043300050629      *         (Autotrasportatore Messaggeria - in default;          *
043400050629      *          1°  F.V.          Messaggeria - in smistamento)      *
043500050614      * 88    - Flag Messaggerie attivo                (RI)           *
043600050629      *         (2°  F.V.          Messaggeria - in smistamento)      *
043700031007      * 90    - Generico di errore                                    *
043800031007      *---------------------------------------------------------------*
043900031007
044000031007     c     *Entry        plist
044100031007     c                   parm                    KPJBA
044200031007      *
044300031007      * Operazioni Iniziali
044400040705     c                   exsr      RoutInz
044500031007      *
044600031007      * Gestione Video
044700031007do  1c                   DOW       $Fine = *off
044800031007      * - Filtro di lancio
044900031007cas 2c     $Video        caseq     '1'           GesD01
045000031007      * - Window con messaggio
045100031007cas 2c     $Video        caseq     'W'           GesW01
045200031106      * - SubFile (Default o Giornaliero, con VDL o senza VDL)
045300031106cas 2c                   cas                     GesSFL
045400031007e   2c                   endcs
045500031007e   1c                   ENDDO
045600031007      *
045700031007      * Fine
045800171103     c                   close     FLTR902X
045900171103     c                   eval      Qcmd = 'DLTOVR  FILE(FLTR902X)'
046000040623     c                   call      'QCMDEXC'
046100040623     c                   parm                    Qcmd
046200040623     c                   parm      50            Qlen
046300040623      *
046400031010     c                   clear                   TRUL06ds
046500031010     c                   eval      D06tla = 'C'
046600031010     c                   movel(p)  TRUL06ds      KPJBU
046700031010     c                   call      'TRUL06R'
046800031010     c                   parm                    KPJBA
046900031010     c                   clear                   FNLV55ds
047000031010     c                   eval      D55tla = 'C'
047100031010     c                   call      'FNLV55R'
047200031010     c                   parm                    FNLV55ds
047300031010     c                   reset                   TIBS02ds
047400031010     c                   eval      T02tla = 'C'
047500031010     c                   call      'TIBS02R'
047600031010     c                   parm                    KPJBA
047700031010     c                   parm                    TIBS02ds
047800031007     c                   movel     *on           *inLR
047900031007      *
048000031007      *---------------------------------------------------------------*
048100031007      * Operazioni Iniziali                                           *
048200031007      *---------------------------------------------------------------*
048300040705     c     RoutInz       BEGSR
048400031007      *
048500031007      * Definizione Key-List
048600171103      * - FLTR902L
048700171103     c     K04FLTR9      klist
048800031009     c                   kfld                    TR9fgs
048900031009     c                   kfld                    TR9npgA
049000031009     c                   kfld                    TR9nfvA
049100171103     c                   kfld                    TR9tps
049200171103     c     K06TR902      klist
049300040623     c                   kfld                    TR9fgs
049400040623     c                   kfld                    XR9npgA
049500040623     c                   kfld                    XR9nfvA
049600171103     c                   kfld                    TR9tps
049700040623     c                   kfld                    TR9pep
049800040621     c                   kfld                    XR9prg
049900171103     c     K07TR902      klist
050000040623     c                   kfld                    TR9fgs
050100040623     c                   kfld                    XR9npgA
050200040623     c                   kfld                    XR9nfvA
050300171103     c                   kfld                    TR9tps
050400040623     c                   kfld                    TR9pep
050500040621     c                   kfld                    XR9prg
050600040621     c                   kfld                    TR9bai
050700031007      * - FIAPD01L
050800031007     c     K02FIAPD      klist
050900031007     c                   kfld                    APDtip
051000031007     c                   kfld                    APDpdr
051100031007     c                   eval      APDtip = 'A'
051200031007      * - FNFVV03L
051300031007     c     K03FNFVV      klist
051400031007     c                   kfld                    FVVfgs
051500031007     c                   kfld                    FVVnpg
051600031007     c                   kfld                    FVVnfv
051700031007     c     K02FNFVV      klist
051800031007     c                   kfld                    FVVfgs
051900031007     c                   kfld                    FVVnpg
052000031009      * - FNFVV08L
052100031009     c     K03FVV08      klist
052200031010     c                   kfld                    FV8fgs
052300031010     c                   kfld                    FV8npg
052400031010     c                   kfld                    FV8dfv
052500031010     c     K02FVV08      klist
052600031010     c                   kfld                    FV8fgs
052700031010     c                   kfld                    FV8npg
052800120613      * - FIDST01L
052900120613     c     K03FIDST      klist
053000120613     c                   kfld                    DSTnpg
053100120613     c                   kfld                    DSTnfv
053200120613     c                   kfld                    DSTfgs
053300040621      * - FNFGV02L
053400040621     c     K03FGV02      klist
053500040621     c                   kfld                    FGVlnp
053600040621     c                   kfld                    FGVdfv
053700040621     c                   kfld                    FGVnfv
053800040621      * - FNFGV01L
053900040621     c     K02FGV01      klist
054000040621     c                   kfld                    FGVnfv
054100040621     c                   kfld                    FGVlnp
054200031007      * - TNTBE01L
054300031007     c     K02TNTBE      klist
054400031007     c                   kfld                    TBEcod
054500031007     c                   kfld                    TBEke1
054600040302      * - TABEL00F
054700040227     c     KTAB          klist
054800040227     c                   kfld                    TBLkut
054900040227     c                   kfld                    TBLcod
055000040227     c                   kfld                    TBLkey
055100040227     c                   eval      TBLkut = 1
055200031007      *
055300031007      * Reperisco dati job
055400031007     c                   exsr      DatiJob
055500031008     c                   movel     SDSpgm        V1Tpgm
055600031007      *
055700031007      * Reperisco la data corrente
055800031007     c                   time                    n14                            *Ora (6)+ Data (8)
055900031007     c                   move      n14           n08                            *Data (8) in g/m/a
056000031106     c                   clear                   WLBda8
056100031007     c                   z-add     n08           G08dat
056200031106     c                   eval      G08err = '0'
056300031007     c                   call      'XSRDA8'
056400031007     c                   parm                    WLBda8
056500031007     c                   z-add     G08inv        Wdate                          *Data corrente a/m/g
056600031010      *
056700031010      * Carico schiera P.O. gestiti (£1)
056800031010     c                   clear                   TRUL06ds
056900031010     c                   movel     '£1'          D06cod
057000040701     c                   movel     DUTpou        D06key
057100031010     c                   movel(p)  TRUL06ds      KPJBU
057200031010     c                   call      'TRUL06R'
057300031010     c                   parm                    KPJBA
057400031010     c                   movel     KPJBU         TRUL06ds
057500040701     c                   movea     Lin           £1
057600040607      *
057700040607      * Carico schiera di tutti i P.O.
057800040607     c                   clear                   xx
057900040607     c                   clear                   $ORGfil
058000040607     c                   clear                   $ORGdes
058100040607     c*** qui non serve: clear                   $ORGnr
058200080129     c*** qui non serve: reset                   $ORGerr
058300040607     c     *loval        setll     AZORG
058400040607     c                   read      AZORG
058500040607do  1c                   dow       NOT %eof(AZORG01L)
058600040607if  2c                   if        ORGfva = *blanks
058700040607     c                   add       1             xx
058800040607     c                   move      ORGfil        $ORGfil(xx)
058900040607     c                   movel     ORGdes        $ORGdes(xx)
059000040611     c                   movel     ORGde3        Og143
059100040611     c                   movel     §OGntw        $ORGntw(xx)
059200040607e   2c                   endif
059300040607     c                   read      AZORG
059400040607e   1c                   enddo
059500171205      *
059600171205      * - ?Reperimento data attivazione del "Doppio Picking" per fil. 006-PD?
059700171205     c                   reset                   dVPOdeco
059800171205     c                   eval      TBEcod = 'VPO'
059900171205     c                   eval      TBEke1 = 'DECO'
060000171205     c     K02TNTBE      chain     TNTBE000
060100171205if  1c                   if        %found(TNTBE01L)
060200171205     c                   eval      dVPOdeco = TBEuni
060300171205e   1c                   endif
060400040623      *
060500040623      * Override
060600171103     c                   eval      Qcmd = 'OVRDBF  FILE(FLTR902X)'
060700171103     c                                  +      ' TOFILE(FLTR902L)'
060800040623     c                   call      'QCMDEXC'
060900040623     c                   parm                    Qcmd             50
061000040623     c                   parm      50            Qlen             15 5
061100171103     c                   open      FLTR902X
061200031007      *
061300031007     c                   ENDSR
061400031007      *
061500031007      *---------------------------------------------------------------*
061600031007      * Reperimento Dati del job (Utente/Operativi)                   *
061700031007      *---------------------------------------------------------------*
061800031007     c     DatiJob       BEGSR
061900031007      *
062000031007     c     *dtaara       define    §azute        azuteds
062100031007     c     *dtaara       define    §datiute      ddatiute
062200031007      *
062300031007     c                   in(E)     *dtaara
062400031007     c                   IF        %ERROR or RSUT = *blanks
062500031007     c                   clear                   Tibs34Ds
062600031007     c                   call      'TIBS34R'
062700031007     c                   parm                    Tibs34Ds
062800031007     c                   in        *dtaara
062900031007     c                   ENDIF
063000031007      *
063100031007     c                   ENDSR
063200031007      *
063300031007      *---------------------------------------------------------------*
063400031007      * Gestione videata D01                                          *
063500031007      *---------------------------------------------------------------*
063600031007     c     GesD01        BEGSR
063700031007      *
063800031007      * Inizializzo la videata
063900031007if  1c                   if        $InzD01  = *on
064000031007     c                   exsr      InzD01
064100031007     c                   movel     *off          $InzD01
064200031007e   1c                   endif
064300031014      * Reimposto il numero F.V. a *blank se *zero (numerico editato)
064400031014if  2c                   if        V1Cnfv =  *zeros
064500031014     c                   eval      V1Cnfv =  *blanks
064600031014e   2c                   endif
064700031007      *
064800031007      * Scrivo la testata e la riga tasti funzionali abilitati
064900170117if  1c*//                if        NOT *in28
065000031007     c                   write     MS03T01
065100031007     c                   write     MS03Z01
065200170117e   1c*//                endif
065300031007      * Emetto la videata
065400031007     c                   exfmt     MS03D01
065500031007     c                   setoff                                       28  90
065600031007     c                   clear                   V1Dmsg
065700031007      *
065800031007sel 1c                   select
065900031007      * F3=Fine
066000031007w   1c                   when      *inKC
066100031007     c                   exsr      F03D01
066200060110     c                   leavesr
066300031007      *
066400031007e   1c                   endsl
066500031007      *
066600031007      * Controllo dati immessi a video
066700031007     c                   exsr      CtrD01
066800060110     c                   if        *in90
066900060110     c                   leavesr
067000060110     c                   endif
067100031009      *
067200031009      * Inizializzazione schiere, flag, variabili, ecc.
067300031009     c                   exsr      InzALL_1
067400031106     c                   eval      $Default = (V1Cnfv = *zeros)
067500040705      *
067600040705      * Testo eventuali sostituzioni tra piano std e piano dpd
067700040705if  1c                   if            $Default = *off
067800040705     c                             and $NewFVa  = *off
067900040705     c                   exsr      Check_PS
068000040705if  2c                   if        *inKC  or  *inKL
068100060110     c                   leavesr
068200040705e   2c                   endif
068300040705e   1c                   endif
068400031009      *
068500031009      * Caricamento dati da tab. MSL
068600031009     c                   exsr      Tab_MSL
068700031007      *
068800031007      * Controllo se P.O. in gestione ha VDL
068900031007     c                   exsr      Ctrl_VDL
069000171123      *
069100171123      * - Caricamento distinte eventualmente già inserite dalla
069200171123      *   fil. 006 per l'altro "Tipo Piano Smistamento"
069300171123     c                   clear                   sk_NFV2
069400171123     c                   clear                   wy
069500171123if  1c                   if        V1Cpog   = c_PO_a  and
069600171123     c                             $Default = *off
069700171123     c                   exsr      sr_CarDist_2
069800171123e   1c                   endif
069900031009      *
070000031110      * Se piano giornaliero:
070100031106      * Intabellamento dati di default   &
070200031106      * Memorizzazione dati per il reperimento dell'ultimo foglio
070300031106      *   consegne per autotrasportatore
070400040309if  1c                   if        $Default = *off
070500031020     c                   exsr      Memo_DFT
070600031009e   1c                   endif
070700031010      *
070800031106      * Passaggio alla videata successiva
070900031010if  1c                   if        $Default = *on
071000031010     c                   movea     '10'          *in(01)
071100031010x   1c                   else
071200031010     c                   movea     '01'          *in(01)
071300031010e   1c                   endif
071400031007      *
071500031007if  1c                   if        $VDL     = *off
071600031007      *
071700031007     c                   eval      $InzS01  = *on
071800031007if  2c                   if        $Default = *on
071900031007     c                   eval      $Video   = '2'
072000031007x   2c                   else
072100031007     c                   eval      $Video   = '4'
072200031007e   2c                   endif
072300031007      *
072400031007x   1c                   else
072500031007      *
072600031007     c                   eval      $InzS02  = *on
072700031007if  2c                   if        $Default = *on
072800031007     c                   eval      $Video   = '3'
072900031007x   2c                   else
073000031007     c                   eval      $Video   = '5'
073100031007e   2c                   endif
073200031007      *
073300031007e   1c                   endif
073400031007      *
073500060110     c                   ENDSR
073600031007      *
073700031007      *---------------------------------------------------------------*
073800031007      * Inizializzazione videata D01                                  *
073900031007      *---------------------------------------------------------------*
074000031007     c     InzD01        BEGSR
074100031007      *
074200031007      * Imposto i dati di default
074300031010if  1c                   if        V1Cpog = *zeros
074400031007     c                   eval      V1Cpog = DUTpou
074500031007     c                   eval      V1Cnpg = 2
074600031010e   1c                   endif
074700031010     c                   clear                   V1Cnfv
074800031014     c                   clear                   V1Dnfv
074900031010     c                   clear                   V1Ddfv
075000031007      *
075100031007     c                   clear                   V1Dmsg
075200031007     c                   setoff                                       28  90
075300031007     c                   movea     *zeros        *in(41)
075400031010      *
075500031010      * Posiziono il cursore sul campo "numero F.V."
075600031010     c                   eval      *in43  = *on
075700040702      *
075800040702      * Proteggo il campo "P.O. in gestione" se P.O. utente di 2° livello
075900040705     c                   eval      *in39  = (DUTlpo   = '2')
076000040705      *
076100040705      * Controllo se P.O. abilitato a piano DPD
076200040705     c                   eval      *in03  = *off
076300040705     c                   eval      xx     = *zeros
076400040705do  1c                   dow       xx     < %elem(£1)
076500040705     c                   eval      xx     = xx + 1
076600040705if  2c                   if        £1(xx) = *zeros
076700040705     c                   leave
076800040705e   2c                   endif
076900040705     c                   eval      yy     = 1
077000040705     c                   eval      *in10  = *off
077100040705     c                   move      £1(xx)        w003a
077200040705     c     w003a         lookup    $ORGfil(yy)                            10
077300040705if  2c                   if            *in10       =  *on
077400040705     c                             and $ORGntw(yy) =  'DPD'
077500040705     c                   eval      *in03  = *on
077600040705     c                   leave
077700040705e   2c                   endif
077800040705e   1c                   enddo
077900070530if  1c                   if            V1Cpog      = 049
078000070530     c                             and *in03       = *on
078100070530     c                   eval      V1Cdpd = 'S'
078200070530e   1c                   endif
078300171102      *
078400171102      * - ?Controllo se Filiale Gestione = 006-PD?
078500171102      *   ?(per abilitazione richiesta "Tipo Piano Smistamento")?
078600171205     c*//                eval      *in05  = (V1Cpog   = c_PO_a)
078700171205     c                   eval      *in05  = (V1Cpog   = c_PO_a  and
078800171205     c                                       wDate   >= dVPOdeco.§VPO2PkPd)
078900171102      *
079000171102      * - ?Impostazione "Tipo Piano Smistamento" di default?
079100171102      *   ?(sempre e comunque - anche se gestito solo per 006-PD)?
079200171103sel 1c                   select
079300171103w   1c                   when      V1Cpog =  SAVpog
079400171103w   1c                   when      V1Cpog =  c_PO_a
079500171103     c                   eval      V1Ctps = 'R'
079600171103x   1c                   other
079700171103     c                   clear                   V1Ctps
079800171103e   1c                   endsl
079900171102     c                   exsr      Ctrl_VDL
080000171102      * - ?Decodifica filiale?
080100171102      *   ?(per evitare inutili riemissioni)?
080200171102     c                   exsr      DecodPOG
080300171102if  1c                   if        *in90   = *off
080400171102     c                   eval      SAVpog  = V1Cpog
080500171102x   1c                   else
080600171102     c                   reset                   SAVpog
080700171102e   1c                   endif
080800031007      *
080900031007     c                   ENDSR
081000031007      *
081100031007      *---------------------------------------------------------------*
081200031007      * Gestione tasto funzionale F03 da videata D01                  *
081300031007      *---------------------------------------------------------------*
081400031007     c     F03D01        BEGSR
081500031007      *
081600031007      * Chiudo il programma
081700031007     c                   eval      $Fine = *on
081800031007      *
081900031007     c                   ENDSR
082000031007      *
082100031007      *---------------------------------------------------------------*
082200031007      * Controllo dati immessi in videata D01                         *
082300031007      *---------------------------------------------------------------*
082400031007     c     CtrD01        BEGSR
082500031007      *
082600031113     c                   reset                   $NewFVa
082700040702     c                   reset                   $DltPS
082800031007     c                   movea     *zeros        *in(41)
082900151109      * -?Spegnimento indicatore di errore SQL?
083000151109     c                   eval      *in34 = *off
083100031007      *
083200031007      * Controllo P.O. in gestione
083300171102     c                   eval      *in05   = *off
083400031007if  1c                   if        V1Cpog <> SAVpog
083500031007     c                   exsr      DecodPOG
083600031009if  2c                   if        *in90   = *off
083700171106if  3c                   if        V1Cpog  = c_PO_a  or
083800171106     c                             SAVpog  = c_PO_a
083900171106     c                   eval      *in90   = *on
084000171106e   3c                   endif
084100031009     c                   eval      SAVpog  = V1Cpog
084200031007x   2c                   else
084300031007     c                   reset                   SAVpog
084400031007e   2c                   endif
084500171103      * - ?Impostazione richiesta "Tipo Piano Smistamento"?
084600171103      *   ?(anche se visualizzata solo per la fil. 006-PD)?
084700171103if  2c                   if        SAVpog  = c_PO_a
084800171103     c                   eval      V1Ctps  = 'R'
084900171103x   2c                   else
085000171103     c                   clear                   V1Ctps
085100171103e   2c                   endif
085200171103     c                   exsr      Ctrl_VDL
085300031007e   1c                   endif
085400171102      * - ?(Ri)Controllo se Filiale Gestione = 006-PD?
085500171102      *   ?(per abilitazione richiesta "Tipo Piano Smistamento")?
085600171205     c*//                eval      *in05   = (V1Cpog = c_PO_a)
085700171205     c                   eval      *in05   = (V1Cpog = c_PO_a  and
085800171205     c                                        wDate >= dVPOdeco.§VPO2PkPd)
085900060110     c                   if        *in90
086000060110     c                   leavesr
086100060110     c                   endif
086200031007      *
086300031007      * Controllo categoria (tipo)
086400031007sel 1c                   select
086500031007w   1c                   when          V1Cnpg =  *zeros
086600031013     c                             and V1Cnfv <> *zeros
086700031013     c                             and V1Cnfv <> *blanks
086800031007     c                   seton                                        422890
086900031007     c                   movel     $Msg(4)       V1Dmsg
087000031007w   1c                   when          V1Cnpg <> *zeros
087100031007     c                             and V1Cnpg <> 2
087200031007     c                   seton                                        422890
087300031007     c                   movel     $Msg(5)       V1Dmsg
087400031007e   1c                   endsl
087500060110     c                   if        *in90
087600060110     c                   leavesr
087700060110     c                   endif
087800040302      *
087900040302      * Controllo categoria da TABEL (se NON default)
088000040302if  1c                   if            V1Cnpg <> *zeros
088100040302     c                             or (V1Cnfv <> *zeros
088200040302     c                             and V1Cnfv <> *blanks)
088300040302      *
088400040227     c                   movel     '7N'          TBLcod
088500040312     c                   movel(p)  V1Cnpg        TBLkey
088600040227     c     KTAB          chain     TABEL
088700040227      * - inesistente
088800040227if  2c                   if        NOT %found(TABEL00F)
088900040227     c                             or  TBLflg <> *blanks
089000040227     c                   seton                                        422890
089100040227     c                   movel     $Msg(5)       V1Dmsg
089200060110     c                   leavesr
089300040227e   2c                   endif
089400040227     c                   movel     TBLuni        ds7N
089500040227      * - non utilizzabile dal P.O. in gestione
089600040227if  2c                   if        §7Nuti = '9'
089700040227     c                   seton                                        422890
089800040227     c                   movel     $Msg(5)       V1Dmsg
089900060110     c                   leavesr
090000040227e   2c                   endif
090100040227      * - utilizzabile solo da P.O. di diverso livello
090200040227if  2c                   if           (§7Nuti =  '1'
090300040227     c                             and V1Cpog <> D55tfp)
090400040227     c                             OR (§7Nuti =  '2'
090500040227     c                             and V1Cpog =  D55tfp)
090600040227     c                   seton                                        422890
090700040227     c                   movel     $Msg(6)       V1Dmsg
090800060110     c                   leavesr
090900040227e   2c                   endif
091000040302      *
091100040302e   1c                   endif
091200040302      *
091300031007      * Controllo numero foglio viaggio
091400031013      * - se *blanks lo considero *zeros (sarebbe numerico)
091500031013if  1c                   if        V1Cnfv =  *blanks
091600031013     c                   eval      V1Cnfv =  *zeros
091700031013e   1c                   endif
091800031013      * - ricerca
091900031013     c     '?'           scan      V1Cnfv                                 10
092000031013if  1c                   if        *in10  =  *on
092100040415      * - - per interrogare il foglio è obbligatorio inserirne il tipo
092200040415      *     => ora fisso a "2"
092300040415if  2c                   if        V1Cnpg =  *zeros
092400040415     c                   seton                                        422890
092500040415     c                   movel     $Msg(5)       V1Dmsg
092600060110     c                   leavesr
092700040415e   2c                   endif
092800031013     c                   eval      V1Cnfv =  *zeros
092900031013     c                   eval      Wcampo =  'V1CNFV    '
093000031013     c                   exsr      srSEARCH
093100040705     c                   eval      *in90  =  *on
093200031013e   1c                   endif
093300031013      * - controlli:
093400031013      * - - numericità
093500031013     c     DigitN        check     V1Cnfv                                 10
093600031013if  1c                   if        *in10  =  *on
093700031013     c                   seton                                        542890
093800031013     c                   movel     $Msg(20)      V1Dmsg
093900060110     c                   leavesr
094000031013e   1c                   endif
094100040415      * - - effettiva immissione
094200040415if  1c                   if            V1Cnpg <> *zeros
094300040415     c                             and V1Cnfv =  *zeros
094400040415     c                   seton                                        432890
094500040415     c                   movel     $Msg(7)       V1Dmsg
094600060110     c                   leavesr
094700040415e   1c                   endif
094800031009      *
094900040701     c*** (già fatto)    reset                   $NewFVa
095000031009if  1c                   IF        V1Cnfv <> *zeros
095100031013      * - - inserimento in FLTR9
095200031009     c                   eval      TR9fgs  = V1Cpog
095300031009     c                   eval      TR9npga = V1Cnpg
095400031013     c                   move      V1Cnfv        TR9nfva
095500171103     c                   eval      TR9tps  = V1Ctps
095600171103     c     K04FLTR9      setll     FLTR9000
095700171103if  2c                   if        NOT %equal(FLTR902L)
095800031009     c                   eval      $NewFVa = *on
095900031013      * - - esistenza in FNFVV
096000031009     c                   eval      FVVfgs  = V1Cpog
096100031009     c                   eval      FVVnpg  = V1Cnpg
096200031013     c                   move      V1Cnfv        FVVnfv
096300031009     c     K03FNFVV      chain     FNFVV000
096400031009if  3c                   if        NOT %found(FNFVV03L)
096500031009     c*** (già da vl)              or  FVVatb <> *blanks
096600031009     c*** (già da vl)              or  FVVfcf <> *blanks
096700151106     c                   seton                                        432890
096800031009     c                   movel     $Msg(8)       V1Dmsg
096900060110     c                   leavesr
097000031010x   3c                   else
097100031106     c                   reset                   WLBda8
097200040705     c                   eval      G08inv  = FVVdfv
097300031010     c                   call      'XSRDA8'
097400031010     c                   parm                    WLBda8
097500040705     c                   eval      V1Ddfv  = G08dat
097600040705     c                   eval      SAVdfva = FVVdfv
097700031009e   3c                   endif
097800031013x   2c                   else
097900031013      * - - già esistente in FLTR9 (=> reperisco la data del F.V.)
098000040705     c                   eval      FVVfgs  = V1Cpog
098100040705     c                   eval      FVVnpg  = V1Cnpg
098200031013     c                   move      V1Cnfv        FVVnfv
098300031013     c     K03FNFVV      chain     FNFVV000
098400031013if  3c                   if            %found(FNFVV03L)
098500031013     c*** (già da vl)              and FVVatb <> *blanks
098600031013     c*** (già da vl)              and FVVfcf <> *blanks
098700031106     c                   reset                   WLBda8
098800040705     c                   eval      G08inv  = FVVdfv
098900031013     c                   call      'XSRDA8'
099000031013     c                   parm                    WLBda8
099100040705     c                   eval      V1Ddfv  = G08dat
099200040705     c                   eval      SAVdfva = FVVdfv
099300031013e   3c                   endif
099400031009e   2c                   endif
099500151106      /free
099600151106
099700151106           // -?Verifica esistenza altri F.V. per la stessa data?
099800151109           //  ?(NON se Piano di Default)?
099900151109           //If  $NewFVA = *on;
100000151109           If  V1Cnpg <> *zero;
100100151106
100200151106             exsr  sr_OpenCursor;
100300151106
100400151106             If  Not *in34;
100500151106
100600151106               exsr  sr_ReadCursor;
100700151106               exsr  sr_CloseCursor;
100800151106
100900151109               //if  ds_NFVxDFV.TR9nfva > *zeros;
101000151109               if  ds_NFVxDFV.TR9nfva > *zero
101100151109                   and        $NewFVA = *on;
101200151106
101300151109                 wDate_Eur = %date( SavDFVa : *ISO );
101400151106                 *in43  = *on;
101500151106                 *in28  = *on;
101600151106                 *in90  = *on;
101700151106                 V1Dmsg = $Msg(55);
101800151109                 V1Dmsg = %replace( %trim( %editc(
101900151109                                    ds_NFVxDFV.TR9nfva : '2' ) )
102000151106                                  : V1Dmsg
102100151109                                  : %scan( '_      ' : V1Dmsg ) );
102200151109                 V1Dmsg = %replace( %editc( %dec( wDate_Eur ) : 'Y' )
102300151106                                  : V1Dmsg
102400151106                                  : %scan( '__/__/____' : V1Dmsg ) );
102500151106                 leavesr;
102600151106
102700151106               endif;
102800151106
102900151106             Else;
103000151106
103100151106               *in28  = *on;
103200151106               *in90  = *on;
103300151106               V1Dmsg = $Msg(56);
103400151106               leavesr;
103500151106
103600151106             EndIf;
103700151106
103800151109           EndIf;
103900151106
104000151106      /end-free
104100031009e   1c                   ENDIF
104200040705      * Anche se ho solo eseguito una ricerca ("?") del F.V.
104300040705      *   riemetto questa videata prima di proseguire con i
104400040705      *   controlli relativi al flag S/N DPD.
104500040705if  1c                   if        *in90   = *on
104600040705     c                   eval      *in44   =*on
104700060110     c                   leavesr
104800040705e   1c                   endif
104900040705      *
105000040705      * Se richiesto un piano di default, ma impostato piano DPD:
105100040705      *   vince il piano di default
105200040705if  1c                   if            V1Cnpg =  *zeros
105300040705     c                             and V1Cnfv =  *zeros
105400040705     c                   clear                   V1Cdpd
105500040705e   1c                   endif
105600040705      *
105700040705      * Controllo se P.O. abilitato a piano DPD
105800040705if  1c                   if        V1Cdpd  = 'S'
105900040705     c                   eval      *in90   = *on
106000040705     c                   eval      xx      = *zeros
106100040705do  2c                   dow       xx      < %elem(£1pog)
106200040705     c                   eval      xx      = xx + 1
106300040705if  3c                   if        £1pog(xx) <= *zeros
106400040705     c                   leave
106500040705e   3c                   endif
106600040705     c                   eval      yy      = 1
106700040705     c                   eval      *in10   = *off
106800040705     c     £1pog(xx)     lookup    $ORGfil(yy)                            10
106900040705if  3c                   if            *in10       =  *on
107000040705     c                             and $ORGntw(yy) =  'DPD'
107100040705     c                   eval      *in90   = *off
107200040705     c                   leave
107300040705e   3c                   endif
107400040705e   2c                   enddo
107500040705if  2c                   if        *in90   = *on
107600040705     c                   seton                                        442890
107700040705     c                   movel     $Msg(12)      V1Dmsg
107800060110     c                   leavesr
107900040705e   2c                   endif
108000040705e   1c                   endif
108100171103      *
108200171103      * - Controllo se richiesto il piano DPD ma con tipo Rulliera
108300171103if  1c                   if             V1Cdpd  = 'S'
108400171103     c                             and  V1Ctps  = 'R'
108500171103     c                   seton                                        452890
108600171103     c                   movel     $Msg(62)      V1Dmsg
108700171103     c                   leavesr
108800171103e   1c                   endif
108900040705      *
109000040705      * Controllo se esistono P.O. DPD nel piano di default
109100040705if  1c                   if        V1Cdpd  = 'S'
109200040705     c                   eval      *in90   = *on
109300040705     c                   eval      TR9fgs  = V1Cpog
109400040705     c                   clear                   TR9npga
109500040705     c                   clear                   TR9nfva
109600171103     c                   eval      TR9tps  = V1Ctps
109700171103     c     K04FLTR9      setll     FLTR9000
109800171103     c     K04FLTR9      reade(n)  FLTR9000
109900171103do  2c                   dow       NOT %eof(FLTR902L)
110000040705     c                   eval      xx      = 1
110100050615     c                   eval      *in10   = *off
110200040705     c     TR9bai        lookup    $ORGfil(xx)                            10
110300040705if  3c                   if            *in10       = *on
110400040705     c                             and $ORGntw(xx) = 'DPD'
110500040705     c                   eval      *in90   = *off
110600040705     c                   leave
110700040705e   3c                   endif
110800171103     c     K04FLTR9      reade(n)  FLTR9000
110900040705e   2c                   enddo
111000040705      * - Se non ne trovo: emetto l'apposito messaggio (xx) in window
111100040705if  2c                   if        *in90   = *on
111200040705     c                   eval      $Video  = 'W'
111300040705     c                   eval      xx      = 48
111400060110     c                   leavesr
111500040705e   2c                   endif
111600040705e   1c                   endif
111700031014      *
111800031014      * imposto numero F.V. in campo numerico editato a video
111900031014     c                   move      V1Cnfv        V1Dnfv
112000031007      *
112100060110     c                   ENDSR
112200171107      *
112300171107      /free
112400171107
112500171107       //--------------------------------------------------------------
112600171107       // ?Caricamento distinte già inserite dalla filiale 006-PD per?
112700171107       // ?l'altro "Tipo Piano Smistamento".?
112800171107       //--------------------------------------------------------------
112900171107       BEGSR  sr_CarDist_2;
113000171107
113100171107         clear  keyFLTR902;
113200171107         k_TR9fgs   = V1Cpog;            // ?(006)?
113300171107         k_TR9npga  = V1Cnpg;            // ?(2)?
113400171107         k_TR9nfva  = %int( V1Cnfv );
113500171107         if  V1Ctps = 'V';
113600171107           k_TR9tps = 'R';
113700171107         else;
113800171107           k_TR9tps = 'V';
113900171107         endif;
114000171107
114100171107         setll  %kds( keyFLTR902 ) FLTR9000;
114200171107         reade(N)  %kds( keyFLTR902 : 4 ) FLTR9000;
114300171107
114400171107         DoW  Not %eof( FLTR902L );
114500171107
114600171107           select;
114700171107             // - ?Distinta?
114800171107             when  TR9npg  = 4;
114900171107               wy += 1;
115000171107               sk_NFV2( wy ) = TR9nfv;
115100171107             // - ?$IMA?
115200171107             //*·when  TR9npg  = 3;
115300171107             // - ?$PO (solo piano VDL)?
115400171107             //*·when  TR9npg  = 1;
115500171107           endsl;
115600171107
115700171107           reade(N)  %kds( keyFLTR902 : 4 ) FLTR9000;
115800171107
115900171107         EndDo;
116000171107
116100171107       ENDSR;
116200171107
116300171107      /free
116400031010      *
116500031010      *---------------------------------------------------------------*
116600031010      * Decodifica P.O. in gestione                                   *
116700031010      *---------------------------------------------------------------*
116800031010     c     DecodPOG      BegSr
116900031010      *
117000031010if  1c                   if        V1Cpog =  *zeros
117100031010     c                   clear                   V1Dpog
117200031010     c                   clear                   OG150
117300031010     c                   seton                                        412890
117400031010     c                   movel     $Msg(1)       V1Dmsg
117500060110     c                   leavesr
117600031010e   1c                   endif
117700031010      *
117800031010     c     V1Cpog        chain     AZORG
117900031010      *
118000031010if  1c                   if        not %found(AZORG01L)
118100031010     c                             or  ORGfva = '*'
118200031010      *
118300031010     c                   movel     *all'? '      V1Dpog
118400031010     c                   clear                   OG150
118500031010     c                   seton                                        412890
118600031010     c                   movel     $Msg(2)       V1Dmsg
118700060110     c                   leavesr
118800031010      *
118900031010x   1c                   else
119000031010      *
119100031010     c                   movel     ORGdes        V1Dpog
119200031010      *
119300031010     c                   eval      *in10  =  *off
119400040701     c     V1Cpog        lookup    £1                                     10
119500031010if  2c                   if        NOT *in10
119600031010     c                   seton                                        412890
119700031010     c                   movel     $Msg(2)       V1Dmsg
119800060110     c                   leavesr
119900031010e   2c                   endif
120000031010      *
120100031010     c                   movel     ORGdf0        OG150
120200031013if  2c                   if        §OGpck <> 'S'
120300040427     c                             and    %subst(KNMUS:1:3) <> 'EDP'
120400031010     c                   seton                                        412890
120500031010     c                   movel     $Msg(3)       V1Dmsg
120600060110     c                   leavesr
120700031010e   2c                   endif
120800031010      *
120900031010      * Se tutto ok reperisco il terminal per verificarne il livello
121000031010     c                   clear                   FNLV55ds
121100031010     c                   eval      D55lin = V1Cpog
121200031010     c                   eval      D55drf = Wdate
121300031010     c                   call      'FNLV55R'
121400031010     c                   parm                    FNLV55ds
121500040705      *
121600040705     c                   clear                   TRUL06ds
121700040705     c                   movel     '£1'          D06cod
121800040705     c                   movel     V1Cpog        D06key
121900040705     c                   movel(p)  TRUL06ds      KPJBU
122000040705     c                   call      'TRUL06R'
122100040705     c                   parm                    KPJBA
122200040705     c                   movel     KPJBU         TRUL06ds
122300040705     c                   movea     D06lia        £1pog
122400031010      *
122500031010e   1c                   endif
122600031010      *
122700060110     c                   ENDSR
122800031009      *
122900031009      *---------------------------------------------------------------*
123000031009      * Inizializzazione schiere, flag, variabili, ecc                *
123100031009      *---------------------------------------------------------------*
123200031009     c     InzALL_1      BEGSR
123300031009      *
123400031009     c                   reset                   $Fine
123500031009     c                   reset                   $InzS01
123600031009     c                   reset                   $InzS02
123700031023     c                   reset                   $EoF
123800031009     c                   reset                   $EndSFL
123900031009     c                   reset                   $Default
124000040701     c*** già impostato: reset                   $NewFVa
124100040701     c*** già impostato: reset                   $DltPS
124200031009     c                   reset                   $VDL
124300040409     c                   reset                   $IMA
124400040423     c                   reset                   $IMAdft
124500040607     c                   reset                   $PO
124600040611     c                   reset                   $POdpd
124700080129     c*** non qui:       reset                   $POerr
124800040607     c                   reset                   $POdft
124900040423     c                   reset                   $Hidden
125000031023     c                   reset                   $Found
125100171102     c                   reset                   $First
125200050629     c                   reset                   $MsgBAIbau
125300031009      *
125400031009     c                   reset                   $MEMat
125500040519     c*** non qui:       reset                   $MEMbp
125600031111     c                   reset                   $MEMfv1
125700031111     c                   reset                   $MEMmp1
125800050701     c                   reset                   $MEMpm1
125900031111     c                   reset                   $MEMfv2
126000031111     c                   reset                   $MEMmp2
126100050701     c                   reset                   $MEMpm2
126200031009     c                   reset                   $MEMxf
126300040615     c                   reset                   $FVxPOa
126400040615     c                   reset                   $FVxPOn
126500040615     c                   reset                   $FVxPOd
126600031009      *
126700031009     c                   reset                   S01nrr
126800031009     c                   reset                   S02nrr
126900031009     c                   reset                   MAXnrr
127000031030     c                   reset                   SAVpep
127100031107     c                   reset                   SAVbau
127200040309      *
127300040423     c                   reset                   Wpost
127400031009      *
127500031009     c                   ENDSR
127600031009      *
127700031021      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
127800031021      *
127900031009     c     InzALL_2      BEGSR
128000031112      *
128100031111     c                   reset                   $FV
128200040315     c                   reset                   $CsrRRN
128300031009     c                   reset                   $PEP
128400031030     c                   reset                   $PEPbai
128500031009     c                   reset                   $BAI
128600031009     c                   reset                   $BAInr
128700050629     c                   reset                   $BAIbau
128800031009     c                   reset                   $BAU
128900040305     c                   reset                   $BAUbai
129000031009     c                   reset                   $BNL
129100031009     c                   reset                   $BNLnr
129200031030     c                   reset                   $NFV
129300040628     c                   reset                   $NFVbp
129400031030     c                   reset                   $NFVnr
129500040519     c                   reset                   $MEMbp
129600040409     c                   reset                   $MPTnr
129700040607     c                   reset                   $ORGnr
129800040607     c                   reset                   $PosPO
129900040628     c                   reset                   $PosPOdpd
130000031009      *
130100031009     c                   reset                   XX
130200031022     c                   reset                   SAVnfvIMA
130300031030     c                   reset                   SAVnfv
130400031022     c                   reset                   SAVpdr
130500040305     c                   reset                   SAVprg
130600031009     c                   reset                   Wpdr
130700031022     c                   reset                   Wnfv
130800031111     c                   reset                   Wnfv7
130900040423     c                   reset                   Wpost
131000031024     c                   reset                   Wast
131100031009      *
131200031009     c                   ENDSR
131300031009      *
131400031009      *---------------------------------------------------------------*
131500031009      * Caricamento dati da tab. MSL                                  *
131600031009      *---------------------------------------------------------------*
131700031009     c     Tab_MSL       BEGSR
131800031009      *
131900031009      * Carico schiera tab. MSL
132000031009     c                   clear                   XX
132100031009     c                   clear                   $MSLpep
132200031009     c                   movel     'MSL'         TBEcod
132300031009     c                   movel(p)  V1Cpog        TBEke1
132400031009     c     K02TNTBE      setll     TNTBE000
132500031009     c     K02TNTBE      reade     TNTBE000
132600031009do  1c                   dow       NOT %eof(TNTBE01L)
132700031009if  2c                   if        TBEatb = *blanks
132800031009     c                   add       1             xx
132900031009     c                   movel     TBEuni        dMSL
133000031009     c                   movel     §MSLpep       $MSLpep(xx)
133100031009e   2c                   endif
133200031009     c     K02TNTBE      reade     TNTBE000
133300031009e   1c                   enddo
133400031009      *
133500031009     c                   ENDSR
133600040408      *
133700040408      *---------------------------------------------------------------*
133800040408      * Caricamento tab. MPT                                          *
133900040408      *---------------------------------------------------------------*
134000040408     c     Tab_MPT       BEGSR
134100040408      *
134200040408      * Carico schiera tab. MPT
134300040408     c                   clear                   XX
134400040408     c                   clear                   $MPT
134500040423     c                   clear                   $MPTdes
134600040409     c                   reset                   $MPTobbl
134700040423     c*** qui non serve: clear                   $MPTnr
134800040408     c                   movel     'MPT'         TBEcod
134900040408     c     TBEcod        setll     TNTBE000
135000040408     c     TBEcod        reade     TNTBE000
135100040408do  1c                   dow       NOT %eof(TNTBE01L)
135200040408if  2c                   if        TBEatb  = *blanks
135300040408     c                   movel     TBEuni        dMPT
135400040409if  3c                   if            $VDL    =  *on
135500040409     c                             and §MPTsiv <> 'N'
135600040409     c                             OR  $VDL    =  *off
135700040409     c                             and §MPTnov <> 'N'
135800040408     c                   add       1             xx
135900040408     c                   movel     TBEke1        $MPT(xx)
136000040423     c                   movel     §MPTdes       $MPTdes(xx)
136100040409if  4c                   if            $VDL    =  *on
136200040409     c                             and §MPTsiv =  'S'
136300040409     c                             OR  $VDL    =  *off
136400040409     c                             and §MPTnov =  'S'
136500040409     c                   movel     *on           $MPTobbl(xx)
136600040409e   4c                   endif
136700040408e   3c                   endif
136800040408e   2c                   endif
136900040408     c     TBEcod        reade     TNTBE000
137000040408e   1c                   enddo
137100040408      *
137200040408      * La postazione "IMA" (generica) la considero sempre valida
137300040408      *   ma dò per scontato che sia inserita in tab. "MPT" !!!
137400040408if  1c***                if        xx      = *zeros
137500040408     c***                eval      $MPT(1) = 'IMA'
137600040408e   1c***                endif
137700040408      *
137800040408     c                   ENDSR
137900031009      *
138000031009      *---------------------------------------------------------------*
138100031009      * Controllo se P.O. in gestione con VDL                         *
138200031009      *---------------------------------------------------------------*
138300031009     c     Ctrl_VDL      BEGSR
138400031009      *
138500031009     c                   reset                   $VDL
138600031009     c                   clear                   dMFP
138700031009      *
138800031009     c                   reset                   TIBS02ds
138900031009     c                   movel     KNSIF         T02sif
139000031009     c                   movel     V1Cpog        T02ke1
139100031009     c                   call      'TIBS02R'
139200031009     c                   parm                    KPJBA
139300031009     c                   parm                    TIBS02ds
139400080121if  1c                   if        T02err  = *blanks
139500031009     c                   movel     T02uni        dMFP
139600031009e   1c                   endif
139700031009      *
139800040630      * Test se presente VdL o Cml per piano
139900171102      *//* - NON per i P.O. "elencati" in C_PO_...
140000161110      * => CML non è più usato!
140100171102      * => ?ORA anche 006-PD può gestire il Piano Smistamento VDL?
140200171102      *    ?(anche SE in maniera "particolare")?
140300171102if  1c*//                if        V1Cpog <> C_PO_a
140400060214      *
140500171102     c*//  'V'           lookup    §MFPap                                 10
140600161110     ***c  n10'C'           lookup    §MFPap                                 10
140700171102     c*//                movel     *in10         $VDL
140800060214      *
140900171102e   1c*//                endif
141000171102      *
141100171103sel 1c                   select
141200171103w   1c                   when      V1Ctps <> *blanks
141300171103w   1c                   when      %lookup( 'V' : §MFPap ) > *zero
141400171103     c                   eval      V1Ctps = 'V'
141500171103x   1c                   other
141600171103     c                   eval      V1Ctps = 'R'
141700171103e   1c                   endsl
141800171102     c                   eval      $VDL = ( V1Ctps = 'V' )
141900040408      *
142000040408      * Caricamento postazioni "IMA" da tab. MPT
142100040408     c                   exsr      Tab_MPT
142200031009      *
142300031009     c                   ENDSR
142400031009      *
142500031009      *---------------------------------------------------------------*
142600031020      * Intabellamento dati dal default                               *
142700031009      *---------------------------------------------------------------*
142800031020     c     Memo_DFT      BEGSR
142900031009      *
143000031009     c                   clear                   TR9npga
143100031009     c                   clear                   TR9nfva
143200171103     c     K04FLTR9      setll     FLTR9000
143300171103     c     K04FLTR9      reade(n)  FLTR9000
143400031009      *
143500171103do  1c                   DOW       NOT %eof(FLTR902L)
143600031020      *
143700031023      * Intabellamento autotrasportatori di default:
143800040701      * serve a "dare priorità" ad essi quando (dopo, qui sotto), si
143900040701      * memorizzeranno i F.V. più recenti di tutti gli autotrasportato-
144000040701      * ri (memorizzati e non): potrebbe andare "buca" all'ultimo auto-
144100040701      * trasportatore del default - se sono già stati memorizzati tanti
144200040701      * autotrasportatori quanti sono gli elementi della schiera $MEMat...
144300040315if  2c                   IF        TR9bnl <> 'S'
144400031023      *
144500031009      * - controllo se cod. autotrasportatore già memorizzato
144600040315if  3c                   if        TR9pdc <> *zeros
144700040615     c                   eval      xx      = 1
144800040615     c                   eval      *in10   = *off
144900031009     c     TR9pdc        lookup    $MEMat(xx)                             10
145000040615if  4c                   if        *in10   = *off
145100031009      * - - memorizzo nuovo cod. autotrasportatore
145200040615     c                   eval      xx      = 1
145300031009     c     *zeros        lookup    $MEMat(xx)                             10
145400040615if  5c                   if        *in10   = *on
145500040615     c                   eval      $MEMat(xx)  = TR9pdc
145600040315x   5c                   else
145700031009      * - - - se già memorizzati il n° massimo di autotrasportatori
145800031009      *       esco dal ciclo di intabellamento
145900031009     c                   leave
146000040315e   5c                   endif
146100040315e   4c                   endif
146200040315e   3c                   endif
146300040309      *
146400040315e   2c                   ENDIF
146500031020      *
146600171103     c     K04FLTR9      reade(n)  FLTR9000
146700040315e   1c                   ENDDO
146800040315      *
146900031023      *
147000031009      * Intabellamento F.V. più recente per ogni autotrasportatore
147100031110      *   prima intabellato (dal default) e NON (basta che abbia un
147200031110      *   f.v. valido per la data del f.v.arrivi impostato)
147300031023      *
147400040615     c                   eval      FV8fgs  = V1Cpog
147500040615     c                   eval      FV8npg  = 4
147600040615     c                   eval      FV8dfv  = FVVdfv
147700031009     c     K03FVV08      setll     FNFVV08
147800031010     c     K02FVV08      reade     FNFVV08
147900031009      *
148000031009do  1c                   DOW       NOT %eof(FNFVV08L)
148100040615if  2c                   if            FV8atb  = *blanks
148200040615     c                             and FV8fcf  = *blanks
148300120613     c                   eval      DSTnpg  = FV8npg
148400120613     c                   eval      DSTnfv  = FV8nfv
148500120613     c                   eval      DSTfgs  = V1Cpog
148600120613     c     K03FIDST      chain     FIDST000
148700120613if  3c                   if        %found(FIDST01L)
148800031009     c                   clear                   DS_FV
148900040615     c                   eval      dsFVdfv     = FV8dfv
149000040615     c                   eval      dsFVnfv     = FV8nfv
149100031022     c                   movel     DS_FV         w0150
149200171107      * - controllo per la fil. 006 se distinta già inserita
149300171107      *   nell'altro piano smistamento (VDL se questo è x Rulliera,
149400171107      *   Rulliera se questo è x VDL)
149500171107if 3Bc                   If        V1Cpog <> c_PO_a   or
149600171107     c                             %lookup( dsFVnfv : sk_NFV2 )
149700171107     c                                     = *zero
149800031009      * - controllo se cod. autotrasportatore già memorizzato
149900040615     c                   eval      xx      = 1
150000040615     c                   eval      *in10   = *off
150100031009     c     DSTpdr        lookup    $MEMat(xx)                             10
150200040615if  4c                   if        *in10   = *off
150300031110     c     *zeros        lookup    $MEMat(xx)                             10
150400040615if  5c                   if        *in10   = *on
150500040615     c                   eval      $MEMat(xx)  = DSTpdr
150600031110e   5c                   endif
150700031110e   4c                   endif
150800031111      * - - memorizzo data e numero distinte più recenti
150900040615if  4c                   if        *in10   = *on
151000050701     c                   movel     FV8spn        dFVVspn
151100031111     c                   add       1             $MEMxf(xx)
151200031111sel 5c                   select
151300031111      * - - - 1° f.v.
151400040615w   5c                   when      $MEMfv1(xx) < w0150
151500040615if  6c                   if        $MEMfv1(xx) > $MEMfv2(xx)
151600040615     c                   eval      $MEMfv2(xx) = $MEMfv1(xx)
151700040615     c                   eval      $MEMmp2(xx) = $MEMmp1(xx)
151800050701     c                   eval      $MEMpm2(xx) = $MEMpm1(xx)
151900031111e   6c                   endif
152000040615     c                   eval      $MEMfv1(xx) = w0150
152100040615     c                   eval      $MEMmp1(xx) = FV8fpp
152200050701if  2c                   eval      $MEMpm1(xx) = §FVVtpm
152300031111      * - - - 2° f.v.
152400040615w   5c                   when      $MEMfv2(xx) < w0150
152500040615     c                   eval      $MEMfv2(xx) = w0150
152600040615     c                   eval      $MEMmp2(xx) = FV8fpp
152700050701if  2c                   eval      $MEMpm2(xx) = §FVVtpm
152800031111e   5c                   endsl
152900031009e   4c                   endif
153000171107e  3Bc                   EndIf
153100031009e   3c                   endif
153200031009e   2c                   endif
153300031010     c     K02FVV08      reade     FNFVV08
153400031009e   1c                   ENDDO
153500040615      *
153600040615      *
153700040615      * Intabellamento F.V. più recente per ogni P.O. DPD
153800040615      *   prima intabellato (dal default) e NON (basta che abbia un
153900040615      *   f.v. valido per la data del f.v.arrivi impostato)
154000040615      *
154100040615     c                   clear                   xx
154200040615     c                   eval      FGVlnp  = V1Cpog
154300040615     c                   eval      FGVdfv  = SAVdfva
154400040615     c                   eval      FGVnfv  = *loval
154500040621     c     K03FGV02      setll     FNFGV000
154600040615     c     FGVlnp        reade     FNFGV000
154700040615      *
154800040615do  1c                   DOW       NOT %eof(FNFGV02L)
154900040615      *
155000040615if  2c                   IF            FGVatb  = *blanks
155100040615     c                             and FGVfcf  = *blanks
155200040705      *
155300040705     c                   clear                   Wnfv7
155400040705      * - 1° controllo con P.O. arrivo
155500040615     c                   eval      xy      = 1
155600040615     c                   eval      *in10   = *off
155700040615     c                   move      FGVlna        Wpost
155800040615     c     Wpost         lookup    $ORGfil(xy)                            10
155900040705      * - - imposto numero distinta
156000070530if  3c                   IF            *in10       = *on
156100040705     c                             and $ORGntw(xy) = 'DPD'
156200040705     c                   eval      Wnfv7   = FGVnfv
156300070530     c                   exsr      Memo_NFV
156400070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
156500070530      *     memorizzabile
156600080121if  4c                   if        xx      = %elem($FVxPOa)
156700070530     c                   leavesr
156800070530e   4c                   endif
156900070530e   3c                   ENDIF
157000040705      *
157100040705      * - 2° controllo con altri P.O. (1) del F.V.
157200040630     c                   clear                   xFGVffv
157300070530if  3c                   IF        FGVttr <> 'L'
157400040630     c                   movea     FGVffv        xFGVffv
157500040630     c                   clear                   wx
157600070530do  4c                   DoW       wx      < %elem(xFGVffv)
157700040630     c                   add       1             wx
157800040630if  5c                   if        xFGVffv(wx)     =  *blanks
157900040630     c                   leave
158000040630e   5c                   endif
158100040630     c                   eval      xy      = 1
158200040630     c                   eval      *in10   = *off
158300040630     c     xFGVffv(wx)   lookup    $ORGfil(xy)                            10
158400040705      * - - imposto numero distinta - negativo
158500040705      *     per memorizzare che il p.o. DPD è tra "altri p.o." del f.v.
158600040705      *     (e non è il p.o. finale di arrivo)
158700040630if  5c                   if            *in10       =  *on
158800040630     c                             and $ORGntw(xy) =  'DPD'
158900040630     c                   move      xFGVffv(wx)   Wpost
159000040705     c                   eval      Wnfv7   = FGVnfv * (-1)
159100070521     c                   exsr      Memo_NFV
159200070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
159300070530      *     memorizzabile
159400080121if  6c                   if        xx      = %elem($FVxPOa)
159500070530     c                   leavesr
159600070530e   6c                   endif
159700040630e   5c                   endif
159800070530e   4c                   EndDo
159900070530e   3c                   ENDIF
160000040705      *
160100040705      * - 3° controllo con altri P.O. (2) del F.V.
160200070530if  3c                   IF        FGVttr <> 'L'
160300040630     c                   clear                   xFGVffv
160400040630     c                   movea     FGVff2        xFGVffv
160500040630     c                   clear                   wx
160600070530do  4c                   DoW       wx      < %elem(xFGVffv)
160700040630     c                   add       1             wx
160800040630if  5c                   if        xFGVffv(wx)     =  *blanks
160900040630     c                   leave
161000040630e   5c                   endif
161100040630     c                   eval      xy      = 1
161200040630     c                   eval      *in10   = *off
161300040630     c     xFGVffv(wx)   lookup    $ORGfil(xy)                            10
161400040705      * - - imposto numero distinta - negativo
161500040705      *     per memorizzare che il p.o. DPD è tra "altri p.o." del f.v.
161600040705      *     (e non è il p.o. finale di arrivo)
161700040630if  5c                   if            *in10       =  *on
161800040630     c                             and $ORGntw(xy) =  'DPD'
161900040630     c                   move      xFGVffv(wx)   Wpost
162000040705     c                   eval      Wnfv7   = FGVnfv * (-1)
162100070521     c                   exsr      Memo_NFV
162200070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
162300070530      *     memorizzabile
162400080121if  6c                   if        xx      = %elem($FVxPOa)
162500070530     c                   leavesr
162600070530e   6c                   endif
162700040630e   5c                   endif
162800070530e   4c                   EndDo
162900070530e   3c                   ENDIF
163000040705      *
163100040705e   2c                   ENDIF
163200040615      *
163300040615     c     FGVlnp        reade     FNFGV000
163400040615      *
163500040615e   1c                   ENDDO
163600031009      *
163700031009     c                   ENDSR
163800070521      *
163900070521      *---------------------------------------------------------------*
164000070521      * Intabellamento F.V. disponibili x filiale                     *
164100070521      *---------------------------------------------------------------*
164200070521     c     Memo_NFV      BEGSR
164300070521      *
164400070521      * - Memorizzo data e numero distinte più recenti
164500070521     c                   eval      xx          = xx + 1
164600070521     c                   eval      $FVxPOa(xx) = Wpost
164700070521     c                   eval      $FVxPOn(xx) = Wnfv7
164800070521     c                   eval      $FVxPOd(xx) = FGVdfv
164900070521      *
165000070521     c                   ENDSR
165100031007      *
165200031007      *---------------------------------------------------------------*
165300031007      * Gestione videata W01                                          *
165400031007      *---------------------------------------------------------------*
165500031007     c     GesW01        BEGSR
165600031007      *
165700031007      * Preparazione videata con messaggio
165800031007     c                   clear                   MS03W01
165900040705     c                   movel     $Msg(xx)      W1Dms2
166000031009     c                   eval      W1Dtfa  = 'F12=Ritorno'
166100031007      *
166200031007      * Emissione messaggio
166300031009do  2c                   dou       *inKL   = *on
166400031007     c                   exfmt     MS03W01
166500031007e   2c                   enddo
166600031007      *
166700031007      * Ritorno alla videata iniziale
166800031007     c                   reset                   $Video
166900040705     c                   eval      *in90   = *off
167000031007      *
167100031007     c                   ENDSR
167200031007      *
167300031007      *---------------------------------------------------------------*
167400031106      * Gestione videata C01/S01 & C03/S03 o C02/S02 & C04/S04        *
167500031007      *---------------------------------------------------------------*
167600031106     c     GesSFL        BEGSR
167700031106      *
167800031106      * Inizializzo la videata
167900031106if  1c                   if            $VDL    = *off
168000031106     c                             and $InzS01 = *on
168100031106     c                             OR  $VDL    = *on
168200031106     c                             and $InzS02 = *on
168300031106     c                   exsr      InzSFL
168400031106     c                   eval      $InzS01     = *off
168500031106     c                   eval      $InzS02     = *off
168600031106e   1c                   endif
168700031106      *
168800031106      * Se non sono stati trovati record NEANCHE con il piano di
168900031106      *   default emetto un messaggio e torno alla 1ª videata
169000031106if  1c                   if        MAXnrr  = *zeros
169100031106     c                   eval      $Video  = 'W'
169200040705     c                   eval      xx      = 9
169300060110     c                   leavesr
169400031106x   1c                   else
169500031106     c                   eval      *in30   = *off
169600031106e   1c                   endif
169700031106      *
169800031106      * Posiziono il cursore
169900031106sel 1c                   select
170000031106w   1c                   when          $VDL   = *off
170100031106     c                             and C01csr > *zeros
170200031106     c                   z-add     C01csr        C01rcd
170300031106w   1c                   when          $VDL   = *on
170400031106     c                             and C02csr > *zeros
170500031106     c                   z-add     C02csr        C02rcd
170600031106w   1c                   when          $VDL   = *off
170700031106     c                             and C01csr = *zeros
170800031106     c                   z-add     1             C01rcd
170900031106w   1c                   when          $VDL   = *on
171000031106     c                             and C02csr = *zeros
171100031106     c                   z-add     1             C02rcd
171200031106e   1c                   endsl
171300031106      *
171400031106      * Scrivo la testata e la riga tasti funzionali abilitati
171500031106if  1c                   if        NOT *in90
171600031106     c                   write     MS03T01
171700031106if  2c                   if        $Default = *on
171800031106     c                   write     MS03Z02
171900031106x   2c                   else
172000031106     c                   write     MS03Z03
172100031106e   2c                   endif
172200031106e   1c                   endif
172300151109      *
172400151109      *?Altro Piano Smistamento già esistente per lo stesso giorno?
172500151109     c                   if        ds_NFVxDFV.TR9nfva > *zero
172600151109     c                             and        $NewFVA = *off
172700151109     c                             and        V1Cnpg <> *zero
172800151109     c                   eval      wDate_Eur = %date( SavDFVa : *ISO )
172900151109     c                   eval      *in28  = *on
173000151109     c                   eval      *in90  = *on
173100151109     c                   eval      V1Dmsg = $Msg(55)
173200151109     c                   eval      V1Dmsg = %replace( %trim( %editc(
173300151109     c                                        ds_NFVxDFV.TR9nfva : '2' ) )
173400151109     c                                      : V1Dmsg
173500151109     c                                      : %scan( '_      ' : V1Dmsg ) )
173600151109     c                   eval      V1Dmsg = %replace(
173700151109     c                                      %editc( %dec( wDate_Eur ) : 'Y' )
173800151109     c                                      : V1Dmsg
173900151109     c                                      : %scan( '__/__/____' : V1Dmsg ) )
174000151109     c                   endif
174100031106      *
174200031106      * Emetto la videata
174300060110     c     §OGpkm        comp      'S'                                    04
174400031106sel 1c                   select
174500031106w   1c                   when      $Video = '2'
174600031106     c                   exfmt     MS03C01
174700031106w   1c                   when      $Video = '3'
174800031106     c                   exfmt     MS03C02
174900031106w   1c                   when      $Video = '4'
175000031106     c                   exfmt     MS03C03
175100031106w   1c                   when      $Video = '5'
175200031106     c                   exfmt     MS03C04
175300031106e   1c                   endsl
175400031106      *
175500031106     c                   setoff                                       28  90
175600031106     c                   clear                   V1Dmsg
175700031106      *
175800031106sel 1c                   select
175900031106      * F3=Fine
176000031106w   1c                   when      *inKC
176100031106     c                   exsr      F03D01
176200060110     c                   leavesr
176300031106      * F12=Ritorno
176400031106w   1c                   when      *inKL
176500031106     c                   exsr      F12SFL
176600060110     c                   leavesr
176700040315      * F16=Cancellazione piano
176800040315w   1c                   when      *inKQ
176900040315     c                   exsr      F16SFL
177000060110     c                   leavesr
177100031106      *
177200031106e   1c                   endsl
177300031106      *
177400151109      *?Altro Piano Smistamento già esistente per lo stesso giorno?
177500151109     c                   if        ds_NFVxDFV.TR9nfva > *zero
177600151109     c                             and        $NewFVA = *off
177700151109     c                   leavesr
177800151109     c                   endif
177900151109      *
178000031106     c                   exsr      InzALL_2
178100031106      *
178200031106      * Controllo dati immessi a video
178300031106if  1c                   if        $VDL = *off
178400031106     c                   exsr      CtrS01
178500031106x   1c                   else
178600031106     c                   exsr      CtrS02
178700031106e   1c                   endif
178800060110     c                   if        *in90
178900060110     c                   leavesr
179000060110     c                   endif
179100031111      *
179200031111      * F7=Interrogazione F.V. non inseriti
179300031111if  1c                   if        *inKG
179400031111     c                   clear                   Wcampo
179500031111     c                   exsr      srSEARCH
179600060110     c                   leavesr
179700031111e   1c                   endif
179800031112      *
179900031112      * Alla conferma (F6 o F8): segnalo se esistono distinte aperte
180000031112      * ------------------------ NON inserite nel piano giornaliero
180100080129      *   e/o rimembra delle filiali DPD lasciate SENZA F.V.
180200040311     c                   reset                   WinKH
180300031112if  1c                   if        (*inKF  or  *inKH)
180400031112     c                             and $Default = *off
180500080129     c                   exsr      CtrlFVdpd
180600080129if  2c                   if        *inKL
180700080129     c                   leavesr
180800080129e   2c                   endif
180900031112     c                   exsr      CtrlDistAp
181000031112e   1c                   endif
181100031106      *
181200031106      * F6=Aggiornamento
181300031106      * F8=Aggiornamento & Stampa
181400031112if  1c                   if        *inKF   or  *inKH
181500031112     c                                     or  WinKH = *on
181600031106     c                   exsr      Upd_FLTR9
181700031112if  2c                   if        *inKH   or  WinKH = *on
181800031106     c                   exsr      F08SFL
181900031106e   2c                   endif
182000031106     c                   reset                   $Video
182100031106     c                   reset                   $InzD01
182200031106e   1c                   endif
182300031106      *
182400060110     c                   ENDSR
182500031007      *
182600031007      *---------------------------------------------------------------*
182700031106      * Inizializzazione videata S01 & S03 o S02 & S04                *
182800031007      *---------------------------------------------------------------*
182900031106     c     InzSFL        BEGSR
183000031007      *
183100031007      * Pulizia subfile
183200031007     c                   seton                                        3031
183300031106sel 1c                   select
183400031106w   1c                   when      $Video = '2'
183500031007     c                   write     MS03C01
183600031106w   1c                   when      $Video = '3'
183700031106     c                   write     MS03C02
183800031106w   1c                   when      $Video = '4'
183900031007     c                   write     MS03C03
184000031106w   1c                   when      $Video = '5'
184100031106     c                   write     MS03C04
184200031106e   1c                   endsl
184300031009     c                   setoff                                         31
184400031007      *
184500031009     c                   clear                   C01rcd
184600031009     c                   clear                   C01csr
184700031009     c                   clear                   S01nrr
184800031106     c                   clear                   C02rcd
184900031106     c                   clear                   C02csr
185000031106     c                   clear                   S02nrr
185100031007     c                   clear                   V1Dmsg
185200031007     c                   setoff                                       28  90
185300031007     c                   movea     *zeros        *in(41)
185400080129      *
185500080129     c                   reset                   $ORGerr
185600031007      *
185700031009      * Caricamento dei dati da presentare nel sfl (max 90 rec)
185800031023      * se è piano di DEFAULT leggo con key *zeros
185900031023      * se è NUOVO piano GIORNALIERO propongo quelli di default
186000040315      * se è VECCHIO piano GIORNALIERO rifaccio la lista per baie /
186100040315      *   postazioni dal piano di default e vi abbino i dati già memo-
186200040315      *   rizzati (questo per mantenere l'elenco delle baie/postazioni
186300040315      *   comprensivo di quelle eventualmente non registrate, visua-
186400040315      *   lizzate con "XXX").
186500031106     c                   eval      TR9fgs  = V1Cpog
186600031106     c                   clear                   TR9npga
186700031106     c                   clear                   TR9nfva
186800040623     c                   eval      XR9npga = V1Cnpg
186900040623     c                   move      V1Cnfv        XR9nfva
187000171103     c                   eval      TR9tps  = V1Ctps
187100031106      *
187200031007     c                   reset                   $EndSFL
187300031007do  1c                   DOW       $EndSFL = *off
187400031007     c                   exsr      Carica_SFL
187500031007e   1c                   ENDDO
187600031007      *
187700031106if  1c                   if        $VDL    = *off
187800031007     c                   eval      MAXnrr  = S01nrr
187900031106x   1c                   else
188000031106     c                   eval      MAXnrr  = S02nrr
188100031106e   1c                   endif
188200031009     c                   eval      *in33   = ($EndSFL = *on)
188300031007      *
188400031007     c                   ENDSR
188500151104      *
188600151106      /free
188700151106
188800151106       //--------------------------------------------------------------
188900151106       //?Preparaz. stringa SQL per estrazione altri piani smistamento ?
189000151106       //?  picking per lo stesso giorno.                              ?
189100151106       //?Preparazione cursore.                                        ?
189200151106       //?Dichiarazione cursore.                                       ?
189300151106       //?Apertura cursore.                                            ?
189400151106       //--------------------------------------------------------------
189500151106       BEGSR  sr_OpenCursor;
189600151109
189700151106         // -?Impostazione opzioni per SQL?
189800151106         exec sql   set  option  DynUsrPrf = *Owner,
189900151106                                 CloSqlCsr = *EndMod;
190000151106
190100151106         // -?Preparazione stringa SQL per estrarre i dati?
190200151106         wSQL = 'with NFV_x_DFV as +
190300151109                              ( select TR9fgs, TR9npga, TR9nfva, TR9fl2, +
190400151109                                       Fvvdfv +
190500151106                                  from FLTR900F inner join FNFVV00F +
190600151106                                    on TR9fgs  = FVVfgs +
190700151106                                   and TR9npga = FVVnpg +
190800151106                                   and TR9nfva = FVVnfv +
190900151106                                 where FVVfcf  = '' '' +
191000151109                              group by TR9fgs, TR9npga, TR9nfva, TR9fl2, +
191100151109                                       FVVdfv +
191200151109                              order by TR9fgs, TR9npga, TR9nfva, TR9fl2, +
191300151109                                       FVVdfv ), +
191400151106
191500151106                      CountNFV  as +
191600151109                              ( select TR9fgs, TR9npga, TR9fl2, FVVdfv, +
191700151106                                       count(*) as wNFVxDFV +
191800151106                                  from NFV_x_DFV +
191900151109                              group by TR9fgs, TR9npga, TR9fl2, FVVdfv ) +
192000151106
192100151106                 select NFV_x_DFV.*, CountNFV.wNFVxDFV +
192200151106
192300151106                   from NFV_x_DFV inner join CountNFV +
192400151106                     on NFV_x_DFV.TR9fgs  = CountNFV.TR9fgs +
192500151106                    and NFV_x_DFV.TR9npgA = CountNFV.TR9npgA +
192600151109                    and NFV_x_DFV.TR9fl2  = CountNFV.TR9fl2 +
192700151106                    and NFV_x_DFV.FVVdfv  = CountNFV.FVVdfv +
192800151106
192900151109                 where NFV_x_DFV.TR9fgs  = ' +
193000151109                       %trim( %editc( V1Cpog : '3' ) ) +
193100151109                 ' and NFV_x_DFV.TR9nfvA <> ' +
193200151109                       %trim( %editc( TR9nfva : '3' ) ) +
193300151109                 ' and NFV_x_DFV.FVVdfv  = ' +
193400151106                       %trim( %editc( SavDFVa : '3' ) );
193500151106
193600151109         if  V1Cdpd = 'S';
193700151109           wSQL +=
193800151109                 ' and NFV_x_DFV.TR9fl2  = ''D''';
193900151109         else;
194000151109           wSQL +=
194100151109                 ' and NFV_x_DFV.TR9fl2  = '' ''';
194200151109         endif;
194300151109
194400151106         // -?Dichiarazione cursore?
194500151106         exec sql   prepare S1   from :wSQL;
194600151106         exec sql   declare C1   cursor for S1;
194700151106
194800151106         if  SQLcode < *zero;
194900151106           *in34 = *on;
195000151109           exsr  sr_CloseCursor;
195100151106           exsr  sr_PrintERR;
195200151106           leavesr;
195300151106         endif;
195400151106
195500151106         // -?Apertura del cursore?
195600151106         exec sql   open C1;
195700151106
195800151106         if  SQLcode < *zero;
195900151106           *in34 = *on;
196000151106           exsr  sr_CloseCursor;
196100151106           exsr  sr_PrintERR;
196200151106           leavesr;
196300151106         endif;
196400151106
196500151106       ENDSR;
196600151106
196700151106       //--------------------------------------------------------------
196800151106       //?Lettura cursore.                                             ?
196900151106       //--------------------------------------------------------------
197000151106       BEGSR  sr_ReadCursor;
197100151106
197200151106         clear  ds_NFVxDFV;
197300151106
197400151106         exec sql   fetch next   from C1   into :ds_NFVxDFV;
197500151106
197600151106         select;
197700151106           when  SQLcode = 100;
197800151106             *in34 = *on;
197900151106             exsr  sr_CloseCursor;
198000151106           when  SQLcode < *zero;
198100151106             *in34 = *on;
198200151106             exsr  sr_CloseCursor;
198300151106             exsr  sr_PrintERR;
198400151106         endsl;
198500151106
198600151106       ENDSR;
198700151106
198800151106       //--------------------------------------------------------------
198900151106       //?Chiusura cursore.                                            ?
199000151106       //--------------------------------------------------------------
199100151106       BEGSR  sr_CloseCursor;
199200151106
199300151106         // -?Chiusura del cursore?
199400151109         exec sql   close C1;
199500151106
199600151106       ENDSR;
199700151106
199800151106       //--------------------------------------------------------------
199900151106       //?Stampa segnalazione dell'errore rilevato via SQL             ?
200000151106       //--------------------------------------------------------------
200100151106       BEGSR  sr_PrintERR;
200200151106
200300151106         // -?Stampa del Dump?
200400151106         Dump(A);
200500151106
200600151106         // -?Stampa del Job-Log?
200700151106         Qcmd = 'DSPJOBLOG job(*) output(*print)';
200800151106         exsr  sr_ExecCmd;
200900151106
201000151106       ENDSR;
201100151106
201200151106       //--------------------------------------------------------------
201300151106       //?Esecuzione del comando (già impostato).                      ?
201400151106       //--------------------------------------------------------------
201500151106       BEGSR  sr_ExecCmd;
201600151106
201700151106         clear Qcap0100;
201800151106         Qcabcsdh = *off;
201900151106         Qcapa    = *off;
202000151106         Qcacmdss = *off;
202100151106         Qcaerved = *allX'00';
202200151106
202300151106         clear Qusec;
202400151106         Qusbprv  = %size(Qusec);
202500151106
202600151106         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
202700151106                           %size(Qcap0100) : 'CPOP0100' : *omit :
202800151106                           0 : 0 : Qusec);
202900151106
203000151106         //if  Qusei <> *blank;
203100151106         //  ...;
203200151106         //endif;
203300151106
203400151106       ENDSR;
203500151106
203600151106      /end-free
203700031029      *
203800031029      *---------------------------------------------------------------*
203900031029      * Gestione tasto funzionale F8  da videata S01/S02/S03/S04      *
204000031029      *---------------------------------------------------------------*
204100031106     c     F08SFL        BEGSR
204200031029      *
204300031029      * Richiamo pgm. di stampa per piano di smistamento in esame
204400031029     c                   clear                   FIMS04ds
204500031029     c                   z-add     V1Cpog        D04pog
204600040408if  1c                   if        V1Cnfv > *zeros
204700031029     c                   z-add     V1Cnpg        D04npga
204800040408e   1c                   endif
204900031029     c                   move      V1Cnfv        D04nfva
205000171102     c                   movel     V1Ctps        D04tps
205100031029      *
205200031029     c                   movel(p)  FIMS04ds      KPJBU
205300031029     c                   call      'FIMS04R'
205400031029     c                   parm                    KPJBA
205500031029      *
205600031029     c                   ENDSR
205700031007      *
205800031007      *---------------------------------------------------------------*
205900031007      * Gestione tasto funzionale F12 da videata S01/S02/S03/S04      *
206000031007      *---------------------------------------------------------------*
206100031106     c     F12SFL        BEGSR
206200031007      *
206300031007      * Torno alla 1ª videata D01
206400031007     c                   reset                   $Video
206500031010     c                   movea     '00'          *in(01)
206600031007      *
206700031007     c                   ENDSR
206800040315      *
206900040315      *---------------------------------------------------------------*
207000040315      * Gestione tasto funzionale F16 da videata S03/S04              *
207100040315      *---------------------------------------------------------------*
207200040315     c     F16SFL        BEGSR
207300040315      *
207400040315      * Cancella record esistenti (se?NON?immissione)
207500040702if  1c                   if            $NewFVa = *off
207600040702     c                             and $DltPS  = *off
207700040315      *
207800040315      * - Richiesta conferma
207900040315     c                   clear                   MS03W01
208000040315     c                   movel     $Msg(19)      W1Dms2
208100040315     c                   eval      W1Dtfa = 'F6=Conferma   '
208200040315     c                                    + 'F12=Ritorno'
208300040315do  2c                   dou       *inKF   or   *inKL
208400040315     c                   exfmt     MS03W01
208500040315e   2c                   enddo
208600060110      * F12-Ritorno
208700060110     c                   if        *inKL
208800060110     c                   leavesr
208900060110     c                   endif
209000060110      * F6-Conferma
209100040315     c                   exsr      Dlt_FLTR9
209200040315      *
209300040315e   1c                   endif
209400040315      *
209500040315      * Torno alla 1ª videata D01
209600040315     c                   reset                   $Video
209700040315     c                   movea     '00'          *in(01)
209800040315      *
209900060110     c                   ENDSR
210000040701      *
210100040701      *---------------------------------------------------------------*
210200040701      * Controllo esistenza piano smistamento STD se richiesto DPD    *
210300040701      *---------------------------------------------------------------*
210400040701     c     Check_PS      BEGSR
210500040701      *
210600040701     c*** (già fatto)    reset                   $DltPS
210700040705     c                   eval      TR9fgs  = V1Cpog
210800040705     c                   eval      TR9npga = V1Cnpg
210900040705     c                   move      V1Cnfv        TR9nfva
211000171103     c                   eval      TR9tps  = V1Ctps
211100040701      *
211200171103     c     K04FLTR9      setll     FLTR9000
211300171103     c     K04FLTR9      reade(n)  FLTR9000
211400040701      *
211500040701sel 1c                   select
211600040701      **** - nuovo piano (IMPOSSIBILE qui)
211700171103w   1c***                when      %eof(FLTR902L)
211800040701     c***                eval      $NewFVa = *on
211900060110     c***                leavesr
212000040701      * - richiesto piano std, esiste piano std: modificabile
212100040701w   1c                   when          V1Cdpd =  *blanks
212200040701     c                             and TR9fl2 =  *blanks
212300060110     c                   leavesr
212400040701      * - richiesto piano std, esiste piano dpd: da sostituire
212500040701w   1c                   when          V1Cdpd =  *blanks
212600040701     c                             and TR9fl2 =  'D'
212700040701     c                   eval      $DltPS  = *on
212800060110     c                   leavesr
212900040701      * - richiesto piano dpd, esiste piano dpd: modificabile
213000040701w   1c                   when          V1Cdpd =  'S'
213100040701     c                             and TR9fl2 =  'D'
213200060110     c                   leavesr
213300040701      **** - richiesto piano dpd, esiste piano std: da sostituire
213400040701w   1c***                when          V1Cdpd =  'S'
213500040701     c***                          and TR9fl2 =  *blanks
213600040701     c*** (sotto)        ...
213700040701e   1c                   endsl
213800040701      *
213900040701      * Richiesto piano dpd, esiste piano std: da sostituire
214000040701      * Richiesta conferma per annullamento piano smistamento picking
214100040701     c                   clear                   MS03W01
214200040701     c                   movel     $Msg(45)      W1Dms1
214300040701     c                   movel     $Msg(46)      W1Dms2
214400040701     c                   movel     $Msg(47)      W1Dms3
214500040701     c                   eval      W1Dtfa  = 'F6=Conferma'
214600040701     c                                     + ' (nuovo piano DPD)   '
214700040701     c                                     + 'F12=Ritorno'
214800040701do  1c                   dou       *inKF   or   *inKL
214900040701     c                   exfmt     MS03W01
215000040701e   1c                   enddo
215100040701      *
215200040701sel 1c                   select
215300040701      * - F6 = cancella
215400040701w   1c                   when      *inKF   = *on
215500040701     c*** non adesso:    exsr      Dlt_FLTR9
215600040701     c                   eval      $DltPS  = *on
215700040701      * - F12 = ritorno
215800040701w   1c                   when      *inKL   = *on
215900040701     c                   reset                   $Video
216000040701     c                   movea     '00'          *in(01)
216100040701e   1c                   endsl
216200040701      *
216300060110     c                   ENDSR
216400031106      *
216500031106      *---------------------------------------------------------------*
216600031106      * Caricamento subfile da file FLTR900F                          *
216700031106      *---------------------------------------------------------------*
216800040705     c     Carica_SFL    BEGSR
216900031106      *
217000031106      * Solo la prima volta:
217100031106      *   posizionamento iniziale + lettura 1° record
217200031106if  1c                   if            S01nrr =  *zeros
217300031106     c                             and S02nrr =  *zeros
217400171102     c                             and $First =  *on
217500171102     c                   eval      $First = *off
217600040705     c                   clear                   TR9npga
217700040705     c                   clear                   TR9nfva
217800171103     c     K04FLTR9      setll     FLTR9000
217900171103     c     K04FLTR9      reade(n)  FLTR9000
218000171103if  2c                   if        %eof(FLTR902L)
218100031106     c                   eval      $EOF   = *on
218200031106e   2c                   endif
218300031106e   1c                   endif
218400031106      *
218500031106      * Pulizia iniziale del record nel sfl in esame
218600031106sel 1c                   select
218700031106w   1c                   when      $Video = '2'
218800031106     c                   clear                   MS03S01
218900031106w   1c                   when      $Video = '3'
219000031106     c                   clear                   MS03S02
219100031106e   1c                   when      $Video = '4'
219200031106     c                   clear                   MS03S03
219300031106w   1c                   when      $Video = '5'
219400031106     c                   clear                   MS03S04
219500031106e   1c                   endsl
219600031106      *
219700031106      * Se NON già rilevato %EOF:
219800031106      *   impostazione campi sfl con quelli dell'ultimo rec. letto
219900031106      *   e lettura singolo record successivo
220000031106if  1c                   IF        $EOF   = *off
220100031106      *
220200040309     c                   reset                   $Found
220300040629     c                   reset                   $IMA
220400040629     c                   reset                   $IMAdft
220500040629     c                   reset                   $PO
220600040629     c                   reset                   $POdpd
220700080129     c                   reset                   $POerr
220800040629     c                   reset                   $POdft
220900171102      *
221000171102      * - ?Selezione per "Tipo Piano Smistamento"?
221100171102if  2c                   if        %subst( TR9des : 28 : 1 )
221200171102     c                                         <> V1Ctps
221300171102     c                   goto      Read_Next
221400171102e   2c                   endif
221500040309      *
221600040702      * - Per PIANO GIORNALIERO già immesso:
221700031106      *   controllo la corrispondenza dei dati di default (che sto
221800031106      *   leggendo) con quelli effettivamente immessi
221900040315if  2c                   if            $Default = *off
222000040315     c                             and $NewFVa  = *off
222100040702     c                             and $DltPS   = *off
222200031106     c                   exsr      RepFVAeff
222300040702      * - Per PIANO DI DEFAULT  &
222400040702      *   Per PIANO GIORNALIERO nuovo:
222500040623      *   imposto i campi del file di comodo (per il piano giornaliero)
222600040623      *   come quelli del file in aggiornamento (per i dati di default)
222700040623x   2c                   else
222800040623     c                   movel     FLTR9ds       FLXR9ds
222900040623     c                   eval      $Found   = *on
223000031106e   2c                   endif
223100040701      *
223200040701      * - Controllo se postazione IMA o P.O.
223300080118if  2c                   if            $Default = *off
223400040702     c                             and TR9bnl  <> 'S'
223500040702     c                   eval      $Hidden  = *on
223600040702     c                   eval      Wpost    = TR9bai
223700040702     c                   exsr      Ctrl_IMA
223800040702     c                   exsr      Ctrl_PO
223900040705e   2c                   endif
224000040705if  2c                   if        $Found   = *on
224100040701     c                   eval      $Hidden  = *off
224200040701     c                   eval      Wpost    = XR9bai
224300040701     c                   exsr      Ctrl_IMA
224400040701     c                   exsr      Ctrl_PO
224500040705e   2c                   endif
224600040701      *
224700040702      * - Selezione per piano giornaliero DPD:
224800040705if  2c                   if            $Default = *off
224900040702     c*** non solo new:            and $NewFVa  = *off
225000040702     c                             and V1Cdpd   = 'S'
225100040702      *   - baia non letti
225200040702     c                             and TR9bnl  <> 'S'
225300040705      *   - postazioni IMx
225400040705     c                             and $IMA     = *off
225500040702      *   - postazioni PO-dpd
225600040702     c                             and $POdpd   = *off
225700040702     c                   goto      Read_Next
225800040705e   2c                   endif
225900040705      *
226000040705      *   - postazioni IMx obbligatorie
226100040705if  2c                   if            $Default = *off
226200040705     c*** non solo new:            and $NewFVa  = *off
226300040705     c                             and V1Cdpd   = 'S'
226400040705     c                             and $IMA     = *on
226500040705     c                   eval      *in10    = *off
226600040705     c                   eval      xx       = 1
226700040705     c     Wpost         lookup    $MPT(xx)                               10
226800040705     c   10$MPTobbl(xx)  cabeq     *off          Read_Next
226900040705e   2c                   endif
227000031106      *
227100031106      * - Default (comuni)
227200040309      * > > P.O. senza VDL
227300040701if  2c                   if        $VDL     = *off
227400040701     c                   exsr      CarNoVDL_1
227500040309      * > > P.O. con VDL
227600040701x   2c                   else
227700040701     c                   exsr      CarSiVDL_1
227800040701e   2c                   endif
227900031106      *
228000031106      * - Impostazione dei F.V. in piano giornaliero
228100031106if  2c                   IF        $Default = *off
228200031106      *
228300031106      * - - da default
228400040702if  3c                   IF            $NewFVa  = *on
228500040702     c                             or  $DltPS   = *on
228600031106      *
228700040309sel 4c                   select
228800040309w   4c                   when      TR9bnl = 'S'
228900040615w   4c                   when      $PO    = *on
229000040705if  5c                   if        $POdpd = *on
229100040705     c                   exsr      PostPOdpd
229200040705e   5c                   endif
229300040309w   4c                   when      $IMA   = *on
229400040615     c                   exsr      PostIMA
229500040309x   4c                   other
229600040615     c                   exsr      PostNoIMA
229700040309e   4c                   endsl
229800031106      *
229900031106      * - - da record già immessi
230000031106x   3c                   ELSE
230100031106      *
230200040621if  4c                   if        $Found   = *on
230300031106     c                   reset                   WLBda8
230400031106     c                   reset                   DSdata8
230500031106     c                   reset                   DSdata6
230600040621if  5c                   if        XR9dfv   > *zeros
230700040623     c                   eval      G08inv   = XR9dfv
230800031106     c                   call      'XSRDA8'
230900031106     c                   parm                    WLBda8
231000031106     c                   move      G08dat        DSdata8
231100031106     c                   z-add     DSdt8gg       DSdt6gg
231200031106     c                   z-add     DSdt8mm       DSdt6mm
231300031106     c                   z-add     DSdt8aa       DSdt6aa
231400040621e   5c                   endif
231500040621e   4c                   endif
231600040309      * > > > P.O. senza VDL
231700031106if  4c                   if        $VDL     = *off
231800040701     c                   exsr      CarNoVDL_2
231900040309      * > > > P.O. con VDL
232000031106x   4c                   else
232100040701     c                   exsr      CarSiVDL_2
232200031106e   4c                   endif
232300031106      *
232400031106e   3c                   ENDIF
232500040705      *
232600040705      * Reimposto il valore assoluto tra i F.V. memorizzati x P.O. DPD
232700040705     c                   eval      $FVxPOn  =  %abs($FVxPOn)
232800031106      *
232900031106      * Per piano giornaliero imposto "*" se più F.V. per il padroncino
233000040615      *   o se più F.V. per la postazione = P.O. DPD (solo VdL)
233100040701     c                   clear                   Wast
233200031106     c                   clear                   Wpdr
233300040615     c                   clear                   Wpost
233400040701sel 3c                   select
233500040701w   3c                   when      XR9pdc   > *zeros
233600040621     c                   eval      Wpdr     = XR9pdc
233700031106     c                   exsr      CtrlAutotr3
233800040701w   3c                   when      $POdpd   = *on
233900040621     c                   eval      Wpost    = XR9bai
234000040615     c                   exsr      AsterFVxPO
234100040701e   3c                   endsl
234200040701if  3c                   if        $VDL     = *off
234300040701     c                   eval      S1Cast   = Wast
234400040701x   3c                   else
234500040701     c                   eval      S2Cast   = Wast
234600040701e   3c                   endIf
234700031106      *
234800031106e   2c                   ENDIF
234900031106      *
235000031106      * Se *zeros li reimposto a *blanks (numerici "editati")
235100031106if  2c                   if        S1Cnfv   = *zeros
235200031106     c                   clear                   S1Cnfv
235300031106e   2c                   endif
235400031106if  2c                   if        S3Cnfv   = *zeros
235500031106     c                   clear                   S1Cnfv
235600031106e   2c                   endif
235700031106if  2c                   if        S2Cnfv   = *zeros
235800031106     c                   clear                   S2Cnfv
235900031106e   2c                   endif
236000040304if  2c                   if        S4Cnfv   = *zeros
236100040304     c                   clear                   S4Cnfv
236200040304e   2c                   endif
236300031106      *
236400090203x   1c***                ELSE
236500031106      *
236600031106      * Se già rilevato %EOF:
236700031106      * - impostazione campi vuoti nei restanti record del sfl
236800031106      *   (se "default")
236900031106      *
237000031106e   1c                   ENDIF
237100040701      *
237200040701      * Scrivo il singolo record nel subfile
237300040701     c                   exsr      WrtSflRec
237400040701      *
237500040701     c     Read_Next     tag
237600031106      *
237700031106      * Leggo il record successivo da caricare
237800031106if  1c                   if        $EOF     = *off
237900171103     c     K04FLTR9      reade(n)  FLTR9000
238000171103if  2c                   if        %eof(FLTR902L)
238100031106     c                   eval      $EoF     = *on
238200031106e   2c                   endif
238300031106e   1c                   endif
238400031106      *
238500031106      * Imposto l'uscita SE:
238600031106      * - ho già scritto il numero massimo di rec. previsti nel sfl
238700031106if  1c                   if            S01nrr  >= C_MaxRecSf
238800031106     c                             or  S02nrr  >= C_MaxRecSf
238900031106      * - è stato rilevato EOF in "Piano Giornaliero" (NON di default)
239000040702      *   non emetto records vuoti (come per il default)
239100031106     c                             or ($Default = *off
239200040702     c                             and $EoF     = *on)
239300031106     c                   eval      $EndSFL  = *on
239400031106e   1c                   endif
239500031106      *
239600031106     c                   ENDSR
239700040701      *
239800040701      *---------------------------------------------------------------*
239900050614      * Scrittura singolo record nel subfile                          *
240000040701      *---------------------------------------------------------------*
240100040701     c     WrtSflRec     BEGSR
240200040701      *
240300040701      * - reimposto indicatori x DSPATR condizionati
240400040701if  1c                   IF        $VDL = *off
240500060110     c                   exsr      AtrS01S03
240600040701x   1c                   ELSE
240700060110     c                   exsr      AtrS02S04
240800040701e   1c                   ENDIF
240900040701      *
241000040701      * Scrittura del singolo record nel sfl in esame
241100040702if  1c                   if            $Default = *on
241200040702     c                             OR  $Default = *off
241300040702     c                             and $EoF     = *off
241400040701     c                   eval      *in32    = *on
241500040701sel 2c                   select
241600040701w   2c                   when      $Video   = '2'
241700040701     c                   add       1             S01nrr
241800040701     c                   write     MS03S01
241900040701w   2c                   when      $Video   = '3'
242000040701     c                   add       1             S02nrr
242100040701     c                   write     MS03S02
242200040701e   2c                   when      $Video   = '4'
242300060110     c                   eval      *in54    = (*in80  = *off)
242400040701     c                   add       1             S01nrr
242500040701     c                   write     MS03S03
242600040701w   2c                   when      $Video   = '5'
242700060110     c                   eval      *in54    = (*in80  = *off)
242800040701     c                   add       1             S02nrr
242900040701     c                   write     MS03S04
243000040701e   2c                   endsl
243100040701e   1c                   endif
243200040701      *
243300040701     c                   ENDSR
243400031106      *
243500031106      *---------------------------------------------------------------*
243600031106      * Reperimento dei dati effettivamente immessi per postazione    *
243700031106      *---------------------------------------------------------------*
243800031106     c     RepFVAeff     BEGSR
243900031106      *
244000031106      * Ctrl se esiste il rec. corrispondente nel piano giornaliero
244100031106      *   già immesso
244200040623     c                   eval      XR9prg = (TR9prg * 2) - 1
244300171103     c     K06TR902      chain     FLTR902
244400171103     c                   eval      $Found = (%found(FLTR902X))
244500040621      *
244600040621      * Se NON lo trovo: segnalo il rec. di dft come omesso ("XXX")
244700040623if  1c                   if        $Found  = *off
244800040623     c*** troppo:        clear                   FLXR9ds
244900040623     c                   clear                   XR9bai
245000161213     c                   clear                   XR9des
245100040623     c                   clear                   XR9pdc
245200040623     c                   clear                   XR9npg
245300040623     c                   clear                   XR9nfv
245400040623     c                   clear                   XR9dfv
245500040623     c                   clear                   XR9fl1
245600040623     c                   clear                   XR9fl2
245700040623     c                   clear                   XR9fl3
245800040623     c                   eval      XR9bai  = C_PostazNull
245900040621e   1c                   endif
246000031106      *
246100040315     c                   ENDSR
246200050614      *
246300050614      *---------------------------------------------------------------*
246400050614      * Verifica se:                                                  *
246500050614      * - autotrasportatore MESSAGGERIA per piano di default          *
246600050614      * - distinta MESSAGGERIA per piano di smistamento               *
246700050614      *---------------------------------------------------------------*
246800050614     c     X_Messaggeria BEGSR
246900050614      *
247000050614if  1c                   if        $Default = *on
247100050614      *
247200050614     c                   clear                   S2Dmes
247300050614      * in piano di default: i dati dell'autotrasportatore sono appena
247400050614      *   stati reperiti
247500050614     c*** (già fatto)    movel     APDflr        dAPDflr
247600050614if  2c                   if        §APDtpm  = 'M'
247700050629     c                   eval      S2Dmes   = C_Messaggeria
247800050614e   2c                   endif
247900050614      *
248000050614x   1c                   else
248100050614      *
248200050614if  2c                   if        wNFVseq  = 1
248300050614     c                   clear                   S2Dmes
248400050614x   2c                   else
248500050614     c                   clear                   S4Dmes
248600050614e   2c                   endif
248700050614      * in piano di smistamento: i dati del F.V. sono appena
248800050614      *   stati reperiti da FNFVV
248900050701     c*** (già fatto)    movel     FVVspn        dFVVspn
249000050614if  2c                   if        §FVVtpm  = 'M'
249100050614if  3c                   if        wNFVseq  = 1
249200050629     c                   eval      S2Dmes   = C_Messaggeria
249300050614x   3c                   else
249400050629     c                   eval      S4Dmes   = C_Messaggeria
249500050614e   3c                   endif
249600050614e   2c                   endif
249700050614      *
249800050614e   1c                   endif
249900050614      *
250000050614     c                   ENDSR
250100040701      *
250200040701      *---------------------------------------------------------------*
250300040701      * Caricamento dati di default in F.V. per P.O. senza VDL        *
250400040701      *---------------------------------------------------------------*
250500040701     c     CarNoVDL_1    BEGSR
250600040701      *
250700040701     c                   move      TR9pep        S1Cpep
250800040701     c                   eval      S1Cdes   = TR9des
250900040701     c                   eval      S1Cbai   = XR9bai
251000040701if  1c                   if        $Found   = *on
251100040701sel 2c                   select
251200040701w   2c                   when      $IMA     = *on
251300040701     c                   movel     $MPTdes(yy)   S1Dpdc
251400040701     c                   movel     $MPTdes(yy)   S3Dpdc
251500040701w   2c*** solo x VDL:    when      $PO      = *on
251600040701w   2c                   when      XR9pdc  <> *zeros
251700040701     c                   move      XR9pdc        S1Cpdc
251800040701     c                   eval      APDpdr   = XR9pdc
251900040701     c                   exsr      CtrlAutotr2
252000040701     c                   movel     APDrsc        S1Dpdc
252100040701     c                   movel     APDrsc        S3Dpdc
252200040701     c                   setoff                                       552890
252300040701     c                   clear                   V1Dmsg
252400040701e   2c                   endsl
252500040701e   1c                   endif
252600040701      * - - - Campi hidden
252700040701if  1c                   if        $Default = *off
252800060113if  2c                   if            $NewFVa = *off
252900060113     c                             and $DltPS  = *off
253000060113     c                             and $Found  = *on
253100060113     c                   eval      H1Cprg   = XR9prg
253200060113x   2c                   else
253300060113     c                   eval      H1Cprg   = (TR9prg * 2) - 1
253400060113e   2c                   endif
253500040701     c                   eval      H1Cbai   = TR9bai
253600040701     c                   eval      H1Cpdc   = TR9pdc
253700040701if  2c                   if            $IMAdft = *off
253800040701     c                             and H1Cpdc <> APDpdr
253900040701     c                   eval      APDpdr   = H1Cpdc
254000040701     c                   exsr      CtrlAutotr2
254100040701     c                   setoff                                       552890
254200040701     c                   clear                   V1Dmsg
254300040701e   2c                   endif
254400040701if  2c                   if        $IMAdft  = *off
254500040701     c                   eval      H1Dpdc   = APDrsc
254600040701x   2c                   else
254700040701     c                   eval      H1Dpdc   = $MPTdes(yy)
254800040701e   2c                   endif
254900040701e   1c                   endif
255000040701      *
255100040701     c                   ENDSR
255200040701      *
255300040701      *---------------------------------------------------------------*
255400040701      * Impostazione dei F.V. in piano giornaliero per P.O. senza VDL *
255500040701      *---------------------------------------------------------------*
255600040701     c     CarNoVDL_2    BEGSR
255700040701      *
255800040701if  1c                   if        $Found   = *off
255900040701      *
256000040701     c                   eval      S1Cbai   = C_PostazNull
256100040701     c                   eval      S1Kdft   = C_Default
256200040701     c                   eval      S1Kbad   = C_Postazione
256300040701     c                   eval      S1Cbad   = H1Cbai
256400040701if  2c                   if        H1Cpdc  <> *zeros
256500040701     c                   eval      S1Katd   = C_Autotr
256600040701     c                   move      H1Cpdc        S1Catd
256700040701e   2c                   endif
256800040701     c                   eval      S1Datd   = H1Dpdc
256900040701      *
257000040701x   1c                   else
257100040701      *
257200040701     c                   move      XR9nfv        S1Cnfv
257300040701     c                   move      DSdata6       S1Ddfv
257400040701     c                   eval      S1Dfpp   = XR9fl1
257500040701if  2c                   if        $IMA     = *off
257600040702     c                             and $PO  = *off
257700040701     c                   exsr      CercaIIfv
257800040701e   2c                   endif
257900040701      *
258000040701e   1c                   endif
258100040701      *
258200040701     c                   ENDSR
258300040701      *
258400040701      *---------------------------------------------------------------*
258500040701      * Caricamento dati di default in F.V. per P.O.  con  VDL        *
258600040701      *---------------------------------------------------------------*
258700040701     c     CarSiVDL_1    BEGSR
258800040701      *
258900040701     c                   movel     TR9pep        S2Cpep
259000040701     c                   eval      S2Cdes   = TR9des
259100040701     c                   eval      S2Cbnl   = TR9bnl
259200040701     c                   eval      S2Cbai   = XR9bai
259300161213     c                   if        $PO      = *on
259400161215     c                   eval      S2Cznc   = %subst( XR9des : 29 )
259500161213     c                   endif
259600040701      *
259700040701if  1c                   IF        $Found   = *on
259800040701      *
259900040701sel 2c                   SELECT
260000040701      *
260100040701w   2c                   WHEN      $IMA     = *on
260200040701     c                   movel     $MPTdes(yy)   S2Dpdc
260300040701     c                   movel     $MPTdes(yy)   S4Dpdc
260400040701      *
260500040701w   2c                   WHEN      $PO      = *on
260600040701     c                   movel     $ORGdes(xy)   S2Dpdc
260700040701     c                   movel     $ORGdes(xy)   S4Dpdc
260800040701      *
260900040701w   2c                   WHEN      XR9pdc  <> *zeros
261000040701     c                   move      XR9pdc        S2Cpdc
261100040701     c                   eval      APDpdr   = XR9pdc
261200040701     c                   exsr      CtrlAutotr2
261300040701     c                   movel     APDrsc        S2Dpdc
261400040701     c                   movel     APDrsc        S4Dpdc
261500050629      * - imposto il flag per autotrasportatore Messaggeria
261600050614if  3c                   if            $Default = *on
261700050614     c                             and §OGpkm   = 'S'
261800050629     c                             and $VDL     = *on
261900050614     c                             and $IMA     = *off
262000050614     c                             and $PO      = *off
262100050701     c                   eval      wNFVseq  = 1
262200050614     c                   movel     APDflr        dAPDflr
262300050629     c                   exsr      X_Messaggeria
262400050614e   3c                   endif
262500040701     c                   setoff                                       552890
262600040701     c                   clear                   V1Dmsg
262700040701      *
262800040701w   2c                   WHEN      TR9bnl   = 'S'
262900040701     c                   eval      S4Dpdc   = C_BaiaNonLetti
263000040701      *
263100040701e   2c                   ENDSL
263200040701      *
263300040701e   1c                   ENDIF
263400040701      *
263500040701      * - - - Campi hidden
263600060113if  1c                   if        $Default = *off
263700060113if  2c                   if            $NewFVa = *off
263800060113     c                             and $DltPS  = *off
263900060113     c                             and $Found  = *on
264000060113     c                   eval      H2Cprg   = XR9prg
264100060113x   2c                   else
264200060113     c                   eval      H2Cprg   = (TR9prg * 2) - 1
264300060113e   2c                   endif
264400060113e   1c                   endif
264500040701if  1c                   IF            $Default = *off
264600040701     c                             and TR9bnl  <> 'S'
264700040701      *
264800040701     c                   eval      H2Cbai   = TR9bai
264900161213if  2c                   if        $POdft   = *on
265000161215     c                   eval      H2Cznc   = %subst( TR9des : 29 )
265100161213e   2c                   endif
265200040701     c                   eval      H2Cpdc   = TR9pdc
265300040701      *
265400040701if  2c                   if            $IMAdft = *off
265500040701     c                             and $POdft  = *off
265600040701     c                             and H2Cpdc <> APDpdr
265700040701     c                   eval      APDpdr   = H2Cpdc
265800040701     c                   exsr      CtrlAutotr2
265900040701     c                   setoff                                       552890
266000040701     c                   clear                   V1Dmsg
266100040701e   2c                   endif
266200040701      *
266300040701sel 2c                   Select
266400040701w   2c                   When      $IMAdft  = *off   and
266500040701     c                             $POdft   = *off
266600040701     c                   eval      H2Dpdc   = APDrsc
266700040701w   2c                   When      $IMAdft  = *on
266800040701     c                   eval      H2Dpdc   = $MPTdes(yy)
266900040701w   2c                   When      $POdft   = *on
267000040701     c                   eval      H2Dpdc   = $ORGdes(xy)
267100040701e   2c                   EndSl
267200040701      *
267300040701e   1c                   ENDIF
267400040701      *
267500040701     c                   ENDSR
267600040701      *
267700040701      *---------------------------------------------------------------*
267800040701      * Impostazione dei F.V. in piano giornaliero per P.O.  con  VDL *
267900040701      *---------------------------------------------------------------*
268000040701     c     CarSiVDL_2    BEGSR
268100040701      *
268200040701sel 1c                   Select
268300040701      *
268400040701w   1c                   When      TR9bnl   = 'S'
268500040701      *
268600040701w   1c                   When          $Found =  *off
268700040701     c                             and TR9bnl <> 'S'
268800040701     c                             and TR9bai <> C_PostazNull
268900040701     c                   eval      S2Cbai   = C_PostazNull
269000040701     c                   eval      S2Kdft   = C_Default
269100040701     c                   eval      S2Kbad   = C_Postazione
269200040701     c                   eval      S2Cbad   = H2Cbai
269300040701if  2c                   if        H2Cpdc  <> *zeros
269400040701     c                   eval      S2Katd   = C_Autotr
269500040701     c                   move      H2Cpdc        S2Catd
269600040701e   2c                   endif
269700040701     c                   eval      S2Datd   = H2Dpdc
269800040701      *
269900040701w   1c                   When          $Found =  *on
270000040701     c                             and XR9nfv <> *zeros
270100040701     c                   move      XR9nfv        S2Cnfv
270200040701     c                   move      DSdata6       S2Ddfv
270300040701     c                   eval      S2Dfpp   = XR9fl1
270400050629      * - reperisco il flag per autotrasportatore Messaggeria
270500050629if  2c                   if            $Default = *off
270600050629     c                             and §OGpkm   = 'S'
270700050629     c                             and $VDL     = *on
270800050629     c                             and $IMA     = *off
270900050629     c                             and $PO      = *off
271000050701     c                   eval      FVVfgs   = V1Cpog
271100050701     c                   eval      FVVnpg   = 4
271200050629     c                   move      XR9nfv        FVVnfv
271300050629     c     K03FNFVV      chain     FNFVV000
271400050629if  3c                   if            %found(FNFVV03L)
271500050701     c                   eval      wNFVseq  = 1
271600050629     c                   movel     FVVspn        dFVVspn
271700050629     c                   exsr      X_Messaggeria
271800050629e   3c                   endif
271900050629e   2c                   endif
272000040701if  2c                   if        $IMA     = *off
272100040702     c                             and $PO  = *off
272200040701     c                   exsr      CercaIIfv
272300050629      * - reperisco il flag per autotrasportatore Messaggeria
272400050629if  3c                   if            $Default = *off
272500050629     c                             and §OGpkm   = 'S'
272600050629     c                             and $VDL     = *on
272700050629     c                             and $IMA     = *off
272800050629     c                             and $PO      = *off
272900171103     c                             and %found(FLTR902X)
273000050629     c                             and XR9nfv   > *zeros
273100050701     c                   eval      FVVfgs   = V1Cpog
273200050701     c                   eval      FVVnpg   = 4
273300050629     c                   move      XR9nfv        FVVnfv
273400050629     c     K03FNFVV      chain     FNFVV000
273500050629if  4c                   if            %found(FNFVV03L)
273600050701     c                   eval      wNFVseq  = 2
273700050629     c                   movel     FVVspn        dFVVspn
273800050629     c                   exsr      X_Messaggeria
273900050629e   4c                   endif
274000050629e   3c                   endif
274100040701e   2c                   endif
274200040701      *
274300040701e   1c                   EndSl
274400040701      *
274500040701     c                   ENDSR
274600040607      *
274700040607      *---------------------------------------------------------------*
274800040615      * Verifica se Postazione = P.O. (...magari DPD) !!!             *
274900040607      *---------------------------------------------------------------*
275000040607     c     Ctrl_PO       BEGSR
275100040607      *
275200040607if  1c                   if        $Hidden  = *off
275300040607     c                   reset                   $PO
275400040621     c                   reset                   $POdpd
275500080129     c                   reset                   $POerr
275600040607x   1c                   else
275700040607     c                   reset                   $POdft
275800040607e   1c                   endif
275900040607      *
276000040607if  1c                   if            Wpost  <> *blanks
276100040607     c                             and Wpost  <> C_PostazNull
276200040607     c                             and $IMA   =  *off
276300040607     c                             and %subst(Wpost:1:1) >= '0'
276400040607     c                             and %subst(Wpost:1:1) <= '9'
276500040607      *
276600040607     c                   eval      xy       = 1
276700040607     c                   eval      *in10    = *off
276800040607     c     Wpost         lookup    $ORGfil(xy)                            10
276900040607if  2c                   if        $Hidden  = *off
277000040607     c                   eval      $PO      = *in10
277100040607x   2c                   else
277200040607     c                   eval      $POdft   = *in10
277300040607e   2c                   endif
277400040607      *
277500040607if  2c                   if        $Hidden  = *off
277600040607     c                             and $PO  = *on
277700040611     c                   eval      $POdpd   = ($ORGntw(xy) = 'DPD')
277800080129     c                   eval      $POerr   = ($ORGerr(xy) = *on)
277900040607     c                   add       1             $ORGnr(xy)
278000040607e   2c                   endif
278100040607      *
278200040607e   1c                   endif
278300040607      *
278400040607     c                   ENDSR
278500040408      *
278600040408      *---------------------------------------------------------------*
278700040408      * Verifica se postazione "IMA"                                  *
278800040408      *---------------------------------------------------------------*
278900040408     c     Ctrl_IMA      BEGSR
279000040408      *
279100040423if  1c                   if        $Hidden  = *off
279200040408     c                   reset                   $IMA
279300040423x   1c                   else
279400040423     c                   reset                   $IMAdft
279500040423e   1c                   endif
279600040408      *
279700040423if  1c                   if            Wpost  <> *blanks
279800040423     c                             and Wpost  <> C_PostazNull
279900040408      *
280000040423     c                   eval      yy       = 1
280100040408     c                   eval      *in10    = *off
280200040423     c     Wpost         lookup    $MPT(yy)                               10
280300040423if  2c                   if        $Hidden  = *off
280400040408     c                   eval      $IMA     = *in10
280500040423x   2c                   else
280600040423     c                   eval      $IMAdft  = *in10
280700040423e   2c                   endif
280800040423      *
280900040408e   1c                   endif
281000040408      *
281100040408     c                   ENDSR
281200040705      *
281300040705      *---------------------------------------------------------------*
281400040705      * Impostazione F.V. da default per postazione = P.O. DPD        *
281500040705      *---------------------------------------------------------------*
281600040705     c     PostPOdpd     BEGSR
281700040705      *
281800040705     c                   eval      xx     = 1
281900040705     c                   eval      *in10  = *off
282000040705     c                   reset                   Wpost
282100040705if  1c                   if        $VDL   = *off
282200040705     c                   move      S1Cbai        Wpost
282300040705x   1c                   else
282400040705     c                   move      S2Cbai        Wpost
282500040705e   1c                   endif
282600040705      *
282700040705do  1c                   DOW       xx    <= %elem($FVxPOa)
282800040705      *
282900040705     c     Wpost         lookup    $FVxPOa(xx)                            10
283000040705      *
283100040705      * Nessun f.v. reperito per il DPD in esame
283200040705if  2c                   if        NOT *in10
283300040705     c                   leave
283400040705e   2c                   endif
283500040705      *
283600040705      * Scarto i f.v. con p.o. DPD tra "altri P.O." del f.v.
283700040705if  2c                   if            *in10       = *on
283800040705     c                             and $FVxPOn(xx) < *zeros
283900040705     c                   eval      xx     = xx + 1
284000040705     c                   iter
284100040705e   2c                   endif
284200040705      *
284300040705      * Propongo il 1° f.v. con LNA = p.o. DPD
284400040705if  2c                   IF        *in10  = *on
284500040705      *
284600040705     c                   reset                   WLBda8
284700040705     c                   reset                   DSdata8
284800040705     c                   reset                   DSdata6
284900040705     c                   eval      G08inv = $FVxPOd(xx)
285000040705     c                   call      'XSRDA8'
285100040705     c                   parm                    WLBda8
285200040705     c                   move      G08dat        DSdata8
285300040705     c                   z-add     DSdt8gg       DSdt6gg
285400040705     c                   z-add     DSdt8mm       DSdt6mm
285500040705     c                   z-add     DSdt8aa       DSdt6aa
285600040705if  3c                   if        $VDL   = *off
285700040705     c                   move      $FVxPOn(xx)   S1Cnfv
285800040705     c                   move      DSdata6       S1Ddfv
285900040705x   3c                   else
286000040705     c                   move      $FVxPOn(xx)   S2Cnfv
286100040705     c                   move      DSdata6       S2Ddfv
286200040705e   3c                   endif
286300040705     c                   leave
286400040705      *
286500040705e   2c                   ENDIF
286600040705      *
286700040705e   1c                   ENDDO
286800040705      *
286900040705     c                   ENDSR
287000031106      *
287100031106      *---------------------------------------------------------------*
287200040615      * Impostazione F.V. da default per postazione IMA               *
287300031106      *---------------------------------------------------------------*
287400040615     c     PostIMA       BEGSR
287500031106      *
287600031106     c                   eval      FVVfgs = V1Cpog
287700031106     c                   eval      FVVnpg = 3
287800031106     c                   eval      FVVnfv = *hival
287900031106     c     K03FNFVV      setgt     FNFVV000
288000031106     c     K02FNFVV      readpe    FNFVV000
288100031106      *
288200031106do  1c                   DOW       NOT %eof(FNFVV03L)
288300031106      *
288400031106if  2c                   IF            FVVatb = *blanks
288500031106     c                             and FVVfcf = *blanks
288600031106     c                             and FVVspg = 'A'
288700031106     c                   reset                   WLBda8
288800031106     c                   reset                   DSdata8
288900031106     c                   reset                   DSdata6
289000031106     c                   eval      G08inv = FVVdfv
289100031106     c                   call      'XSRDA8'
289200031106     c                   parm                    WLBda8
289300031106     c                   move      G08dat        DSdata8
289400031106     c                   z-add     DSdt8gg       DSdt6gg
289500031106     c                   z-add     DSdt8mm       DSdt6mm
289600031106     c                   z-add     DSdt8aa       DSdt6aa
289700031106if  3c                   if        $VDL   = *off
289800031106     c                   move      *zeros        S1Cnfv
289900031106     c                   move      FVVnfv        S1Cnfv
290000031106     c                   move      DSdata6       S1Ddfv
290100031106     c                   eval      S1Dfpp = FVVfpp
290200031106x   3c                   else
290300031106     c                   move      *zeros        S2Cnfv
290400031106     c                   move      FVVnfv        S2Cnfv
290500031106     c                   move      DSdata6       S2Ddfv
290600031106     c                   eval      S2Dfpp = FVVfpp
290700031106e   3c                   endif
290800031106     c                   leave
290900031106e   2c                   ENDIF
291000031106      *
291100031106     c     K02FNFVV      readpe    FNFVV000
291200031106e   1c                   ENDDO
291300031106      *
291400031106     c                   ENDSR
291500031106      *
291600031106      *---------------------------------------------------------------*
291700040615      * Impostazione F.V. da default per postazione non IMA           *
291800031106      *---------------------------------------------------------------*
291900040615     c     PostNoIMA     BEGSR
292000031106      *
292100031106     c                   eval      xx     = 1
292200031106     c                   eval      *in10  = *off
292300031106     c                   movel     V1Cpog        Wpdr
292400031106if  1c                   if        $VDL   = *off
292500031106     c                   move      S1Cpdc        Wpdr
292600031106x   1c                   else
292700031106     c                   move      S2Cpdc        Wpdr
292800031106e   1c                   endif
292900031106     c     Wpdr          lookup    $MEMat(xx)                             10
293000050701      *
293100031111if  1c                   IF        *in10  = *on
293200050701      *
293300050701     c                   clear                   dFVVspn
293400031111      * - 1° f.v.
293500031111if  2c                   IF        $MEMfv1(xx) > *zeros
293600031106      *
293700031111     c                   move      $MEMfv1(xx)   DS_FV
293800031106     c                   reset                   WLBda8
293900031106     c                   reset                   DSdata8
294000031106     c                   reset                   DSdata6
294100031106     c                   eval      G08inv = dsFVdfv
294200031106     c                   call      'XSRDA8'
294300031106     c                   parm                    WLBda8
294400031106     c                   move      G08dat        DSdata8
294500031106     c                   z-add     DSdt8gg       DSdt6gg
294600031106     c                   z-add     DSdt8mm       DSdt6mm
294700031106     c                   z-add     DSdt8aa       DSdt6aa
294800031111if  3c                   if        $VDL   = *off
294900031106     c                   move      dsFVnfv       S1Cnfv
295000031106     c                   move      DSdata6       S1Ddfv
295100031111     c                   eval      S1Dfpp = $MEMmp1(xx)
295200031111x   3c                   else
295300031106     c                   move      dsFVnfv       S2Cnfv
295400031106     c                   move      DSdata6       S2Ddfv
295500031111     c                   eval      S2Dfpp = $MEMmp1(xx)
295600050629      *   - imposto il flag per autotrasportatore Messaggeria
295700050614if  4c                   if            $Default = *off
295800050614     c                             and §OGpkm   = 'S'
295900050629     c                             and $VDL     = *on
296000050614     c                             and $IMA     = *off
296100050614     c                             and $PO      = *off
296200050701     c                   eval      wNFVseq = 1
296300050701     c                   eval      §FVVtpm = $MEMpm1(xx)
296400050614     c                   exsr      X_Messaggeria
296500050614e   4c                   endif
296600031111e   3c                   endif
296700031111      *
296800031111e   2c                   ENDIF
296900031111      * - 2° f.v.
297000031111if  2c                   IF        $MEMfv2(xx) > *zeros
297100031111      *
297200031111     c                   move      $MEMfv2(xx)   DS_FV
297300031111     c                   reset                   WLBda8
297400031111     c                   reset                   DSdata8
297500031111     c                   reset                   DSdata6
297600031111     c                   eval      G08inv = dsFVdfv
297700031111     c                   call      'XSRDA8'
297800031111     c                   parm                    WLBda8
297900031111     c                   move      G08dat        DSdata8
298000031111     c                   z-add     DSdt8gg       DSdt6gg
298100031111     c                   z-add     DSdt8mm       DSdt6mm
298200031111     c                   z-add     DSdt8aa       DSdt6aa
298300031111if  3c                   if        $VDL   = *off
298400031111     c                   move      dsFVnfv       S3Cnfv
298500031111     c                   move      DSdata6       S3Ddfv
298600031111     c                   eval      S3Dfpp = $MEMmp2(xx)
298700031111x   3c                   else
298800031111     c                   move      dsFVnfv       S4Cnfv
298900031111     c                   move      DSdata6       S4Ddfv
299000031111     c                   eval      S4Dfpp = $MEMmp2(xx)
299100050629      *   - imposto il flag per autotrasportatore Messaggeria
299200050614if  4c                   if            $Default = *off
299300050614     c                             and §OGpkm   = 'S'
299400050629     c                             and $VDL     = *on
299500050614     c                             and $IMA     = *off
299600050614     c                             and $PO      = *off
299700050701     c                   eval      wNFVseq = 2
299800050701     c                   eval      §FVVtpm = $MEMpm2(xx)
299900050614     c                   exsr      X_Messaggeria
300000050614e   4c                   endif
300100031111e   3c                   endif
300200031111      *
300300031111e   2c                   ENDIF
300400031106      *
300500031106e   1c                   ENDIF
300600031106      *
300700031106     c                   ENDSR
300800031106      *
300900031106      *---------------------------------------------------------------*
301000031106      * Reperimento eventuale secondo F.V. per postazione/baia        *
301100031106      *---------------------------------------------------------------*
301200031106     c     CercaIIfv     BegSr
301300031106      *
301400040621      * (Se modifica del "piano giornaliero" per P.O. con/senza VDL)
301500040621      *
301600040621     c                   eval      XR9prg = XR9prg + 1
301700171103     c     K07TR902      chain     FLTR902
301800031106      *
301900040621      * Se esiste: ne visualizzo i dati del f.v.
302000171103if  1c                   if        %found(FLTR902X)
302100040309     c                   reset                   WLBda8
302200040309     c                   reset                   DSdata8
302300040309     c                   reset                   DSdata6
302400040621if  2c                   if        XR9dfv   > *zeros
302500040621     c                   eval      G08inv   = XR9dfv
302600040309     c                   call      'XSRDA8'
302700040309     c                   parm                    WLBda8
302800040309     c                   move      G08dat        DSdata8
302900040309     c                   z-add     DSdt8gg       DSdt6gg
303000040309     c                   z-add     DSdt8mm       DSdt6mm
303100040309     c                   z-add     DSdt8aa       DSdt6aa
303200040309e   2c                   endif
303300040315if  2c                   if        $VDL   = *off
303400040621     c                   move      XR9nfv        S3Cnfv
303500040621     c                   eval      S3Dfpp   = XR9fl1
303600040621if  3c                   if        XR9dfv   > *zeros
303700031106     c                   move      DSdata6       S3Ddfv
303800040309e   3c                   endif
303900040315x   2c                   else
304000040621     c                   move      XR9nfv        S4Cnfv
304100040621     c                   eval      S4Dfpp   = XR9fl1
304200040621if  3c                   if        XR9dfv   > *zeros
304300040315     c                   move      DSdata6       S4Ddfv
304400040315e   3c                   endif
304500040315e   2c                   endif
304600031106e   1c                   endif
304700031106      *
304800031106     c                   ENDSR
304900031007      *
305000031007      *---------------------------------------------------------------*
305100031007      * Controllo dati immessi in videata S01 & S03                   *
305200031007      *---------------------------------------------------------------*
305300031007     c     CtrS01        BEGSR
305400031007      *
305500031007      * Ciclo di lettura del sfl
305600031007if  1c                   if        $Default = *on
305700031007     c                   readc     MS03S01
305800031007x   1c                   else
305900031007     c                   readc     MS03S03
306000031007e   1c                   endif
306100031020      *
306200031007do  1c                   DOW       NOT %eof(FIMS03D)
306300031007      *
306400031007     c                   movea     *zeros        *in(41)
306500031007     c                   z-add     S01nrr        C01rcd
306600031007      * - ctrl su singola riga
306700031014if  2c                   if            S1Cpep  <> *blanks
306800031014     c                             or  S1Cdes  <> *blanks
306900031014     c                             or  S1Cbai  <> *blanks
307000031014     c                             or (S1Cpdc  <> *zeros
307100031014     c                             and S1Cpdc  <> *blanks)
307200031014     c                             or (S1Cnfv  <> *blanks
307300031014     c                             and S1Cnfv  <> *zeros)
307400031007     c                   exsr      CtrS01rec
307500031020if  3c                   if            $Default = *off
307600031007     c                             and *in90    = *off
307700040518     c                             and S1Cbai  <> C_PostazNull
307800031007     c                   exsr      CtrS03rec
307900031007e   3c                   endif
308000031007x   2c                   else
308100031007     c                   clear                   S1Ddfv
308200031021     c                   clear                   S1Dfpp
308300031022     c                   clear                   S3Ddfv
308400031022     c                   clear                   S3Dfpp
308500031023     c                   clear                   S1Cast
308600031007     c                   clear                   S1Cpdc
308700031007     c                   clear                   S1Dpdc
308800031007     c                   clear                   S3Dpdc
308900031023     c                   clear                   S1Cdes
309000031023     c                   clear                   S1Kdft
309100031023     c                   clear                   S1Kbad
309200031023     c                   clear                   S1Cbad
309300031023     c                   clear                   S1Katd
309400031023     c                   clear                   S1Catd
309500031023     c                   clear                   S1Datd
309600031007e   2c                   endif
309700031010      *
309800031010      * - reimposto indicatori x DSPATR condizionati
309900040702      *   se NON rilevati errori
310000040702if  2c                   if        NOT *in90
310100040702     c                             and $Default = *on
310200040702     c                             or  $Default = *off
310300060110     c                   exsr      AtrS01S03
310400040702e   2c                   endif
310500031007      *
310600031007      * - riattivo SFLNXTCHG (non solo se rilevato errore)
310700031007     c                   eval      *in32    = *on
310800031007      * - aggiorno il SFL
310900031007if  2c                   if        $Default = *on
311000031007     c                   update    MS03S01
311100031007x   2c                   else
311200031007     c                   update    MS03S03
311300031007e   2c                   endif
311400031007     c                   eval      *in32    = *off
311500031007      * - se rilevato errore esco dal ciclo di controllo
311600031007if  2c                   if        *in28    = *on
311700031104     c                   z-add     C01rcd        C01csr
311800031007     c                   leave
311900031007e   2c                   endif
312000031007      * - leggo il record successivo
312100031007if  2c                   if        $Default = *on
312200031007     c                   readc     MS03S01
312300031007x   2c                   else
312400031007     c                   readc     MS03S03
312500031007e   2c                   endif
312600031007      *
312700031007e   1c                   ENDDO
312800031020      *
312900040423      * Controlli finali "generali"
313000031021      *   (se NON ho già rilevato altri errori da segnalare)
313100040423if  1c                   if        *in28    = *off
313200031021     c                   exsr      CtrBAIxPEP
313300031020e   1c                   endif
313400031007      *
313500031007     c                   ENDSR
313600031007      *
313700031007      *---------------------------------------------------------------*
313800031007      * Controllo singolo record del subfile S01 & S03                *
313900031007      *---------------------------------------------------------------*
314000031007     c     CtrS01rec     BEGSR
314100031023      *
314200031023if  1c                   if        $Default = *off
314300031023     c                   clear                   S1Kdft
314400031023     c                   clear                   S1Kbad
314500031023     c                   clear                   S1Cbad
314600031023     c                   clear                   S1Katd
314700031023     c                   clear                   S1Catd
314800031023     c                   clear                   S1Datd
314900031023e   1c                   endif
315000040423      *
315100040423     c                   reset                   $IMA
315200040423     c                   reset                   $IMAdft
315300031007      *
315400040305      * Controllo codice baia
315500031007if  1c                   if        S1Cpep = *blanks
315600031007     c                   seton                                        512890
315700031009     c                   movel     $Msg(10)      V1Dmsg
315800031007x   1c                   else
315900031010     c                   move(p)   S1Cpep        w014a
316000031010     c     w014a         lookup    $MSLpep                                10
316100031007if  2c                   if        NOT *in10
316200031007     c                   seton                                        512890
316300031009     c                   movel     $Msg(11)      V1Dmsg
316400060110     c                   leavesr
316500031007e   2c                   endif
316600040226     c                   exsr      ContaPEP
316700031007e   1c                   endif
316800060110     c                   if        *in90
316900060110     c                   leavesr
317000060110     c                   endif
317100031007      *
317200031007      * Controllo descrizione
317300031014      * (non obbligatoria)
317400031007      *
317500040305      * Controllo codice postazione
317600040423      * - ricerca
317700040423     c     '?'           scan      S1Cbai                                 10
317800040423if  1c                   if        *in10  =  *on
317900040423     c                   eval      S1Cbai =  *blanks
318000040423     c                   eval      Wcampo =  'S1CBAI    '
318100040423     c                   exsr      srSEARCH
318200040423e   1c                   endif
318300040423      * - controllo
318400040423if  1c                   if        $Default = *off
318500040423     c                   eval      $Hidden  = *on
318600040423     c                   eval      Wpost    = H1Cbai
318700040423     c                   exsr      Ctrl_IMA
318800040423if  2c                   if        $IMAdft  = *on
318900040423     c                   eval      H1Dpdc   = $MPTdes(yy)
319000040423e   2c                   endif
319100040423e   1c                   endif
319200040423     c                   eval      $Hidden  = *off
319300040423     c                   eval      Wpost    = S1Cbai
319400040408     c                   exsr      Ctrl_IMA
319500040423if  1c                   if        $IMA     = *on
319600040423     c                   eval      S1Dpdc   = $MPTdes(yy)
319700040423     c                   eval      S3Dpdc   = $MPTdes(yy)
319800040423e   1c                   endif
319900040423if  1c                   if            $Default = *off
320000040423     c                             and $IMAdft  = *on
320100040423     c                             and H1Cbai  <> S1Cbai
320200040423     c                   eval      S1Datd   = H1Dpdc
320300040423e   2c                   endif
320400031007sel 1c                   select
320500031020w   1c                   when          $Default = *off
320600040316     c                             and S1Cbai   = C_PostazNull
320700031020     c                   clear                   S1Cnfv
320800031020     c                   clear                   S1Ddfv
320900031021     c                   clear                   S1Dfpp
321000031211     c                   clear                   S3Cnfv
321100031211     c                   clear                   S3Ddfv
321200031211     c                   clear                   S3Dfpp
321300031020     c                   clear                   S3Dpdc
321400040408     c                   clear                   S1Cpdc
321500031007w   1c                   when      S1Cbai   =  *blanks
321600031007     c                   seton                                        532890
321700031009     c                   movel     $Msg(13)      V1Dmsg
321800070411w   1c                   when         (%subst(S1Cbai:1:1) >= 'A'
321900070411     c                             and %subst(S1Cbai:1:1) <= 'Z')
322000031010     c                             and %subst(S1Cbai:2:2) >= '01'
322100031010     c                             and %subst(S1Cbai:2:2) <= '99'
322200031007     c                   move      S1Cbai        w002a
322300031007     c     DigitN        check     w002a                                  10
322400031009if  2c                   if        *in10  =  *on
322500031007     c                   seton                                        532890
322600031009     c                   movel     $Msg(14)      V1Dmsg
322700031007x   2c                   else
322800040423     c                   eval      Wpost  =  S1Cbai
322900031007     c                   exsr      ContaBAI
323000031007if  3c                   if        $BAInr(xx) >  1
323100031007     c                   seton                                        532890
323200031009     c                   movel     $Msg(15)      V1Dmsg
323300031007e   3c                   endif
323400031007e   2c                   endif
323500040408w   1c                   when      $IMA   =  *off
323600031007     c                   seton                                        532890
323700031009     c                   movel     $Msg(14)      V1Dmsg
323800031007e   1c                   endsl
323900060110     c                   if        *in90
324000060110     c                   leavesr
324100060110     c                   endif
324200040316      *
324300040316      * Controllo la postazione sia "compatibile" con le altre già
324400040316      *   immesse nella stessa baia
324500161110      * -?Su richiesta di Nicola:?tolto controllo postaz. IMA/non-IMA
324600161110      *   (ora IMA e AUT possono stare nella stessa baia;
324700161110      *   sono le FILIALI che continuano a richiedere una loro baia -
324800161110      *   - MA SONO INSERIBILI SOLO NEI PIANI VDL, non in questi)
324900040316      *
325000040305      * ...e ne memorizzo gli utilizzi per baia
325100040316if  1c                   if        S1Cbai <> C_PostazNull
325200031021     c                   exsr      MemBAIxPEP
325300031020e   1c                   endif
325400040315      *
325500040305      * Visualizzo la postazione di default se variata in piano
325600040305      *   giornaliero
325700031023if  1c                   if            $Default = *off
325800031023     c                             and S1Cbai  <> H1Cbai
325900040423     c                   eval      S1Kdft  = C_Default
326000040625     c                   eval      S1Kbad  = C_Postazione
326100040423     c                   eval      S1Cbad  = H1Cbai
326200040423if  2c                   if        $IMAdft = *on
326300040423     c                   eval      S1Datd  = H1Dpdc
326400040423e   2c                   endif
326500031023e   1c                   endif
326600031007      *
326700031007      * Controllo codice autotrasportatore
326800031007      *   (in S03 e S04 è campo protetto)
326900031015      * Per il piano giornaliero farò appositi controlli nella subr.
327000031015      *   CTRS03REC
327100031007if  1c                   if        $Default = *on
327200040423if  2c                   if        $IMA     = *off
327300031007     c                   clear                   S1Dpdc
327400031009     c                   clear                   S3Dpdc
327500040423e   2c                   endif
327600031007      * - se *blanks lo considero *zeros (sarebbe numerico)
327700031007if  2c                   if        S1Cpdc =  *blanks
327800031007     c                   eval      S1Cpdc =  *zeros
327900031007e   2c                   endif
328000031007      * - ricerca
328100031007     c     '?'           scan      S1Cpdc                                 10
328200031007if  2c                   if        *in10  =  *on
328300031007     c                   eval      S1Cpdc =  *zeros
328400031007     c                   eval      Wcampo =  'S1CPDC    '
328500031007     c                   exsr      srSEARCH
328600031007e   2c                   endif
328700031007      * - controllo
328800031007sel 2c                   select
328900040409w   2c                   when         ($IMA   =  *on
329000040409     c                              or S1Cbai =  C_PostazNull)
329100031010     c                             and S1Cpdc =  *zeros
329200040409w   2c                   when          $IMA   =  *on
329300031007     c                             and S1Cpdc <> *zeros
329400031007     c                   seton                                        552890
329500031009     c                   movel     $Msg(16)      V1Dmsg
329600031015x   2c                   other
329700031021     c                   movel     V1Cpog        APDpdr
329800031021     c                   move      S1Cpdc        APDpdr
329900031014     c                   exsr      CtrlAutotr2
330000031007     c                   movel     APDrsc        S1Dpdc
330100031009     c                   movel     APDrsc        S3Dpdc
330200031007e   2c                   endsl
330300060110     c                   if        *in90
330400060110     c                   leavesr
330500060110     c                   endif
330600031007      * - se *zeros lo reimposto a *blanks (numerico "editato")
330700031007if  2c                   if        S1Cpdc =  *zeros
330800031007     c                   eval      S1Cpdc =  *blanks
330900031007e   2c                   endif
331000031007e   1c                   endif
331100031027      *
331200040305      * In caso di piano giornaliero controllo se è stata tolta la
331300040305      *   postazione ("XXX"), perchè in tal caso non verrà eseguita
331400040305      *   la subr. per i controlli specifici, pertanto non verrebbe
331500040409      *   evidenziato l'autotrasportatore pre-impostato nel default
331600031027if  1c                   if            $Default = *off
331700040316     c                             and S1Cbai   = C_PostazNull
331800031027     c                   clear                   S1Cpdc
331900031027     c                   clear                   S3Dpdc
332000031027     c                   exsr      DecS03pdr
332100031027e   1c                   endif
332200031007      *
332300060110     c                   ENDSR
332400031022      *
332500031022      *---------------------------------------------------------------*
332600031022      * Controlli aggiuntivi per singolo record del subfile S03       *
332700031022      *---------------------------------------------------------------*
332800031022     c     CtrS03rec     BEGSR
332900031022      *
333000031022     c                   reset                   SAVnfv
333100031022      *
333200031022      * Se impostato solo il 2° numero F.V. lo sposto al 1°
333300031022if  1c                   if            (S1Cnfv  =  *blanks
333400031022     c                             or   S1Cnfv  =  *zeros)
333500031022     c                             and (S3Cnfv  <> *blanks
333600031022     c                             and  S3Cnfv  <> *zeros)
333700031022     c                   eval      S1Cnfv  =  S3Cnfv
333800031022     c                   clear                   S3Cnfv
333900031022     c                   clear                   S3Ddfv
334000031022     c                   clear                   S3Dfpp
334100031022e   1c                   endif
334200031022      *
334300031022      * Controllo 1° numero foglio viaggio
334400031022     c                   clear                   S1Ddfv
334500031022     c                   clear                   S1Dfpp
334600031022     c                   eval      wNFVseq =  1
334700031022     c                   eval      Wnfv    =  S1Cnfv
334800031106     c                   exsr      Ctrl_1FV
334900031022     c                   eval      S1Cnfv  =  Wnfv
335000060110     c                   if        *in90
335100060110     c                   leavesr
335200060110     c                   endif
335300031024      * Se tutto bene: ne imposto i dati a video
335400040305if  1c                   if        Wnfv    <> *blanks
335500031022     c                   eval      S1Dfpp  =  FVVfpp
335600031022     c                   move      DSdata6       S1Ddfv
335700040305e   1c                   endif
335800031024      *
335900031024      * Imposto flag e decodifico variazioni relative all'autotrasp.
336000031024      *   (che deve essere comune ad entrambi i F.V.)
336100040312if  1c                   if            S1Cnfv  <> *blanks
336200040408     c                             and $IMA    =  *off
336300031024     c                   move      APDpdr        S1Cpdc
336400031024     c                   movel     APDrsc        S3Dpdc
336500031024x   1c                   else
336600031024     c                   clear                   S1Cpdc
336700040423if  2c                   if        $IMA     = *off
336800031024     c                   clear                   S3Dpdc
336900040423e   2c                   endif
337000031024e   1c                   endif
337100031024     c                   exsr      DecS03pdr
337200031022      *
337300040312      * Controllo 2° numero foglio viaggio (se immesso)
337400031022     c                   clear                   S3Ddfv
337500031022     c                   clear                   S3Dfpp
337600040312if  1c                   if            S3Cnfv <> *blanks
337700040312     c                             and S3Cnfv <> *zeros
337800031022     c                   eval      wNFVseq =  2
337900031022     c                   eval      Wnfv    =  S3Cnfv
338000031106     c                   exsr      Ctrl_1FV
338100031022     c                   eval      S3Cnfv  =  Wnfv
338200060110     c                   if        *in90
338300060110     c                   leavesr
338400060110     c                   endif
338500031024      * Se tutto bene: ne imposto i dati a video
338600031024if  2c                   if        Wnfv    <> *blanks
338700031022     c                   eval      S3Dfpp  =  FVVfpp
338800031022     c                   move      DSdata6       S3Ddfv
338900031024e   2c                   endif
339000031111      * e rieseguo il controllo sul numero di F.V. x autotrasportatore
339100031111      *   (se ne ho selezionato uno da ricerca)
339200031111if  2c                   if        S1Cast  =  '?'
339300031111     c                   clear                   S1Cast
339400031111     c                   exsr      CtrlAutotr3
339500031111     c                   eval      S1Cast =  Wast
339600031111e   2c                   endif
339700040312x   1c                   else
339800040312     c                   clear                   S3Cnfv
339900040312e   1c                   endif
340000031022      *
340100060110     c                   ENDSR
340200031007      *
340300031007      *---------------------------------------------------------------*
340400031106      * Controlli per singolo F.V. del subfile S03 o S04              *
340500031007      *---------------------------------------------------------------*
340600031106     c     Ctrl_1FV      BEGSR
340700040621      *
340800040621     c                   reset                   Wdfv
340900031007      *
341000031007      * Controllo numero foglio viaggio
341100031007      * - se *blanks lo considero *zeros (sarebbe numerico)
341200031106if  1c                   if        Wnfv    =  *blanks
341300031106     c                   eval      Wnfv    =  *zeros
341400031007e   1c                   endif
341500031007      * - ricerca
341600031022     c     '?'           scan      Wnfv                                   10
341700031106if  1c                   if        *in10   =  *on
341800031106     c                   eval      Wnfv    =  *zeros
341900031106sel 2c                   select
342000031106w   2c                   when          $VDL    =  *off
342100031106     c                             and wNFVseq =  1
342200031106     c                   eval      Wcampo  =  'S1CNFV    '
342300031106w   2c                   when          $VDL    =  *off
342400031106     c                             and wNFVseq =  2
342500031106     c                   eval      Wcampo  =  'S3CNFV    '
342600031106w   2c                   when          $VDL    =  *on
342700031106     c                             and wNFVseq =  1
342800031106     c                   eval      Wcampo  =  'S2CNFV    '
342900031106w   2c                   when          $VDL    =  *on
343000031106     c                             and wNFVseq =  2
343100031106     c                   eval      Wcampo  =  'S4CNFV    '
343200031106e   2c                   endsl
343300031007     c                   exsr      srSEARCH
343400031022      * - - se non selezionato nulla sul 2° F.V. -> esco
343500031022if  2c                   if            wNFVseq =  2
343600031022     c                             and Wnfv    =  *zeros
343700031106     c                   eval      Wnfv    =  *blanks
343800060110     c                   leavesr
343900031022e   2c                   endif
344000031111      * - - se selezionato: memorizzo che dovrò ri-controllare se
344100031111      *     padroncino con altri f.v. (non visualizzati)
344200031111     c                   eval      S1Cast =  '?'
344300031007e   1c                   endif
344400031007      * - controlli:
344500031007      * - - numericità
344600031022     c     DigitN        check     Wnfv                                   10
344700031106if  1c                   if        *in10   =  *on
344800031106if  2c                   if        wNFVseq =  2
344900040309     c                   eval      *in56   =  *on
345000031022x   2c                   else
345100040309     c                   eval      *in54   =  *on
345200031022e   2c                   endif
345300031022     c                   seton                                          2890
345400040309     c                   eval      V1Dmsg  =  $Msg(20)
345500060110     c                   leavesr
345600031007e   1c                   endif
345700031007      *
345800031010      * - - effettiva immissione
345900031106if  1c                   if        Wnfv    =  *zeros
346000031106      *
346100031106     c                   clear                   FVVdfv
346200031106     c                   clear                   FVVfpp
346300031106      *
346400080129sel 2c                   select
346500080129      * - - - F.V.?non?più obbligatorio per fil. DPD
346600080129      *       (visualizzato solo un msg di avvertimento)
346700080129w   2c                   when          $VDL    =  *on
346800080129     c                             and $POdpd  =  *on
346900080129     c                             and $POerr  =  *off
347000080129     c                   eval      $POerr  = *on
347100080129     c                   seton                                        542890
347200080129     c                   eval      V1Dmsg  =  $Msg(50)
347300080129      * - - - F.V. obbligatorio per postazione regolare
347400080129w   2c                   when      $POdpd  =  *off
347500080129if  3c                   if        wNFVseq =  2
347600040309     c                   eval      *in56   =  *on
347700080129x   3c                   else
347800040309     c                   eval      *in54   =  *on
347900080129e   3c                   endif
348000031022     c                   seton                                          2890
348100080129if  3c                   if        $IMA    =  *off
348200040312     c                   eval      V1Dmsg  =  $Msg(7)
348300080129x   3c                   else
348400040309     c                   eval      V1Dmsg  =  $Msg(30)
348500080129e   3c                   endif
348600080129e   2c                   endsl
348700080129     c                   if        *in90
348800060110     c                   leavesr
348900080129     c                   endif
349000031106      *
349100031010x   1c                   else
349200031106      *
349300040611      * - - univocità per postazione
349400031030sel 2c                   select
349500031106w   2c                   when      wNFVseq =  1
349600031106     c                   eval      SAVnfv  =  Wnfv
349700040610      * - - - ammesso un solo F.V. per postazione "IMA"
349800031106w   2c                   when          wNFVseq =  2
349900040408     c                             and $IMA    =  *on
350000031106     c                   seton                                        562890
350100040309     c                   eval      V1Dmsg  =  $Msg(37)
350200040611      * - - - ammesso un solo F.V. per postazione P.O. DPD
350300040610w   2c                   when          wNFVseq =  2
350400040611     c                             and $POdpd  =  *on
350500040610     c                   seton                                        562890
350600040610     c                   eval      V1Dmsg  =  $Msg(42)
350700040610      * - - - non ammessi 2 F.V. identici
350800031106w   2c                   when          wNFVseq =  2
350900031106     c                             and SAVnfv  =  Wnfv
351000040408     c                             and $IMA    =  *off
351100031022     c                   seton                                        562890
351200040309     c                   eval      V1Dmsg  =  $Msg(35)
351300031106e   2c                   endsl
351400060110     c                   if        *in90
351500060110     c                   leavesr
351600060110     c                   endif
351700031106      *
351800171107      * - - univocità per foglio arrivi
351900171107      *     (per la fil. 006-PD: sia nel piano VDL che Rulliera)
352000171107if  2c                   if             V1Cpog  =  c_PO_a
352100171107     c                             and  $IMA    =  *off
352200171107     c                             and  $PO     =  *off
352300171107     c                             and  %lookup( %int( wNFV ) :
352400171107     c                                           sk_NFV2 ) > *zero
352500171107if  3c                   if        wNFVseq     =  2
352600171107     c                   eval      *in56   =  *on
352700171107x   3c                   else
352800171107     c                   eval      *in54   =  *on
352900171107e   3c                   endif
353000171107     c                   seton                                          2890
353100171107     c                   if        V1Ctps  = 'V'
353200171107     c                   eval      V1Dmsg  =  %trimR( $Msg(40) ) +
353300171107     c                                       ' Rulliera'
353400171107     c                   else
353500171107     c                   eval      V1Dmsg  =  %trimR( $Msg(40) ) +
353600171107     c                                       ' VDL'
353700171107     c                   endif
353800171107     c                   leavesr
353900171107e   2c                   endif
354000031030      * - - univocità per foglio arrivi
354100171107      *     (in questo piano smistamento)
354200040607if  2c                   if            $IMA    =  *off
354300040607     c                             and $PO     =  *off
354400031030     c                   exsr      ContaNFV
354500031106if  3c                   if        $NFVnr(xx)  >  1
354600031106if  4c                   if        wNFVseq     =  2
354700040309     c                   eval      *in56   =  *on
354800031030x   4c                   else
354900040309     c                   eval      *in54   =  *on
355000031030e   4c                   endif
355100031030     c                   seton                                          2890
355200040518     c                   eval      V1Dmsg  =  %replace(dsBPpos:
355300031106     c                                        $Msg(39):
355400040312     c                                        %scan('&PS':$Msg(39)))
355500060110     c                   leavesr
355600031030e   3c                   endif
355700031030e   2c                   endif
355800031007      * - - esistenza in FNFVV
355900040615if  2c                   if        $PO     =  *off
356000031106     c                   eval      FVVfgs  =  V1Cpog
356100040615if  3c                   if        $IMA    =  *on
356200031106     c                   eval      FVVnpg  =  3
356300040615x   3c                   else
356400031106     c                   eval      FVVnpg  =  4
356500040615e   3c                   endif
356600031022     c                   move      Wnfv          FVVnfv
356700031007     c     K03FNFVV      chain     FNFVV000
356800040615if  3c                   if        NOT %found(FNFVV03L)
356900031106     c*** (già da vl)              or  FVVatb  <> *blanks
357000031106     c*** (già da vl)              or  FVVfcf  <> *blanks
357100040414     c                             or ($IMA    =  *on
357200040408     c                             and FVVspg  <> 'A')
357300040615if  4c                   if        wNFVseq =  2
357400040309     c                   eval      *in56   =  *on
357500040615x   4c                   else
357600040309     c                   eval      *in54   =  *on
357700040615e   4c                   endif
357800031022     c                   seton                                          2890
357900040309     c                   eval      V1Dmsg  =  $Msg(21)
358000060110     c                   leavesr
358100040615e   3c                   endif
358200040621     c                   eval      Wdfv    =  FVVdfv
358300050614      * - - - distinta Messaggeria per piano di smistamento
358400050701if  3c                   if            §OGpkm  =  'S'
358500050701     c                             and $VDL    =  *on
358600050701     c                             and $IMA    =  *off
358700050701     c                             and $PO     =  *off
358800050701     c                             and S2Cbai  <> *blanks
358900050614     c                             and S2Cbai  <> C_PostazNull
359000050629     c                   movel     FVVspn        dFVVspn
359100050614     c                   exsr      X_Messaggeria
359200050614e   3c                   endif
359300040615      * - - esistenza in FNFGV
359400040615x   2c                   else
359500040615     c                   exsr      CtrlFVxPO
359600060110     c                   if        *in90
359700060110     c                   leavesr
359800060110     c                   endif
359900040615e   2c                   endif
360000031007      * - - univocità F.V. IMA nello stesso piano
360100040408if  2c                   if        $IMA    =  *on
360200031106if  3c                   if        SAVnfvIMA   =  *zeros
360300031022     c                   move      Wnfv          SAVnfvIMA
360400031007e   3c                   endif
360500031106if  3c                   if        Wnfv    <> SAVnfvIMA
360600040615if  4c                   if        wNFVseq =  2
360700040309     c                   eval      *in56   =  *on
360800031022x   4c                   else
360900040309     c                   eval      *in54   =  *on
361000031022e   4c                   endif
361100031022     c                   seton                                          2890
361200040518     c                   eval      V1Dmsg  =  %replace(SAVnfvIMA:
361300031106     c                                        $Msg(22):
361400040408     c                                        %scan('&FIMA':$Msg(22)))
361500060110     c                   leavesr
361600031007e   3c                   endif
361700040408      * - - - pulisco l'eventuale cod. autotrasportatore se F.V. IMA
361800040423      *       e ne imposto la decodifica da tab. "MPT"
361900040408if  3c                   if        $VDL    =  *off
362000040408     c                   clear                   S1Cpdc
362100040423     c*** già decodif.:  clear                   S3Dpdc
362200040408x   3c                   else
362300040408     c                   clear                   S2Cpdc
362400040423     c*** già decodif.:  clear                   S4Dpdc
362500040408e   3c                   endif
362600031007e   2c                   endif
362700040615      * - - controllo F.V. non IMA né per P.O. DPD
362800040408if  2c                   if        $IMA    =  *off
362900040615     c                             and $PO =  *off
363000031022      * - - - F.V. di data antecedente a quella del F.V. arrivi
363100031106if  3c                   if        FVVdfv  <  SAVdfva
363200031106if  4c                   if        wNFVseq =  2
363300040309     c                   eval      *in56   =  *on
363400031022x   4c                   else
363500040309     c                   eval      *in54   =  *on
363600031022e   4c                   endif
363700031022     c                   seton                                          2890
363800040309     c                   eval      V1Dmsg  =  $Msg(38)
363900060110     c                   leavesr
364000031022e   3c                   endif
364100031022      * - - - F.V. non IMA assegnato ad altro autotrasportatore
364200040519      *       o ad autotrasportatore già impegnato in altra baia/post.
364300031014     c                   exsr      CtrlAutotr1
364400060110     c                   if        *in90
364500060110     c                   leavesr
364600060110     c                   endif
364700040519      * - - - Controllo/decodifica codice autotrasportatore
364800031106if  3c                   if        wNFVseq <  2
364900031106     c                   eval      APDpdr  =  DSTpdr
365000031020     c                   exsr      CtrlAutotr2
365100060110     c                   if        *in90
365200060110     c                   leavesr
365300060110     c                   endif
365400031022e   3c                   endif
365500031024e   2c                   endif
365600031024e   1c                   endif
365700031007      *
365800031007      * Se OK imposto la data
365900031106     c                   reset                   WLBda8
366000031022     c                   reset                   DSdata8
366100031022     c                   reset                   DSdata6
366200031106if  1c                   if        Wnfv    <> *zeros
366300040621     c                   eval      G08inv  =  Wdfv
366400031007     c                   call      'XSRDA8'
366500031007     c                   parm                    WLBda8
366600031021     c                   move      G08dat        DSdata8
366700031021     c                   z-add     DSdt8gg       DSdt6gg
366800031021     c                   z-add     DSdt8mm       DSdt6mm
366900031021     c                   z-add     DSdt8aa       DSdt6aa
367000031106e   1c                   endif
367100031014      * Se *zeros lo reimposto a *blanks (numerico "editato")
367200031106if  1c                   if        Wnfv    =  *zeros
367300031106     c                   eval      Wnfv    =  *blanks
367400031014e   1c                   endif
367500031111      * memorizzo il F.V. tra quelli immessi nel SFL
367600031111if  1c                   if        Wnfv    >  *zeros
367700031111     c                   eval      xx      =  1
367800031111     c                   eval      *in10   =  *off
367900031111     c     *zeros        lookup    $FV(xx)                                10
368000031111if  2c                   if        *in10   =  *on
368100031111     c                   move      Wnfv          $FV(xx)
368200031111e   2c                   endif
368300031111e   1c                   endif
368400031007      *
368500060110     c                   ENDSR
368600040615      *
368700040615      *---------------------------------------------------------------*
368800040621      * Controlli per singolo foglio viaggio per punto operativo      *
368900040615      *---------------------------------------------------------------*
369000040615     c     CtrlFVxPO     BEGSR
369100040615      *
369200040621      * - Verifica esistenza tra i f.v. già memorizzati
369300040621      *   (con data >= a quella del f.v. V1CNFV)
369400040621     c                   clear                   Wnfv7
369500040621     c                   move      Wnfv          Wnfv7
369600040615     c                   eval      xx      =  1
369700040615     c                   eval      *in10   =  *off
369800040621     c     Wnfv7         lookup    $FVxPOn(xx)                            10
369900070521      * - verifica se F.V. assegnabile a più filiali di "passaggio"
370000070521if  1c                   dow           *in10       =  *on
370100070521     c                             and $FVxPOa(xx) <> Wpost
370200070521     c                             and xx          <  %elem($FVxPOn)
370300070521     c                   eval      xx      =  xx + 1
370400070521     c                   eval      *in10   =  *off
370500070521     c     Wnfv7         lookup    $FVxPOn(xx)                            10
370600070521e   1c                   enddo
370700040621sel 1c                   select
370800040621      * - - ok
370900040621w   1c                   when          *in10       =  *on
371000040621     c                             and $FVxPOa(xx) =  Wpost
371100040621     c                   eval      Wdfv    =  $FVxPOd(xx)
371200060110     c                   leavesr
371300040621      * - - assegnato a P.O. diverso da quello indicato come postazione
371400070521w   1c                   when          *in10       =  *on
371500070521     c                             and $FVxPOa(xx) <> Wpost
371600070521if  2c                   if        wNFVseq =  2
371700070521     c                   eval      *in56   =  *on
371800070521x   2c                   else
371900070521     c                   eval      *in54   =  *on
372000070521e   2c                   endif
372100070521     c                   seton                                          2890
372200070521     c                   eval      V1Dmsg  =  %replace($FVxPOa(xx):
372300070521     c                                        $Msg(43):
372400070521     c                                        %scan('&PS':$Msg(43)))
372500070521     c                   leavesr
372600040621e   1c                   endsl
372700040621      *
372800040621      * - Verifica esistenza tra i f.v. NON memorizzati
372900040621     c                   move      Wnfv          FGVnfv
373000040621     c                   move      S2Cbai        FGVlnp
373100040621     c     K02FGV01      chain     FNFGV01
373200040621sel 1c                   select
373300040621w   1c                   when      NOT %found(FNFGV01L)
373400040615     c                             or  %found(FNFGV01L)
373500040615     c                             and (FGVatb <> *blanks
373600040615     c                              or  FGVfcf <> *blanks)
373700040621if  2c                   if        wNFVseq =  2
373800040615     c                   eval      *in56   =  *on
373900040621x   2c                   else
374000040615     c                   eval      *in54   =  *on
374100040621e   2c                   endif
374200040615     c                   seton                                          2890
374300040615     c                   eval      V1Dmsg  =  %replace(S2Cbai:
374400040615     c                                        $Msg(23):
374500040615     c                                        %scan('&PS':$Msg(23)))
374600060110     c                   leavesr
374700040621w   1c                   when      FGVdfv  <  SAVdfvA
374800040621if  2c                   if        wNFVseq =  2
374900040621     c                   eval      *in56   =  *on
375000040621x   2c                   else
375100040621     c                   eval      *in54   =  *on
375200040621e   2c                   endif
375300040621     c                   seton                                          2890
375400040621     c                   eval      V1Dmsg  =  $Msg(38)
375500040621e   1c                   endsl
375600060110     c                   if        *in90
375700060110     c                   leavesr
375800060110     c                   endif
375900040621     c                   eval      Wdfv    =  FGVdfv
376000040615      *
376100060110     c                   ENDSR
376200031024      *
376300031024      *---------------------------------------------------------------*
376400031024      * Impostazione campi descrittivi per l'autotrasportatore        *
376500031024      *---------------------------------------------------------------*
376600031024     c     DecS03pdr     BEGSR
376700031024      *
376800031024     c                   clear                   Wpdr
376900031024if  1c                   if        S1Cpdc >  *zeros
377000031024     c                   move      S1Cpdc        Wpdr
377100031024e   1c                   endif
377200031024      *
377300031024      * Visualizzazione "*" se più F.V. per il padroncino
377400031024     c                   clear                   S1Cast
377500031024if  1c                   if        S1Cpdc >  *zeros
377600031024     c                   exsr      CtrlAutotr3
377700031024     c                   eval      S1Cast =  Wast
377800031024e   1c                   endif
377900031024      *
378000031024      * Visualizzazione padroncino di default se variato
378100031024if  1c                   if        Wpdr   =  H1Cpdc
378200031024     c                   movel     H1Dpdc        S3Dpdc
378300031024x   1c                   else
378400040408if  2c                   if            $IMA   =  *off
378500040316     c                             and S1Cbai <> C_PostazNull
378600040423     c                             and S1Cpdc >  *zeros
378700031024     c                   move      APDpdr        S1Cpdc
378800031024     c                   movel     APDrsc        S3Dpdc
378900031024e   2c                   endif
379000031024     c                   eval      S1Kdft =  C_Default
379100031024if  2c                   if        H1Cpdc <> *zeros
379200040315     c                   eval      S1Katd =  C_Autotr
379300031024     c                   move      H1Cpdc        S1Catd
379400031024e   2c                   endif
379500031024     c                   eval      S1Datd =  H1Dpdc
379600031024e   1c                   endif
379700031024      *
379800031024     c                   ENDSR
379900031007      *
380000031007      *---------------------------------------------------------------*
380100031007      * Controllo dati immessi in videata S02 & S04                   *
380200031007      *---------------------------------------------------------------*
380300031010     c     CtrS02        BEGSR
380400061011      *
380500061011     c                   clear                   $PObau
380600061011     c                   clear                   $POpos
380700161212     c                   clear                   sk_Fil
380800161212     c                   clear                   sk_Rrn
380900090203     c                   clear                   Free_Rec_Sfl
381000031007      *
381100031007      * Ciclo di lettura del sfl
381200031007if  1c                   if        $Default = *on
381300031007     c                   readc     MS03S02
381400031007x   1c                   else
381500031007     c                   readc     MS03S04
381600031007e   1c                   endif
381700031020      *
381800031007do  1c                   DOW       NOT %eof(FIMS03D)
381900031007      *
382000031007     c                   movea     *zeros        *in(41)
382100031007     c                   z-add     S02nrr        C02rcd
382200031007      * - ctrl su singola riga
382300031014if  2c                   if            S2Cpep  <> *zeros
382400031014     c                             or  S2Cdes  <> *blanks
382500031014     c                             or  S2Cbnl  <> *blanks
382600040304     c                             or  S2Cbai  <> *blanks
382700161215     c                             or  S2Cznc  <> *blanks
382800031014     c                             or (S2Cpdc  <> *zeros
382900031014     c                             and S2Cpdc  <> *blanks)
383000031014     c                             or (S2Cnfv  <> *blanks
383100031014     c                             and S2Cnfv  <> *zeros)
383200031007     c                   exsr      CtrS02rec
383300031007if  3c                   if            $Default = *off
383400031007     c                             and *in90    = *off
383500040611     c                             and ($PO     = *off
383600040611     c                              or  $POdpd  = *on)
383700040518     c                             and S2Cbai  <> C_PostazNull
383800040309     c                             and S2Cbnl  <> 'S'
383900031007     c                   exsr      CtrS04rec
384000031007e   3c                   endif
384100031007x   2c                   else
384200040304     c                   clear                   S2Cbai
384300161213     c                   clear                   S2Cznc
384400031023     c                   clear                   S2Ddfv
384500031106     c                   clear                   S2Dfpp
384600050628     c                   clear                   S2Dmes
384700031106     c                   clear                   S4Ddfv
384800031106     c                   clear                   S4Dfpp
384900050628     c                   clear                   S4Dmes
385000031106     c                   clear                   S2Cast
385100031007     c                   clear                   S2Cpdc
385200031007     c                   clear                   S2Dpdc
385300031007     c                   clear                   S4Dpdc
385400031023     c                   clear                   S2Cdes
385500031023     c                   clear                   S2Kdft
385600040305     c                   clear                   S2Kbad
385700040305     c                   clear                   S2Cbad
385800031023     c                   clear                   S2Katd
385900031023     c                   clear                   S2Catd
386000031023     c                   clear                   S2Datd
386100090203      * - Memorizzazione primo rec. vuoto nel sfl
386200090203if  3c                   if        Free_Rec_Sfl = *zero
386300090203     c                   eval      Free_Rec_Sfl = S02nrr
386400090203e   3c                   endif
386500031007e   2c                   endif
386600031024      *
386700031024      * - reimposto indicatori x DSPATR condizionati
386800040702      *   se NON rilevati errori
386900040702if  2c                   if        NOT *in90
387000040702     c                             and $Default = *on
387100040702     c                             or  $Default = *off
387200060110     c                   exsr      AtrS02S04
387300040702e   2c                   endif
387400031007      *
387500031007      * - riattivo SFLNXTCHG (non solo se rilevato errore)
387600031007     c                   eval      *in32    = *on
387700031007      * - aggiorno il SFL
387800031007if  2c                   if        $Default = *on
387900031007     c                   update    MS03S02
388000031007x   2c                   else
388100031007     c                   update    MS03S04
388200031007e   2c                   endif
388300031007     c                   eval      *in32    = *off
388400031007      * - se rilevato errore esco dal ciclo di controllo
388500031007if  2c                   if        *in28    = *on
388600031106     c                   z-add     C02rcd        C02csr
388700090203     c                   leavesr
388800031007e   2c                   endif
388900031007      * - leggo il record successivo
389000031007if  2c                   if        $Default = *on
389100031007     c                   readc     MS03S02
389200031007x   2c                   else
389300031007     c                   readc     MS03S04
389400031007e   2c                   endif
389500031007      *
389600031007e   1c                   ENDDO
389700040312      *
389800040423      * Controlli finali "generali"
389900040312      *   (se NON ho già rilevato altri errori da segnalare)
390000161212      *
390100161212      /free
390200161212
390300161212         // -?Controllo inserimento Zona (cioè Turno) 99 - OBBLIGATORIA) -?
390400161212         //  ?per ogni Baia = Filiale?
390500161212         For  xx = 1  To  %elem(sk_Fil);
390600161212
390700161213           if  sk_Fil(xx) <= *zero;
390800161212             leave;
390900161212           endif;
391000161212
391100161212           If  sk_Rrn(xx) < *zero;
391200161212
391300161212             S02nrr = sk_Rrn(xx) * (-1);
391400161212             C02csr = S02nrr;
391500161212             if  $Video = '3';
391600161213               chain  S02nrr  MS03S02;
391700161212             else;
391800161213               chain  S02nrr  MS03S04;
391900161212             endif;
392000161212             *in58  = *on;
392100161212             *in28  = *on;
392200161212             *in90  = *on;
392300161213             V1Dmsg = %replace( sk_Fil(xx) : $Msg(58)
392400161212                              : %scan( '___' : $MSG(58) ) );
392500161212             if  %found(FIMS03D);
392600161213               *in32 = *on;
392700161212               if   $Video = '3';
392800161212                 update  MS03S02;
392900161212               else;
393000161212                 update  MS03S04;
393100161212               endif;
393200161212             endif;
393300161212             leavesr;
393400161212
393500161212           EndIf;
393600161212
393700161212         EndFor;
393800161212
393900161212      /end-free
394000090203      *
394100090203      * - Baia Non Letti obbligatoria
394200090203      *   (dovrebbe riscontrarsi solo nel piano di default)
394300090203if  1c                   if        %xfoot($BNLnr) = *zero
394400090203     c                   eval      S02nrr = Free_Rec_Sfl
394500090203     c                   eval      C02csr = S02nrr
394600090203if  2c                   if        $Video = '3'
394700090203     c     S02nrr        chain     MS03S02
394800090203x   2c                   else
394900090203     c     S02nrr        chain     MS03S04
395000090203e   2c                   endif
395100090203     c                   seton                                        572890
395200090203     c                   movel     $Msg(54)      V1Dmsg
395300090203if  2c                   if        %found(FIMS03D)
395400090203if  3c                   if        $Video = '3'
395500090203     c                   update    MS03S02
395600090203x   3c                   else
395700090203     c                   update    MS03S04
395800090203e   3c                   endif
395900090203e   2c                   endif
396000090203     c                   leavesr
396100090203e   1c                   endif
396200090203      *
396300090203      * - Controllo postazioni per baia
396400040312     c                   exsr      CtrBAIxPEP
396500031007      *
396600031007     c                   ENDSR
396700031007      *
396800031007      *---------------------------------------------------------------*
396900031007      * Controllo singolo record del subfile S02 & S04                *
397000031007      *---------------------------------------------------------------*
397100031007     c     CtrS02rec     BEGSR
397200031027      *
397300031027if  1c                   if        $Default = *off
397400031027     c                   clear                   S2Kdft
397500040304     c                   clear                   S2Kbad
397600040304     c                   clear                   S2Cbad
397700031027     c                   clear                   S2Katd
397800031027     c                   clear                   S2Catd
397900031027     c                   clear                   S2Datd
398000031027e   1c                   endif
398100040423      *
398200040423     c                   reset                   $IMA
398300040423     c                   reset                   $IMAdft
398400040607     c                   reset                   $PO
398500040611     c                   reset                   $POdpd
398600080129     c                   reset                   $POerr
398700040607     c                   reset                   $POdft
398800031007      *
398900031014      * Controllo baia di uscita
399000031007sel 1c                   select
399100031007w   1c                   when      S2Cpep = *zeros
399200031007     c                   seton                                        512890
399300031009     c                   movel     $Msg(24)      V1Dmsg
399400031007w   1c                   when      S2Cpep < 01  or  S2Cpep > 98
399500031007     c                   seton                                        512890
399600031009     c                   movel     $Msg(25)      V1Dmsg
399700031007x   1c                   other
399800031007     c                   exsr      ContaBAU
399900031007e   1c                   endsl
400000060110     c                   if        *in90
400100060110     c                   leavesr
400200060110     c                   endif
400300031007      *
400400031007      * Controllo descrizione
400500031014      * (non obbligatoria)
400600031007      *
400700031007      * Controllo flag baia non letti
400800031007if  1c                   if            S2Cbnl <> *blanks
400900031007     c                             and S2Cbnl <> 'S'
401000040304     c                   seton                                        572890
401100031009     c                   movel     $Msg(27)      V1Dmsg
401200031007x   1c                   else
401300040309if  2c                   if        S2Cbnl     =  'S'
401400031007     c                   exsr      ContaBNL
401500031007if  3c                   if        $BNLnr(xx) >  1
401600040304     c                   seton                                        572890
401700031009     c                   movel     $Msg(28)      V1Dmsg
401800031007e   3c                   endif
401900031007e   2c                   endif
402000031007e   1c                   endif
402100060110     c                   if        *in90
402200060110     c                   leavesr
402300060110     c                   endif
402400040305      *
402500040305      * Controllo codice postazione
402600040423      * - ricerca
402700040423     c     '?'           scan      S2Cbai                                 10
402800040423if  1c                   if        *in10  =  *on
402900040423     c                   eval      S2Cbai =  *blanks
403000040423     c                   eval      Wcampo =  'S2CBAI    '
403100040423     c                   exsr      srSEARCH
403200040423e   1c                   endif
403300040423      * - controllo
403400040423if  1c                   if        $Default = *off
403500040423     c                   eval      $Hidden  = *on
403600040423     c                   eval      Wpost    = H2Cbai
403700040423     c                   exsr      Ctrl_IMA
403800040423if  2c                   if        $IMAdft  = *on
403900040423     c                   eval      H2Dpdc   = $MPTdes(yy)
404000040423e   2c                   endif
404100040625     c                   exsr      Ctrl_PO
404200040625if  2c                   if        $POdft   = *on
404300040625     c                   eval      H2Dpdc   = $ORGdes(xy)
404400040625e   2c                   endif
404500040423e   1c                   endif
404600040423     c                   eval      $Hidden  = *off
404700040423     c                   eval      Wpost    = S2Cbai
404800040408     c                   exsr      Ctrl_IMA
404900040423if  1c                   if        $IMA     = *on
405000040423     c                   eval      S2Dpdc   = $MPTdes(yy)
405100040423     c                   eval      S4Dpdc   = $MPTdes(yy)
405200040423e   1c                   endif
405300040607     c                   exsr      Ctrl_PO
405400040607if  1c                   if        $PO      = *on
405500040611if  2c                   if        $POdpd   = *off
405600040611     c                   clear                   S2Cnfv
405700040611     c                   clear                   S2Dfpp
405800040611     c                   clear                   S2Ddfv
405900040611     c                   clear                   S4Cnfv
406000040611     c                   clear                   S4Dfpp
406100040611     c                   clear                   S4Ddfv
406200040611     c                   clear                   S2Cast
406300040611e   2c                   endif
406400061011     c                   clear                   S2Cpdc
406500040607     c                   eval      S2Dpdc   = $ORGdes(xy)
406600040607     c                   eval      S4Dpdc   = $ORGdes(xy)
406700040607e   1c                   endif
406800040423if  1c                   if            $Default = *off
406900040423     c                             and $IMAdft  = *on
407000040423     c                             and H2Cbai  <> S2Cbai
407100040423     c                   eval      S2Datd   = H2Dpdc
407200040423e   2c                   endif
407300080902if  1c                   if            $Default = *on
407400080902     c                             and S2Cbnl   = 'S'
407500080902     c                             and S2Cbai  <> *blanks
407600080902     c                   seton                                        532890
407700080902     c                   movel     $Msg(26)      V1Dmsg
407800080902     c                   leavesr
407900080902e   1c                   endif
408000040409if  1c                   if            $Default = *on
408100040409     c                             and S2Cbnl  <> 'S'
408200040409     c                             and S2Cbai   = *blanks
408300040409     c                   eval      S2Cbai   = C_PostazNull
408400040409e   1c                   endif
408500040305sel 1c                   select
408600040305w   1c                   when          $Default = *on
408700040305     c                             and S2Cbnl   = 'S'
408800040305     c                             and S2Cbai   = *blanks
408900040409w   1c                   when      S2Cbai   = C_PostazNull
409000040409if  2c                   if        $Default = *off
409100040305     c                   clear                   S2Cnfv
409200040305     c                   clear                   S2Ddfv
409300040305     c                   clear                   S2Dfpp
409400040305     c                   clear                   S4Cnfv
409500040305     c                   clear                   S4Ddfv
409600040305     c                   clear                   S4Dfpp
409700040305     c                   clear                   S4Dpdc
409800040409e   2c                   endif
409900161216     c                   clear                   S2Cznc
410000040409     c                   clear                   S2Cpdc
410100040305w   1c                   when          S2Cbai =  *blanks
410200040305     c                             and S2Cbnl <> 'S'
410300040305     c                   seton                                        532890
410400040305     c                   movel     $Msg(13)      V1Dmsg
410500040305w   1c                   when          S2Cbai <> *blanks
410600040305     c                             and S2Cbnl =  'S'
410700040305     c                   seton                                        532890
410800040305     c                   movel     $Msg(26)      V1Dmsg
410900040305w   1c                   when         (%subst(S2Cbai:1:1) >= 'A'
411000040305     c                             and %subst(S2Cbai:1:1) <= 'Z')
411100040305     c                             and %subst(S2Cbai:2:2) >= '01'
411200040305     c                             and %subst(S2Cbai:2:2) <= '99'
411300040305     c                   move      S2Cbai        w002a
411400040305     c     DigitN        check     w002a                                  10
411500040305if  2c                   if        *in10  =  *on
411600040305     c                   seton                                        532890
411700040305     c                   movel     $Msg(14)      V1Dmsg
411800040305x   2c                   else
411900040423     c                   eval      Wpost  =  S2Cbai
412000040305     c                   exsr      ContaBAI
412100040305if  3c                   if        $BAInr(xx) >  1
412200040305     c                   seton                                        532890
412300040305     c                   movel     $Msg(15)      V1Dmsg
412400040305e   3c                   endif
412500040305e   2c                   endif
412600040607w   1c                   when          S2Cbnl =  *blanks
412700040607     c                             and $IMA   =  *off
412800040607     c                             and $PO    =  *on
412900061010     c                             and $POdpd =  *on
413000040607if  3c                   if        $ORGnr(xy) >  1
413100040607     c                   seton                                        532890
413200040607     c                   movel     $Msg(33)      V1Dmsg
413300040607e   3c                   endif
413400040305w   1c                   when          S2Cbnl =  *blanks
413500040408     c                             and $IMA   =  *off
413600040607     c                             and $PO    =  *off
413700040305     c                             OR  S2Cbnl =  'S'
413800040305     c                             and S2Cbai <> *blanks
413900040305     c                   seton                                        532890
414000040305     c                   movel     $Msg(14)      V1Dmsg
414100040305e   1c                   endsl
414200060110     c                   if        *in90
414300060110     c                   leavesr
414400060110     c                   endif
414500040316      *
414600040316      * Controllo la postazione sia "compatibile" con le altre già
414700040316      *   immesse nella stessa baia
414800040316if  1c                   if        S2Cbai <> C_PostazNull
414900040409     c                             and
415000040409     c                             S2Cbai <> *blanks
415100040316     c                   eval      xx     =  1
415200040316     c                   eval      *in10  =  *off
415300040316     c     S2Cpep        lookup    $BAU(xx)                               10
415400040628sel 2c                   select
415500040628      * - Unica (o prima) postazione della baia in esame
415600040628w   2c                   when      NOT  *in10
415700040628      * - Postazione IMA/non-IMA o P.O./non-P.O. nella stessa baia
415800161110      *  ?Su richiesta di Nicola:?tolto controllo postaz. IMA/non-IMA
415900161110      *   (ora IMA e AUT possono stare nella stessa baia;
416000161110      *   sono le FILIALI che continuano a richiedere una loro baia)
416100161110      * - Postazione P.O./non-P.O. nella stessa baia
416200161110w   2c                   when           $BAUbai(xx) <> *blanks
416300161110     c                             and  $PosPO(xx)  <> $PO
416400040316     c                   seton                                        532890
416500161110     c                   eval      V1Dmsg =  $Msg(57)
416600060110     c                   leavesr
416700040628      * - Postazione P.O. DPD non unico nella baia
416800040628w   2c                   when           $BAUbai(xx)    <> *blanks
416900040628     c                             and  $BAUbai(xx)    <> S2Cbai
417000040628     c                             and  $PosPOdpd(xx)  <> $POdpd
417100040628     c                   seton                                        532890
417200040628     c                   eval      V1Dmsg =  $Msg(44)
417300060110     c                   leavesr
417400040607w   2c***                when           *in10       =  *on
417500040607     c***                          and  $PosIMA(xx) =  $IMA
417600061011     c***                          and  $PosPO(xx)  =  $PO
417700040316      ***  Ok. Vediamo il prossimo...
417800040628e   2c                   endsl
417900040316e   1c                   endif
418000061011      *
418100061011      * Controllo l'eventuale utilizzo dei P.O. per baia
418200061011if  1c                   if             S2Cbnl  = *blanks
418300061011     c                             and  $IMA    = *off
418400061011     c                             and  $PO     = *on
418500061011     c                             and  $POdpd  = *off
418600061011      *
418700061011     c                   eval      WX     =  1
418800061011     c     S2Cbai        lookup    $POpos(wx)                             10
418900061011do  2c                   dow       *in10  =  *on
419000061011sel 3c                   select
419100061011w   3c                   when      S2Cpep =  $PObau(wx)
419200061011     c                   seton                                        532890
419300161212     c                   eval      V1Dmsg =  %trimR($Msg(33))
419400061012     c                                    +  ' nella stessa baia'
419500061011     c                   leavesr
419600061011w   3c                   when      wx     >= %elem($POpos)
419700061011     c                   leave
419800061011x   3c                   other
419900061011     c                   add       1             wx
420000061013     c     S2Cbai        lookup    $POpos(wx)                             10
420100061011e   3c                   endsl
420200061011e   2c                   enddo
420300061011      * Controllo progressivo per postazione = P.O. (FL3)
420400061011     c                   clear                   wFL3
420500061011do  2c                   do        S02nrr        wx
420600061012if  3c                   if        S2Cbai =  $POpos(wx)
420700061011     c                   add       1             wFL3
420800061012if  4c                   if        wFL3   >= 9
420900061011     c                   seton                                        532890
421000161212     c                   eval      V1Dmsg =  %trimR($Msg(33))
421100061012     c                                    +  ' in oltre 9 baie'
421200061011     c                   leavesr
421300061011e   4c                   endif
421400061011e   3c                   endif
421500061011e   2c                   enddo
421600061011e   1c                   endif
421700161212      *
421800161215      /free
421900161215
422000161215         // -?Controllo inserimento Zona (cioè Turno) solo se Baia = Filiale?
422100161215         //  ?(99 = generica)?
422200161215         Select;
422300161215           // - -?Non richiesta ma inserita?
422400161215           When  ( S2Cbnl <> *blank  or
422500161215                   $IMA    = *on     or
422600161215                   $PO     = *off )  and
422700161215                 S2Cznc   <> *blank;
422800161215             //*·clear  S2Cznc;
422900161215             *in58  =  *on;
423000161215             *in28  =  *on;
423100161215             *in90  =  *on;
423200161215             V1Dmsg =  $Msg(59);
423300161215             leavesr;
423400161215           // - -?Richiesta ma non inserita?
423500161215           When  S2Cbnl   =  *blank  and
423600161215                 $IMA     =  *off    and
423700161215                 $PO      =  *on     and
423800161215                 S2Cznc   =  *blank;
423900161215             //*·S2Cznc = 99;
424000161215             *in58  =  *on;
424100161215             *in28  =  *on;
424200161215             *in90  =  *on;
424300170117             //*·V1Dmsg =  %replace( sk_Fil(xx) : $Msg(60)
424400170117             V1Dmsg =  %replace( S2Cbai : $Msg(60)
424500161215                               : %scan( '___' : $Msg(60) ) );
424600161215             leavesr;
424700161215         EndSl;
424800161215         // - -?Numericità (se inserita)?
424900161215         If  S2Cznc <> *blank  and
425000161215             %check( DigitN : S2Cznc ) > *zero;
425100161215           *in58  =  *on;
425200161215           *in28  =  *on;
425300161215           *in90  =  *on;
425400161215           V1Dmsg =  $Msg(61);
425500161215           leavesr;
425600161215         EndIf;
425700161215
425800161215      /end-free
425900040316      *
426000040305      * ...e ne memorizzo gli utilizzi per baia
426100040409      *   (ammesse postazioni *blank per la "baia non letti")
426200040316if  1c                   if        S2Cbai <> C_PostazNull
426300040305     c                   exsr      MemBAIxPEP
426400040305e   1c                   endif
426500040312      *
426600040305      * Visualizzo la postazione di default se variata in piano
426700040305      *   giornaliero
426800161213if  1c                   if            $Default =  *off
426900161213     c                             and (S2Cbai  <> H2Cbai
427000161213     c                              or  S2Cznc  <> H2Cznc)
427100040423     c                   eval      S2Kdft  = C_Default
427200161213if  2c                   if        S2Cbai <> H2Cbai
427300040625     c                   eval      S2Kbad  = C_Postazione
427400040423     c                   eval      S2Cbad  = H2Cbai
427500161213if  3c                   if           $IMAdft = *on
427600040625     c                             or $POdft  = *on
427700040423     c                   eval      S2Datd  = H2Dpdc
427800161213e   3c                   endif
427900161213x   2c                   else
428000161213     c                   eval      S2Kbad  = 'Zona'
428100161215     c                   eval      S2Cbad  = H2Cznc
428200161213e   2c                   endif
428300040305e   1c                   endif
428400031007      *
428500031007      * Controllo codice autotrasportatore
428600031007      *   (in S03 e S04 è campo protetto)
428700031015      * Per il piano giornaliero farò appositi controlli nella subr.
428800031015      *   CTRS04REC
428900050614     c                   clear                   S2Dmes
429000050614     c                   clear                   S4Dmes
429100031007if  1c                   if        $Default = *on
429200040607if  2c                   if            $IMA = *off
429300040607     c                             and $PO  = *off
429400031007     c                   clear                   S2Dpdc
429500031009     c                   clear                   S4Dpdc
429600040423e   2c                   endif
429700031007      * - se *blanks lo considero *zeros (sarebbe numerico)
429800031007if  2c                   if        S2Cpdc =  *blanks
429900031007     c                   eval      S2Cpdc =  *zeros
430000031007e   2c                   endif
430100031007      * - ricerca
430200031007     c     '?'           scan      S2Cpdc                                 10
430300031007if  2c                   if        *in10  =  *on
430400031007     c                   eval      S2Cpdc =  *zeros
430500031007     c                   eval      Wcampo =  'S2CPDC    '
430600031007     c                   exsr      srSEARCH
430700031007e   2c                   endif
430800031007      * - controllo
430900031007sel 2c                   select
431000040305w   2c                   when          S2Cbnl =  *blanks
431100040409     c                             and ($IMA  =  *on
431200040607     c                              or $PO    =  *on
431300040409     c                              or S2Cbai =  C_PostazNull)
431400040305     c                             and S2Cpdc =  *zeros
431500031007w   2c                   when          S2Cbnl =  'S'
431600031007     c                             and S2Cpdc <> *zeros
431700031007     c                   seton                                        552890
431800031009     c                   movel     $Msg(29)      V1Dmsg
431900040408w   2c                   when          $IMA   =  *on
432000040305     c                             and S2Cpdc <> *zeros
432100040305     c                   seton                                        552890
432200040305     c                   movel     $Msg(16)      V1Dmsg
432300040607w   2c                   when          $PO    =  *on
432400040607     c                             and S2Cpdc <> *zeros
432500040607     c                   seton                                        552890
432600040607     c                   movel     $Msg(18)      V1Dmsg
432700040305w   2c                   when          S2Cbnl =  *blanks
432800040408     c                             and $IMA   =  *off
432900040607     c                             and $PO    =  *off
433000031021     c                   movel     V1Cpog        APDpdr
433100031021     c                   move      S2Cpdc        APDpdr
433200031014     c                   exsr      CtrlAutotr2
433300031007     c                   movel     APDrsc        S2Dpdc
433400040519if  3c                   if        NOT *in90
433500040519     c                   eval      Wpdr   =  APDpdr
433600040519     c                   exsr      CtrlAutotr4
433700040519e   3c                   endif
433800031007e   2c                   endsl
433900060110     c                   if        *in90
434000060110     c                   leavesr
434100060110     c                   endif
434200050629      * - imposto il flag per autotrasportoatore Messaggeria
434300050614if  3c                   if            $Default = *on
434400050614     c                             and §OGpkm   = 'S'
434500050629     c                             and $VDL     = *on
434600050614     c                             and $IMA     = *off
434700050614     c                             and $PO      = *off
434800050614     c                             and S2Cbai  <> C_PostazNull
434900050614     c                             and S2Cbai  <> *blanks
435000050701     c                   eval      wNFVseq = 1
435100050629     c                   movel     APDflr        dAPDflr
435200050614     c                   exsr      X_Messaggeria
435300050614e   3c                   endif
435400031007      * - se *zeros lo reimposto a *blanks (numerico "editato")
435500031014if  2c                   if        S2Cpdc =  *zeros
435600031014     c                   eval      S2Cpdc =  *blanks
435700031007e   2c                   endif
435800031007e   1c                   endif
435900040305      *
436000040305      * In caso di piano giornaliero controllo se è stata tolta la
436100040305      *   postazione ("XXX"), perchè in tal caso non verrà eseguita
436200040305      *   la subr. per i controlli specifici, pertanto non verrebbe
436300040305      *   evidenziato l'autotrasportatore pre-impostato
436400061011if  1c                   if             $Default = *off
436500061011     c                             and (S2Cbai   = C_PostazNull
436600061011     c                             or   $PO      = *on)
436700040305     c                   clear                   S2Cpdc
436800061011if  2c                   if        $PO    =  *off
436900040305     c                   clear                   S4Dpdc
437000061011e   2c                   endif
437100040305     c                   exsr      DecS04pdr
437200040305e   1c                   endif
437300031007      *
437400060110     c                   ENDSR
437500031007      *
437600031007      *---------------------------------------------------------------*
437700031007      * Controlli aggiuntivi per singolo record del subfile S04       *
437800031007      *---------------------------------------------------------------*
437900031007     c     CtrS04rec     BEGSR
438000031023      *
438100031106     c                   reset                   SAVnfv
438200031106      *
438300031106      * Se impostato solo il 2° numero F.V. lo sposto al 1°
438400031106if  1c                   if            (S2Cnfv  =  *blanks
438500031106     c                             or   S2Cnfv  =  *zeros)
438600031106     c                             and (S4Cnfv  <> *blanks
438700031106     c                             and  S4Cnfv  <> *zeros)
438800031106     c                   eval      S2Cnfv  =  S4Cnfv
438900031106     c                   clear                   S4Cnfv
439000031106     c                   clear                   S4Ddfv
439100031106     c                   clear                   S4Dfpp
439200050628     c                   clear                   S4Dmes
439300031106e   1c                   endif
439400031106      *
439500031106      * Controllo 1° numero foglio viaggio
439600031007     c                   clear                   S2Ddfv
439700031027     c                   clear                   S2Dfpp
439800050628     c                   clear                   S2Dmes
439900031106     c                   eval      wNFVseq =  1
440000031106     c                   eval      Wnfv    =  S2Cnfv
440100031106     c                   exsr      Ctrl_1FV
440200031106     c                   eval      S2Cnfv  =  Wnfv
440300080129if  1c                   if            $PO     =  *on
440400080129     c                             and $POdpd  =  *on
440500080130     c                   eval      $ORGerr(xy) =  (Wnfv = *blanks   or
440600080130     c                                             Wnfv = *zeros)
440700080129e   1c                   endif
440800060110     c                   if        *in90
440900060110     c                   leavesr
441000060110     c                   endif
441100031106      * Se tutto bene: ne imposto i dati a video
441200031106if  1c                   if        Wnfv    <> *blanks
441300040621if  2c                   if        $PO     =  *off
441400031106     c                   eval      S2Dfpp  =  FVVfpp
441500031106     c                   move      DSdata6       S2Ddfv
441600040621x   2c                   else
441700040621     c***                clear                   S2Dfpp
441800040621     c                   move      DSdata6       S2Ddfv
441900040621e   2c                   endif
442000031106e   1c                   endif
442100031106      *
442200031106      * Imposto flag e decodifico variazioni relative all'autotrasp.
442300031106      *   (che deve essere comune ad entrambi i F.V.)
442400040610if  1c                   if            $IMA    =  *off
442500040610     c                             and $PO     =  *off
442600040610if  2c                   if        S2Cnfv  <> *blanks
442700031106     c                   move      APDpdr        S2Cpdc
442800031106     c                   movel     APDrsc        S4Dpdc
442900040610x   2c                   else
443000040305     c                   clear                   S2Cpdc
443100040305     c                   clear                   S4Dpdc
443200040423e   2c                   endif
443300031106e   1c                   endif
443400031106     c                   exsr      DecS04pdr
443500040615      *
443600040615      * Imposto flag relativo alla postazione = P.O. DPD con più F.V.
443700040615if  1c                   if        $POdpd   =  *on
443800040621     c*** decod. P.O.    clear                   S2Cpdc
443900040621     c*** decod. P.O.    clear                   S4Dpdc
444000040621     c                   eval      Wpost    = S2Cbai
444100040615     c                   exsr      AsterFVxPO
444200040615     c                   eval      S2Cast   = Wast
444300040615e   1c                   endif
444400031106      *
444500031106      * Controllo 2° numero foglio viaggio (se immesso)
444600031106     c                   clear                   S4Ddfv
444700031106     c                   clear                   S4Dfpp
444800050628     c                   clear                   S4Dmes
444900031106if  1c                   if            S4Cnfv <> *blanks
445000031106     c                             and S4Cnfv <> *zeros
445100031106     c                   eval      wNFVseq =  2
445200031106     c                   eval      Wnfv    =  S4Cnfv
445300031106     c                   exsr      Ctrl_1FV
445400031106     c                   eval      S4Cnfv  =  Wnfv
445500060110     c                   if        *in90
445600060110     c                   leavesr
445700060110     c                   endif
445800031106      * Se tutto bene: ne imposto i dati a video
445900031106if  2c                   if        Wnfv    <> *blanks
446000031106     c                   eval      S4Dfpp  =  FVVfpp
446100031106     c                   move      DSdata6       S4Ddfv
446200031106e   2c                   endif
446300040305      * e rieseguo il controllo sul numero di F.V. x autotrasportatore
446400040305      *   (se ne ho selezionato uno da ricerca)
446500040305if  2c                   if        S2Cast  =  '?'
446600040305     c                   clear                   S2Cast
446700040305     c                   exsr      CtrlAutotr3
446800040305     c                   eval      S2Cast =  Wast
446900040305e   2c                   endif
447000031106x   1c                   else
447100031106     c                   clear                   S4Cnfv
447200031106e   1c                   endif
447300031106      *
447400060110     c                   ENDSR
447500031024      *
447600031024      *---------------------------------------------------------------*
447700031024      * Impostazione campi descrittivi per l'autotrasportatore        *
447800031024      *---------------------------------------------------------------*
447900031024     c     DecS04pdr     BEGSR
448000031024      *
448100031024     c                   clear                   Wpdr
448200031024if  1c                   if        S2Cpdc >  *zeros
448300031024     c                   move      S2Cpdc        Wpdr
448400031024e   1c                   endif
448500031024      *
448600031024      * Visualizzazione "*" se più F.V. per il padroncino
448700031024     c                   clear                   S2Cast
448800031024if  1c                   if        S2Cpdc >  *zeros
448900031024     c                   exsr      CtrlAutotr3
449000031024     c                   eval      S2Cast =  Wast
449100031024e   1c                   endif
449200031024      *
449300031024      * Visualizzazione padroncino di default se variato
449400031024if  1c                   if        Wpdr   =  H2Cpdc
449500040625if  2c                   if            $IMA   =  *off
449600040625     c                             and $PO    =  *off
449700040625     c                             and S2Cbai <> C_PostazNull
449800031024     c                   movel     H2Dpdc        S4Dpdc
449900040423e   2c                   endif
450000031024x   1c                   else
450100040305if  2c                   if            S2Cbnl =  *blanks
450200040408     c                             and $IMA   =  *off
450300040625     c                             and $PO    =  *off
450400040316     c                             and S2Cbai <> C_PostazNull
450500040305if  3c                   if        S2Cpdc >  *zeros
450600031024     c                   move      APDpdr        S2Cpdc
450700031024     c                   movel     APDrsc        S4Dpdc
450800040305e   3c                   endif
450900031024e   2c                   endif
451000031024     c                   eval      S2Kdft =  C_Default
451100031024if  2c                   if        H2Cpdc <> *zeros
451200040315     c                   eval      S2Katd =  C_Autotr
451300031024     c                   move      H2Cpdc        S2Catd
451400031024e   2c                   endif
451500031024     c                   eval      S2Datd =  H2Dpdc
451600031024e   1c                   endif
451700031024      *
451800031024     c                   ENDSR
451900060110      *
452000060110      *---------------------------------------------------------------*
452100060110      * Impostazione settaggi attributi di visualizzazione x S01/S03  *
452200060110      *---------------------------------------------------------------*
452300060110     c     AtrS01S03     BEGSR
452400060110      *
452500060110      * Autotrasportatore Messaggeria
452600060110     c                   eval      *in04 = (§OGpkm = 'S')
452700060110      * Baia non letti
452800060110     c                   eval      *in80 = (S2Cbnl = 'S')
452900060110      * Padroncino e/o Postazione variati
453000060110     c                   clear                   Wpdr
453100060110if  1c                   if            S1Cpdc <> *blanks
453200060110     c                             and S1Cpdc <> *zeros
453300060110     c                   movel     V1Cpog        Wpdr
453400060110     c                   move      S1Cpdc        Wpdr
453500060110e   1c                   endif
453600060110     c                   eval      *in85 = (Wpdr   <> H1Cpdc   or
453700060110     c                                      S1Cbai <> H1Cbai)
453800060110      * Postazione = "IMx" o P.O.
453900060110     c                   eval      *in86 = ($IMA   =  *on      or
454000060110     c                                      $PO    =  *on)
454100060110      *
454200060110     c                   ENDSR
454300060110      *
454400060110      *---------------------------------------------------------------*
454500060110      * Impostazione settaggi attributi di visualizzazione x S02/S04  *
454600060110      *---------------------------------------------------------------*
454700060110     c     AtrS02S04     BEGSR
454800060110      *
454900060110      * Autotrasportatore Messaggeria
455000060110     c                   eval      *in04 = (§OGpkm = 'S')
455100060110      * Baia non letti
455200060110     c                   eval      *in80 = (S2Cbnl = 'S')
455300060110      * Padroncino e/o Postazione variati
455400060110     c                   clear                   Wpdr
455500060110if  1c                   if            S2Cpdc <> *blanks
455600060110     c                             and S2Cpdc <> *zeros
455700060110     c                   movel     V1Cpog        Wpdr
455800060110     c                   move      S2Cpdc        Wpdr
455900060110e   1c                   endif
456000060110     c                   eval      *in85 = (Wpdr   <> H2Cpdc   or
456100161213     c                                      S2Cbai <> H2Cbai   or
456200161213     c                                      S2Cznc <> H2Cznc)
456300060110      * Postazione = "IMx" o P.O.
456400060110     c                   eval      *in86 = ($IMA   =  *on      or
456500060110     c                                      $PO    =  *on)
456600060110      * Autotrasportatore Messaggeria
456700060110      * - 1° F.V. o Default Messaggeria
456800060110     c                   eval      *in87 = (S2Dmes <> *blanks)
456900060110      * - 2° F.V. Messaggeria
457000060110     c                   if        $Default    = *off
457100060110     c                   eval      *in88 = (S4Dmes <> *blanks)
457200060110     c                   endif
457300060110      *
457400060110     c                   ENDSR
457500031014      *
457600031014      *---------------------------------------------------------------*
457700031030      * Controllo codice autotrasportatore per singolo F.V. / baia    *
457800031014      *---------------------------------------------------------------*
457900031014     c     CtrlAutotr1   BEGSR
458000031014      *
458100031014      * Reperisco il padroncino memorizzato nel F.V. di default
458200031014     c                   clear                   DS_FV
458300031014     c                   eval      dsFVdfv    =  FVVdfv
458400031014     c                   eval      dsFVnfv    =  FVVnfv
458500031022     c                   move      DS_FV         w0150
458600031014     c                   eval      xx    = 1
458700031014     c                   eval      *in10 = *off
458800031111     c     w0150         lookup    $MEMfv1(xx)                            10
458900031111if  1c                   if        *in10 = *off
459000031111     c     w0150         lookup    $MEMfv2(xx)                            10
459100031111e   1c                   endif
459200031014if  1c                   if        *in10 = *on
459300031014     c                   eval      DSTpdr     =  $MEMat(xx)
459400031014x   1c                   else
459500031111      * - Se non reperito il F.V. tra quelli automaticamente proposti
459600031014      *   (per es. durante la variazione di un piano giornaliero)
459700031014      *   aggancio il file Fogli Distinte Consegne & memorizzo il F.V.
459800120613     c                   eval      DSTnpg     =  FVVnpg
459900120613     c                   eval      DSTnfv     =  FVVnfv
460000120613     c                   eval      DSTfgs     =  FVVfgs
460100120613     c     K03FIDST      chain     FIDST000
460200120613if  2c                   if        NOT %found(FIDST01L)
460300031022if  3c                   if        wNFVseq    =  2
460400031022     c                   movel     *on           *in56
460500031022x   3c                   else
460600031022     c                   movel     *on           *in54
460700031022e   3c                   endif
460800031022     c                   seton                                          2890
460900031014     c                   eval      V1Dmsg = $Msg(21)
461000060110     c                   leavesr
461100031014x   2c                   else
461200031111      * - - controllo se padroncino già memorizzato
461300031111      *     (se non lo è ancora  : lo memorizzo ora;
461400031111      *        se non ho più spazio: esco dal ciclo senza memorizzare)
461500031014     c                   eval      xx    = 1
461600031014     c                   eval      *in10 = *off
461700031111do  3c                   DOW       *in10 = *off
461800031111     c     DSTpdr        lookup    $MEMat(xx)                             10
461900031111if  4c                   if        *in10 = *off
462000031111     c     *zeros        lookup    $MEMat(xx)                             10
462100031111if  5c                   if        *in10 = *on
462200031111     c                   eval      $MEMat(xx)  =  DSTpdr
462300031111x   5c                   else
462400031111     c                   leave
462500031111e   5c                   endif
462600031111e   4c                   endif
462700031111      * - - per il padroncino cerco il primo elemento libero tra le
462800031111      *     schiere $MEMfv1 o $MEMfv2;
462900031111      *     se non lo trovo: torno sopra per ri-memorizzare il codice
463000031111      *       padroncino e questo nuovo f.v.
463100050701     c                   movel     FVVspn        dFVVspn
463200031111sel 4c                   select
463300031111w   4c                   when      *in10       = *on    and
463400031111     c                             $MEMfv1(xx) = *loval
463500031111     c                   eval      dsFVdfv     =  FVVdfv
463600031111     c                   eval      dsFVnfv     =  FVVnfv
463700031111     c                   move      DS_FV         w0150
463800031111     c                   eval      $MEMfv1(xx) =  w0150
463900031111     c                   eval      $MEMmp1(xx) =  FVVfpp
464000050701     c                   eval      $MEMpm1(xx) =  §FVVtpm
464100031111w   4c                   when      *in10       = *on    and
464200031111     c                             $MEMfv2(xx) = *loval
464300031111     c                   eval      dsFVdfv     =  FVVdfv
464400031111     c                   eval      dsFVnfv     =  FVVnfv
464500031111     c                   move      DS_FV         w0150
464600031111     c                   eval      $MEMfv2(xx) =  w0150
464700031111     c                   eval      $MEMmp2(xx) =  FVVfpp
464800050701     c                   eval      $MEMpm2(xx) =  §FVVtpm
464900031111x   4c                   other
465000031111if  5c                   if        xx    < %elem($MEMat)
465100031111     c                   eval      xx    = xx + 1
465200031111     c                   eval      *in10 = *off
465300031111x   5c                   else
465400031111     c                   leave
465500031111e   5c                   endif
465600031111e   4c                   endsl
465700031111e   3c                   enddo
465800031014e   2c                   endif
465900031014e   1c                   endif
466000031022      *
466100031106      * Se piano giornaliero (per P.O. con o senza VDL):
466200031022if  1c                   if        $Default   =  *off
466300040519      *?Solo per VdL (almeno per ora):?
466400040519      * Controllo che il codice autotrasportatore NON sia già impostato
466500040519      *   per altra baia/postazione
466600040519      *   (se variato - perchè, altrimenti, risulta GIA' controllato
466700040519      *   nella subr. "CtrS01rec")
466800040519if  2c                   if            $VDL    =  *on
466900040519     c                             and wNFVseq =  1
467000040519     c                   eval      Wpdr   =  DSTpdr
467100040519     c                   exsr      CtrlAutotr4
467200060110     c                   if        *in90
467300060110     c                   leavesr
467400060110     c                   endif
467500040519e   2c                   endif
467600040519      * Confronto autotrasportatori dei 2 F.V. x baia
467700031022sel 2c                   select
467800040519w   2c                   when      wNFVseq     =  1
467900040519      * - memorizzo l'autotrasportatore del 1° F.V. per confrontarlo
468000031022      *   con quello del 2° F.V. (per baia)
468100040519     c                   eval      SAVpdr      =  DSTpdr
468200040519w   2c                   when          wNFVseq =  2
468300040519     c                             and DSTpdr  <> SAVpdr
468400031022      * - l'autotrasportatore del 2° F.V. deve essere uguale a quello
468500031022      *   del 1° F.V. inserito (per baia)
468600031022     c                   seton                                        562890
468700031022     c                   eval      V1Dmsg = %replace(
468800031022     c                                      %editw(DSTpdr:'     0 '):
468900031022     c                                      $Msg(36):
469000031022     c                                      %scan('&PADR  ':$Msg(36)))
469100060110     c                   leavesr
469200031022e   2c                   endsl
469300031022e   1c                   endif
469400031014      *
469500060110     c                   ENDSR
469600031014      *
469700031014      *---------------------------------------------------------------*
469800031014      * Controllo codice autotrasportatore                            *
469900031014      *---------------------------------------------------------------*
470000031022     c     CtrlAutotr2   BEGSR
470100031014      *
470200031014     c     K02FIAPD      chain     FIAPD000
470300031014      *
470400031014if  1c                   if        NOT %found(FIAPD01L)
470500031014     c                             or  APDatb <> *blanks
470600031014     c                   clear                   APDrsc
470700031014     c                   seton                                        552890
470800031014     c                   movel     $Msg(17)      V1Dmsg
470900031014e   1c                   endif
471000031014      *
471100031014     c                   ENDSR
471200031024      *
471300031024      *---------------------------------------------------------------*
471400031024      * Evidenzia autotrasportatore con più F.V.                      *
471500031024      *---------------------------------------------------------------*
471600031024     c     CtrlAutotr3   BEGSR
471700031024      *
471800031024     c                   clear                   Wast
471900031024      * Default o giornaliero, evidenzio più F.V. per il padroncino
472000031024     c                   movel     V1Cpog        Wpdr
472100031024     c                   eval      xx     =  1
472200031024     c                   eval      *in10  =  *off
472300031024     c     Wpdr          lookup    $MEMat(xx)                             10
472400031114if  1c                   if            *in10      = *on
472500031114     c                             and $MEMxf(xx) > 1
472600031024     c                   eval      Wast   =  '*'
472700031024e   1c                   endif
472800031024      *
472900031024     c                   ENDSR
473000040519      *
473100040519      *---------------------------------------------------------------*
473200040519      * Controllo codice autotrasportatore per singola baia/postaz.   *
473300040519      *---------------------------------------------------------------*
473400040519     c     CtrlAutotr4   BEGSR
473500040519      *
473600040519     c                   clear                   ds_BaiaPost
473700040519if  1c                   if        $VDL   =  *off
473800040519     c                   movel     S1Cpep        dsBPbai
473900040519     c                   movel     S1Cbai        dsBPpos
474000040519x   1c                   else
474100040519     c                   movel     S2Cpep        dsBPbai
474200040519     c                   movel     S2Cbai        dsBPpos
474300040519e   1c                   endif
474400040519      *
474500040519     c                   eval      xx     = 1
474600040519     c                   eval      *in10  = *off
474700040519     c     Wpdr          lookup    $MEMat(xx)                             10
474800040519      *
474900040519sel 1c                   select
475000040519      * autotrasportatore non ancora memorizzato
475100040519w   1c                   when      NOT *in10
475200040519     c     *zeros        lookup    $MEMat(xx)                             10
475300040519if  2c                   if        *in10
475400040519     c                   eval      $MEMat(xx) =  Wpdr
475500040519     c                   eval      $MEMbp(xx) =  ds_BaiaPost
475600040519e   2c                   endif
475700040519      * 1° utilizzo dell'autotrasportatore: ne memorizzo baia/postaz.
475800040519w   1c                   when      $MEMbp(xx) =  *blanks
475900040519     c                   movel     ds_BaiaPost   $MEMbp(xx)
476000040519      * autotrasportatore già utilizzato in altre baie/postazioni
476100040519w   1c                   when      $MEMbp(xx) <> ds_BaiaPost
476200040519     c                   movel     $MEMbp(xx)    ds_BaiaPost
476300040519if  2c                   if        $Default   =  *on
476400040519     c                   eval      *in55  =  *on
476500040519x   2c                   else
476600040519     c                   eval      *in54  =  *on
476700040519e   2c                   endif
476800040519     c                   seton                                          2890
476900040519     c                   eval      V1Dmsg = %replace(
477000040519     c                                      %editw(Wpdr:'     0 '):
477100040519     c                                      $Msg(34):
477200040519     c                                      %scan('&PADR  ':$Msg(34)))
477300040519     c                   eval      V1Dmsg = %replace(dsBPpos:V1Dmsg:
477400040519     c                                      %scan('&PS':V1Dmsg))
477500040519     c                   eval      V1Dmsg = %replace(dsBPbai:V1Dmsg:
477600040519     c                                      %scan('&B        ':V1Dmsg))
477700060110     c                   leavesr
477800040519      * ... "beccata" baia/postazione in esame
477900040519w   1c*** ok:            when      $MEMbp(xx) =  ds_BaiaPost
478000040519e   1c                   endsl
478100040519      *
478200060110     c                   ENDSR
478300040615      *
478400040615      *---------------------------------------------------------------*
478500040615      * Evidenzia P.O. con più F.V.                                   *
478600040615      *---------------------------------------------------------------*
478700040615     c     AsterFVxPO    BEGSR
478800040615      *
478900040615     c                   clear                   Wast
479000040615      *
479100040615     c                   eval      xx     =  1
479200040615     c                   eval      *in10  =  *off
479300040615     c     Wpost         lookup    $FVxPOa(xx)                            10
479400040615      *
479500040615if  1c                   if        *in10  =  *on
479600040615      *
479700040615     c                   eval      xx     =  xx + 1
479800040615     c                   eval      *in10  =  *off
479900040615     c     Wpost         lookup    $FVxPOa(xx)                            10
480000040615      *
480100040615if  2c                   if        *in10  =  *on
480200040615     c                   eval      Wast   =  '*'
480300040615e   2c                   endif
480400040615      *
480500040615e   1c                   endif
480600040615      *
480700040615     c                   ENDSR
480800031021      *
480900031021      *---------------------------------------------------------------*
481000040408      * Memorizzo (l'ultima) postazione immessa per ogni codice baia  *
481100031021      *---------------------------------------------------------------*
481200031021     c     MemBAIxPEP    BEGSR
481300031021      *
481400031021     c                   eval      xx     = 1
481500031021     c                   eval      *in10  = *off
481600040305      *
481700040305if  1c                   if        $VDL   = *off
481800040305      *
481900040305     c     S1Cpep        lookup    $PEP(xx)                               10
482000040305      * ...lo trovo sicuro, perchè li ho già memorizzati nelle subr.
482100040305      *   "ContaPEP"
482200040305if  2c                   if            *in10       =  *on
482300040409     c                             and $PEPbai(xx) =  *blanks
482400040305     c                   eval      $PEPbai(xx) = S1Cbai
482500040305e   2c                   endif
482600040305      *
482700040305x   1c                   else
482800040305      *
482900040305     c     S2Cpep        lookup    $BAU(xx)                               10
483000040305      * ...lo trovo sicuro, perchè li ho già memorizzati nelle subr.
483100040305      *   "ContaBAU"
483200040305if  2c                   if            *in10       =  *on
483300040409     c                             and $BAUbai(xx) =  *blanks
483400040309if  3c                   if        S2Cbnl      = 'S'
483500040309     c                   eval      $BAUbai(xx) = S2Cbnl
483600040309x   3c                   else
483700040305     c                   eval      $BAUbai(xx) = S2Cbai
483800040309e   3c                   endif
483900040628     c                   eval      $PosPO(xx)      =  $PO
484000040628     c                   eval      $PosPOdpd(xx)   =  $POdpd
484100040305e   2c                   endif
484200040305      *
484300040305e   1c                   endif
484400040409      *
484500040409      * Memorizzo le postazioni "IMA" immesse per controllare la
484600040409      *  effettiva immissione di quelle obbligatorie
484700161115      *?N.B.?-?Ora le postazioni IMA possono essere nella stessa?
484800161115      *       ?baia con altre postazioni NON IMA.?
484900040409if  1c                   if            *in10       =  *on
485000161115     c                             and $IMA        =  *on
485100040409     c                   eval      xx     = 1
485200040409     c                   eval      *in10  = *off
485300040409if  2c                   if        $VDL   = *off
485400040409     c     S1Cbai        lookup    $MPT(xx)                               10
485500040409x   2c                   else
485600040409     c     S2Cbai        lookup    $MPT(xx)                               10
485700040409e   2c                   endif
485800040409      * - la trova sicuramente !
485900040409if  2c                   if        *in10  = *on
486000040409     c                   add       1             $MPTnr(xx)
486100040409e   2c                   endif
486200040409e   1c                   endif
486300061011      *
486400061011      * Aggiorno anche le postazioni utilizzate per baia
486500061011      *   (per ora utile solo al controllo dell'utilizzo dei P.O.)
486600061011if  1c                   if        $VDL   = *on
486700061011     c                   eval      $PObau(S02nrr)  =  S2Cpep
486800061011     c                   eval      $POpos(S02nrr)  =  S2Cbai
486900061011e   1c                   endif
487000161212      *
487100161212      /free
487200161212
487300161212         // -?Memorizzazione SflRrn per Postazione = Filiale per il?
487400161212         //  ?controllo della Zona (cioè del Turno)?
487500161212         If    $VDL   =  *on    and
487600161212             ( $PO    =  *on  or
487700161212               $POdpd =  *on );
487800161212
487900161212           clear  xx;
488000161212           xx = %lookup ( S2Cbai : sk_Fil );
488100161212
488200161212           if  xx = *zero;
488300161213             xx = %lookup( *blank : sk_Fil );
488400161212             if  xx > *zero;
488500161212               sk_Fil(xx) = S2Cbai;
488600161212             endif;
488700161212           endif;
488800161212
488900161212           if  xx > *zero;
489000161213             select;
489100161213               // -?Memorizzazione zona generica (99)?
489200161215               when  S2Cznc  = '99';
489300161213                 sk_Rrn(xx)  = S02nrr;
489400161213               // -?Memorizzazione zona specifica SE non già?
489500161213               //  ?memorizzata una zona generica (99)?
489600161213               when  sk_Rrn(xx) <= *zero;
489700161213                 sk_Rrn(xx)  = S02nrr * (-1);
489800161213             endsl;
489900161212           endif;
490000161212
490100161212         EndIf;
490200161212
490300161212      /end-free
490400031021      *
490500031021     c                   ENDSR
490600031021      *
490700031021      *---------------------------------------------------------------*
490800040312      * Controllo delle postazioni immesse (per baia)                 *
490900031021      *---------------------------------------------------------------*
491000031021     c     CtrBAIxPEP    BEGSR
491100031021      *
491200040408      * Controllo se immessa ALMENO una postazione "IMA"
491300040408      *                   ed ALMENO una postazione "IM0"
491400040408     c                   clear                   xx
491500040408do  1c                   dou       xx   >= %elem($MPTobbl)
491600040408     c                   eval      xx    = xx + 1
491700040409if  2c                   if            $MPTobbl(xx) = *on
491800040409     c                             and $MPTnr(xx)   = *zeros
491900040408     c                   seton                                          2890
492000040519     c                   eval      V1Dmsg  =  %replace($MPT(xx):
492100040409     c                                        $Msg(31):
492200040409     c                                        %scan('IMA':$Msg(31)))
492300060110     c                   leavesr
492400040408e   2c                   endif
492500040408e   1c                   enddo
492600040312      *
492700040309      * Controllo se immessa almeno una postazione per ogni baia
492800040409      *   (ora SOLO per NO VdL)
492900060110if  1c                   if        $VDL   =  *on
493000060110     c                   leavesr
493100060110e   1c                   endif
493200060110      *
493300031021do  1c                   DO        C_MaxRecSf    xx
493400031021      *
493500040305if  2c                   if            $VDL     =  *off
493600040305     c                             and $PEP(xx) =  *blanks
493700040305     c                             OR  $VDL     =  *on
493800040305     c                             and $BAU(xx) =  *zeros
493900031021     c                   leave
494000031021e   2c                   endif
494100031021      *
494200040305if  2c                   if            $VDL        =  *off
494300040305     c                             and $PEPbai(xx) =  *blanks
494400040305     c                             OR  $VDL        =  *on
494500040305     c                             and $BAUbai(xx) =  *blanks
494600031021     c                   seton                                          2890
494700040315if  2c                   if        $VDL   =  *off
494800040315     c                   eval      C01csr = $CsrRRN(xx)
494900040519     c                   eval      V1Dmsg = %replace($PEP(xx):
495000031021     c                                      $Msg(32):
495100031021     c                                      %scan('&PEP':$Msg(32)))
495200040315x   2c                   else
495300040315     c                   eval      C02csr = $CsrRRN(xx)
495400040315     c                   eval      V1Dmsg = %replace(
495500040315     c                                      %editw($BAU(xx):'0  ')+' ':
495600040315     c                                      $Msg(32):
495700040315     c                                      %scan('&PEP':$Msg(32)))
495800040315e   2c                   endif
495900031021     c                   leave
496000040305e   2c                   endif
496100031021      *
496200031021e   1c                   ENDDO
496300031021      *
496400060110     c                   ENDSR
496500031007      *
496600031007      *---------------------------------------------------------------*
496700040628      * Controllo immissione codice baia                              *
496800031007      *---------------------------------------------------------------*
496900031007     c     ContaPEP      BegSr
497000031007      *
497100031007     c                   eval      xx       =  1
497200031007     c                   eval      *in10    =  *off
497300031007      * Controllo se già immesso
497400031010     c     S1Cpep        lookup    $PEP(xx)                               10
497500031007      * Se NON già immesso: memorizzo
497600031007if  1c                   if        *in10    =  *off
497700031007     c     *blanks       lookup    $PEP(xx)                               10
497800031007if  2c                   if        *in10
497900031007     c                   eval      $PEP(xx) =  S1Cpep
498000031007x   2c                   else
498100031007     c                   eval      xx       =  %elem($PEP)
498200031007     c                   eval      $PEP(xx) =  *hival
498300031007e   2c                   endif
498400040315     c                   eval      $CsrRRN(xx) = S01nrr
498500031007e   1c                   endif
498600031007      *
498700031007     c                   ENDSR
498800031007      *
498900031007      *---------------------------------------------------------------*
499000031007      * Controllo immissione codice baia                              *
499100031007      *---------------------------------------------------------------*
499200031007     c     ContaBAI      BegSr
499300031007      *
499400031007     c                   eval      xx       =  1
499500031007     c                   eval      *in10    =  *off
499600031007      * Controllo se già immesso
499700040423     c     Wpost         lookup    $BAI(xx)                               10
499800031007      * Se NON già immesso: memorizzo
499900031007if  1c                   if        *in10    =  *off
500000031007     c     *blanks       lookup    $BAI(xx)                               10
500100031007if  2c                   if        *in10
500200040423     c                   eval      $BAI(xx) =  Wpost
500300040628x   2c                   else
500400040628     c                   eval      xx       =  %elem($BAI)
500500040628     c                   eval      $BAI(xx) =  *hival
500600031007e   2c                   endif
500700031007e   1c                   endif
500800031007      * Aggiorno conteggio x codice
500900031007     c                   add       1             $BAInr(xx)
501000050629      *
501100050629      *?Se picking su VdL x Messaggeria:?
501200050629      * consento l'ulizzo della stessa postazione più volte,
501300050629      * ma solo nella stessa baia
501400050629if  1c                   if             §OGpkm      =  'S'
501500050629     c                             and  $VDL        =  *on
501600050629     c                             and  $IMA        =  *off
501700050629     c                             and  $PO         =  *off
501800050629      *
501900050629if  2c                   if        $BAIbau(xx) =  *zeros
502000050629     c                   eval      $BAIbau(xx) =  S2Cpep
502100050629e   2c                   endif
502200050629      * - impostazione messaggio di avvertimento
502300050629if  2c                   if             $BAInr(xx)  >  1
502400050629     c                             and  $BAIbau(xx) =  S2Cpep
502500050629     c                   eval      $BAInr(xx)  =  1
502600050629if  3c                   if        $MsgBAIbau  =  *off
502700050629     c                   seton                                        532890
502800050629     c                   eval      V1Dmsg      =  $Msg(49)
502900050629     c                   eval      $MsgBAIbau  =  *on
503000050629     c***                leavesr
503100050629e   3c                   endif
503200050629e   2c                   endif
503300050629      *
503400050629e   1c                   endif
503500031007      *
503600031007     c                   ENDSR
503700031007      *
503800031007      *---------------------------------------------------------------*
503900031007      * Controllo immissione codice baia di uscita                    *
504000031007      *---------------------------------------------------------------*
504100031007     c     ContaBAU      BegSr
504200031007      *
504300031007     c                   eval      xx       =  1
504400031007     c                   eval      *in10    =  *off
504500031007      * Controllo se già immesso
504600031007     c     S2Cpep        lookup    $BAU(xx)                               10
504700031007      * Se NON già immesso: memorizzo
504800031007if  1c                   if        *in10    =  *off
504900031007     c     *zeros        lookup    $BAU(xx)                               10
505000031007if  2c                   if        *in10
505100031007     c                   eval      $BAU(xx) =  S2Cpep
505200040628x   2c                   else
505300040628     c                   eval      xx       =  %elem($BAU)
505400040628     c                   eval      $BAU(xx) =  *hival
505500031007e   2c                   endif
505600040315     c                   eval      $CsrRRN(xx) = S02nrr
505700031007e   1c                   endif
505800031007      *
505900031007     c                   ENDSR
506000031030      *
506100031030      *---------------------------------------------------------------*
506200031030      * Controllo immissione numero foglio viaggio                    *
506300031030      *---------------------------------------------------------------*
506400031030     c     ContaNFV      BegSr
506500031030      *
506600031030     c                   eval      xx       =  1
506700031030     c                   eval      *in10    =  *off
506800031030      * Controllo se già immesso
506900031030     c                   clear                   Wnfv7
507000031030     c                   move      Wnfv          Wnfv7
507100031030     c     Wnfv7         lookup    $NFV(xx)                               10
507200031030      * Se NON già immesso: memorizzo
507300031030if  1c                   if        *in10    =  *off
507400031030     c     *zeros        lookup    $NFV(xx)                               10
507500031030if  2c                   if        *in10
507600031030     c                   eval      $NFV(xx) =  Wnfv7
507700031030x   2c                   else
507800031030     c                   eval      xx       =  %elem($NFV)
507900031030     c                   eval      $NFV(xx) =  *hival
508000031030e   2c                   endif
508100031030      * Se NON già immesso: memorizzo postazione/baia
508200040519     c                   clear                   ds_BaiaPost
508300031030if  2c                   if        $VDL     =  *off
508400040519     c                   move      S1Cpep        dsBPbai
508500040519     c                   movel     S1Cbai        dsBPpos
508600031030x   2c                   else
508700040519     c                   movel     S2Cpep        dsBPbai
508800040519     c                   movel     S2Cbai        dsBPpos
508900031030e   2c                   endif
509000040628     c                   movel     ds_BaiaPost   $NFVbp(xx)
509100040628      * Se già immesso: ne reperisco baia/postazione precedente
509200040628x   1c                   else
509300040628     c                   movel     $NFVbp(xx)    ds_BaiaPost
509400031030e   1c                   endif
509500031030      * Aggiorno conteggio x codice
509600031030     c                   add       1             $NFVnr(xx)
509700031030      *
509800031030     c                   ENDSR
509900031007      *
510000031007      *---------------------------------------------------------------*
510100031007      * Controllo immissione flag baia non letti                      *
510200031007      *---------------------------------------------------------------*
510300031007     c     ContaBNL      BegSr
510400031007      *
510500031007     c                   eval      xx       =  1
510600031007     c                   eval      *in10    =  *off
510700031007      * Controllo se già immesso
510800031007     c     S2Cbnl        lookup    $BNL(xx)                               10
510900031007      * Se NON già immesso: memorizzo
511000031007if  1c                   if        *in10    =  *off
511100031007     c     *blanks       lookup    $BNL(xx)                               10
511200031007if  2c                   if        *in10
511300031007     c                   eval      $BNL(xx) =  S2Cbnl
511400031007x   2c                   else
511500031007     c                   eval      xx       =  %elem($BNL)
511600031007     c                   eval      $BNL(xx) =  *hival
511700031007e   2c                   endif
511800031007e   1c                   endif
511900031007      * Aggiorno conteggio x codice
512000031007     c                   add       1             $BNLnr(xx)
512100031007      *
512200031007     c                   ENDSR
512300031007      *
512400031007      *---------------------------------------------------------------*
512500031007      * Ricerche                                                      *
512600031007      *---------------------------------------------------------------*
512700031007     c     srSEARCH      BEGSR
512800031007      *
512900031007sel 1c                   select
513000040423      *
513100040423      * Postazione (tab. MPT)
513200040423w   1c                   when          Wcampo   = 'S1CBAI    '
513300040423     c                             or  Wcampo   = 'S2CBAI    '
513400040423     c                   clear                   TIBS02ds
513500040423     c                   eval      T02mod = 'R'
513600040423     c                   eval      T02cod = 'MPT'
513700040423     c                   movel     KNSIF         T02sif
513800040423     c                   call      'TIBS02R'
513900040423     c                   parm                    KPJBA
514000040423     c                   parm                    TIBS02ds
514100040423if  1c                   if        T02err = *blanks
514200040423if  2c                   if        Wcampo = 'S1CBAI    '
514300040423     c                   movel     T02ke1        S1Cbai
514400040423x   2c                   else
514500040423     c                   movel     T02ke1        S2Cbai
514600040423e   2c                   endif
514700040423     c                   movel     T02uni        dMFP
514800040423e   1c                   endif
514900031007      *
515000031007      * Numero foglio viaggio
515100031013w   1c                   when          Wcampo   = 'V1CNFV    '
515200031013     c                             or (Wcampo   = 'S1CNFV    '
515300031022     c                              or Wcampo   = 'S2CNFV    '
515400031106     c                              or Wcampo   = 'S3CNFV    '
515500031106     c                              or Wcampo   = 'S4CNFV    ')
515600031007     c                             and $Default = *off
515700040611     c                             and $PO      = *off
515800031111     c                             or  *inKG    = *on
515900031015     c                   reset                   DSLR01
516000031013sel 2c                   select
516100031013w   2c                   when      Wcampo = 'V1CNFV    '
516200031013     c                   eval      DLRnpg = V1Cnpg                              Categoria foglio
516300040408w   2c                   when         (Wcampo = 'S1CNFV    '
516400040408     c                              or Wcampo = 'S3CNFV    '
516500040408     c                              or Wcampo = 'S2CNFV    '
516600040408     c                              or Wcampo = 'S4CNFV    ')
516700040408     c                             and $IMA   = *on
516800031013     c                   eval      DLRnpg = 3                                   Categoria foglio
516900031013e   2c                   endsl
517000031111if  2c                   if        Wcampo <> 'V1CNFV    '
517100031111     c                   z-add     V1Ddfv        DLRgda                         Dal gg/mm/aaaa
517200031111     c                   z-add     SAVdfva       DLRada                         Dal aaaa/mm/gg
517300031111e   2c                   endif
517400031007     c                   z-add     V1Cpog        DLRfgs                         P.O. gestione
517500031007     c                   movel     SDSpgm        DLRpgm                         Nome programma
517600031015     c                   movel(p)  DSLR01        KPJBU
517700031111if  2c                   if        *inKG  = *off
517800031007     c                   call      'FNLR02R'
517900031007     c                   parm                    KPJBA
518000031111x   2c                   else
518100031111     c                   call      'FNLR02R'
518200031111     c                   parm                    KPJBA
518300031111     c                   parm                    $FV
518400031111e   2c                   endif
518500031015     c                   movel     KPJBU         DSLR01
518600031013sel 2c                   select
518700031013w   2c                   when      Wcampo = 'V1CNFV    '
518800031013     c                   move      DLRnfv        V1Cnfv
518900031013w   2c                   when      Wcampo = 'S1CNFV    '
519000031007     c                   move      DLRnfv        S1Cnfv
519100031106     c                   move      DLRnfv        Wnfv
519200031106w   2c                   when      Wcampo = 'S2CNFV    '
519300031106     c                   move      DLRnfv        S2Cnfv
519400031106     c                   move      DLRnfv        Wnfv
519500031022w   2c                   when      Wcampo = 'S3CNFV    '
519600031022     c                   move      DLRnfv        S3Cnfv
519700031022     c                   move      DLRnfv        Wnfv
519800031106w   2c                   when      Wcampo = 'S4CNFV    '
519900031106     c                   move      DLRnfv        S4Cnfv
520000031106     c                   move      DLRnfv        Wnfv
520100031013e   2c                   endsl
520200040611      *
520300040611      * Numero foglio viaggio x P.O. DPD
520400040630      * (Con p.o. 195 si interrogano ANCHE f.v. per P.O. 330)
520500080118      * (...Anche con fil. 196 o 197)
520600040611w   1c                   when         (Wcampo   = 'S2CNFV    '
520700040611     c                              or Wcampo   = 'S4CNFV    ')
520800040611     c                             and $Default = *off
520900040611     c                             and $POdpd   = *on
521000130506     c                   reset                   fnlst3ds
521100040611     c                   eval      DLSdti =  V1Ddfv
521200040621     c                   eval      DLSdtf =  31129999
521300130506     c                   movel(p)  fnlst3ds      KPJBU
521400040611     c                   move      S2Cbai        DLSpo
521500040630     c                   clear                   DLSpo2
521600070521if  2c                   if            S2Cbai   = '195'
521700080118     c                             or  S2Cbai   = '196'
521800070521     c                             or  S2Cbai   = '197'
521900040630     c                   eval      DLSpo2 =  330
522000040630e   2c                   endif
522100130506     c                   call      'FNLST3R'
522200040611     c                   parm                    KPJBA
522300040611     c                   parm                    DLSpo
522400040630     c                   parm                    DLSpo2
522500130506     c                   movel     KPJBU         fnlst3ds
522600040611sel 2c                   select
522700040611      * - non selezionato alcun f.v.
522800040611w   2c                   when      DLSnfv = *zeros
522900040611      * - selezionato il 1° f.v.
523000040621w   2c                   when      Wcampo = 'S2CNFV    '
523100040611     c                   move      DLSnfv        S2Cnfv
523200040611     c                   move      DLSnfv        Wnfv
523300040611      * - selezionato il 2° f.v. (sarebbe proibito!!!)
523400040611w   2c                   when      Wcampo = 'S4CNFV    '
523500040611     c                   move      DLSnfv        S4Cnfv
523600040611     c                   move      DLSnfv        Wnfv
523700040611e   2c                   endsl
523800031007      *
523900031007      * Autotrasportatore
524000031007w   1c                   when          Wcampo   = 'S1CPDC    '
524100031007     c                             or  Wcampo   = 'S2CPDC    '
524200031007     c                   reset                   FNLV24ds
524300031007     c                   z-add     V1Cpog        D24fil
524400031007     c                   movel(p)  FNLV24ds      KPJBU
524500031007     c                   call      'FNLV24R'
524600031007     c                   parm                    KPJBA
524700031007     c                   movel     KPJBU         FNLV24ds
524800031007if  2c                   if        D24pdr <> *zeros
524900031007sel 3c                   select
525000031007w   3c                   when          Wcampo   = 'S1CPDC    '
525100031007     c                             and $Default = *on
525200031007     c                   move      D24pdr        S1Cpdc
525300031007     c                   movel     D24rsc        S1Dpdc
525400031007w   3c                   when          Wcampo   = 'S2CPDC    '
525500031007     c                             and $Default = *on
525600031007     c                   move      D24pdr        S2Cpdc
525700031007     c                   movel     D24rsc        S2Dpdc
525800031007w   3c                   when          Wcampo   = 'S1CPDC    '
525900031007     c                             and $Default = *off
526000031007     c                   move      D24pdr        S1Cpdc
526100031007     c                   movel     D24rsc        S3Dpdc
526200031007w   3c                   when          Wcampo   = 'S2CPDC    '
526300031007     c                             and $Default = *off
526400031007     c                   move      D24pdr        S2Cpdc
526500031007     c                   movel     D24rsc        S4Dpdc
526600031007e   3c                   endsl
526700031007e   2c                   endif
526800031007      *
526900031007e   1c                   endsl
527000031007      *
527100031007     c                   ENDSR
527200031112      *
527300031112      *---------------------------------------------------------------*
527400031112      * Controllo esistenza di distinte aperte non inserite nel piano *
527500031112      *---------------------------------------------------------------*
527600031112     c     CtrlDistAp    BEGSR
527700031112      *
527800031112      * Memorizzo il tasto funzionale F7=Salvataggio e Stampa
527900031112     c                   movel     *inKH         WinKH
528000031112      *
528100031112      * Richiamo pgm. FNLR02R per fare il controllo
528200031112     c                   reset                   DSLR01
528300031112     c                   movel     '3'           DLRtfv
528400031112     c                   z-add     V1Ddfv        DLRgda                         Dal gg/mm/aaaa
528500031112     c                   z-add     SAVdfva       DLRada                         Dal aaaa/mm/gg
528600031112     c                   z-add     V1Cpog        DLRfgs                         P.O. gestione
528700031112     c                   movel     SDSpgm        DLRpgm                         Nome programma
528800031112     c                   movel(p)  DSLR01        KPJBU
528900031112     c                   call      'FNLR02R'
529000031112     c                   parm                    KPJBA
529100031112     c                   parm                    $FV
529200031112     c                   movel     KPJBU         DSLR01
529300031112      *
529400031112      * Emetto (eventualmente) segnalazione a video
529500031112if  1c                   if        DLRfs1 = *on
529600031112     c                   clear                   MS03W01
529700031112     c                   movel     $Msg(41)      W1Dms2
529800031112     c                   eval      W1Dtfa = 'F6=Conferma   '
529900031112     c                                    + 'F12=Ritorno'
530000031112do  2c                   dou       *inKF   or   *inKL
530100031112     c                   exfmt     MS03W01
530200031112e   2c                   enddo
530300031112if  2c                   if        *inKL
530400031112     c                   movel     *off          WinKH
530500031112e   2c                   endif
530600031112e   1c                   endif
530700031112      *
530800031112     c                   ENDSR
530900080129      *
531000080129      *---------------------------------------------------------------*
531100080129      * Controllo mancaato inserimento di F.V. per filiali DPD        *
531200080129      *---------------------------------------------------------------*
531300080129     c     CtrlFVdpd     BEGSR
531400080129      *
531500080129      * Memorizzo il tasto funzionale F7=Salvataggio e Stampa
531600080129     c                   movel     *inKH         WinKH
531700080129      *
531800080129      * Conteggio delle segnalazioni di filiali DPD senza F.V.
531900080129     c                   clear                   wx
532000080129     c                   clear                   wy
532100080129do  1c                   DOW       wx < %elem($ORGerr)
532200080129     c                   add       1             wx
532300080129if  2c                   if        $ORGerr(wx) =  *on
532400080129     c                   eval      wy = wy + 1
532500080129e   2c                   endif
532600080129e   1c                   ENDDO
532700080129if  1c                   if        wy = *zeros
532800080129     c                   leavesr
532900080129e   1c                   endif
533000080129      *
533100080129      * Avviso del totale di tali segnalazioni già effettuate
533200080129     c                   clear                   MS03W01
533300080129     c                   movel     $Msg(51)      W1Dms1
533400080129     c                   eval      W1Dms1  =  %replace(
533500080129     c                                          %editc(wy:'Z'):
533600080129     c                                          $Msg(51):
533700080129     c                                          %scan( '&FV':$Msg(51) ) )
533800080129     c                   eval      W1Dms2  = $Msg(52)
533900080129     c                   eval      W1Dms3  = $Msg(53)
534000080129     c                   eval      W1Dtfa  = 'Enter=Conferma'
534100080129     c                                     + '   '
534200080129     c                                     + 'F12=Ritorno'
534300080129do  1c                   dou           $Tasto = C_Enter
534400080129     c                             or  $Tasto = C_F12
534500080129     c                   exfmt     MS03W01
534600080129e   1c                   enddo
534700080129      *
534800080129sel 1c                   select
534900080129w   1c                   when      $Tasto  = C_F12
535000080129     c                   eval      WinKH   = *off
535100080129w   1c                   when      WinKH   = *off
535200080129     c                   eval      *inKF   = *on
535300080211x   1c                   other
535400080211     c                   eval      *inKH   = WinKH
535500080129e   1c                   endsl
535600080129      *
535700080129     c                   ENDSR
535800031007      *
535900031007      *---------------------------------------------------------------*
536000031007      * Aggiornamento dati nel file FLTR900F                          *
536100031007      *---------------------------------------------------------------*
536200031007     c     Upd_FLTR9     BEGSR
536300031007      *
536400031007      * Cancellazione dei record esistenti
536500031007     c                   exsr      Dlt_FLTR9
536600031007      *
536700031007      * Aggiunta dei record impostati a video nel sfl
536800031107     c                   reset                   SAVpep
536900031107     c                   reset                   SAVbau
537000031107     c                   reset                   SAVpdr
537100031007     c                   clear                   S01nrr
537200031007     c                   clear                   S02nrr
537300031107      *
537400031009do  1c                   DO        MAXnrr
537500031007      *
537600031007sel 2c                   select
537700031007w   2c                   when      $Video = '2'
537800031007     c                   add       1             S01nrr
537900031007     c     S01nrr        chain     MS03S01
538000031007w   2c                   when      $Video = '3'
538100031007     c                   add       1             S02nrr
538200031007     c     S02nrr        chain     MS03S02
538300031007w   2c                   when      $Video = '4'
538400031007     c                   add       1             S01nrr
538500031007     c     S01nrr        chain     MS03S03
538600031007w   2c                   when      $Video = '5'
538700031007     c                   add       1             S02nrr
538800031007     c     S02nrr        chain     MS03S04
538900031007e   2c                   endsl
539000031007      *
539100031007if  2c                   if        NOT %found(FIMS03D)
539200031007     c                   leave
539300031007e   2c                   endif
539400031007      *
539500040409if  2c                   if             $VDL     = *off
539600040409     c                             and (S1Cpep   = *blanks
539700040409     c                             and  S1Cdes   = *blanks
539800040409     c                             and  S1Cbai   = *blanks
539900040409     c                             and (S1Cnfv   = *blanks
540000040409     c                              or  S1Cnfv   = *zeros)
540100040409     c                             and (S1Cpdc   = *blanks
540200040409     c                              or  S1Cpdc   = *zeros))
540300040409     c                             OR   $VDL     = *on
540400040409     c                             and (S2Cpep   = *zeros
540500040409     c                             and  S2Cdes   = *blanks
540600040409     c                             and  S2Cbnl   = *blanks
540700040409     c                             and  S2Cbai   = *blanks
540800161215     c                             and  S2Cznc   = *blanks
540900040409     c                             and (S2Cnfv   = *blanks
541000040409     c                              or  S2Cnfv   = *zeros)
541100040409     c                             and (S2Cpdc   = *blanks
541200040409     c                              or  S2Cpdc   = *zeros))
541300031007     c                   iter
541400031007e   2c                   endif
541500031007      *
541600031007     c                   exsr      Wrt_FLTR9
541700031007      *
541800031007e   1c                   ENDDO
541900031007      *
542000031007     c                   ENDSR
542100031007      *
542200031007      *---------------------------------------------------------------*
542300031007      * Delete da FLTR900F dei dati esistenti                         *
542400031007      *---------------------------------------------------------------*
542500031007     c     Dlt_FLTR9     BEGSR
542600031007      *
542700031007     c                   eval      TR9fgs   = V1Cpog
542800040415if  1c                   if        V1Cnfv   = *zeros
542900040415     c                   clear                   TR9npga
543000040415     c                   clear                   TR9nfva
543100040415     c                   else
543200040415     c                   move      V1Cnpg        TR9npga
543300031013     c                   move      V1Cnfv        TR9nfva
543400040415e   1c                   endif
543500171103     c                   eval      TR9tps   = V1Ctps
543600171103     c     K04FLTR9      setll     FLTR9000
543700031007      *
543800171103     c                   dou       NOT  %found(FLTR902L)
543900171103     c     K04FLTR9      delete    FLTR9000
544000031007     c                   enddo
544100031007      *
544200031007     c                   ENDSR
544300031007      *
544400031007      *---------------------------------------------------------------*
544500031007      * Write  in FLTR900F del singolo record impostato nel subfile   *
544600031007      *---------------------------------------------------------------*
544700031007     c     Wrt_FLTR9     BEGSR
544800040305      *
544900060113      * Calcolo del progressivo per baia in caso di immissione
545000060113      *   postazione in default
545100060113      *   (nel piano giornaliero NON è possibile inserire altre baie o
545200060113      *   postazioni...)
545300040305sel 1c                   select
545400060111      *
545500060113w   1c                   when           $Default = *on
545600060113     c                             and  $VDL     = *off
545700060113     c                             and  S1Cpep  <> SAVpep
545800060111     c                   eval      *in10    = *off
545900060111     c                   eval      xx       = 1
546000060111     c     S1Cpep        lookup    $PEP(xx)                               10
546100060111if  2c                   if        NOT *in10
546200060111     c     *blanks       lookup    $PEP(xx)                               10
546300060111if  3c                   if        *in10
546400060113     c                   eval      $PEP(xx) =  S1Cpep
546500060111e   3c                   endif
546600060111e   2c                   endif
546700060113     c                   eval      SAVpep   = S1Cpep
546800060111      *
546900060113w   1c                   when           $Default = *on
547000060113     c                             and  $VDL     = *on
547100060113     c                             and  S2Cpep  <> SAVbau
547200060111     c                   eval      *in10    = *off
547300060111     c                   eval      xx       = 1
547400060111     c     S2Cpep        lookup    $BAU(xx)                               10
547500060111if  2c                   if        NOT *in10
547600060111     c     *zeros        lookup    $BAU(xx)                               10
547700060111if  3c                   if        *in10
547800060111     c                   eval      $BAU(xx) = S2Cpep
547900060111e   3c                   endif
548000060111e   2c                   endif
548100040305     c                   eval      SAVbau   = S2Cpep
548200060111      *
548300040305e   1c                   endsl
548400060111      *
548500040621      * incremento progressivo per baia (anche per "XXX")
548600060113sel 1c                   select
548700060113      * - Baia "XXX" in piano giornaliero
548800060113if  1c                   when           $VDL     = *off
548900040621     c                             and  $Default = *off
549000040621     c                             and  S1Cbai   = C_PostazNull
549100040621     c                             OR   $VDL     = *on
549200040621     c                             and  $Default = *off
549300040621     c                             and  S2Cbai   = C_PostazNull
549400060113     c                   add       2             SAVprg
549500040621     c                   leavesr
549600060113      * - Piano giornaliero VDL
549700060113w   1c                   when           $VDL     = *on
549800060113     c                             and  $Default = *off
549900060113     c                   eval      SAVprg   = H2Cprg
550000161110      * - Piano giornaliero NON VDL
550100060113w   1c                   when           $VDL     = *off
550200060113     c                             and  $Default = *off
550300060113     c                   eval      SAVprg   = H1Cprg
550400060113      * - Piano di default
550500060113w   1c                   when           $Default = *on
550600060113     c                   add       1             SAVprg
550700060113e   1c                   endsl
550800061011      *
550900061011      * Incremento progressivo per postazione = P.O.
551000061011     c                   clear                   wFL3
551100061011if  1c                   if        $VDL     = *on
551200061011do  2c                   do        S02nrr        wx
551300061011if  3c                   if        S2Cbai   = $POpos(wx)
551400061011     c                   add       1             wFL3
551500061011e   3c                   endif
551600061011e   2c                   enddo
551700061011      * (almeno il record in esame è già stato conteggiato
551800061011      *  SE NON piando di default)
551900061011if  2c                   if        wFL3     = *zeros
552000061011     c                   eval      wFL3     = 1
552100061011e   2c                   endif
552200061011e   1c                   endif
552300031007      *
552400031007     c                   clear                   FLTR9000
552500031007      *
552600031007     c                   eval      TR9fgs   = V1Cpog
552700040408if  1c                   if        V1Cnfv   > *zeros
552800031007     c                   eval      TR9npga  = V1Cnpg
552900031013     c                   move      V1Cnfv        TR9nfva
553000040415e   1c                   endif
553100171103     c*//                eval      TR9tps   = V1Ctps
553200040305     c                   move      SAVprg        TR9prg
553300031007      *
553400031007      * campi x NO VDL
553500031007if  1c                   if        $VDL     = *off
553600031007      *
553700031010     c                   move      S1Cpep        TR9pep
553800031007     c                   eval      TR9des   = S1Cdes
553900171102if  2c                   eval      %subst( TR9des : 28 : 1 ) = V1Ctps
554000031030     c*** (solo VDL)     eval      TR9bnl   = *blanks
554100031007     c                   eval      TR9bai   = S1Cbai
554200031013if  2c                   if            S1Cpdc <> *blanks
554300031013     c                             and S1Cpdc <> *zeros
554400031021     c                   movel     V1Cpog        TR9pdc
554500031007     c                   move      S1Cpdc        TR9pdc
554600031010e   2c                   endif
554700031007if  2c                   if        $Default = *off
554800040423     c                   eval      $Hidden  = *off
554900040423     c                   eval      Wpost    = S1Cbai
555000040408     c                   exsr      Ctrl_IMA
555100040408if  3c                   if        $IMA     = *on
555200031007     c                   eval      TR9npg   = 3
555300031007x   3c                   else
555400031007     c                   eval      TR9npg   = 4
555500031007e   3c                   endif
555600031007     c                   move      S1Cnfv        TR9nfv
555700031007     c                   clear                   WLBda8
555800031007     c                   eval      G08dat   = S1Ddfv
555900031007     c                   eval      G08err   = '0'
556000031007     c                   call      'XSRDA8'
556100031007     c                   parm                    WLBda8
556200031021     c                   eval      TR9dfv   = G08inv
556300031022     c                   eval      TR9fl1   = S1Dfpp
556400031007e   2c                   endif
556500031007      *
556600031007      * campi x SÌ VDL
556700031007x   1c                   else
556800031007      *
556900031007     c                   movel(p)  S2Cpep        TR9pep
557000031007     c                   eval      TR9des   = S2Cdes
557100171102if  2c                   eval      %subst( TR9des : 28 : 1 ) = V1Ctps
557200170214if  2c*//?NON impostato:?if        $PO      = *off         ?($PO non è ancora impostato)?
557300170214if  2c                   if        S2Cznc  <> *blank
557400161215     c                   eval      %subst( TR9des : 29 ) = S2Cznc
557500161213e   2c                   endif
557600031007     c                   eval      TR9bnl   = S2Cbnl
557700040305     c                   eval      TR9bai   = S2Cbai
557800031013if  2c                   if            S2Cpdc <> *blanks
557900031013     c                             and S2Cpdc <> *zeros
558000031021     c                   movel     V1Cpog        TR9pdc
558100031007     c                   move      S2Cpdc        TR9pdc
558200031010e   2c                   endif
558300031007if  2c                   if        $Default = *off
558400031007if  3c                   if        S2Cnfv   > *zeros
558500040423     c                   eval      $Hidden  = *off
558600040423     c                   eval      Wpost    = S2Cbai
558700040408     c                   exsr      Ctrl_IMA
558800040615     c                   exsr      Ctrl_PO
558900040615sel 4c                   select
559000161213w   4c                   when      $PO      = *on
559100040615     c                   eval      TR9npg   = 1
559200040615w   4c                   when      $IMA     = *on
559300040309     c                   eval      TR9npg   = 3
559400040615x   4c                   other
559500040309     c                   eval      TR9npg   = 4
559600040615e   4c                   endsl
559700040309e   3c                   endif
559800031007     c                   move      S2Cnfv        TR9nfv
559900031007     c                   clear                   WLBda8
560000031007     c                   eval      G08dat   = S2Ddfv
560100031007     c                   eval      G08err   = '0'
560200031007     c                   call      'XSRDA8'
560300031007     c                   parm                    WLBda8
560400031021     c                   eval      TR9dfv   = G08inv
560500031029     c                   eval      TR9fl1   = S2Dfpp
560600031007e   2c                   endif
560700061011     c                   move      wFL3          TR9fl3
560800031007      *
560900031007e   1c                   endif
561000040702      *
561100040702      * campi "comuni"
561200040702if  1c                   if        V1Cdpd   = 'S'
561300040702     c                   move      'D'           TR9fl2
561400040702e   1c                   endif
561500040702      *                  __________________
561600031007     c                   WRITE     FLTR9000
561700040702      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
561800031022      *
561900031022      * EVENTUALE 2° record per 2° F.V.
562000031022      *
562100040621if  1c                   if           $Default = *off
562200040621     c                   add       1             SAVprg
562300040621e   1c                   endif
562400040415if  1c                   if           $Default = *off
562500040415     c                             and $VDL    = *off
562600031022     c                             and S3Cnfv <> *blanks
562700031022     c                             and S3Cnfv <> *zeros
562800040415     c                             OR $Default = *off
562900040415     c                             and $VDL    = *on
563000031106     c                             and S4Cnfv <> *blanks
563100031106     c                             and S4Cnfv <> *zeros
563200031022      *
563300040621     c*** già sopra:     add       1             SAVprg
563400040305     c                   eval      TR9prg   = SAVprg
563500031106     c                   clear                   WLBda8
563600031106     c                   eval      G08err   = '0'
563700031106if  2c                   if        $VDL     = *off
563800031022     c                   move      S3Cnfv        TR9nfv
563900031022     c                   eval      G08dat   = S3Ddfv
564000031022     c                   call      'XSRDA8'
564100031022     c                   parm                    WLBda8
564200031022     c                   eval      TR9dfv   = G08inv
564300031022     c                   eval      TR9fl1   = S3Dfpp
564400031106x   2c                   else
564500031106     c                   move      S4Cnfv        TR9nfv
564600031106     c                   eval      G08dat   = S4Ddfv
564700031106     c                   call      'XSRDA8'
564800031106     c                   parm                    WLBda8
564900031106     c                   eval      TR9dfv   = G08inv
565000031106     c                   eval      TR9fl1   = S4Dfpp
565100031106e   2c                   endif
565200040702      *                  __________________
565300031022     c                   WRITE     FLTR9000
565400040702      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
565500031022e   1c                   endif
565600031007      *
565700031007     c                   ENDSR
565800031007
565900031007      *---------------------------------------------------------------*
566000031007      * SCHIERE A TEMPO DI COMPILAZIONE                               *
566100031007      *---------------------------------------------------------------*
566200151106** -- $MSG ------------------------------------------------------------------*
566300070530Immettere la filiale gestione                                                     1
566400070530Filiale gestione errata                                                           2
566500070530Filiale non abilitata al picking diretto                                          3
566600031007Categoria foglio obbligatoria                                                     4
566700031007Categoria foglio errata                                                           5
566800070530Filiale gestione non abilitata all'utilizzo della categoria foglio immessa        6
566900040312Immettere il numero Foglio Viaggio                                                7
567000031010Immettere un numero foglio arrivi esistente e non annullato                       8
567100031009             Mancano ANCHE i dati di default...                                   9
567200031211Immettere il codice baia                                                         10
567300031211Codice baia errato                                                               11
567400070530Filiale non abilitata al piano DPD                                               12
567500031211Immettere il codice postazione                                                   13
567600031211Codice postazione errato                                                         14
567700031211Codice postazione già inserito                                                   15
567800040629Impossibile specificare codice AUT per codice postazione "IMA"                   16
567900031009Codice autotrasportatore errato                                                  17
568000070530Impossibile specificare codice AUT per codice postazione "Filiale"               18
568100040315         CANCELLARE il piano giornaliero in esame ?                              19
568200031009Numero Foglio Viaggio errato                                                     20
568300040702Immettere un numero Foglio Viaggio valido                                        21
568400040408F.V. IMA diverso da quello già impostato in questo piano (&FIMA)                 22
568500070530Immettere un numero F.V. valido e con filiale arrivo &PS                         23
568600031009Immettere la baia di uscita                                                      24
568700031009Baia di uscita errata (deve essere compresa tra 01 e 98)                         25
568800040305Impossibile specificare codice postazione per "baia non letti"                   26
568900031009Flag "Baia Non Letti" errato: 'S' = sì / ' ' = no                                27
569000031009Flag "Baia Non Letti" già specificato per un'altra baia di uscita                28
569100031009Impossibile specificare codice autotrasportatore per "baia non letti"            29
569200031009Immettere il numero foglio IMA                                                   30
569300031211Immettere almeno una postazione "IMA"                                            31
569400031211Immettere almeno una postazione valida per la baia &PEP                          32
569500070530Filiale già specificata come postazione                                          33
569600040519Autotrasportatore &PADR   già impegnato in postazione &PS di baia &B             34
569700031022Immettere un 2° F.V. diverso dal 1° F.V.                                         35
5698000310222° F.V. assegnato ad autotrasportatore (&PADR  ) diverso da quello del 1° F.V.   36
569900040629Immettere SOLO un F.V. per postazione "IMA"                                      37
570000031022F.V. di data antecedente a quella del F.V. arrivi                                38
570100040312Foglio Viaggio già inserito per la postazione &PS                                39
570200171107Foglio Viaggio già inserito nel piano smistamento per                            40
570300031112ATTENZIONE: esistono distinte aperte non inserite nel piano.                     41
570400070530Immettere SOLO un F.V. per postazione "Filiale DPD"                              42
570500070530Foglio Viaggio con filiale di arrivo &PS                                         43
570600070530La postazione "Filiale DPD"  deve essere univoca per baia di uscita              44
570700060110?         Esiste un piano smistamento per picking.          |                   ?45
570800060110   Premere F6 per cancellarlo e proseguire con piano DPD,   |                    46
570900060110   altrimenti premere F12.                                  |                    47
571000070530     Mancano postazioni "Filiale DPD" nel piano di default.                      48
571100050629Esistono postazioni impostate più volte nella stessa baia - ENTER per forzare    49
571200080130Filiale DPD senza F.V. - ENTER per forzare                                       50
571300080129?           Lasciate &FV filiali DPD senza F.V.             |                   ?51
571400080130   Premere Invio per confermare; altrimenti premere F12.    |                    52
571500080130                                                            |                    53
571600090203Immettere una "Baia Non Letti" (obbligatoria)                                    54
571700151109Esiste già il Piano Smistamento Picking _         per il giorno __/__/____       55
571800151106Rilevato errore nell'estrazione dei dati del F.V. arrivi: avvisare CED           56
571900161110La stessa baia può contenere SOLO postaz. IMA/per autotrasp. o SOLO "Filiali"    57
572000161212Per la linea di defluenza ___ è obbligatoria la zona 99 in almeno una baia       58
572100161214La ZONA è inseribile solo per linee di defluenza                                 59
572200161214Immettere una Zona per la linea di defluenza ___                                 60
572300161215Zona errata                                                                      61
572400171103Il piano smistamento DPD può essere SOLO di tipo VDL                             62
