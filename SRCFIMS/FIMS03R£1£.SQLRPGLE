000100101130     /*CMD ovrdbf file(FLTR901X) tofile(FLTR901L) ovrscope(*calllvl)
000200101130     /*PRM dbgview(*source)
000300101130     /*END dltovr file(FLTR901X) lvl(*)
000400101130     /*END
000500031007      *===============================================================*
000600031009      * FIMS03R * Gestione Piano di Smistamento Picking               *
000700031007      *===============================================================*
000800040621
000900040621      *Per compilare:
 OVRDBF FILE(FLTR901X) TOFILE(FLTR901L) ?
001000031014
001100031022     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200151104     h alwnull(*inputonly)
001300031007
001400031014      *---------------------------------------------------------------*
001500031014
001600040621     fFLTR901X  if   e           k disk    rename(FLTR9000:FLTR901)
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
003000031023     fFLTR901L  Uf A e           k disk
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
007000161215     d $Msg            s             78    dim(61) ctdata  perrcd(1)
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
016100031030      * - Numeri F.V. immessi nel sfl
016200031111     d $NFV            s                   dim(C_MaxRecPf) inz
016300031111     d                                     like(TR9nfv)
016400040628      * - Baia/Postazione nel sfl x F.V.
016500040628     d $NFVbp          s                   dim(C_MaxRecPf) inz
016600040628     d                                     like(ds_BaiaPost)
016700031030      * - Numero record del sfl x F.V
016800031111     d $NFVnr          s              3  0 dim(C_MaxRecPf) inz
016900031009      * - - codici autotrasportatori
017000040519     d $MEMat          s                   dim(C_MaxRecSf) inz
017100031111     d                                     like(TR9pdc)
017200040519      * - - codici baia/postazione x autotrasportatore
017300040519     d $MEMbp          s                   dim(C_MaxRecSf) inz
017400040519     d                                     like(ds_BaiaPost)
017500031009      * - - data/numero ultimi F.V. x autotrasportatore
017600040519     d $MEMfv1         s             15  0 dim(C_MaxRecSf) inz(*loval)
017700040519     d $MEMfv2         s             15  0 dim(C_MaxRecSf) inz(*loval)
017800031022      * - - flag prestazione autotrasportatore (M-mattino P-pomeriggio)
017900040519     d $MEMmp1         s              1    dim(C_MaxRecSf) inz
018000040519     d $MEMmp2         s              1    dim(C_MaxRecSf) inz
018100050701      * - - tipo zona cons. x F.V./autotrasp. (P-Parcel M-Messaggerie)
018200050701     d $MEMpm1         s              1    dim(C_MaxRecSf) inz
018300050701     d $MEMpm2         s              1    dim(C_MaxRecSf) inz
018400031111      * - - numero F.V. x autotrasportatore
018500031111      *     per evidenziare "più F.V. per autotrasportatore"
018600040519     d $MEMxf          s              3  0 dim(C_MaxRecSf) inz
018700040630      *
018800040630      * - Schiera P.O. x F.V. da FNFGV
018900040630     d xFGVffv         s              3    dim(80)         inz
019000031007      *
019100031007      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
019200031007      *
019300031007     d KPJBA         e ds
019400080129      *
019500031007      * - Parametri x Controllo profilo utenti
019600080122     d TIBS34ds      e ds                  inz
019700031007      * - Ds di riferimento al file esterno AZUTE00F
019800080122     d AZUTEds       e ds                  extname(AZUTE00F)
019900031007      * - Ds per dati organigramma
020000080122     d dDatiUte      e ds
020100040623      * - Strutture dati "di comodo"
020200040623     d FLTR9ds       e ds                  extname(FLTR900F) inz
020300040623     d FLXR9ds       e ds                  extname(FLTR900F) inz
020400040623     d                                     prefix(XR9:3)
020500031010      * - Parametri x Caricamento £1
020600080122     d TRUL06ds      e ds                  inz
020700080122     d   Lin                   1     90  0 dim(30)
020800031010      * - Parametri x reperimento terminal di linea
020900040628     d FNLV55ds      e ds                  inz
021000040611      * - Ds descrizione 143 dell'organigramma
021100040611     d OG143         e ds                  inz
021200031007      * - Ds descrizione 150 dell'organigramma
021300031007     d OG150         e ds                  inz
021400050614      * - Ds lettura campo APDFLR di FIAPD00F
021500050614     d dAPDflr       e ds                  inz
021600050614      * - Ds lettura campo FVVSPN di FNFVV00F
021700050614     d dFVVspn       e ds                  inz
021800031015      * - Parametri standard nuovi x SRCFNLS
021900031015     d DSLR01        e ds                  inz
022000031015     d   DLRric      e                     inz('S')
022100031113     d   DLRnpg      e                     inz(4)
022200031015     d   DLRtfv      e                     inz('2')
022300031113     d   DLRgal      e                     inz(31129999)
022400031113     d   DLRaal      e                     inz(99991231)
022500031007      * - Parametri x Interrogazione tabelle
022600080122     d TIBS02ds      e ds                  inz
022700031007     d   T02mod      e                     inz('C')
022800031007     d   T02cod      e                     inz('MFP')
022900031007      * - Tab. "MFP"
023000031007     d dMFP          e ds                  inz
023100031007     d   §MFPap                1     10    dim(10)
023200040408      * - Tab. "MPT"
023300040408     d dMPT          e ds                  inz
023400031007      * - Tab. "MSL"
023500031007     d dMSL          e ds                  inz
023600040302      * - Tab. "7N"
023700040302     d ds7N          e ds                  inz
023800031007      * - Parametri x interrogazione autotrasportatori
023900031007     d FNLV24ds      e ds                  inz
024000031007     d   D24tip      e                     inz('A')
024100031007     d   D24flg      e                     inz('R')
024200031029      * - Parametri x lista Piano di Smistamento per Picking
024300031029     d FIMS04ds      e ds                  inz
024400031007      *
024500031007     d Status         sds           333
024600031007     d   SDSpgm          *proc
024700080129      *
024800080129     d InfDspF         ds           512
024900080129     d  $Tasto               369    369                                         AID byte
025000031009      *
025100031022     d DS_FV           ds            15    inz
025200031009     d   dsFVdfv                      8s 0 inz
025300031009     d   dsFVnfv                      7s 0 inz
025400040519      *
025500040519     d DS_BaiaPost     ds            13    inz
025600040519     d   dsBPbai                     -4    like(TR9pep) inz
025700040519     d   dsBPpos                           like(TR9bai) inz
025800031007      *
025900031007     d WLBda8          ds                  inz
026000031007     d   G08dat                       8  0 inz
026100031007     d   G08inv                       8  0 inz
026200031106     d   G08err                       1    inz('3')
026300031007     d   G08tgi                       5  0 inz
026400031021      *
026500031021     d DSdata8         ds             8    inz
026600031021     d   DSdt8gg                      2  0 inz
026700031021     d   DSdt8mm                      2  0 inz
026800031021     d   DSdt8aa                      4  0 inz
026900031021     d DSdata6         ds             6    inz
027000031021     d   DSdt6gg                      2  0 inz
027100031021     d   DSdt6mm                      2  0 inz
027200031021     d   DSdt6aa                      2  0 inz
027300040611      *
027400130506     d fnlst3ds        ds                  inz
027500040611     d   DLSfl1                       1    inz('R')
027600040611     d   DLSfl2                       1    inz
027700040611     d   DLSnfv                       5p 0 inz
027800040611     d   DLSdti                       9p 0 inz
027900040611     d   DLSdtf                       9p 0 inz
028000040611     d   DLStfv                       1    inz('2')
028100040611     d   DLSfv                        5    inz      dim(45)
028200040611     d   DLSfgs                       3  0 inz
028300151106
028400151106       // -?Dati estratti via SQL?
028500151106     d ds_NFVxDFV      ds                  inz  qualified
028600151106     d   TR9fgs                            inz  like(TR9fgs)
028700151106     d   TR9npga                           inz  like(TR9npgA)
028800151106     d   TR9nfva                           inz  like(TR9nfvA)
028900151109     d   TR9fl2                            inz  like(TR9fl2)
029000151106     d   FVVdfv                            inz  like(FVVdfv)
029100151106     d   wNFVxDFV                     5  0 inz
029200031007      *
029300031007      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
029400031007      *
029500031007      * - Flags
029600031007     d $Fine           s              1    inz(*off)
029700031007     d $InzD01         s              1    inz(*on)
029800031007     d $InzS01         s              1    inz(*on)
029900031007     d $InzS02         s              1    inz(*on)
030000031023     d $EoF            s              1    inz(*off)
030100031007     d $EndSFL         s              1    inz(*off)
030200031007     d $Default        s              1    inz(*off)
030300031009     d $NewFVa         s              1    inz(*off)
030400040701     d $DltPS          s              1    inz(*off)
030500031007     d $VDL            s              1    inz(*off)
030600040408     d $IMA            s              1    inz(*off)
030700040423     d $IMAdft         s              1    inz(*off)
030800040607     d $PO             s              1    inz(*off)
030900040611     d $POdpd          s              1    inz(*off)
031000080129     d $POerr          s              1    inz(*off)
031100040607     d $POdft          s              1    inz(*off)
031200040423     d $Hidden         s              1    inz(*off)
031300031023     d $Found          s              1    inz(*off)
031400050629     d $MsgBAIbau      s              1    inz(*off)
031500031112     d WinKH           s              1    inz(*off)
031600031020      *
031700031007     d $Video          s              1    inz('1')
031800031007      *
031900031007      * - Indici di schiera
032000031007     d XX              s              3  0 inz
032100031107     d YY              s              3  0 inz
032200040607     d XY              s              3  0 inz
032300040630     d WX              s              3  0 inz
032400080129     d WY              s              3  0 inz
032500031007      *
032600031007      * - Variabili riferite al data base o al display file
032700031007     d SAVpog          s                   like(V1Cpog) inz(*loval)
032800031106     d SAVpep          s                   like(S1Cpep) inz(*loval)
032900031107     d SAVbau          s                   like(S2Cpep) inz(*loval)
033000031022     d SAVnfvIMA       s                   like(S1Cnfv) inz(*zeros)
033100031022     d SAVnfv          s                   like(S1Cnfv) inz(*zeros)
033200031022     d SAVdfva         s                   like(FVVdfv) inz
033300031022     d SAVpdr          s                   like(DSTpdr) inz
033400040305     d SAVprg          s                   like(TR9prg) inz
033500031007     d S01nrr          s                   like(C01rcd) inz
033600031007     d S02nrr          s                   like(C02rcd) inz
033700031007     d MAXnrr          s                   like(C01rcd) inz
033800090203     d Free_Rec_Sfl    s                   like(C01rcd) inz
033900031007     d Wpdr            s                   like(DSTpdr) inz
034000040621     d Wdfv            s                   like(FVVdfv) inz
034100031022     d Wnfv            s                   like(S1Cnfv) inz
034200031030     d Wnfv7           s                   like(TR9nfv) inz
034300040423     d Wpost           s                   like(TR9bai) inz
034400031024     d Wast            s                   like(S1Cast) inz
034500040611     d DLSpo           s                   like(TR9fgs) inz
034600040630     d DLSpo2          s                   like(TR9fgs) inz
034700031007      *
034800031007      * - Variabili definite a programma
034900031112     d n08             s              8  0                                      *numerico 8
035000031112     d n14             s             14  0                                      *numerico 14
035100031007     d Wdate           s              8  0                                      *data corrente a/m/g
035200031022     d wNFVseq         s              1  0 inz                                  *sequenza F.V.
035300061011     d wFL3            s              2  0 inz
035400031009     d w0150           s             15  0 inz
035500031007     d Wcampo          s             10    inz
035600031023     d w002a           s              2    inz
035700040705     d w003a           s              3    inz
035800031010     d w014a           s             14    inz
035900151106     d wDate_Eur       s               d   inz   datfmt(*EUR)
036000151106
036100151106       // -?Stringhe SQL da eseguire?
036200151106     d wSQL            s          32740    inz   varying
036300151106
036400151106       //--------------------------------------------------------------
036500151106       //?Definizione prototipi procedure.                             ?
036600151106       //--------------------------------------------------------------
036700151106
036800151106       // -?Parametri API QCAPCMD (Process Commands)?
036900151106     d*// Qcmd            s           2048    inz  varying
037000151106      /copy qSysInc/qRpgleSrc,QCAPCMD
037100151106       // -?API QCAPCMD (Process Commands)?
037200151106      /copy gaitrasrc/srcProtoPR,QCAPCMD
037300151106
037400151106       // -?Parametri gestione errori API.?
037500151106      /copy qsysinc/qrpglesrc,QUSEC
037600031007
037700031007      *---------------------------------------------------------------*
037800031007      *  RIEPILOGO INDICATORI                                         *
037900031007      *---------------------------------------------------------------*
038000031010      * 01    - Visualizza "Piano di default"  come sotto-titolo      *
038100031010      * 02    - Visualizza "Piano giornaliero" come sotto-titolo      *
038200040705      * 03    - Visualizza richiesta per "Piano DPD" in D01           *
038300050615      * 04    - Visualizza dati per gestione picking MESSAGGERIA.     *
038400171106      * 05    - Visualizza richiesta per "Tipo Piano Smistamento" (PD)*
038500031007      * 10    - Comodo                                                *
038600031007      * 28    - Emette il messaggio di errore a video                 *
038700031007      * 30    - Ripulisce il subfile                                  *
038800031007      * 31    - NON emette il subfile                                 *
038900031007      * 32    - Record modificato nel subfile (sflnxtchg)             *
039000031007      * 33    - Fine dati nel subfile         (sflend)                *
039100040702      * 39    - Codice P.O. in gestione   protetto     (PR)           *
039200040702      *        ?se?livello P.O. utente = '2'                          *
039300031007      * 41    - P.O. in gestione errato                               *
039400031007      * 42    - Categoria foglio errato                               *
039500031007      * 43    - Numero F.V. errato                                    *
039600040705      * 44    - P.O. non abilitato al piano smistamento DPD           *
039700031007      * 51    - Codice postazione / baia-uscita  errato               *
039800031022      * 52    - Descrizione mancante (NON usato)                      *
039900031007      * 53    - Codice baia  errato                                   *
040000031022      * 54    - Numero foglio viaggio  errato                         *
040100031007      * 55    - Codice autotrasportatore  errato                      *
040200031022      * 56    - 2° Numero foglio viaggio  errato                      *
040300040304      * 57    - Flag "baia non letti"  errato                         *
040400161212      * 58    - Zona (cioè Turno) per Baia errata                     *
040500040623      * 80    - Tutti i campi I/O del sfl protetti     (PR)           *
040600040623      *         descr. autotrasportatore  sottolineato (UL)           *
040700040309      *         (baia "non letti" in piano giornaliero per VdL)       *
040800040623      * 85    - Codice autotrasportatore  in evidenza  (RI)           *
040900031023      *         (variato in piano giornaliero rispetto al default)    *
041000040628      *         (variata postazione in piano giornaliero rispetto al  *
041100040628      *         default)                                              *
041200040623      * 86    - Descr. autotrasportatore  in evidenza  (HI)           *
041300040628      *         (postazione IMx o P.O.)                               *
041400050629      * 87    - Flag Messaggeria attivo                (RI)           *
041500050629      *         (Autotrasportatore Messaggeria - in default;          *
041600050629      *          1°  F.V.          Messaggeria - in smistamento)      *
041700050614      * 88    - Flag Messaggerie attivo                (RI)           *
041800050629      *         (2°  F.V.          Messaggeria - in smistamento)      *
041900031007      * 90    - Generico di errore                                    *
042000031007      *---------------------------------------------------------------*
042100031007
042200031007     c     *Entry        plist
042300031007     c                   parm                    KPJBA
042400031007      *
042500031007      * Operazioni Iniziali
042600040705     c                   exsr      RoutInz
042700031007      *
042800031007      * Gestione Video
042900031007do  1c                   DOW       $Fine = *off
043000031007      * - Filtro di lancio
043100031007cas 2c     $Video        caseq     '1'           GesD01
043200031007      * - Window con messaggio
043300031007cas 2c     $Video        caseq     'W'           GesW01
043400031106      * - SubFile (Default o Giornaliero, con VDL o senza VDL)
043500031106cas 2c                   cas                     GesSFL
043600031007e   2c                   endcs
043700031007e   1c                   ENDDO
043800031007      *
043900031007      * Fine
044000040623     c                   close     FLTR901X
044100040623     c                   eval      Qcmd = 'DLTOVR  FILE(FLTR901X)'
044200040623     c                   call      'QCMDEXC'
044300040623     c                   parm                    Qcmd
044400040623     c                   parm      50            Qlen
044500040623      *
044600031010     c                   clear                   TRUL06ds
044700031010     c                   eval      D06tla = 'C'
044800031010     c                   movel(p)  TRUL06ds      KPJBU
044900031010     c                   call      'TRUL06R'
045000031010     c                   parm                    KPJBA
045100031010     c                   clear                   FNLV55ds
045200031010     c                   eval      D55tla = 'C'
045300031010     c                   call      'FNLV55R'
045400031010     c                   parm                    FNLV55ds
045500031010     c                   reset                   TIBS02ds
045600031010     c                   eval      T02tla = 'C'
045700031010     c                   call      'TIBS02R'
045800031010     c                   parm                    KPJBA
045900031010     c                   parm                    TIBS02ds
046000031007     c                   movel     *on           *inLR
046100031007      *
046200031007      *---------------------------------------------------------------*
046300031007      * Operazioni Iniziali                                           *
046400031007      *---------------------------------------------------------------*
046500040705     c     RoutInz       BEGSR
046600031007      *
046700031007      * Definizione Key-List
046800031023      * - FLTR901L
046900031007     c     K03FLTR9      klist
047000031009     c                   kfld                    TR9fgs
047100031009     c                   kfld                    TR9npgA
047200031009     c                   kfld                    TR9nfvA
047300040621     c     K05TR901      klist
047400040623     c                   kfld                    TR9fgs
047500040623     c                   kfld                    XR9npgA
047600040623     c                   kfld                    XR9nfvA
047700040623     c                   kfld                    TR9pep
047800040621     c                   kfld                    XR9prg
047900040621     c     K06TR901      klist
048000040623     c                   kfld                    TR9fgs
048100040623     c                   kfld                    XR9npgA
048200040623     c                   kfld                    XR9nfvA
048300040623     c                   kfld                    TR9pep
048400040621     c                   kfld                    XR9prg
048500040621     c                   kfld                    TR9bai
048600031007      * - FIAPD01L
048700031007     c     K02FIAPD      klist
048800031007     c                   kfld                    APDtip
048900031007     c                   kfld                    APDpdr
049000031007     c                   eval      APDtip = 'A'
049100031007      * - FNFVV03L
049200031007     c     K03FNFVV      klist
049300031007     c                   kfld                    FVVfgs
049400031007     c                   kfld                    FVVnpg
049500031007     c                   kfld                    FVVnfv
049600031007     c     K02FNFVV      klist
049700031007     c                   kfld                    FVVfgs
049800031007     c                   kfld                    FVVnpg
049900031009      * - FNFVV08L
050000031009     c     K03FVV08      klist
050100031010     c                   kfld                    FV8fgs
050200031010     c                   kfld                    FV8npg
050300031010     c                   kfld                    FV8dfv
050400031010     c     K02FVV08      klist
050500031010     c                   kfld                    FV8fgs
050600031010     c                   kfld                    FV8npg
050700120613      * - FIDST01L
050800120613     c     K03FIDST      klist
050900120613     c                   kfld                    DSTnpg
051000120613     c                   kfld                    DSTnfv
051100120613     c                   kfld                    DSTfgs
051200040621      * - FNFGV02L
051300040621     c     K03FGV02      klist
051400040621     c                   kfld                    FGVlnp
051500040621     c                   kfld                    FGVdfv
051600040621     c                   kfld                    FGVnfv
051700040621      * - FNFGV01L
051800040621     c     K02FGV01      klist
051900040621     c                   kfld                    FGVnfv
052000040621     c                   kfld                    FGVlnp
052100031007      * - TNTBE01L
052200031007     c     K02TNTBE      klist
052300031007     c                   kfld                    TBEcod
052400031007     c                   kfld                    TBEke1
052500040302      * - TABEL00F
052600040227     c     KTAB          klist
052700040227     c                   kfld                    TBLkut
052800040227     c                   kfld                    TBLcod
052900040227     c                   kfld                    TBLkey
053000040227     c                   eval      TBLkut = 1
053100031007      *
053200031007      * Reperisco dati job
053300031007     c                   exsr      DatiJob
053400031008     c                   movel     SDSpgm        V1Tpgm
053500031007      *
053600031007      * Reperisco la data corrente
053700031007     c                   time                    n14                            *Ora (6)+ Data (8)
053800031007     c                   move      n14           n08                            *Data (8) in g/m/a
053900031106     c                   clear                   WLBda8
054000031007     c                   z-add     n08           G08dat
054100031106     c                   eval      G08err = '0'
054200031007     c                   call      'XSRDA8'
054300031007     c                   parm                    WLBda8
054400031007     c                   z-add     G08inv        Wdate                          *Data corrente a/m/g
054500031010      *
054600031010      * Carico schiera P.O. gestiti (£1)
054700031010     c                   clear                   TRUL06ds
054800031010     c                   movel     '£1'          D06cod
054900040701     c                   movel     DUTpou        D06key
055000031010     c                   movel(p)  TRUL06ds      KPJBU
055100031010     c                   call      'TRUL06R'
055200031010     c                   parm                    KPJBA
055300031010     c                   movel     KPJBU         TRUL06ds
055400040701     c                   movea     Lin           £1
055500040607      *
055600040607      * Carico schiera di tutti i P.O.
055700040607     c                   clear                   xx
055800040607     c                   clear                   $ORGfil
055900040607     c                   clear                   $ORGdes
056000040607     c*** qui non serve: clear                   $ORGnr
056100080129     c*** qui non serve: reset                   $ORGerr
056200040607     c     *loval        setll     AZORG
056300040607     c                   read      AZORG
056400040607do  1c                   dow       NOT %eof(AZORG01L)
056500040607if  2c                   if        ORGfva = *blanks
056600040607     c                   add       1             xx
056700040607     c                   move      ORGfil        $ORGfil(xx)
056800040607     c                   movel     ORGdes        $ORGdes(xx)
056900040611     c                   movel     ORGde3        Og143
057000040611     c                   movel     §OGntw        $ORGntw(xx)
057100040607e   2c                   endif
057200040607     c                   read      AZORG
057300040607e   1c                   enddo
057400040623      *
057500040623      * Override
057600040623     c                   eval      Qcmd = 'OVRDBF  FILE(FLTR901X)'
057700040623     c                                  +      ' TOFILE(FLTR901L)'
057800040623     c                   call      'QCMDEXC'
057900040623     c                   parm                    Qcmd             50
058000040623     c                   parm      50            Qlen             15 5
058100040623     c                   open      FLTR901X
058200031007      *
058300031007     c                   ENDSR
058400031007      *
058500031007      *---------------------------------------------------------------*
058600031007      * Reperimento Dati del job (Utente/Operativi)                   *
058700031007      *---------------------------------------------------------------*
058800031007     c     DatiJob       BEGSR
058900031007      *
059000031007     c     *dtaara       define    §azute        azuteds
059100031007     c     *dtaara       define    §datiute      ddatiute
059200031007      *
059300031007     c                   in(E)     *dtaara
059400031007     c                   IF        %ERROR or RSUT = *blanks
059500031007     c                   clear                   Tibs34Ds
059600031007     c                   call      'TIBS34R'
059700031007     c                   parm                    Tibs34Ds
059800031007     c                   in        *dtaara
059900031007     c                   ENDIF
060000031007      *
060100031007     c                   ENDSR
060200031007      *
060300031007      *---------------------------------------------------------------*
060400031007      * Gestione videata D01                                          *
060500031007      *---------------------------------------------------------------*
060600031007     c     GesD01        BEGSR
060700031007      *
060800031007      * Inizializzo la videata
060900031007if  1c                   if        $InzD01  = *on
061000031007     c                   exsr      InzD01
061100031007     c                   movel     *off          $InzD01
061200031007e   1c                   endif
061300031014      * Reimposto il numero F.V. a *blank se *zero (numerico editato)
061400031014if  2c                   if        V1Cnfv =  *zeros
061500031014     c                   eval      V1Cnfv =  *blanks
061600031014e   2c                   endif
061700031007      *
061800031007      * Scrivo la testata e la riga tasti funzionali abilitati
061900170117if  1c*//                if        NOT *in28
062000031007     c                   write     MS03T01
062100031007     c                   write     MS03Z01
062200170117e   1c*//                endif
062300031007      * Emetto la videata
062400031007     c                   exfmt     MS03D01
062500031007     c                   setoff                                       28  90
062600031007     c                   clear                   V1Dmsg
062700031007      *
062800031007sel 1c                   select
062900031007      * F3=Fine
063000031007w   1c                   when      *inKC
063100031007     c                   exsr      F03D01
063200060110     c                   leavesr
063300031007      *
063400031007e   1c                   endsl
063500031007      *
063600031007      * Controllo dati immessi a video
063700031007     c                   exsr      CtrD01
063800060110     c                   if        *in90
063900060110     c                   leavesr
064000060110     c                   endif
064100031009      *
064200031009      * Inizializzazione schiere, flag, variabili, ecc.
064300031009     c                   exsr      InzALL_1
064400031106     c                   eval      $Default = (V1Cnfv = *zeros)
064500040705      *
064600040705      * Testo eventuali sostituzioni tra piano std e piano dpd
064700040705if  1c                   if            $Default = *off
064800040705     c                             and $NewFVa  = *off
064900040705     c                   exsr      Check_PS
065000040705if  2c                   if        *inKC  or  *inKL
065100060110     c                   leavesr
065200040705e   2c                   endif
065300040705e   1c                   endif
065400031009      *
065500031009      * Caricamento dati da tab. MSL
065600031009     c                   exsr      Tab_MSL
065700031007      *
065800031007      * Controllo se P.O. in gestione ha VDL
065900031007     c                   exsr      Ctrl_VDL
066000031009      *
066100031110      * Se piano giornaliero:
066200031106      * Intabellamento dati di default   &
066300031106      * Memorizzazione dati per il reperimento dell'ultimo foglio
066400031106      *   consegne per autotrasportatore
066500040309if  1c                   if        $Default = *off
066600031020     c                   exsr      Memo_DFT
066700031009e   1c                   endif
066800031010      *
066900031106      * Passaggio alla videata successiva
067000031010if  1c                   if        $Default = *on
067100031010     c                   movea     '10'          *in(01)
067200031010x   1c                   else
067300031010     c                   movea     '01'          *in(01)
067400031010e   1c                   endif
067500031007      *
067600031007if  1c                   if        $VDL     = *off
067700031007      *
067800031007     c                   eval      $InzS01  = *on
067900031007if  2c                   if        $Default = *on
068000031007     c                   eval      $Video   = '2'
068100031007x   2c                   else
068200031007     c                   eval      $Video   = '4'
068300031007e   2c                   endif
068400031007      *
068500031007x   1c                   else
068600031007      *
068700031007     c                   eval      $InzS02  = *on
068800031007if  2c                   if        $Default = *on
068900031007     c                   eval      $Video   = '3'
069000031007x   2c                   else
069100031007     c                   eval      $Video   = '5'
069200031007e   2c                   endif
069300031007      *
069400031007e   1c                   endif
069500031007      *
069600060110     c                   ENDSR
069700031007      *
069800031007      *---------------------------------------------------------------*
069900031007      * Inizializzazione videata D01                                  *
070000031007      *---------------------------------------------------------------*
070100031007     c     InzD01        BEGSR
070200031007      *
070300031007      * Imposto i dati di default
070400031010if  1c                   if        V1Cpog = *zeros
070500031007     c                   eval      V1Cpog = DUTpou
070600031007     c                   eval      V1Cnpg = 2
070700031010e   1c                   endif
070800031010     c                   clear                   V1Cnfv
070900031014     c                   clear                   V1Dnfv
071000031010     c                   clear                   V1Ddfv
071100031007      *
071200031007     c                   clear                   V1Dmsg
071300031007     c                   setoff                                       28  90
071400031007     c                   movea     *zeros        *in(41)
071500031010      *
071600031010      * Posiziono il cursore sul campo "numero F.V."
071700031010     c                   eval      *in43  = *on
071800040702      *
071900040702      * Proteggo il campo "P.O. in gestione" se P.O. utente di 2° livello
072000040705     c                   eval      *in39  = (DUTlpo   = '2')
072100040705      *
072200040705      * Controllo se P.O. abilitato a piano DPD
072300040705     c                   eval      *in03  = *off
072400040705     c                   eval      xx     = *zeros
072500040705do  1c                   dow       xx     < %elem(£1)
072600040705     c                   eval      xx     = xx + 1
072700040705if  2c                   if        £1(xx) = *zeros
072800040705     c                   leave
072900040705e   2c                   endif
073000040705     c                   eval      yy     = 1
073100040705     c                   eval      *in10  = *off
073200040705     c                   move      £1(xx)        w003a
073300040705     c     w003a         lookup    $ORGfil(yy)                            10
073400040705if  2c                   if            *in10       =  *on
073500040705     c                             and $ORGntw(yy) =  'DPD'
073600040705     c                   eval      *in03  = *on
073700040705     c                   leave
073800040705e   2c                   endif
073900040705e   1c                   enddo
074000070530if  1c                   if            V1Cpog      = 049
074100070530     c                             and *in03       = *on
074200070530     c                   eval      V1Cdpd = 'S'
074300070530e   1c                   endif
074400171106      *
074500171106      * - ?Controllo se Filiale Gestione = 006-PD?
074600171106      *   ?(per abilitazione richiesta "Tipo Piano Smistamento")?
074700171106     c*//*               eval      *in05  = (V1Cpog   = c_PO_a)
074800171106     c                   eval      *in05  = *off
074900031007      *
075000031007     c                   ENDSR
075100031007      *
075200031007      *---------------------------------------------------------------*
075300031007      * Gestione tasto funzionale F03 da videata D01                  *
075400031007      *---------------------------------------------------------------*
075500031007     c     F03D01        BEGSR
075600031007      *
075700031007      * Chiudo il programma
075800031007     c                   eval      $Fine = *on
075900031007      *
076000031007     c                   ENDSR
076100031007      *
076200031007      *---------------------------------------------------------------*
076300031007      * Controllo dati immessi in videata D01                         *
076400031007      *---------------------------------------------------------------*
076500031007     c     CtrD01        BEGSR
076600031007      *
076700031113     c                   reset                   $NewFVa
076800040702     c                   reset                   $DltPS
076900031007     c                   movea     *zeros        *in(41)
077000151109      * -?Spegnimento indicatore di errore SQL?
077100151109     c                   eval      *in34 = *off
077200031007      *
077300031007      * Controllo P.O. in gestione
077400171106     c                   eval      *in05   = *off
077500031007if  1c                   if        V1Cpog <> SAVpog
077600031007     c                   exsr      DecodPOG
077700031009if  2c                   if        *in90   = *off
077800031009     c                   eval      SAVpog  = V1Cpog
077900031007x   2c                   else
078000031007     c                   reset                   SAVpog
078100031007e   2c                   endif
078200031007e   1c                   endif
078300060110     c                   if        *in90
078400060110     c                   leavesr
078500060110     c                   endif
078600031007      *
078700031007      * Controllo categoria (tipo)
078800031007sel 1c                   select
078900031007w   1c                   when          V1Cnpg =  *zeros
079000031013     c                             and V1Cnfv <> *zeros
079100031013     c                             and V1Cnfv <> *blanks
079200031007     c                   seton                                        422890
079300031007     c                   movel     $Msg(4)       V1Dmsg
079400031007w   1c                   when          V1Cnpg <> *zeros
079500031007     c                             and V1Cnpg <> 2
079600031007     c                   seton                                        422890
079700031007     c                   movel     $Msg(5)       V1Dmsg
079800031007e   1c                   endsl
079900060110     c                   if        *in90
080000060110     c                   leavesr
080100060110     c                   endif
080200040302      *
080300040302      * Controllo categoria da TABEL (se NON default)
080400040302if  1c                   if            V1Cnpg <> *zeros
080500040302     c                             or (V1Cnfv <> *zeros
080600040302     c                             and V1Cnfv <> *blanks)
080700040302      *
080800040227     c                   movel     '7N'          TBLcod
080900040312     c                   movel(p)  V1Cnpg        TBLkey
081000040227     c     KTAB          chain     TABEL
081100040227      * - inesistente
081200040227if  2c                   if        NOT %found(TABEL00F)
081300040227     c                             or  TBLflg <> *blanks
081400040227     c                   seton                                        422890
081500040227     c                   movel     $Msg(5)       V1Dmsg
081600060110     c                   leavesr
081700040227e   2c                   endif
081800040227     c                   movel     TBLuni        ds7N
081900040227      * - non utilizzabile dal P.O. in gestione
082000040227if  2c                   if        §7Nuti = '9'
082100040227     c                   seton                                        422890
082200040227     c                   movel     $Msg(5)       V1Dmsg
082300060110     c                   leavesr
082400040227e   2c                   endif
082500040227      * - utilizzabile solo da P.O. di diverso livello
082600040227if  2c                   if           (§7Nuti =  '1'
082700040227     c                             and V1Cpog <> D55tfp)
082800040227     c                             OR (§7Nuti =  '2'
082900040227     c                             and V1Cpog =  D55tfp)
083000040227     c                   seton                                        422890
083100040227     c                   movel     $Msg(6)       V1Dmsg
083200060110     c                   leavesr
083300040227e   2c                   endif
083400040302      *
083500040302e   1c                   endif
083600040302      *
083700031007      * Controllo numero foglio viaggio
083800031013      * - se *blanks lo considero *zeros (sarebbe numerico)
083900031013if  1c                   if        V1Cnfv =  *blanks
084000031013     c                   eval      V1Cnfv =  *zeros
084100031013e   1c                   endif
084200031013      * - ricerca
084300031013     c     '?'           scan      V1Cnfv                                 10
084400031013if  1c                   if        *in10  =  *on
084500040415      * - - per interrogare il foglio è obbligatorio inserirne il tipo
084600040415      *     => ora fisso a "2"
084700040415if  2c                   if        V1Cnpg =  *zeros
084800040415     c                   seton                                        422890
084900040415     c                   movel     $Msg(5)       V1Dmsg
085000060110     c                   leavesr
085100040415e   2c                   endif
085200031013     c                   eval      V1Cnfv =  *zeros
085300031013     c                   eval      Wcampo =  'V1CNFV    '
085400031013     c                   exsr      srSEARCH
085500040705     c                   eval      *in90  =  *on
085600031013e   1c                   endif
085700031013      * - controlli:
085800031013      * - - numericità
085900031013     c     DigitN        check     V1Cnfv                                 10
086000031013if  1c                   if        *in10  =  *on
086100031013     c                   seton                                        542890
086200031013     c                   movel     $Msg(20)      V1Dmsg
086300060110     c                   leavesr
086400031013e   1c                   endif
086500040415      * - - effettiva immissione
086600040415if  1c                   if            V1Cnpg <> *zeros
086700040415     c                             and V1Cnfv =  *zeros
086800040415     c                   seton                                        432890
086900040415     c                   movel     $Msg(7)       V1Dmsg
087000060110     c                   leavesr
087100040415e   1c                   endif
087200031009      *
087300040701     c*** (già fatto)    reset                   $NewFVa
087400031009if  1c                   IF        V1Cnfv <> *zeros
087500031013      * - - inserimento in FLTR9
087600031009     c                   eval      TR9fgs  = V1Cpog
087700031009     c                   eval      TR9npga = V1Cnpg
087800031013     c                   move      V1Cnfv        TR9nfva
087900031009     c     K03FLTR9      setll     FLTR9000
088000031023if  2c                   if        NOT %equal(FLTR901L)
088100031009     c                   eval      $NewFVa = *on
088200031013      * - - esistenza in FNFVV
088300031009     c                   eval      FVVfgs  = V1Cpog
088400031009     c                   eval      FVVnpg  = V1Cnpg
088500031013     c                   move      V1Cnfv        FVVnfv
088600031009     c     K03FNFVV      chain     FNFVV000
088700031009if  3c                   if        NOT %found(FNFVV03L)
088800031009     c*** (già da vl)              or  FVVatb <> *blanks
088900031009     c*** (già da vl)              or  FVVfcf <> *blanks
089000151106     c                   seton                                        432890
089100031009     c                   movel     $Msg(8)       V1Dmsg
089200060110     c                   leavesr
089300031010x   3c                   else
089400031106     c                   reset                   WLBda8
089500040705     c                   eval      G08inv  = FVVdfv
089600031010     c                   call      'XSRDA8'
089700031010     c                   parm                    WLBda8
089800040705     c                   eval      V1Ddfv  = G08dat
089900040705     c                   eval      SAVdfva = FVVdfv
090000031009e   3c                   endif
090100031013x   2c                   else
090200031013      * - - già esistente in FLTR9 (=> reperisco la data del F.V.)
090300040705     c                   eval      FVVfgs  = V1Cpog
090400040705     c                   eval      FVVnpg  = V1Cnpg
090500031013     c                   move      V1Cnfv        FVVnfv
090600031013     c     K03FNFVV      chain     FNFVV000
090700031013if  3c                   if            %found(FNFVV03L)
090800031013     c*** (già da vl)              and FVVatb <> *blanks
090900031013     c*** (già da vl)              and FVVfcf <> *blanks
091000031106     c                   reset                   WLBda8
091100040705     c                   eval      G08inv  = FVVdfv
091200031013     c                   call      'XSRDA8'
091300031013     c                   parm                    WLBda8
091400040705     c                   eval      V1Ddfv  = G08dat
091500040705     c                   eval      SAVdfva = FVVdfv
091600031013e   3c                   endif
091700031009e   2c                   endif
091800151106      /free
091900151106
092000151106           // -?Verifica esistenza altri F.V. per la stessa data?
092100151109           //  ?(NON se Piano di Default)?
092200151109           //If  $NewFVA = *on;
092300151109           If  V1Cnpg <> *zero;
092400151106
092500151106             exsr  sr_OpenCursor;
092600151106
092700151106             If  Not *in34;
092800151106
092900151106               exsr  sr_ReadCursor;
093000151106               exsr  sr_CloseCursor;
093100151106
093200151109               //if  ds_NFVxDFV.TR9nfva > *zeros;
093300151109               if  ds_NFVxDFV.TR9nfva > *zero
093400151109                   and        $NewFVA = *on;
093500151106
093600151109                 wDate_Eur = %date( SavDFVa : *ISO );
093700151106                 *in43  = *on;
093800151106                 *in28  = *on;
093900151106                 *in90  = *on;
094000151106                 V1Dmsg = $Msg(55);
094100151109                 V1Dmsg = %replace( %trim( %editc(
094200151109                                    ds_NFVxDFV.TR9nfva : '2' ) )
094300151106                                  : V1Dmsg
094400151109                                  : %scan( '_      ' : V1Dmsg ) );
094500151109                 V1Dmsg = %replace( %editc( %dec( wDate_Eur ) : 'Y' )
094600151106                                  : V1Dmsg
094700151106                                  : %scan( '__/__/____' : V1Dmsg ) );
094800151106                 leavesr;
094900151106
095000151106               endif;
095100151106
095200151106             Else;
095300151106
095400151106               *in28  = *on;
095500151106               *in90  = *on;
095600151106               V1Dmsg = $Msg(56);
095700151106               leavesr;
095800151106
095900151106             EndIf;
096000151106
096100151109           EndIf;
096200151106
096300151106      /end-free
096400031009e   1c                   ENDIF
096500040705      * Anche se ho solo eseguito una ricerca ("?") del F.V.
096600040705      *   riemetto questa videata prima di proseguire con i
096700040705      *   controlli relativi al flag S/N DPD.
096800040705if  1c                   if        *in90   = *on
096900040705     c                   eval      *in44   =*on
097000060110     c                   leavesr
097100040705e   1c                   endif
097200040705      *
097300040705      * Se richiesto un piano di default, ma impostato piano DPD:
097400040705      *   vince il piano di default
097500040705if  1c                   if            V1Cnpg =  *zeros
097600040705     c                             and V1Cnfv =  *zeros
097700040705     c                   clear                   V1Cdpd
097800040705e   1c                   endif
097900040705      *
098000040705      * Controllo se P.O. abilitato a piano DPD
098100040705if  1c                   if        V1Cdpd  = 'S'
098200040705     c                   eval      *in90   = *on
098300040705     c                   eval      xx      = *zeros
098400040705do  2c                   dow       xx      < %elem(£1pog)
098500040705     c                   eval      xx      = xx + 1
098600040705if  3c                   if        £1pog(xx) <= *zeros
098700040705     c                   leave
098800040705e   3c                   endif
098900040705     c                   eval      yy      = 1
099000040705     c                   eval      *in10   = *off
099100040705     c     £1pog(xx)     lookup    $ORGfil(yy)                            10
099200040705if  3c                   if            *in10       =  *on
099300040705     c                             and $ORGntw(yy) =  'DPD'
099400040705     c                   eval      *in90   = *off
099500040705     c                   leave
099600040705e   3c                   endif
099700040705e   2c                   enddo
099800040705if  2c                   if        *in90   = *on
099900040705     c                   seton                                        442890
100000040705     c                   movel     $Msg(12)      V1Dmsg
100100060110     c                   leavesr
100200040705e   2c                   endif
100300040705e   1c                   endif
100400040705      *
100500040705      * Controllo se esistono P.O. DPD nel piano di default
100600040705if  1c                   if        V1Cdpd  = 'S'
100700040705     c                   eval      *in90   = *on
100800040705     c                   eval      TR9fgs  = V1Cpog
100900040705     c                   clear                   TR9npga
101000040705     c                   clear                   TR9nfva
101100040705     c     K03FLTR9      setll     FLTR9000
101200040705     c     K03FLTR9      reade(n)  FLTR9000
101300040705do  2c                   dow       NOT %eof(FLTR901L)
101400040705     c                   eval      xx      = 1
101500050615     c                   eval      *in10   = *off
101600040705     c     TR9bai        lookup    $ORGfil(xx)                            10
101700040705if  3c                   if            *in10       = *on
101800040705     c                             and $ORGntw(xx) = 'DPD'
101900040705     c                   eval      *in90   = *off
102000040705     c                   leave
102100040705e   3c                   endif
102200040705     c     K03FLTR9      reade(n)  FLTR9000
102300040705e   2c                   enddo
102400040705      * - Se non ne trovo: emetto l'apposito messaggio (xx) in window
102500040705if  2c                   if        *in90   = *on
102600040705     c                   eval      $Video  = 'W'
102700040705     c                   eval      xx      = 48
102800060110     c                   leavesr
102900040705e   2c                   endif
103000040705e   1c                   endif
103100031014      *
103200031014      * imposto numero F.V. in campo numerico editato a video
103300031014     c                   move      V1Cnfv        V1Dnfv
103400031007      *
103500060110     c                   ENDSR
103600031010      *
103700031010      *---------------------------------------------------------------*
103800031010      * Decodifica P.O. in gestione                                   *
103900031010      *---------------------------------------------------------------*
104000031010     c     DecodPOG      BegSr
104100031010      *
104200031010if  1c                   if        V1Cpog =  *zeros
104300031010     c                   clear                   V1Dpog
104400031010     c                   clear                   OG150
104500031010     c                   seton                                        412890
104600031010     c                   movel     $Msg(1)       V1Dmsg
104700060110     c                   leavesr
104800031010e   1c                   endif
104900031010      *
105000031010     c     V1Cpog        chain     AZORG
105100031010      *
105200031010if  1c                   if        not %found(AZORG01L)
105300031010     c                             or  ORGfva = '*'
105400031010      *
105500031010     c                   movel     *all'? '      V1Dpog
105600031010     c                   clear                   OG150
105700031010     c                   seton                                        412890
105800031010     c                   movel     $Msg(2)       V1Dmsg
105900060110     c                   leavesr
106000031010      *
106100031010x   1c                   else
106200031010      *
106300031010     c                   movel     ORGdes        V1Dpog
106400031010      *
106500031010     c                   eval      *in10  =  *off
106600040701     c     V1Cpog        lookup    £1                                     10
106700031010if  2c                   if        NOT *in10
106800031010     c                   seton                                        412890
106900031010     c                   movel     $Msg(2)       V1Dmsg
107000060110     c                   leavesr
107100031010e   2c                   endif
107200031010      *
107300031010     c                   movel     ORGdf0        OG150
107400031013if  2c                   if        §OGpck <> 'S'
107500040427     c                             and    %subst(KNMUS:1:3) <> 'EDP'
107600031010     c                   seton                                        412890
107700031010     c                   movel     $Msg(3)       V1Dmsg
107800060110     c                   leavesr
107900031010e   2c                   endif
108000031010      *
108100031010      * Se tutto ok reperisco il terminal per verificarne il livello
108200031010     c                   clear                   FNLV55ds
108300031010     c                   eval      D55lin = V1Cpog
108400031010     c                   eval      D55drf = Wdate
108500031010     c                   call      'FNLV55R'
108600031010     c                   parm                    FNLV55ds
108700040705      *
108800040705     c                   clear                   TRUL06ds
108900040705     c                   movel     '£1'          D06cod
109000040705     c                   movel     V1Cpog        D06key
109100040705     c                   movel(p)  TRUL06ds      KPJBU
109200040705     c                   call      'TRUL06R'
109300040705     c                   parm                    KPJBA
109400040705     c                   movel     KPJBU         TRUL06ds
109500040705     c                   movea     D06lia        £1pog
109600031010      *
109700031010e   1c                   endif
109800031010      *
109900060110     c                   ENDSR
110000031009      *
110100031009      *---------------------------------------------------------------*
110200031009      * Inizializzazione schiere, flag, variabili, ecc                *
110300031009      *---------------------------------------------------------------*
110400031009     c     InzALL_1      BEGSR
110500031009      *
110600031009     c                   reset                   $Fine
110700031009     c                   reset                   $InzS01
110800031009     c                   reset                   $InzS02
110900031023     c                   reset                   $EoF
111000031009     c                   reset                   $EndSFL
111100031009     c                   reset                   $Default
111200040701     c*** già impostato: reset                   $NewFVa
111300040701     c*** già impostato: reset                   $DltPS
111400031009     c                   reset                   $VDL
111500040409     c                   reset                   $IMA
111600040423     c                   reset                   $IMAdft
111700040607     c                   reset                   $PO
111800040611     c                   reset                   $POdpd
111900080129     c*** non qui:       reset                   $POerr
112000040607     c                   reset                   $POdft
112100040423     c                   reset                   $Hidden
112200031023     c                   reset                   $Found
112300050629     c                   reset                   $MsgBAIbau
112400031009      *
112500031009     c                   reset                   $MEMat
112600040519     c*** non qui:       reset                   $MEMbp
112700031111     c                   reset                   $MEMfv1
112800031111     c                   reset                   $MEMmp1
112900050701     c                   reset                   $MEMpm1
113000031111     c                   reset                   $MEMfv2
113100031111     c                   reset                   $MEMmp2
113200050701     c                   reset                   $MEMpm2
113300031009     c                   reset                   $MEMxf
113400040615     c                   reset                   $FVxPOa
113500040615     c                   reset                   $FVxPOn
113600040615     c                   reset                   $FVxPOd
113700031009      *
113800031009     c                   reset                   S01nrr
113900031009     c                   reset                   S02nrr
114000031009     c                   reset                   MAXnrr
114100031030     c                   reset                   SAVpep
114200031107     c                   reset                   SAVbau
114300040309      *
114400040423     c                   reset                   Wpost
114500031009      *
114600031009     c                   ENDSR
114700031009      *
114800031021      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
114900031021      *
115000031009     c     InzALL_2      BEGSR
115100031112      *
115200031111     c                   reset                   $FV
115300040315     c                   reset                   $CsrRRN
115400031009     c                   reset                   $PEP
115500031030     c                   reset                   $PEPbai
115600031009     c                   reset                   $BAI
115700031009     c                   reset                   $BAInr
115800050629     c                   reset                   $BAIbau
115900031009     c                   reset                   $BAU
116000040305     c                   reset                   $BAUbai
116100031009     c                   reset                   $BNL
116200031009     c                   reset                   $BNLnr
116300031030     c                   reset                   $NFV
116400040628     c                   reset                   $NFVbp
116500031030     c                   reset                   $NFVnr
116600040519     c                   reset                   $MEMbp
116700040409     c                   reset                   $MPTnr
116800040607     c                   reset                   $ORGnr
116900040607     c                   reset                   $PosPO
117000040628     c                   reset                   $PosPOdpd
117100031009      *
117200031009     c                   reset                   XX
117300031022     c                   reset                   SAVnfvIMA
117400031030     c                   reset                   SAVnfv
117500031022     c                   reset                   SAVpdr
117600040305     c                   reset                   SAVprg
117700031009     c                   reset                   Wpdr
117800031022     c                   reset                   Wnfv
117900031111     c                   reset                   Wnfv7
118000040423     c                   reset                   Wpost
118100031024     c                   reset                   Wast
118200031009      *
118300031009     c                   ENDSR
118400031009      *
118500031009      *---------------------------------------------------------------*
118600031009      * Caricamento dati da tab. MSL                                  *
118700031009      *---------------------------------------------------------------*
118800031009     c     Tab_MSL       BEGSR
118900031009      *
119000031009      * Carico schiera tab. MSL
119100031009     c                   clear                   XX
119200031009     c                   clear                   $MSLpep
119300031009     c                   movel     'MSL'         TBEcod
119400031009     c                   movel(p)  V1Cpog        TBEke1
119500031009     c     K02TNTBE      setll     TNTBE000
119600031009     c     K02TNTBE      reade     TNTBE000
119700031009do  1c                   dow       NOT %eof(TNTBE01L)
119800031009if  2c                   if        TBEatb = *blanks
119900031009     c                   add       1             xx
120000031009     c                   movel     TBEuni        dMSL
120100031009     c                   movel     §MSLpep       $MSLpep(xx)
120200031009e   2c                   endif
120300031009     c     K02TNTBE      reade     TNTBE000
120400031009e   1c                   enddo
120500031009      *
120600031009     c                   ENDSR
120700040408      *
120800040408      *---------------------------------------------------------------*
120900040408      * Caricamento tab. MPT                                          *
121000040408      *---------------------------------------------------------------*
121100040408     c     Tab_MPT       BEGSR
121200040408      *
121300040408      * Carico schiera tab. MPT
121400040408     c                   clear                   XX
121500040408     c                   clear                   $MPT
121600040423     c                   clear                   $MPTdes
121700040409     c                   reset                   $MPTobbl
121800040423     c*** qui non serve: clear                   $MPTnr
121900040408     c                   movel     'MPT'         TBEcod
122000040408     c     TBEcod        setll     TNTBE000
122100040408     c     TBEcod        reade     TNTBE000
122200040408do  1c                   dow       NOT %eof(TNTBE01L)
122300040408if  2c                   if        TBEatb  = *blanks
122400040408     c                   movel     TBEuni        dMPT
122500040409if  3c                   if            $VDL    =  *on
122600040409     c                             and §MPTsiv <> 'N'
122700040409     c                             OR  $VDL    =  *off
122800040409     c                             and §MPTnov <> 'N'
122900040408     c                   add       1             xx
123000040408     c                   movel     TBEke1        $MPT(xx)
123100040423     c                   movel     §MPTdes       $MPTdes(xx)
123200040409if  4c                   if            $VDL    =  *on
123300040409     c                             and §MPTsiv =  'S'
123400040409     c                             OR  $VDL    =  *off
123500040409     c                             and §MPTnov =  'S'
123600040409     c                   movel     *on           $MPTobbl(xx)
123700040409e   4c                   endif
123800040408e   3c                   endif
123900040408e   2c                   endif
124000040408     c     TBEcod        reade     TNTBE000
124100040408e   1c                   enddo
124200040408      *
124300040408      * La postazione "IMA" (generica) la considero sempre valida
124400040408      *   ma dò per scontato che sia inserita in tab. "MPT" !!!
124500040408if  1c***                if        xx      = *zeros
124600040408     c***                eval      $MPT(1) = 'IMA'
124700040408e   1c***                endif
124800040408      *
124900040408     c                   ENDSR
125000031009      *
125100031009      *---------------------------------------------------------------*
125200031009      * Controllo se P.O. in gestione con VDL                         *
125300031009      *---------------------------------------------------------------*
125400031009     c     Ctrl_VDL      BEGSR
125500031009      *
125600031009     c                   reset                   $VDL
125700031009     c                   clear                   dMFP
125800031009      *
125900031009     c                   reset                   TIBS02ds
126000031009     c                   movel     KNSIF         T02sif
126100031009     c                   movel     V1Cpog        T02ke1
126200031009     c                   call      'TIBS02R'
126300031009     c                   parm                    KPJBA
126400031009     c                   parm                    TIBS02ds
126500080121if  1c                   if        T02err  = *blanks
126600031009     c                   movel     T02uni        dMFP
126700031009e   1c                   endif
126800031009      *
126900040630      * Test se presente VdL o Cml per piano
127000060214      * - NON per i P.O. "elencati" in C_PO_...
127100161110      * => CML non è più usato!
127200060214if  1c                   if        V1Cpog <> C_PO_a
127300060214      *
127400031009     c     'V'           lookup    §MFPap                                 10
127500161110     ***c  n10'C'           lookup    §MFPap                                 10
127600031009     c                   movel     *in10         $VDL
127700060214      *
127800060214e   1c                   endif
127900040408      *
128000040408      * Caricamento postazioni "IMA" da tab. MPT
128100040408     c                   exsr      Tab_MPT
128200031009      *
128300031009     c                   ENDSR
128400031009      *
128500031009      *---------------------------------------------------------------*
128600031020      * Intabellamento dati dal default                               *
128700031009      *---------------------------------------------------------------*
128800031020     c     Memo_DFT      BEGSR
128900031009      *
129000031009     c                   clear                   TR9npga
129100031009     c                   clear                   TR9nfva
129200031009     c     K03FLTR9      setll     FLTR9000
129300040624     c     K03FLTR9      reade(n)  FLTR9000
129400031009      *
129500040315do  1c                   DOW       NOT %eof(FLTR901L)
129600031020      *
129700031023      * Intabellamento autotrasportatori di default:
129800040701      * serve a "dare priorità" ad essi quando (dopo, qui sotto), si
129900040701      * memorizzeranno i F.V. più recenti di tutti gli autotrasportato-
130000040701      * ri (memorizzati e non): potrebbe andare "buca" all'ultimo auto-
130100040701      * trasportatore del default - se sono già stati memorizzati tanti
130200040701      * autotrasportatori quanti sono gli elementi della schiera $MEMat...
130300040315if  2c                   IF        TR9bnl <> 'S'
130400031023      *
130500031009      * - controllo se cod. autotrasportatore già memorizzato
130600040315if  3c                   if        TR9pdc <> *zeros
130700040615     c                   eval      xx      = 1
130800040615     c                   eval      *in10   = *off
130900031009     c     TR9pdc        lookup    $MEMat(xx)                             10
131000040615if  4c                   if        *in10   = *off
131100031009      * - - memorizzo nuovo cod. autotrasportatore
131200040615     c                   eval      xx      = 1
131300031009     c     *zeros        lookup    $MEMat(xx)                             10
131400040615if  5c                   if        *in10   = *on
131500040615     c                   eval      $MEMat(xx)  = TR9pdc
131600040315x   5c                   else
131700031009      * - - - se già memorizzati il n° massimo di autotrasportatori
131800031009      *       esco dal ciclo di intabellamento
131900031009     c                   leave
132000040315e   5c                   endif
132100040315e   4c                   endif
132200040315e   3c                   endif
132300040309      *
132400040315e   2c                   ENDIF
132500031020      *
132600040624     c     K03FLTR9      reade(n)  FLTR9000
132700040315e   1c                   ENDDO
132800040315      *
132900031023      *
133000031009      * Intabellamento F.V. più recente per ogni autotrasportatore
133100031110      *   prima intabellato (dal default) e NON (basta che abbia un
133200031110      *   f.v. valido per la data del f.v.arrivi impostato)
133300031023      *
133400040615     c                   eval      FV8fgs  = V1Cpog
133500040615     c                   eval      FV8npg  = 4
133600040615     c                   eval      FV8dfv  = FVVdfv
133700031009     c     K03FVV08      setll     FNFVV08
133800031010     c     K02FVV08      reade     FNFVV08
133900031009      *
134000031009do  1c                   DOW       NOT %eof(FNFVV08L)
134100040615if  2c                   if            FV8atb  = *blanks
134200040615     c                             and FV8fcf  = *blanks
134300120613     c                   eval      DSTnpg  = FV8npg
134400120613     c                   eval      DSTnfv  = FV8nfv
134500120613     c                   eval      DSTfgs  = V1Cpog
134600120613     c     K03FIDST      chain     FIDST000
134700120613if  3c                   if        %found(FIDST01L)
134800031009     c                   clear                   DS_FV
134900040615     c                   eval      dsFVdfv     = FV8dfv
135000040615     c                   eval      dsFVnfv     = FV8nfv
135100031022     c                   movel     DS_FV         w0150
135200031009      * - controllo se cod. autotrasportatore già memorizzato
135300040615     c                   eval      xx      = 1
135400040615     c                   eval      *in10   = *off
135500031009     c     DSTpdr        lookup    $MEMat(xx)                             10
135600040615if  4c                   if        *in10   = *off
135700031110     c     *zeros        lookup    $MEMat(xx)                             10
135800040615if  5c                   if        *in10   = *on
135900040615     c                   eval      $MEMat(xx)  = DSTpdr
136000031110e   5c                   endif
136100031110e   4c                   endif
136200031111      * - - memorizzo data e numero distinte più recenti
136300040615if  4c                   if        *in10   = *on
136400050701     c                   movel     FV8spn        dFVVspn
136500031111     c                   add       1             $MEMxf(xx)
136600031111sel 5c                   select
136700031111      * - - - 1° f.v.
136800040615w   5c                   when      $MEMfv1(xx) < w0150
136900040615if  6c                   if        $MEMfv1(xx) > $MEMfv2(xx)
137000040615     c                   eval      $MEMfv2(xx) = $MEMfv1(xx)
137100040615     c                   eval      $MEMmp2(xx) = $MEMmp1(xx)
137200050701     c                   eval      $MEMpm2(xx) = $MEMpm1(xx)
137300031111e   6c                   endif
137400040615     c                   eval      $MEMfv1(xx) = w0150
137500040615     c                   eval      $MEMmp1(xx) = FV8fpp
137600050701if  2c                   eval      $MEMpm1(xx) = §FVVtpm
137700031111      * - - - 2° f.v.
137800040615w   5c                   when      $MEMfv2(xx) < w0150
137900040615     c                   eval      $MEMfv2(xx) = w0150
138000040615     c                   eval      $MEMmp2(xx) = FV8fpp
138100050701if  2c                   eval      $MEMpm2(xx) = §FVVtpm
138200031111e   5c                   endsl
138300031009e   4c                   endif
138400031009e   3c                   endif
138500031009e   2c                   endif
138600031010     c     K02FVV08      reade     FNFVV08
138700031009e   1c                   ENDDO
138800040615      *
138900040615      *
139000040615      * Intabellamento F.V. più recente per ogni P.O. DPD
139100040615      *   prima intabellato (dal default) e NON (basta che abbia un
139200040615      *   f.v. valido per la data del f.v.arrivi impostato)
139300040615      *
139400040615     c                   clear                   xx
139500040615     c                   eval      FGVlnp  = V1Cpog
139600040615     c                   eval      FGVdfv  = SAVdfva
139700040615     c                   eval      FGVnfv  = *loval
139800040621     c     K03FGV02      setll     FNFGV000
139900040615     c     FGVlnp        reade     FNFGV000
140000040615      *
140100040615do  1c                   DOW       NOT %eof(FNFGV02L)
140200040615      *
140300040615if  2c                   IF            FGVatb  = *blanks
140400040615     c                             and FGVfcf  = *blanks
140500040705      *
140600040705     c                   clear                   Wnfv7
140700040705      * - 1° controllo con P.O. arrivo
140800040615     c                   eval      xy      = 1
140900040615     c                   eval      *in10   = *off
141000040615     c                   move      FGVlna        Wpost
141100040615     c     Wpost         lookup    $ORGfil(xy)                            10
141200040705      * - - imposto numero distinta
141300070530if  3c                   IF            *in10       = *on
141400040705     c                             and $ORGntw(xy) = 'DPD'
141500040705     c                   eval      Wnfv7   = FGVnfv
141600070530     c                   exsr      Memo_NFV
141700070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
141800070530      *     memorizzabile
141900080121if  4c                   if        xx      = %elem($FVxPOa)
142000070530     c                   leavesr
142100070530e   4c                   endif
142200070530e   3c                   ENDIF
142300040705      *
142400040705      * - 2° controllo con altri P.O. (1) del F.V.
142500040630     c                   clear                   xFGVffv
142600070530if  3c                   IF        FGVttr <> 'L'
142700040630     c                   movea     FGVffv        xFGVffv
142800040630     c                   clear                   wx
142900070530do  4c                   DoW       wx      < %elem(xFGVffv)
143000040630     c                   add       1             wx
143100040630if  5c                   if        xFGVffv(wx)     =  *blanks
143200040630     c                   leave
143300040630e   5c                   endif
143400040630     c                   eval      xy      = 1
143500040630     c                   eval      *in10   = *off
143600040630     c     xFGVffv(wx)   lookup    $ORGfil(xy)                            10
143700040705      * - - imposto numero distinta - negativo
143800040705      *     per memorizzare che il p.o. DPD è tra "altri p.o." del f.v.
143900040705      *     (e non è il p.o. finale di arrivo)
144000040630if  5c                   if            *in10       =  *on
144100040630     c                             and $ORGntw(xy) =  'DPD'
144200040630     c                   move      xFGVffv(wx)   Wpost
144300040705     c                   eval      Wnfv7   = FGVnfv * (-1)
144400070521     c                   exsr      Memo_NFV
144500070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
144600070530      *     memorizzabile
144700080121if  6c                   if        xx      = %elem($FVxPOa)
144800070530     c                   leavesr
144900070530e   6c                   endif
145000040630e   5c                   endif
145100070530e   4c                   EndDo
145200070530e   3c                   ENDIF
145300040705      *
145400040705      * - 3° controllo con altri P.O. (2) del F.V.
145500070530if  3c                   IF        FGVttr <> 'L'
145600040630     c                   clear                   xFGVffv
145700040630     c                   movea     FGVff2        xFGVffv
145800040630     c                   clear                   wx
145900070530do  4c                   DoW       wx      < %elem(xFGVffv)
146000040630     c                   add       1             wx
146100040630if  5c                   if        xFGVffv(wx)     =  *blanks
146200040630     c                   leave
146300040630e   5c                   endif
146400040630     c                   eval      xy      = 1
146500040630     c                   eval      *in10   = *off
146600040630     c     xFGVffv(wx)   lookup    $ORGfil(xy)                            10
146700040705      * - - imposto numero distinta - negativo
146800040705      *     per memorizzare che il p.o. DPD è tra "altri p.o." del f.v.
146900040705      *     (e non è il p.o. finale di arrivo)
147000040630if  5c                   if            *in10       =  *on
147100040630     c                             and $ORGntw(xy) =  'DPD'
147200040630     c                   move      xFGVffv(wx)   Wpost
147300040705     c                   eval      Wnfv7   = FGVnfv * (-1)
147400070521     c                   exsr      Memo_NFV
147500070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
147600070530      *     memorizzabile
147700080121if  6c                   if        xx      = %elem($FVxPOa)
147800070530     c                   leavesr
147900070530e   6c                   endif
148000040630e   5c                   endif
148100070530e   4c                   EndDo
148200070530e   3c                   ENDIF
148300040705      *
148400040705e   2c                   ENDIF
148500040615      *
148600040615     c     FGVlnp        reade     FNFGV000
148700040615      *
148800040615e   1c                   ENDDO
148900031009      *
149000031009     c                   ENDSR
149100070521      *
149200070521      *---------------------------------------------------------------*
149300070521      * Intabellamento F.V. disponibili x filiale                     *
149400070521      *---------------------------------------------------------------*
149500070521     c     Memo_NFV      BEGSR
149600070521      *
149700070521      * - Memorizzo data e numero distinte più recenti
149800070521     c                   eval      xx          = xx + 1
149900070521     c                   eval      $FVxPOa(xx) = Wpost
150000070521     c                   eval      $FVxPOn(xx) = Wnfv7
150100070521     c                   eval      $FVxPOd(xx) = FGVdfv
150200070521      *
150300070521     c                   ENDSR
150400031007      *
150500031007      *---------------------------------------------------------------*
150600031007      * Gestione videata W01                                          *
150700031007      *---------------------------------------------------------------*
150800031007     c     GesW01        BEGSR
150900031007      *
151000031007      * Preparazione videata con messaggio
151100031007     c                   clear                   MS03W01
151200040705     c                   movel     $Msg(xx)      W1Dms2
151300031009     c                   eval      W1Dtfa  = 'F12=Ritorno'
151400031007      *
151500031007      * Emissione messaggio
151600031009do  2c                   dou       *inKL   = *on
151700031007     c                   exfmt     MS03W01
151800031007e   2c                   enddo
151900031007      *
152000031007      * Ritorno alla videata iniziale
152100031007     c                   reset                   $Video
152200040705     c                   eval      *in90   = *off
152300031007      *
152400031007     c                   ENDSR
152500031007      *
152600031007      *---------------------------------------------------------------*
152700031106      * Gestione videata C01/S01 & C03/S03 o C02/S02 & C04/S04        *
152800031007      *---------------------------------------------------------------*
152900031106     c     GesSFL        BEGSR
153000031106      *
153100031106      * Inizializzo la videata
153200031106if  1c                   if            $VDL    = *off
153300031106     c                             and $InzS01 = *on
153400031106     c                             OR  $VDL    = *on
153500031106     c                             and $InzS02 = *on
153600031106     c                   exsr      InzSFL
153700031106     c                   eval      $InzS01     = *off
153800031106     c                   eval      $InzS02     = *off
153900031106e   1c                   endif
154000031106      *
154100031106      * Se non sono stati trovati record NEANCHE con il piano di
154200031106      *   default emetto un messaggio e torno alla 1ª videata
154300031106if  1c                   if        MAXnrr  = *zeros
154400031106     c                   eval      $Video  = 'W'
154500040705     c                   eval      xx      = 9
154600060110     c                   leavesr
154700031106x   1c                   else
154800031106     c                   eval      *in30   = *off
154900031106e   1c                   endif
155000031106      *
155100031106      * Posiziono il cursore
155200031106sel 1c                   select
155300031106w   1c                   when          $VDL   = *off
155400031106     c                             and C01csr > *zeros
155500031106     c                   z-add     C01csr        C01rcd
155600031106w   1c                   when          $VDL   = *on
155700031106     c                             and C02csr > *zeros
155800031106     c                   z-add     C02csr        C02rcd
155900031106w   1c                   when          $VDL   = *off
156000031106     c                             and C01csr = *zeros
156100031106     c                   z-add     1             C01rcd
156200031106w   1c                   when          $VDL   = *on
156300031106     c                             and C02csr = *zeros
156400031106     c                   z-add     1             C02rcd
156500031106e   1c                   endsl
156600031106      *
156700031106      * Scrivo la testata e la riga tasti funzionali abilitati
156800031106if  1c                   if        NOT *in90
156900031106     c                   write     MS03T01
157000031106if  2c                   if        $Default = *on
157100031106     c                   write     MS03Z02
157200031106x   2c                   else
157300031106     c                   write     MS03Z03
157400031106e   2c                   endif
157500031106e   1c                   endif
157600151109      *
157700151109      *?Altro Piano Smistamento già esistente per lo stesso giorno?
157800151109     c                   if        ds_NFVxDFV.TR9nfva > *zero
157900151109     c                             and        $NewFVA = *off
158000151109     c                             and        V1Cnpg <> *zero
158100151109     c                   eval      wDate_Eur = %date( SavDFVa : *ISO )
158200151109     c                   eval      *in28  = *on
158300151109     c                   eval      *in90  = *on
158400151109     c                   eval      V1Dmsg = $Msg(55)
158500151109     c                   eval      V1Dmsg = %replace( %trim( %editc(
158600151109     c                                        ds_NFVxDFV.TR9nfva : '2' ) )
158700151109     c                                      : V1Dmsg
158800151109     c                                      : %scan( '_      ' : V1Dmsg ) )
158900151109     c                   eval      V1Dmsg = %replace(
159000151109     c                                      %editc( %dec( wDate_Eur ) : 'Y' )
159100151109     c                                      : V1Dmsg
159200151109     c                                      : %scan( '__/__/____' : V1Dmsg ) )
159300151109     c                   endif
159400031106      *
159500031106      * Emetto la videata
159600060110     c     §OGpkm        comp      'S'                                    04
159700031106sel 1c                   select
159800031106w   1c                   when      $Video = '2'
159900031106     c                   exfmt     MS03C01
160000031106w   1c                   when      $Video = '3'
160100031106     c                   exfmt     MS03C02
160200031106w   1c                   when      $Video = '4'
160300031106     c                   exfmt     MS03C03
160400031106w   1c                   when      $Video = '5'
160500031106     c                   exfmt     MS03C04
160600031106e   1c                   endsl
160700031106      *
160800031106     c                   setoff                                       28  90
160900031106     c                   clear                   V1Dmsg
161000031106      *
161100031106sel 1c                   select
161200031106      * F3=Fine
161300031106w   1c                   when      *inKC
161400031106     c                   exsr      F03D01
161500060110     c                   leavesr
161600031106      * F12=Ritorno
161700031106w   1c                   when      *inKL
161800031106     c                   exsr      F12SFL
161900060110     c                   leavesr
162000040315      * F16=Cancellazione piano
162100040315w   1c                   when      *inKQ
162200040315     c                   exsr      F16SFL
162300060110     c                   leavesr
162400031106      *
162500031106e   1c                   endsl
162600031106      *
162700151109      *?Altro Piano Smistamento già esistente per lo stesso giorno?
162800151109     c                   if        ds_NFVxDFV.TR9nfva > *zero
162900151109     c                             and        $NewFVA = *off
163000151109     c                   leavesr
163100151109     c                   endif
163200151109      *
163300031106     c                   exsr      InzALL_2
163400031106      *
163500031106      * Controllo dati immessi a video
163600031106if  1c                   if        $VDL = *off
163700031106     c                   exsr      CtrS01
163800031106x   1c                   else
163900031106     c                   exsr      CtrS02
164000031106e   1c                   endif
164100060110     c                   if        *in90
164200060110     c                   leavesr
164300060110     c                   endif
164400031111      *
164500031111      * F7=Interrogazione F.V. non inseriti
164600031111if  1c                   if        *inKG
164700031111     c                   clear                   Wcampo
164800031111     c                   exsr      srSEARCH
164900060110     c                   leavesr
165000031111e   1c                   endif
165100031112      *
165200031112      * Alla conferma (F6 o F8): segnalo se esistono distinte aperte
165300031112      * ------------------------ NON inserite nel piano giornaliero
165400080129      *   e/o rimembra delle filiali DPD lasciate SENZA F.V.
165500040311     c                   reset                   WinKH
165600031112if  1c                   if        (*inKF  or  *inKH)
165700031112     c                             and $Default = *off
165800080129     c                   exsr      CtrlFVdpd
165900080129if  2c                   if        *inKL
166000080129     c                   leavesr
166100080129e   2c                   endif
166200031112     c                   exsr      CtrlDistAp
166300031112e   1c                   endif
166400031106      *
166500031106      * F6=Aggiornamento
166600031106      * F8=Aggiornamento & Stampa
166700031112if  1c                   if        *inKF   or  *inKH
166800031112     c                                     or  WinKH = *on
166900031106     c                   exsr      Upd_FLTR9
167000031112if  2c                   if        *inKH   or  WinKH = *on
167100031106     c                   exsr      F08SFL
167200031106e   2c                   endif
167300031106     c                   reset                   $Video
167400031106     c                   reset                   $InzD01
167500031106e   1c                   endif
167600031106      *
167700060110     c                   ENDSR
167800031007      *
167900031007      *---------------------------------------------------------------*
168000031106      * Inizializzazione videata S01 & S03 o S02 & S04                *
168100031007      *---------------------------------------------------------------*
168200031106     c     InzSFL        BEGSR
168300031007      *
168400031007      * Pulizia subfile
168500031007     c                   seton                                        3031
168600031106sel 1c                   select
168700031106w   1c                   when      $Video = '2'
168800031007     c                   write     MS03C01
168900031106w   1c                   when      $Video = '3'
169000031106     c                   write     MS03C02
169100031106w   1c                   when      $Video = '4'
169200031007     c                   write     MS03C03
169300031106w   1c                   when      $Video = '5'
169400031106     c                   write     MS03C04
169500031106e   1c                   endsl
169600031009     c                   setoff                                         31
169700031007      *
169800031009     c                   clear                   C01rcd
169900031009     c                   clear                   C01csr
170000031009     c                   clear                   S01nrr
170100031106     c                   clear                   C02rcd
170200031106     c                   clear                   C02csr
170300031106     c                   clear                   S02nrr
170400031007     c                   clear                   V1Dmsg
170500031007     c                   setoff                                       28  90
170600031007     c                   movea     *zeros        *in(41)
170700080129      *
170800080129     c                   reset                   $ORGerr
170900031007      *
171000031009      * Caricamento dei dati da presentare nel sfl (max 90 rec)
171100031023      * se è piano di DEFAULT leggo con key *zeros
171200031023      * se è NUOVO piano GIORNALIERO propongo quelli di default
171300040315      * se è VECCHIO piano GIORNALIERO rifaccio la lista per baie /
171400040315      *   postazioni dal piano di default e vi abbino i dati già memo-
171500040315      *   rizzati (questo per mantenere l'elenco delle baie/postazioni
171600040315      *   comprensivo di quelle eventualmente non registrate, visua-
171700040315      *   lizzate con "XXX").
171800031106     c                   eval      TR9fgs  = V1Cpog
171900031106     c                   clear                   TR9npga
172000031106     c                   clear                   TR9nfva
172100040623     c                   eval      XR9npga = V1Cnpg
172200040623     c                   move      V1Cnfv        XR9nfva
172300031106      *
172400031007     c                   reset                   $EndSFL
172500031007do  1c                   DOW       $EndSFL = *off
172600031007     c                   exsr      Carica_SFL
172700031007e   1c                   ENDDO
172800031007      *
172900031106if  1c                   if        $VDL    = *off
173000031007     c                   eval      MAXnrr  = S01nrr
173100031106x   1c                   else
173200031106     c                   eval      MAXnrr  = S02nrr
173300031106e   1c                   endif
173400031009     c                   eval      *in33   = ($EndSFL = *on)
173500031007      *
173600031007     c                   ENDSR
173700151104      *
173800151106      /free
173900151106
174000151106       //--------------------------------------------------------------
174100151106       //?Preparaz. stringa SQL per estrazione altri piani smistamento ?
174200151106       //?  picking per lo stesso giorno.                              ?
174300151106       //?Preparazione cursore.                                        ?
174400151106       //?Dichiarazione cursore.                                       ?
174500151106       //?Apertura cursore.                                            ?
174600151106       //--------------------------------------------------------------
174700151106       BEGSR  sr_OpenCursor;
174800151109
174900151106         // -?Impostazione opzioni per SQL?
175000151106         exec sql   set  option  DynUsrPrf = *Owner,
175100151106                                 CloSqlCsr = *EndMod;
175200151106
175300151106         // -?Preparazione stringa SQL per estrarre i dati?
175400151106         wSQL = 'with NFV_x_DFV as +
175500151109                              ( select TR9fgs, TR9npga, TR9nfva, TR9fl2, +
175600151109                                       Fvvdfv +
175700151106                                  from FLTR900F inner join FNFVV00F +
175800151106                                    on TR9fgs  = FVVfgs +
175900151106                                   and TR9npga = FVVnpg +
176000151106                                   and TR9nfva = FVVnfv +
176100151106                                 where FVVfcf  = '' '' +
176200151109                              group by TR9fgs, TR9npga, TR9nfva, TR9fl2, +
176300151109                                       FVVdfv +
176400151109                              order by TR9fgs, TR9npga, TR9nfva, TR9fl2, +
176500151109                                       FVVdfv ), +
176600151106
176700151106                      CountNFV  as +
176800151109                              ( select TR9fgs, TR9npga, TR9fl2, FVVdfv, +
176900151106                                       count(*) as wNFVxDFV +
177000151106                                  from NFV_x_DFV +
177100151109                              group by TR9fgs, TR9npga, TR9fl2, FVVdfv ) +
177200151106
177300151106                 select NFV_x_DFV.*, CountNFV.wNFVxDFV +
177400151106
177500151106                   from NFV_x_DFV inner join CountNFV +
177600151106                     on NFV_x_DFV.TR9fgs  = CountNFV.TR9fgs +
177700151106                    and NFV_x_DFV.TR9npgA = CountNFV.TR9npgA +
177800151109                    and NFV_x_DFV.TR9fl2  = CountNFV.TR9fl2 +
177900151106                    and NFV_x_DFV.FVVdfv  = CountNFV.FVVdfv +
178000151106
178100151109                 where NFV_x_DFV.TR9fgs  = ' +
178200151109                       %trim( %editc( V1Cpog : '3' ) ) +
178300151109                 ' and NFV_x_DFV.TR9nfvA <> ' +
178400151109                       %trim( %editc( TR9nfva : '3' ) ) +
178500151109                 ' and NFV_x_DFV.FVVdfv  = ' +
178600151106                       %trim( %editc( SavDFVa : '3' ) );
178700151106
178800151109         if  V1Cdpd = 'S';
178900151109           wSQL +=
179000151109                 ' and NFV_x_DFV.TR9fl2  = ''D''';
179100151109         else;
179200151109           wSQL +=
179300151109                 ' and NFV_x_DFV.TR9fl2  = '' ''';
179400151109         endif;
179500151109
179600151106         // -?Dichiarazione cursore?
179700151106         exec sql   prepare S1   from :wSQL;
179800151106         exec sql   declare C1   cursor for S1;
179900151106
180000151106         if  SQLcode < *zero;
180100151106           *in34 = *on;
180200151109           exsr  sr_CloseCursor;
180300151106           exsr  sr_PrintERR;
180400151106           leavesr;
180500151106         endif;
180600151106
180700151106         // -?Apertura del cursore?
180800151106         exec sql   open C1;
180900151106
181000151106         if  SQLcode < *zero;
181100151106           *in34 = *on;
181200151106           exsr  sr_CloseCursor;
181300151106           exsr  sr_PrintERR;
181400151106           leavesr;
181500151106         endif;
181600151106
181700151106       ENDSR;
181800151106
181900151106       //--------------------------------------------------------------
182000151106       //?Lettura cursore.                                             ?
182100151106       //--------------------------------------------------------------
182200151106       BEGSR  sr_ReadCursor;
182300151106
182400151106         clear  ds_NFVxDFV;
182500151106
182600151106         exec sql   fetch next   from C1   into :ds_NFVxDFV;
182700151106
182800151106         select;
182900151106           when  SQLcode = 100;
183000151106             *in34 = *on;
183100151106             exsr  sr_CloseCursor;
183200151106           when  SQLcode < *zero;
183300151106             *in34 = *on;
183400151106             exsr  sr_CloseCursor;
183500151106             exsr  sr_PrintERR;
183600151106         endsl;
183700151106
183800151106       ENDSR;
183900151106
184000151106       //--------------------------------------------------------------
184100151106       //?Chiusura cursore.                                            ?
184200151106       //--------------------------------------------------------------
184300151106       BEGSR  sr_CloseCursor;
184400151106
184500151106         // -?Chiusura del cursore?
184600151109         exec sql   close C1;
184700151106
184800151106       ENDSR;
184900151106
185000151106       //--------------------------------------------------------------
185100151106       //?Stampa segnalazione dell'errore rilevato via SQL             ?
185200151106       //--------------------------------------------------------------
185300151106       BEGSR  sr_PrintERR;
185400151106
185500151106         // -?Stampa del Dump?
185600151106         Dump(A);
185700151106
185800151106         // -?Stampa del Job-Log?
185900151106         Qcmd = 'DSPJOBLOG job(*) output(*print)';
186000151106         exsr  sr_ExecCmd;
186100151106
186200151106       ENDSR;
186300151106
186400151106       //--------------------------------------------------------------
186500151106       //?Esecuzione del comando (già impostato).                      ?
186600151106       //--------------------------------------------------------------
186700151106       BEGSR  sr_ExecCmd;
186800151106
186900151106         clear Qcap0100;
187000151106         Qcabcsdh = *off;
187100151106         Qcapa    = *off;
187200151106         Qcacmdss = *off;
187300151106         Qcaerved = *allX'00';
187400151106
187500151106         clear Qusec;
187600151106         Qusbprv  = %size(Qusec);
187700151106
187800151106         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
187900151106                           %size(Qcap0100) : 'CPOP0100' : *omit :
188000151106                           0 : 0 : Qusec);
188100151106
188200151106         //if  Qusei <> *blank;
188300151106         //  ...;
188400151106         //endif;
188500151106
188600151106       ENDSR;
188700151106
188800151106      /end-free
188900031029      *
189000031029      *---------------------------------------------------------------*
189100031029      * Gestione tasto funzionale F8  da videata S01/S02/S03/S04      *
189200031029      *---------------------------------------------------------------*
189300031106     c     F08SFL        BEGSR
189400031029      *
189500031029      * Richiamo pgm. di stampa per piano di smistamento in esame
189600031029     c                   clear                   FIMS04ds
189700031029     c                   z-add     V1Cpog        D04pog
189800040408if  1c                   if        V1Cnfv > *zeros
189900031029     c                   z-add     V1Cnpg        D04npga
190000040408e   1c                   endif
190100031029     c                   move      V1Cnfv        D04nfva
190200031029      *
190300031029     c                   movel(p)  FIMS04ds      KPJBU
190400031029     c                   call      'FIMS04R'
190500031029     c                   parm                    KPJBA
190600031029      *
190700031029     c                   ENDSR
190800031007      *
190900031007      *---------------------------------------------------------------*
191000031007      * Gestione tasto funzionale F12 da videata S01/S02/S03/S04      *
191100031007      *---------------------------------------------------------------*
191200031106     c     F12SFL        BEGSR
191300031007      *
191400031007      * Torno alla 1ª videata D01
191500031007     c                   reset                   $Video
191600031010     c                   movea     '00'          *in(01)
191700031007      *
191800031007     c                   ENDSR
191900040315      *
192000040315      *---------------------------------------------------------------*
192100040315      * Gestione tasto funzionale F16 da videata S03/S04              *
192200040315      *---------------------------------------------------------------*
192300040315     c     F16SFL        BEGSR
192400040315      *
192500040315      * Cancella record esistenti (se?NON?immissione)
192600040702if  1c                   if            $NewFVa = *off
192700040702     c                             and $DltPS  = *off
192800040315      *
192900040315      * - Richiesta conferma
193000040315     c                   clear                   MS03W01
193100040315     c                   movel     $Msg(19)      W1Dms2
193200040315     c                   eval      W1Dtfa = 'F6=Conferma   '
193300040315     c                                    + 'F12=Ritorno'
193400040315do  2c                   dou       *inKF   or   *inKL
193500040315     c                   exfmt     MS03W01
193600040315e   2c                   enddo
193700060110      * F12-Ritorno
193800060110     c                   if        *inKL
193900060110     c                   leavesr
194000060110     c                   endif
194100060110      * F6-Conferma
194200040315     c                   exsr      Dlt_FLTR9
194300040315      *
194400040315e   1c                   endif
194500040315      *
194600040315      * Torno alla 1ª videata D01
194700040315     c                   reset                   $Video
194800040315     c                   movea     '00'          *in(01)
194900040315      *
195000060110     c                   ENDSR
195100040701      *
195200040701      *---------------------------------------------------------------*
195300040701      * Controllo esistenza piano smistamento STD se richiesto DPD    *
195400040701      *---------------------------------------------------------------*
195500040701     c     Check_PS      BEGSR
195600040701      *
195700040701     c*** (già fatto)    reset                   $DltPS
195800040705     c                   eval      TR9fgs  = V1Cpog
195900040705     c                   eval      TR9npga = V1Cnpg
196000040705     c                   move      V1Cnfv        TR9nfva
196100040701      *
196200040705     c     K03FLTR9      setll     FLTR9000
196300040705     c     K03FLTR9      reade(n)  FLTR9000
196400040701      *
196500040701sel 1c                   select
196600040701      **** - nuovo piano (IMPOSSIBILE qui)
196700040705w   1c***                when      %eof(FLTR901L)
196800040701     c***                eval      $NewFVa = *on
196900060110     c***                leavesr
197000040701      * - richiesto piano std, esiste piano std: modificabile
197100040701w   1c                   when          V1Cdpd =  *blanks
197200040701     c                             and TR9fl2 =  *blanks
197300060110     c                   leavesr
197400040701      * - richiesto piano std, esiste piano dpd: da sostituire
197500040701w   1c                   when          V1Cdpd =  *blanks
197600040701     c                             and TR9fl2 =  'D'
197700040701     c                   eval      $DltPS  = *on
197800060110     c                   leavesr
197900040701      * - richiesto piano dpd, esiste piano dpd: modificabile
198000040701w   1c                   when          V1Cdpd =  'S'
198100040701     c                             and TR9fl2 =  'D'
198200060110     c                   leavesr
198300040701      **** - richiesto piano dpd, esiste piano std: da sostituire
198400040701w   1c***                when          V1Cdpd =  'S'
198500040701     c***                          and TR9fl2 =  *blanks
198600040701     c*** (sotto)        ...
198700040701e   1c                   endsl
198800040701      *
198900040701      * Richiesto piano dpd, esiste piano std: da sostituire
199000040701      * Richiesta conferma per annullamento piano smistamento picking
199100040701     c                   clear                   MS03W01
199200040701     c                   movel     $Msg(45)      W1Dms1
199300040701     c                   movel     $Msg(46)      W1Dms2
199400040701     c                   movel     $Msg(47)      W1Dms3
199500040701     c                   eval      W1Dtfa  = 'F6=Conferma'
199600040701     c                                     + ' (nuovo piano DPD)   '
199700040701     c                                     + 'F12=Ritorno'
199800040701do  1c                   dou       *inKF   or   *inKL
199900040701     c                   exfmt     MS03W01
200000040701e   1c                   enddo
200100040701      *
200200040701sel 1c                   select
200300040701      * - F6 = cancella
200400040701w   1c                   when      *inKF   = *on
200500040701     c*** non adesso:    exsr      Dlt_FLTR9
200600040701     c                   eval      $DltPS  = *on
200700040701      * - F12 = ritorno
200800040701w   1c                   when      *inKL   = *on
200900040701     c                   reset                   $Video
201000040701     c                   movea     '00'          *in(01)
201100040701e   1c                   endsl
201200040701      *
201300060110     c                   ENDSR
201400031106      *
201500031106      *---------------------------------------------------------------*
201600031106      * Caricamento subfile da file FLTR900F                          *
201700031106      *---------------------------------------------------------------*
201800040705     c     Carica_SFL    BEGSR
201900031106      *
202000031106      * Solo la prima volta:
202100031106      *   posizionamento iniziale + lettura 1° record
202200031106if  1c                   if            S01nrr =  *zeros
202300031106     c                             and S02nrr =  *zeros
202400040705     c                   clear                   TR9npga
202500040705     c                   clear                   TR9nfva
202600031106     c     K03FLTR9      setll     FLTR9000
202700040624     c     K03FLTR9      reade(n)  FLTR9000
202800031106if  2c                   if        %eof(FLTR901L)
202900031106     c                   eval      $EOF   = *on
203000031106e   2c                   endif
203100031106e   1c                   endif
203200031106      *
203300031106      * Pulizia iniziale del record nel sfl in esame
203400031106sel 1c                   select
203500031106w   1c                   when      $Video = '2'
203600031106     c                   clear                   MS03S01
203700031106w   1c                   when      $Video = '3'
203800031106     c                   clear                   MS03S02
203900031106e   1c                   when      $Video = '4'
204000031106     c                   clear                   MS03S03
204100031106w   1c                   when      $Video = '5'
204200031106     c                   clear                   MS03S04
204300031106e   1c                   endsl
204400031106      *
204500031106      * Se NON già rilevato %EOF:
204600031106      *   impostazione campi sfl con quelli dell'ultimo rec. letto
204700031106      *   e lettura singolo record successivo
204800031106if  1c                   IF        $EOF   = *off
204900031106      *
205000040309     c                   reset                   $Found
205100040629     c                   reset                   $IMA
205200040629     c                   reset                   $IMAdft
205300040629     c                   reset                   $PO
205400040629     c                   reset                   $POdpd
205500080129     c                   reset                   $POerr
205600040629     c                   reset                   $POdft
205700040309      *
205800040702      * - Per PIANO GIORNALIERO già immesso:
205900031106      *   controllo la corrispondenza dei dati di default (che sto
206000031106      *   leggendo) con quelli effettivamente immessi
206100040315if  2c                   if            $Default = *off
206200040315     c                             and $NewFVa  = *off
206300040702     c                             and $DltPS   = *off
206400031106     c                   exsr      RepFVAeff
206500040702      * - Per PIANO DI DEFAULT  &
206600040702      *   Per PIANO GIORNALIERO nuovo:
206700040623      *   imposto i campi del file di comodo (per il piano giornaliero)
206800040623      *   come quelli del file in aggiornamento (per i dati di default)
206900040623x   2c                   else
207000040623     c                   movel     FLTR9ds       FLXR9ds
207100040623     c                   eval      $Found   = *on
207200031106e   2c                   endif
207300040701      *
207400040701      * - Controllo se postazione IMA o P.O.
207500080118if  2c                   if            $Default = *off
207600040702     c                             and TR9bnl  <> 'S'
207700040702     c                   eval      $Hidden  = *on
207800040702     c                   eval      Wpost    = TR9bai
207900040702     c                   exsr      Ctrl_IMA
208000040702     c                   exsr      Ctrl_PO
208100040705e   2c                   endif
208200040705if  2c                   if        $Found   = *on
208300040701     c                   eval      $Hidden  = *off
208400040701     c                   eval      Wpost    = XR9bai
208500040701     c                   exsr      Ctrl_IMA
208600040701     c                   exsr      Ctrl_PO
208700040705e   2c                   endif
208800040701      *
208900040702      * - Selezione per piano giornaliero DPD:
209000040705if  2c                   if            $Default = *off
209100040702     c*** non solo new:            and $NewFVa  = *off
209200040702     c                             and V1Cdpd   = 'S'
209300040702      *   - baia non letti
209400040702     c                             and TR9bnl  <> 'S'
209500040705      *   - postazioni IMx
209600040705     c                             and $IMA     = *off
209700040702      *   - postazioni PO-dpd
209800040702     c                             and $POdpd   = *off
209900040702     c                   goto      Read_Next
210000040705e   2c                   endif
210100040705      *
210200040705      *   - postazioni IMx obbligatorie
210300040705if  2c                   if            $Default = *off
210400040705     c*** non solo new:            and $NewFVa  = *off
210500040705     c                             and V1Cdpd   = 'S'
210600040705     c                             and $IMA     = *on
210700040705     c                   eval      *in10    = *off
210800040705     c                   eval      xx       = 1
210900040705     c     Wpost         lookup    $MPT(xx)                               10
211000040705     c   10$MPTobbl(xx)  cabeq     *off          Read_Next
211100040705e   2c                   endif
211200031106      *
211300031106      * - Default (comuni)
211400040309      * > > P.O. senza VDL
211500040701if  2c                   if        $VDL     = *off
211600040701     c                   exsr      CarNoVDL_1
211700040309      * > > P.O. con VDL
211800040701x   2c                   else
211900040701     c                   exsr      CarSiVDL_1
212000040701e   2c                   endif
212100031106      *
212200031106      * - Impostazione dei F.V. in piano giornaliero
212300031106if  2c                   IF        $Default = *off
212400031106      *
212500031106      * - - da default
212600040702if  3c                   IF            $NewFVa  = *on
212700040702     c                             or  $DltPS   = *on
212800031106      *
212900040309sel 4c                   select
213000040309w   4c                   when      TR9bnl = 'S'
213100040615w   4c                   when      $PO    = *on
213200040705if  5c                   if        $POdpd = *on
213300040705     c                   exsr      PostPOdpd
213400040705e   5c                   endif
213500040309w   4c                   when      $IMA   = *on
213600040615     c                   exsr      PostIMA
213700040309x   4c                   other
213800040615     c                   exsr      PostNoIMA
213900040309e   4c                   endsl
214000031106      *
214100031106      * - - da record già immessi
214200031106x   3c                   ELSE
214300031106      *
214400040621if  4c                   if        $Found   = *on
214500031106     c                   reset                   WLBda8
214600031106     c                   reset                   DSdata8
214700031106     c                   reset                   DSdata6
214800040621if  5c                   if        XR9dfv   > *zeros
214900040623     c                   eval      G08inv   = XR9dfv
215000031106     c                   call      'XSRDA8'
215100031106     c                   parm                    WLBda8
215200031106     c                   move      G08dat        DSdata8
215300031106     c                   z-add     DSdt8gg       DSdt6gg
215400031106     c                   z-add     DSdt8mm       DSdt6mm
215500031106     c                   z-add     DSdt8aa       DSdt6aa
215600040621e   5c                   endif
215700040621e   4c                   endif
215800040309      * > > > P.O. senza VDL
215900031106if  4c                   if        $VDL     = *off
216000040701     c                   exsr      CarNoVDL_2
216100040309      * > > > P.O. con VDL
216200031106x   4c                   else
216300040701     c                   exsr      CarSiVDL_2
216400031106e   4c                   endif
216500031106      *
216600031106e   3c                   ENDIF
216700040705      *
216800040705      * Reimposto il valore assoluto tra i F.V. memorizzati x P.O. DPD
216900040705     c                   eval      $FVxPOn  =  %abs($FVxPOn)
217000031106      *
217100031106      * Per piano giornaliero imposto "*" se più F.V. per il padroncino
217200040615      *   o se più F.V. per la postazione = P.O. DPD (solo VdL)
217300040701     c                   clear                   Wast
217400031106     c                   clear                   Wpdr
217500040615     c                   clear                   Wpost
217600040701sel 3c                   select
217700040701w   3c                   when      XR9pdc   > *zeros
217800040621     c                   eval      Wpdr     = XR9pdc
217900031106     c                   exsr      CtrlAutotr3
218000040701w   3c                   when      $POdpd   = *on
218100040621     c                   eval      Wpost    = XR9bai
218200040615     c                   exsr      AsterFVxPO
218300040701e   3c                   endsl
218400040701if  3c                   if        $VDL     = *off
218500040701     c                   eval      S1Cast   = Wast
218600040701x   3c                   else
218700040701     c                   eval      S2Cast   = Wast
218800040701e   3c                   endIf
218900031106      *
219000031106e   2c                   ENDIF
219100031106      *
219200031106      * Se *zeros li reimposto a *blanks (numerici "editati")
219300031106if  2c                   if        S1Cnfv   = *zeros
219400031106     c                   clear                   S1Cnfv
219500031106e   2c                   endif
219600031106if  2c                   if        S3Cnfv   = *zeros
219700031106     c                   clear                   S1Cnfv
219800031106e   2c                   endif
219900031106if  2c                   if        S2Cnfv   = *zeros
220000031106     c                   clear                   S2Cnfv
220100031106e   2c                   endif
220200040304if  2c                   if        S4Cnfv   = *zeros
220300040304     c                   clear                   S4Cnfv
220400040304e   2c                   endif
220500031106      *
220600090203x   1c***                ELSE
220700031106      *
220800031106      * Se già rilevato %EOF:
220900031106      * - impostazione campi vuoti nei restanti record del sfl
221000031106      *   (se "default")
221100031106      *
221200031106e   1c                   ENDIF
221300040701      *
221400040701      * Scrivo il singolo record nel subfile
221500040701     c                   exsr      WrtSflRec
221600040701      *
221700040701     c     Read_Next     tag
221800031106      *
221900031106      * Leggo il record successivo da caricare
222000031106if  1c                   if        $EOF     = *off
222100040624     c     K03FLTR9      reade(n)  FLTR9000
222200031106if  2c                   if        %eof(FLTR901L)
222300031106     c                   eval      $EoF     = *on
222400031106e   2c                   endif
222500031106e   1c                   endif
222600031106      *
222700031106      * Imposto l'uscita SE:
222800031106      * - ho già scritto il numero massimo di rec. previsti nel sfl
222900031106if  1c                   if            S01nrr  >= C_MaxRecSf
223000031106     c                             or  S02nrr  >= C_MaxRecSf
223100031106      * - è stato rilevato EOF in "Piano Giornaliero" (NON di default)
223200040702      *   non emetto records vuoti (come per il default)
223300031106     c                             or ($Default = *off
223400040702     c                             and $EoF     = *on)
223500031106     c                   eval      $EndSFL  = *on
223600031106e   1c                   endif
223700031106      *
223800031106     c                   ENDSR
223900040701      *
224000040701      *---------------------------------------------------------------*
224100050614      * Scrittura singolo record nel subfile                          *
224200040701      *---------------------------------------------------------------*
224300040701     c     WrtSflRec     BEGSR
224400040701      *
224500040701      * - reimposto indicatori x DSPATR condizionati
224600040701if  1c                   IF        $VDL = *off
224700060110     c                   exsr      AtrS01S03
224800040701x   1c                   ELSE
224900060110     c                   exsr      AtrS02S04
225000040701e   1c                   ENDIF
225100040701      *
225200040701      * Scrittura del singolo record nel sfl in esame
225300040702if  1c                   if            $Default = *on
225400040702     c                             OR  $Default = *off
225500040702     c                             and $EoF     = *off
225600040701     c                   eval      *in32    = *on
225700040701sel 2c                   select
225800040701w   2c                   when      $Video   = '2'
225900040701     c                   add       1             S01nrr
226000040701     c                   write     MS03S01
226100040701w   2c                   when      $Video   = '3'
226200040701     c                   add       1             S02nrr
226300040701     c                   write     MS03S02
226400040701e   2c                   when      $Video   = '4'
226500060110     c                   eval      *in54    = (*in80  = *off)
226600040701     c                   add       1             S01nrr
226700040701     c                   write     MS03S03
226800040701w   2c                   when      $Video   = '5'
226900060110     c                   eval      *in54    = (*in80  = *off)
227000040701     c                   add       1             S02nrr
227100040701     c                   write     MS03S04
227200040701e   2c                   endsl
227300040701e   1c                   endif
227400040701      *
227500040701     c                   ENDSR
227600031106      *
227700031106      *---------------------------------------------------------------*
227800031106      * Reperimento dei dati effettivamente immessi per postazione    *
227900031106      *---------------------------------------------------------------*
228000031106     c     RepFVAeff     BEGSR
228100031106      *
228200031106      * Ctrl se esiste il rec. corrispondente nel piano giornaliero
228300031106      *   già immesso
228400040623     c                   eval      XR9prg = (TR9prg * 2) - 1
228500040621     c     K05TR901      chain     FLTR901
228600040621     c                   eval      $Found = (%found(FLTR901X))
228700040621      *
228800040621      * Se NON lo trovo: segnalo il rec. di dft come omesso ("XXX")
228900040623if  1c                   if        $Found  = *off
229000040623     c*** troppo:        clear                   FLXR9ds
229100040623     c                   clear                   XR9bai
229200161213     c                   clear                   XR9des
229300040623     c                   clear                   XR9pdc
229400040623     c                   clear                   XR9npg
229500040623     c                   clear                   XR9nfv
229600040623     c                   clear                   XR9dfv
229700040623     c                   clear                   XR9fl1
229800040623     c                   clear                   XR9fl2
229900040623     c                   clear                   XR9fl3
230000040623     c                   eval      XR9bai  = C_PostazNull
230100040621e   1c                   endif
230200031106      *
230300040315     c                   ENDSR
230400050614      *
230500050614      *---------------------------------------------------------------*
230600050614      * Verifica se:                                                  *
230700050614      * - autotrasportatore MESSAGGERIA per piano di default          *
230800050614      * - distinta MESSAGGERIA per piano di smistamento               *
230900050614      *---------------------------------------------------------------*
231000050614     c     X_Messaggeria BEGSR
231100050614      *
231200050614if  1c                   if        $Default = *on
231300050614      *
231400050614     c                   clear                   S2Dmes
231500050614      * in piano di default: i dati dell'autotrasportatore sono appena
231600050614      *   stati reperiti
231700050614     c*** (già fatto)    movel     APDflr        dAPDflr
231800050614if  2c                   if        §APDtpm  = 'M'
231900050629     c                   eval      S2Dmes   = C_Messaggeria
232000050614e   2c                   endif
232100050614      *
232200050614x   1c                   else
232300050614      *
232400050614if  2c                   if        wNFVseq  = 1
232500050614     c                   clear                   S2Dmes
232600050614x   2c                   else
232700050614     c                   clear                   S4Dmes
232800050614e   2c                   endif
232900050614      * in piano di smistamento: i dati del F.V. sono appena
233000050614      *   stati reperiti da FNFVV
233100050701     c*** (già fatto)    movel     FVVspn        dFVVspn
233200050614if  2c                   if        §FVVtpm  = 'M'
233300050614if  3c                   if        wNFVseq  = 1
233400050629     c                   eval      S2Dmes   = C_Messaggeria
233500050614x   3c                   else
233600050629     c                   eval      S4Dmes   = C_Messaggeria
233700050614e   3c                   endif
233800050614e   2c                   endif
233900050614      *
234000050614e   1c                   endif
234100050614      *
234200050614     c                   ENDSR
234300040701      *
234400040701      *---------------------------------------------------------------*
234500040701      * Caricamento dati di default in F.V. per P.O. senza VDL        *
234600040701      *---------------------------------------------------------------*
234700040701     c     CarNoVDL_1    BEGSR
234800040701      *
234900040701     c                   move      TR9pep        S1Cpep
235000040701     c                   eval      S1Cdes   = TR9des
235100040701     c                   eval      S1Cbai   = XR9bai
235200040701if  1c                   if        $Found   = *on
235300040701sel 2c                   select
235400040701w   2c                   when      $IMA     = *on
235500040701     c                   movel     $MPTdes(yy)   S1Dpdc
235600040701     c                   movel     $MPTdes(yy)   S3Dpdc
235700040701w   2c*** solo x VDL:    when      $PO      = *on
235800040701w   2c                   when      XR9pdc  <> *zeros
235900040701     c                   move      XR9pdc        S1Cpdc
236000040701     c                   eval      APDpdr   = XR9pdc
236100040701     c                   exsr      CtrlAutotr2
236200040701     c                   movel     APDrsc        S1Dpdc
236300040701     c                   movel     APDrsc        S3Dpdc
236400040701     c                   setoff                                       552890
236500040701     c                   clear                   V1Dmsg
236600040701e   2c                   endsl
236700040701e   1c                   endif
236800040701      * - - - Campi hidden
236900040701if  1c                   if        $Default = *off
237000060113if  2c                   if            $NewFVa = *off
237100060113     c                             and $DltPS  = *off
237200060113     c                             and $Found  = *on
237300060113     c                   eval      H1Cprg   = XR9prg
237400060113x   2c                   else
237500060113     c                   eval      H1Cprg   = (TR9prg * 2) - 1
237600060113e   2c                   endif
237700040701     c                   eval      H1Cbai   = TR9bai
237800040701     c                   eval      H1Cpdc   = TR9pdc
237900040701if  2c                   if            $IMAdft = *off
238000040701     c                             and H1Cpdc <> APDpdr
238100040701     c                   eval      APDpdr   = H1Cpdc
238200040701     c                   exsr      CtrlAutotr2
238300040701     c                   setoff                                       552890
238400040701     c                   clear                   V1Dmsg
238500040701e   2c                   endif
238600040701if  2c                   if        $IMAdft  = *off
238700040701     c                   eval      H1Dpdc   = APDrsc
238800040701x   2c                   else
238900040701     c                   eval      H1Dpdc   = $MPTdes(yy)
239000040701e   2c                   endif
239100040701e   1c                   endif
239200040701      *
239300040701     c                   ENDSR
239400040701      *
239500040701      *---------------------------------------------------------------*
239600040701      * Impostazione dei F.V. in piano giornaliero per P.O. senza VDL *
239700040701      *---------------------------------------------------------------*
239800040701     c     CarNoVDL_2    BEGSR
239900040701      *
240000040701if  1c                   if        $Found   = *off
240100040701      *
240200040701     c                   eval      S1Cbai   = C_PostazNull
240300040701     c                   eval      S1Kdft   = C_Default
240400040701     c                   eval      S1Kbad   = C_Postazione
240500040701     c                   eval      S1Cbad   = H1Cbai
240600040701if  2c                   if        H1Cpdc  <> *zeros
240700040701     c                   eval      S1Katd   = C_Autotr
240800040701     c                   move      H1Cpdc        S1Catd
240900040701e   2c                   endif
241000040701     c                   eval      S1Datd   = H1Dpdc
241100040701      *
241200040701x   1c                   else
241300040701      *
241400040701     c                   move      XR9nfv        S1Cnfv
241500040701     c                   move      DSdata6       S1Ddfv
241600040701     c                   eval      S1Dfpp   = XR9fl1
241700040701if  2c                   if        $IMA     = *off
241800040702     c                             and $PO  = *off
241900040701     c                   exsr      CercaIIfv
242000040701e   2c                   endif
242100040701      *
242200040701e   1c                   endif
242300040701      *
242400040701     c                   ENDSR
242500040701      *
242600040701      *---------------------------------------------------------------*
242700040701      * Caricamento dati di default in F.V. per P.O.  con  VDL        *
242800040701      *---------------------------------------------------------------*
242900040701     c     CarSiVDL_1    BEGSR
243000040701      *
243100040701     c                   movel     TR9pep        S2Cpep
243200040701     c                   eval      S2Cdes   = TR9des
243300040701     c                   eval      S2Cbnl   = TR9bnl
243400040701     c                   eval      S2Cbai   = XR9bai
243500161213     c                   if        $PO      = *on
243600161215     c                   eval      S2Cznc   = %subst( XR9des : 29 )
243700161213     c                   endif
243800040701      *
243900040701if  1c                   IF        $Found   = *on
244000040701      *
244100040701sel 2c                   SELECT
244200040701      *
244300040701w   2c                   WHEN      $IMA     = *on
244400040701     c                   movel     $MPTdes(yy)   S2Dpdc
244500040701     c                   movel     $MPTdes(yy)   S4Dpdc
244600040701      *
244700040701w   2c                   WHEN      $PO      = *on
244800040701     c                   movel     $ORGdes(xy)   S2Dpdc
244900040701     c                   movel     $ORGdes(xy)   S4Dpdc
245000040701      *
245100040701w   2c                   WHEN      XR9pdc  <> *zeros
245200040701     c                   move      XR9pdc        S2Cpdc
245300040701     c                   eval      APDpdr   = XR9pdc
245400040701     c                   exsr      CtrlAutotr2
245500040701     c                   movel     APDrsc        S2Dpdc
245600040701     c                   movel     APDrsc        S4Dpdc
245700050629      * - imposto il flag per autotrasportatore Messaggeria
245800050614if  3c                   if            $Default = *on
245900050614     c                             and §OGpkm   = 'S'
246000050629     c                             and $VDL     = *on
246100050614     c                             and $IMA     = *off
246200050614     c                             and $PO      = *off
246300050701     c                   eval      wNFVseq  = 1
246400050614     c                   movel     APDflr        dAPDflr
246500050629     c                   exsr      X_Messaggeria
246600050614e   3c                   endif
246700040701     c                   setoff                                       552890
246800040701     c                   clear                   V1Dmsg
246900040701      *
247000040701w   2c                   WHEN      TR9bnl   = 'S'
247100040701     c                   eval      S4Dpdc   = C_BaiaNonLetti
247200040701      *
247300040701e   2c                   ENDSL
247400040701      *
247500040701e   1c                   ENDIF
247600040701      *
247700040701      * - - - Campi hidden
247800060113if  1c                   if        $Default = *off
247900060113if  2c                   if            $NewFVa = *off
248000060113     c                             and $DltPS  = *off
248100060113     c                             and $Found  = *on
248200060113     c                   eval      H2Cprg   = XR9prg
248300060113x   2c                   else
248400060113     c                   eval      H2Cprg   = (TR9prg * 2) - 1
248500060113e   2c                   endif
248600060113e   1c                   endif
248700040701if  1c                   IF            $Default = *off
248800040701     c                             and TR9bnl  <> 'S'
248900040701      *
249000040701     c                   eval      H2Cbai   = TR9bai
249100161213if  2c                   if        $POdft   = *on
249200161215     c                   eval      H2Cznc   = %subst( TR9des : 29 )
249300161213e   2c                   endif
249400040701     c                   eval      H2Cpdc   = TR9pdc
249500040701      *
249600040701if  2c                   if            $IMAdft = *off
249700040701     c                             and $POdft  = *off
249800040701     c                             and H2Cpdc <> APDpdr
249900040701     c                   eval      APDpdr   = H2Cpdc
250000040701     c                   exsr      CtrlAutotr2
250100040701     c                   setoff                                       552890
250200040701     c                   clear                   V1Dmsg
250300040701e   2c                   endif
250400040701      *
250500040701sel 2c                   Select
250600040701w   2c                   When      $IMAdft  = *off   and
250700040701     c                             $POdft   = *off
250800040701     c                   eval      H2Dpdc   = APDrsc
250900040701w   2c                   When      $IMAdft  = *on
251000040701     c                   eval      H2Dpdc   = $MPTdes(yy)
251100040701w   2c                   When      $POdft   = *on
251200040701     c                   eval      H2Dpdc   = $ORGdes(xy)
251300040701e   2c                   EndSl
251400040701      *
251500040701e   1c                   ENDIF
251600040701      *
251700040701     c                   ENDSR
251800040701      *
251900040701      *---------------------------------------------------------------*
252000040701      * Impostazione dei F.V. in piano giornaliero per P.O.  con  VDL *
252100040701      *---------------------------------------------------------------*
252200040701     c     CarSiVDL_2    BEGSR
252300040701      *
252400040701sel 1c                   Select
252500040701      *
252600040701w   1c                   When      TR9bnl   = 'S'
252700040701      *
252800040701w   1c                   When          $Found =  *off
252900040701     c                             and TR9bnl <> 'S'
253000040701     c                             and TR9bai <> C_PostazNull
253100040701     c                   eval      S2Cbai   = C_PostazNull
253200040701     c                   eval      S2Kdft   = C_Default
253300040701     c                   eval      S2Kbad   = C_Postazione
253400040701     c                   eval      S2Cbad   = H2Cbai
253500040701if  2c                   if        H2Cpdc  <> *zeros
253600040701     c                   eval      S2Katd   = C_Autotr
253700040701     c                   move      H2Cpdc        S2Catd
253800040701e   2c                   endif
253900040701     c                   eval      S2Datd   = H2Dpdc
254000040701      *
254100040701w   1c                   When          $Found =  *on
254200040701     c                             and XR9nfv <> *zeros
254300040701     c                   move      XR9nfv        S2Cnfv
254400040701     c                   move      DSdata6       S2Ddfv
254500040701     c                   eval      S2Dfpp   = XR9fl1
254600050629      * - reperisco il flag per autotrasportatore Messaggeria
254700050629if  2c                   if            $Default = *off
254800050629     c                             and §OGpkm   = 'S'
254900050629     c                             and $VDL     = *on
255000050629     c                             and $IMA     = *off
255100050629     c                             and $PO      = *off
255200050701     c                   eval      FVVfgs   = V1Cpog
255300050701     c                   eval      FVVnpg   = 4
255400050629     c                   move      XR9nfv        FVVnfv
255500050629     c     K03FNFVV      chain     FNFVV000
255600050629if  3c                   if            %found(FNFVV03L)
255700050701     c                   eval      wNFVseq  = 1
255800050629     c                   movel     FVVspn        dFVVspn
255900050629     c                   exsr      X_Messaggeria
256000050629e   3c                   endif
256100050629e   2c                   endif
256200040701if  2c                   if        $IMA     = *off
256300040702     c                             and $PO  = *off
256400040701     c                   exsr      CercaIIfv
256500050629      * - reperisco il flag per autotrasportatore Messaggeria
256600050629if  3c                   if            $Default = *off
256700050629     c                             and §OGpkm   = 'S'
256800050629     c                             and $VDL     = *on
256900050629     c                             and $IMA     = *off
257000050629     c                             and $PO      = *off
257100050629     c                             and %found(FLTR901X)
257200050629     c                             and XR9nfv   > *zeros
257300050701     c                   eval      FVVfgs   = V1Cpog
257400050701     c                   eval      FVVnpg   = 4
257500050629     c                   move      XR9nfv        FVVnfv
257600050629     c     K03FNFVV      chain     FNFVV000
257700050629if  4c                   if            %found(FNFVV03L)
257800050701     c                   eval      wNFVseq  = 2
257900050629     c                   movel     FVVspn        dFVVspn
258000050629     c                   exsr      X_Messaggeria
258100050629e   4c                   endif
258200050629e   3c                   endif
258300040701e   2c                   endif
258400040701      *
258500040701e   1c                   EndSl
258600040701      *
258700040701     c                   ENDSR
258800040607      *
258900040607      *---------------------------------------------------------------*
259000040615      * Verifica se Postazione = P.O. (...magari DPD) !!!             *
259100040607      *---------------------------------------------------------------*
259200040607     c     Ctrl_PO       BEGSR
259300040607      *
259400040607if  1c                   if        $Hidden  = *off
259500040607     c                   reset                   $PO
259600040621     c                   reset                   $POdpd
259700080129     c                   reset                   $POerr
259800040607x   1c                   else
259900040607     c                   reset                   $POdft
260000040607e   1c                   endif
260100040607      *
260200040607if  1c                   if            Wpost  <> *blanks
260300040607     c                             and Wpost  <> C_PostazNull
260400040607     c                             and $IMA   =  *off
260500040607     c                             and %subst(Wpost:1:1) >= '0'
260600040607     c                             and %subst(Wpost:1:1) <= '9'
260700040607      *
260800040607     c                   eval      xy       = 1
260900040607     c                   eval      *in10    = *off
261000040607     c     Wpost         lookup    $ORGfil(xy)                            10
261100040607if  2c                   if        $Hidden  = *off
261200040607     c                   eval      $PO      = *in10
261300040607x   2c                   else
261400040607     c                   eval      $POdft   = *in10
261500040607e   2c                   endif
261600040607      *
261700040607if  2c                   if        $Hidden  = *off
261800040607     c                             and $PO  = *on
261900040611     c                   eval      $POdpd   = ($ORGntw(xy) = 'DPD')
262000080129     c                   eval      $POerr   = ($ORGerr(xy) = *on)
262100040607     c                   add       1             $ORGnr(xy)
262200040607e   2c                   endif
262300040607      *
262400040607e   1c                   endif
262500040607      *
262600040607     c                   ENDSR
262700040408      *
262800040408      *---------------------------------------------------------------*
262900040408      * Verifica se postazione "IMA"                                  *
263000040408      *---------------------------------------------------------------*
263100040408     c     Ctrl_IMA      BEGSR
263200040408      *
263300040423if  1c                   if        $Hidden  = *off
263400040408     c                   reset                   $IMA
263500040423x   1c                   else
263600040423     c                   reset                   $IMAdft
263700040423e   1c                   endif
263800040408      *
263900040423if  1c                   if            Wpost  <> *blanks
264000040423     c                             and Wpost  <> C_PostazNull
264100040408      *
264200040423     c                   eval      yy       = 1
264300040408     c                   eval      *in10    = *off
264400040423     c     Wpost         lookup    $MPT(yy)                               10
264500040423if  2c                   if        $Hidden  = *off
264600040408     c                   eval      $IMA     = *in10
264700040423x   2c                   else
264800040423     c                   eval      $IMAdft  = *in10
264900040423e   2c                   endif
265000040423      *
265100040408e   1c                   endif
265200040408      *
265300040408     c                   ENDSR
265400040705      *
265500040705      *---------------------------------------------------------------*
265600040705      * Impostazione F.V. da default per postazione = P.O. DPD        *
265700040705      *---------------------------------------------------------------*
265800040705     c     PostPOdpd     BEGSR
265900040705      *
266000040705     c                   eval      xx     = 1
266100040705     c                   eval      *in10  = *off
266200040705     c                   reset                   Wpost
266300040705if  1c                   if        $VDL   = *off
266400040705     c                   move      S1Cbai        Wpost
266500040705x   1c                   else
266600040705     c                   move      S2Cbai        Wpost
266700040705e   1c                   endif
266800040705      *
266900040705do  1c                   DOW       xx    <= %elem($FVxPOa)
267000040705      *
267100040705     c     Wpost         lookup    $FVxPOa(xx)                            10
267200040705      *
267300040705      * Nessun f.v. reperito per il DPD in esame
267400040705if  2c                   if        NOT *in10
267500040705     c                   leave
267600040705e   2c                   endif
267700040705      *
267800040705      * Scarto i f.v. con p.o. DPD tra "altri P.O." del f.v.
267900040705if  2c                   if            *in10       = *on
268000040705     c                             and $FVxPOn(xx) < *zeros
268100040705     c                   eval      xx     = xx + 1
268200040705     c                   iter
268300040705e   2c                   endif
268400040705      *
268500040705      * Propongo il 1° f.v. con LNA = p.o. DPD
268600040705if  2c                   IF        *in10  = *on
268700040705      *
268800040705     c                   reset                   WLBda8
268900040705     c                   reset                   DSdata8
269000040705     c                   reset                   DSdata6
269100040705     c                   eval      G08inv = $FVxPOd(xx)
269200040705     c                   call      'XSRDA8'
269300040705     c                   parm                    WLBda8
269400040705     c                   move      G08dat        DSdata8
269500040705     c                   z-add     DSdt8gg       DSdt6gg
269600040705     c                   z-add     DSdt8mm       DSdt6mm
269700040705     c                   z-add     DSdt8aa       DSdt6aa
269800040705if  3c                   if        $VDL   = *off
269900040705     c                   move      $FVxPOn(xx)   S1Cnfv
270000040705     c                   move      DSdata6       S1Ddfv
270100040705x   3c                   else
270200040705     c                   move      $FVxPOn(xx)   S2Cnfv
270300040705     c                   move      DSdata6       S2Ddfv
270400040705e   3c                   endif
270500040705     c                   leave
270600040705      *
270700040705e   2c                   ENDIF
270800040705      *
270900040705e   1c                   ENDDO
271000040705      *
271100040705     c                   ENDSR
271200031106      *
271300031106      *---------------------------------------------------------------*
271400040615      * Impostazione F.V. da default per postazione IMA               *
271500031106      *---------------------------------------------------------------*
271600040615     c     PostIMA       BEGSR
271700031106      *
271800031106     c                   eval      FVVfgs = V1Cpog
271900031106     c                   eval      FVVnpg = 3
272000031106     c                   eval      FVVnfv = *hival
272100031106     c     K03FNFVV      setgt     FNFVV000
272200031106     c     K02FNFVV      readpe    FNFVV000
272300031106      *
272400031106do  1c                   DOW       NOT %eof(FNFVV03L)
272500031106      *
272600031106if  2c                   IF            FVVatb = *blanks
272700031106     c                             and FVVfcf = *blanks
272800031106     c                             and FVVspg = 'A'
272900031106     c                   reset                   WLBda8
273000031106     c                   reset                   DSdata8
273100031106     c                   reset                   DSdata6
273200031106     c                   eval      G08inv = FVVdfv
273300031106     c                   call      'XSRDA8'
273400031106     c                   parm                    WLBda8
273500031106     c                   move      G08dat        DSdata8
273600031106     c                   z-add     DSdt8gg       DSdt6gg
273700031106     c                   z-add     DSdt8mm       DSdt6mm
273800031106     c                   z-add     DSdt8aa       DSdt6aa
273900031106if  3c                   if        $VDL   = *off
274000031106     c                   move      *zeros        S1Cnfv
274100031106     c                   move      FVVnfv        S1Cnfv
274200031106     c                   move      DSdata6       S1Ddfv
274300031106     c                   eval      S1Dfpp = FVVfpp
274400031106x   3c                   else
274500031106     c                   move      *zeros        S2Cnfv
274600031106     c                   move      FVVnfv        S2Cnfv
274700031106     c                   move      DSdata6       S2Ddfv
274800031106     c                   eval      S2Dfpp = FVVfpp
274900031106e   3c                   endif
275000031106     c                   leave
275100031106e   2c                   ENDIF
275200031106      *
275300031106     c     K02FNFVV      readpe    FNFVV000
275400031106e   1c                   ENDDO
275500031106      *
275600031106     c                   ENDSR
275700031106      *
275800031106      *---------------------------------------------------------------*
275900040615      * Impostazione F.V. da default per postazione non IMA           *
276000031106      *---------------------------------------------------------------*
276100040615     c     PostNoIMA     BEGSR
276200031106      *
276300031106     c                   eval      xx     = 1
276400031106     c                   eval      *in10  = *off
276500031106     c                   movel     V1Cpog        Wpdr
276600031106if  1c                   if        $VDL   = *off
276700031106     c                   move      S1Cpdc        Wpdr
276800031106x   1c                   else
276900031106     c                   move      S2Cpdc        Wpdr
277000031106e   1c                   endif
277100031106     c     Wpdr          lookup    $MEMat(xx)                             10
277200050701      *
277300031111if  1c                   IF        *in10  = *on
277400050701      *
277500050701     c                   clear                   dFVVspn
277600031111      * - 1° f.v.
277700031111if  2c                   IF        $MEMfv1(xx) > *zeros
277800031106      *
277900031111     c                   move      $MEMfv1(xx)   DS_FV
278000031106     c                   reset                   WLBda8
278100031106     c                   reset                   DSdata8
278200031106     c                   reset                   DSdata6
278300031106     c                   eval      G08inv = dsFVdfv
278400031106     c                   call      'XSRDA8'
278500031106     c                   parm                    WLBda8
278600031106     c                   move      G08dat        DSdata8
278700031106     c                   z-add     DSdt8gg       DSdt6gg
278800031106     c                   z-add     DSdt8mm       DSdt6mm
278900031106     c                   z-add     DSdt8aa       DSdt6aa
279000031111if  3c                   if        $VDL   = *off
279100031106     c                   move      dsFVnfv       S1Cnfv
279200031106     c                   move      DSdata6       S1Ddfv
279300031111     c                   eval      S1Dfpp = $MEMmp1(xx)
279400031111x   3c                   else
279500031106     c                   move      dsFVnfv       S2Cnfv
279600031106     c                   move      DSdata6       S2Ddfv
279700031111     c                   eval      S2Dfpp = $MEMmp1(xx)
279800050629      *   - imposto il flag per autotrasportatore Messaggeria
279900050614if  4c                   if            $Default = *off
280000050614     c                             and §OGpkm   = 'S'
280100050629     c                             and $VDL     = *on
280200050614     c                             and $IMA     = *off
280300050614     c                             and $PO      = *off
280400050701     c                   eval      wNFVseq = 1
280500050701     c                   eval      §FVVtpm = $MEMpm1(xx)
280600050614     c                   exsr      X_Messaggeria
280700050614e   4c                   endif
280800031111e   3c                   endif
280900031111      *
281000031111e   2c                   ENDIF
281100031111      * - 2° f.v.
281200031111if  2c                   IF        $MEMfv2(xx) > *zeros
281300031111      *
281400031111     c                   move      $MEMfv2(xx)   DS_FV
281500031111     c                   reset                   WLBda8
281600031111     c                   reset                   DSdata8
281700031111     c                   reset                   DSdata6
281800031111     c                   eval      G08inv = dsFVdfv
281900031111     c                   call      'XSRDA8'
282000031111     c                   parm                    WLBda8
282100031111     c                   move      G08dat        DSdata8
282200031111     c                   z-add     DSdt8gg       DSdt6gg
282300031111     c                   z-add     DSdt8mm       DSdt6mm
282400031111     c                   z-add     DSdt8aa       DSdt6aa
282500031111if  3c                   if        $VDL   = *off
282600031111     c                   move      dsFVnfv       S3Cnfv
282700031111     c                   move      DSdata6       S3Ddfv
282800031111     c                   eval      S3Dfpp = $MEMmp2(xx)
282900031111x   3c                   else
283000031111     c                   move      dsFVnfv       S4Cnfv
283100031111     c                   move      DSdata6       S4Ddfv
283200031111     c                   eval      S4Dfpp = $MEMmp2(xx)
283300050629      *   - imposto il flag per autotrasportatore Messaggeria
283400050614if  4c                   if            $Default = *off
283500050614     c                             and §OGpkm   = 'S'
283600050629     c                             and $VDL     = *on
283700050614     c                             and $IMA     = *off
283800050614     c                             and $PO      = *off
283900050701     c                   eval      wNFVseq = 2
284000050701     c                   eval      §FVVtpm = $MEMpm2(xx)
284100050614     c                   exsr      X_Messaggeria
284200050614e   4c                   endif
284300031111e   3c                   endif
284400031111      *
284500031111e   2c                   ENDIF
284600031106      *
284700031106e   1c                   ENDIF
284800031106      *
284900031106     c                   ENDSR
285000031106      *
285100031106      *---------------------------------------------------------------*
285200031106      * Reperimento eventuale secondo F.V. per postazione/baia        *
285300031106      *---------------------------------------------------------------*
285400031106     c     CercaIIfv     BegSr
285500031106      *
285600040621      * (Se modifica del "piano giornaliero" per P.O. con/senza VDL)
285700040621      *
285800040621     c                   eval      XR9prg = XR9prg + 1
285900040621     c     K06TR901      chain     FLTR901
286000031106      *
286100040621      * Se esiste: ne visualizzo i dati del f.v.
286200040621if  1c                   if        %found(FLTR901X)
286300040309     c                   reset                   WLBda8
286400040309     c                   reset                   DSdata8
286500040309     c                   reset                   DSdata6
286600040621if  2c                   if        XR9dfv   > *zeros
286700040621     c                   eval      G08inv   = XR9dfv
286800040309     c                   call      'XSRDA8'
286900040309     c                   parm                    WLBda8
287000040309     c                   move      G08dat        DSdata8
287100040309     c                   z-add     DSdt8gg       DSdt6gg
287200040309     c                   z-add     DSdt8mm       DSdt6mm
287300040309     c                   z-add     DSdt8aa       DSdt6aa
287400040309e   2c                   endif
287500040315if  2c                   if        $VDL   = *off
287600040621     c                   move      XR9nfv        S3Cnfv
287700040621     c                   eval      S3Dfpp   = XR9fl1
287800040621if  3c                   if        XR9dfv   > *zeros
287900031106     c                   move      DSdata6       S3Ddfv
288000040309e   3c                   endif
288100040315x   2c                   else
288200040621     c                   move      XR9nfv        S4Cnfv
288300040621     c                   eval      S4Dfpp   = XR9fl1
288400040621if  3c                   if        XR9dfv   > *zeros
288500040315     c                   move      DSdata6       S4Ddfv
288600040315e   3c                   endif
288700040315e   2c                   endif
288800031106e   1c                   endif
288900031106      *
289000031106     c                   ENDSR
289100031007      *
289200031007      *---------------------------------------------------------------*
289300031007      * Controllo dati immessi in videata S01 & S03                   *
289400031007      *---------------------------------------------------------------*
289500031007     c     CtrS01        BEGSR
289600031007      *
289700031007      * Ciclo di lettura del sfl
289800031007if  1c                   if        $Default = *on
289900031007     c                   readc     MS03S01
290000031007x   1c                   else
290100031007     c                   readc     MS03S03
290200031007e   1c                   endif
290300031020      *
290400031007do  1c                   DOW       NOT %eof(FIMS03D)
290500031007      *
290600031007     c                   movea     *zeros        *in(41)
290700031007     c                   z-add     S01nrr        C01rcd
290800031007      * - ctrl su singola riga
290900031014if  2c                   if            S1Cpep  <> *blanks
291000031014     c                             or  S1Cdes  <> *blanks
291100031014     c                             or  S1Cbai  <> *blanks
291200031014     c                             or (S1Cpdc  <> *zeros
291300031014     c                             and S1Cpdc  <> *blanks)
291400031014     c                             or (S1Cnfv  <> *blanks
291500031014     c                             and S1Cnfv  <> *zeros)
291600031007     c                   exsr      CtrS01rec
291700031020if  3c                   if            $Default = *off
291800031007     c                             and *in90    = *off
291900040518     c                             and S1Cbai  <> C_PostazNull
292000031007     c                   exsr      CtrS03rec
292100031007e   3c                   endif
292200031007x   2c                   else
292300031007     c                   clear                   S1Ddfv
292400031021     c                   clear                   S1Dfpp
292500031022     c                   clear                   S3Ddfv
292600031022     c                   clear                   S3Dfpp
292700031023     c                   clear                   S1Cast
292800031007     c                   clear                   S1Cpdc
292900031007     c                   clear                   S1Dpdc
293000031007     c                   clear                   S3Dpdc
293100031023     c                   clear                   S1Cdes
293200031023     c                   clear                   S1Kdft
293300031023     c                   clear                   S1Kbad
293400031023     c                   clear                   S1Cbad
293500031023     c                   clear                   S1Katd
293600031023     c                   clear                   S1Catd
293700031023     c                   clear                   S1Datd
293800031007e   2c                   endif
293900031010      *
294000031010      * - reimposto indicatori x DSPATR condizionati
294100040702      *   se NON rilevati errori
294200040702if  2c                   if        NOT *in90
294300040702     c                             and $Default = *on
294400040702     c                             or  $Default = *off
294500060110     c                   exsr      AtrS01S03
294600040702e   2c                   endif
294700031007      *
294800031007      * - riattivo SFLNXTCHG (non solo se rilevato errore)
294900031007     c                   eval      *in32    = *on
295000031007      * - aggiorno il SFL
295100031007if  2c                   if        $Default = *on
295200031007     c                   update    MS03S01
295300031007x   2c                   else
295400031007     c                   update    MS03S03
295500031007e   2c                   endif
295600031007     c                   eval      *in32    = *off
295700031007      * - se rilevato errore esco dal ciclo di controllo
295800031007if  2c                   if        *in28    = *on
295900031104     c                   z-add     C01rcd        C01csr
296000031007     c                   leave
296100031007e   2c                   endif
296200031007      * - leggo il record successivo
296300031007if  2c                   if        $Default = *on
296400031007     c                   readc     MS03S01
296500031007x   2c                   else
296600031007     c                   readc     MS03S03
296700031007e   2c                   endif
296800031007      *
296900031007e   1c                   ENDDO
297000031020      *
297100040423      * Controlli finali "generali"
297200031021      *   (se NON ho già rilevato altri errori da segnalare)
297300040423if  1c                   if        *in28    = *off
297400031021     c                   exsr      CtrBAIxPEP
297500031020e   1c                   endif
297600031007      *
297700031007     c                   ENDSR
297800031007      *
297900031007      *---------------------------------------------------------------*
298000031007      * Controllo singolo record del subfile S01 & S03                *
298100031007      *---------------------------------------------------------------*
298200031007     c     CtrS01rec     BEGSR
298300031023      *
298400031023if  1c                   if        $Default = *off
298500031023     c                   clear                   S1Kdft
298600031023     c                   clear                   S1Kbad
298700031023     c                   clear                   S1Cbad
298800031023     c                   clear                   S1Katd
298900031023     c                   clear                   S1Catd
299000031023     c                   clear                   S1Datd
299100031023e   1c                   endif
299200040423      *
299300040423     c                   reset                   $IMA
299400040423     c                   reset                   $IMAdft
299500031007      *
299600040305      * Controllo codice baia
299700031007if  1c                   if        S1Cpep = *blanks
299800031007     c                   seton                                        512890
299900031009     c                   movel     $Msg(10)      V1Dmsg
300000031007x   1c                   else
300100031010     c                   move(p)   S1Cpep        w014a
300200031010     c     w014a         lookup    $MSLpep                                10
300300031007if  2c                   if        NOT *in10
300400031007     c                   seton                                        512890
300500031009     c                   movel     $Msg(11)      V1Dmsg
300600060110     c                   leavesr
300700031007e   2c                   endif
300800040226     c                   exsr      ContaPEP
300900031007e   1c                   endif
301000060110     c                   if        *in90
301100060110     c                   leavesr
301200060110     c                   endif
301300031007      *
301400031007      * Controllo descrizione
301500031014      * (non obbligatoria)
301600031007      *
301700040305      * Controllo codice postazione
301800040423      * - ricerca
301900040423     c     '?'           scan      S1Cbai                                 10
302000040423if  1c                   if        *in10  =  *on
302100040423     c                   eval      S1Cbai =  *blanks
302200040423     c                   eval      Wcampo =  'S1CBAI    '
302300040423     c                   exsr      srSEARCH
302400040423e   1c                   endif
302500040423      * - controllo
302600040423if  1c                   if        $Default = *off
302700040423     c                   eval      $Hidden  = *on
302800040423     c                   eval      Wpost    = H1Cbai
302900040423     c                   exsr      Ctrl_IMA
303000040423if  2c                   if        $IMAdft  = *on
303100040423     c                   eval      H1Dpdc   = $MPTdes(yy)
303200040423e   2c                   endif
303300040423e   1c                   endif
303400040423     c                   eval      $Hidden  = *off
303500040423     c                   eval      Wpost    = S1Cbai
303600040408     c                   exsr      Ctrl_IMA
303700040423if  1c                   if        $IMA     = *on
303800040423     c                   eval      S1Dpdc   = $MPTdes(yy)
303900040423     c                   eval      S3Dpdc   = $MPTdes(yy)
304000040423e   1c                   endif
304100040423if  1c                   if            $Default = *off
304200040423     c                             and $IMAdft  = *on
304300040423     c                             and H1Cbai  <> S1Cbai
304400040423     c                   eval      S1Datd   = H1Dpdc
304500040423e   2c                   endif
304600031007sel 1c                   select
304700031020w   1c                   when          $Default = *off
304800040316     c                             and S1Cbai   = C_PostazNull
304900031020     c                   clear                   S1Cnfv
305000031020     c                   clear                   S1Ddfv
305100031021     c                   clear                   S1Dfpp
305200031211     c                   clear                   S3Cnfv
305300031211     c                   clear                   S3Ddfv
305400031211     c                   clear                   S3Dfpp
305500031020     c                   clear                   S3Dpdc
305600040408     c                   clear                   S1Cpdc
305700031007w   1c                   when      S1Cbai   =  *blanks
305800031007     c                   seton                                        532890
305900031009     c                   movel     $Msg(13)      V1Dmsg
306000070411w   1c                   when         (%subst(S1Cbai:1:1) >= 'A'
306100070411     c                             and %subst(S1Cbai:1:1) <= 'Z')
306200031010     c                             and %subst(S1Cbai:2:2) >= '01'
306300031010     c                             and %subst(S1Cbai:2:2) <= '99'
306400031007     c                   move      S1Cbai        w002a
306500031007     c     DigitN        check     w002a                                  10
306600031009if  2c                   if        *in10  =  *on
306700031007     c                   seton                                        532890
306800031009     c                   movel     $Msg(14)      V1Dmsg
306900031007x   2c                   else
307000040423     c                   eval      Wpost  =  S1Cbai
307100031007     c                   exsr      ContaBAI
307200031007if  3c                   if        $BAInr(xx) >  1
307300031007     c                   seton                                        532890
307400031009     c                   movel     $Msg(15)      V1Dmsg
307500031007e   3c                   endif
307600031007e   2c                   endif
307700040408w   1c                   when      $IMA   =  *off
307800031007     c                   seton                                        532890
307900031009     c                   movel     $Msg(14)      V1Dmsg
308000031007e   1c                   endsl
308100060110     c                   if        *in90
308200060110     c                   leavesr
308300060110     c                   endif
308400040316      *
308500040316      * Controllo la postazione sia "compatibile" con le altre già
308600040316      *   immesse nella stessa baia
308700161110      * -?Su richiesta di Nicola:?tolto controllo postaz. IMA/non-IMA
308800161110      *   (ora IMA e AUT possono stare nella stessa baia;
308900161110      *   sono le FILIALI che continuano a richiedere una loro baia -
309000161110      *   - MA SONO INSERIBILI SOLO NEI PIANI VDL, non in questi)
309100040316      *
309200040305      * ...e ne memorizzo gli utilizzi per baia
309300040316if  1c                   if        S1Cbai <> C_PostazNull
309400031021     c                   exsr      MemBAIxPEP
309500031020e   1c                   endif
309600040315      *
309700040305      * Visualizzo la postazione di default se variata in piano
309800040305      *   giornaliero
309900031023if  1c                   if            $Default = *off
310000031023     c                             and S1Cbai  <> H1Cbai
310100040423     c                   eval      S1Kdft  = C_Default
310200040625     c                   eval      S1Kbad  = C_Postazione
310300040423     c                   eval      S1Cbad  = H1Cbai
310400040423if  2c                   if        $IMAdft = *on
310500040423     c                   eval      S1Datd  = H1Dpdc
310600040423e   2c                   endif
310700031023e   1c                   endif
310800031007      *
310900031007      * Controllo codice autotrasportatore
311000031007      *   (in S03 e S04 è campo protetto)
311100031015      * Per il piano giornaliero farò appositi controlli nella subr.
311200031015      *   CTRS03REC
311300031007if  1c                   if        $Default = *on
311400040423if  2c                   if        $IMA     = *off
311500031007     c                   clear                   S1Dpdc
311600031009     c                   clear                   S3Dpdc
311700040423e   2c                   endif
311800031007      * - se *blanks lo considero *zeros (sarebbe numerico)
311900031007if  2c                   if        S1Cpdc =  *blanks
312000031007     c                   eval      S1Cpdc =  *zeros
312100031007e   2c                   endif
312200031007      * - ricerca
312300031007     c     '?'           scan      S1Cpdc                                 10
312400031007if  2c                   if        *in10  =  *on
312500031007     c                   eval      S1Cpdc =  *zeros
312600031007     c                   eval      Wcampo =  'S1CPDC    '
312700031007     c                   exsr      srSEARCH
312800031007e   2c                   endif
312900031007      * - controllo
313000031007sel 2c                   select
313100040409w   2c                   when         ($IMA   =  *on
313200040409     c                              or S1Cbai =  C_PostazNull)
313300031010     c                             and S1Cpdc =  *zeros
313400040409w   2c                   when          $IMA   =  *on
313500031007     c                             and S1Cpdc <> *zeros
313600031007     c                   seton                                        552890
313700031009     c                   movel     $Msg(16)      V1Dmsg
313800031015x   2c                   other
313900031021     c                   movel     V1Cpog        APDpdr
314000031021     c                   move      S1Cpdc        APDpdr
314100031014     c                   exsr      CtrlAutotr2
314200031007     c                   movel     APDrsc        S1Dpdc
314300031009     c                   movel     APDrsc        S3Dpdc
314400031007e   2c                   endsl
314500060110     c                   if        *in90
314600060110     c                   leavesr
314700060110     c                   endif
314800031007      * - se *zeros lo reimposto a *blanks (numerico "editato")
314900031007if  2c                   if        S1Cpdc =  *zeros
315000031007     c                   eval      S1Cpdc =  *blanks
315100031007e   2c                   endif
315200031007e   1c                   endif
315300031027      *
315400040305      * In caso di piano giornaliero controllo se è stata tolta la
315500040305      *   postazione ("XXX"), perchè in tal caso non verrà eseguita
315600040305      *   la subr. per i controlli specifici, pertanto non verrebbe
315700040409      *   evidenziato l'autotrasportatore pre-impostato nel default
315800031027if  1c                   if            $Default = *off
315900040316     c                             and S1Cbai   = C_PostazNull
316000031027     c                   clear                   S1Cpdc
316100031027     c                   clear                   S3Dpdc
316200031027     c                   exsr      DecS03pdr
316300031027e   1c                   endif
316400031007      *
316500060110     c                   ENDSR
316600031022      *
316700031022      *---------------------------------------------------------------*
316800031022      * Controlli aggiuntivi per singolo record del subfile S03       *
316900031022      *---------------------------------------------------------------*
317000031022     c     CtrS03rec     BEGSR
317100031022      *
317200031022     c                   reset                   SAVnfv
317300031022      *
317400031022      * Se impostato solo il 2° numero F.V. lo sposto al 1°
317500031022if  1c                   if            (S1Cnfv  =  *blanks
317600031022     c                             or   S1Cnfv  =  *zeros)
317700031022     c                             and (S3Cnfv  <> *blanks
317800031022     c                             and  S3Cnfv  <> *zeros)
317900031022     c                   eval      S1Cnfv  =  S3Cnfv
318000031022     c                   clear                   S3Cnfv
318100031022     c                   clear                   S3Ddfv
318200031022     c                   clear                   S3Dfpp
318300031022e   1c                   endif
318400031022      *
318500031022      * Controllo 1° numero foglio viaggio
318600031022     c                   clear                   S1Ddfv
318700031022     c                   clear                   S1Dfpp
318800031022     c                   eval      wNFVseq =  1
318900031022     c                   eval      Wnfv    =  S1Cnfv
319000031106     c                   exsr      Ctrl_1FV
319100031022     c                   eval      S1Cnfv  =  Wnfv
319200060110     c                   if        *in90
319300060110     c                   leavesr
319400060110     c                   endif
319500031024      * Se tutto bene: ne imposto i dati a video
319600040305if  1c                   if        Wnfv    <> *blanks
319700031022     c                   eval      S1Dfpp  =  FVVfpp
319800031022     c                   move      DSdata6       S1Ddfv
319900040305e   1c                   endif
320000031024      *
320100031024      * Imposto flag e decodifico variazioni relative all'autotrasp.
320200031024      *   (che deve essere comune ad entrambi i F.V.)
320300040312if  1c                   if            S1Cnfv  <> *blanks
320400040408     c                             and $IMA    =  *off
320500031024     c                   move      APDpdr        S1Cpdc
320600031024     c                   movel     APDrsc        S3Dpdc
320700031024x   1c                   else
320800031024     c                   clear                   S1Cpdc
320900040423if  2c                   if        $IMA     = *off
321000031024     c                   clear                   S3Dpdc
321100040423e   2c                   endif
321200031024e   1c                   endif
321300031024     c                   exsr      DecS03pdr
321400031022      *
321500040312      * Controllo 2° numero foglio viaggio (se immesso)
321600031022     c                   clear                   S3Ddfv
321700031022     c                   clear                   S3Dfpp
321800040312if  1c                   if            S3Cnfv <> *blanks
321900040312     c                             and S3Cnfv <> *zeros
322000031022     c                   eval      wNFVseq =  2
322100031022     c                   eval      Wnfv    =  S3Cnfv
322200031106     c                   exsr      Ctrl_1FV
322300031022     c                   eval      S3Cnfv  =  Wnfv
322400060110     c                   if        *in90
322500060110     c                   leavesr
322600060110     c                   endif
322700031024      * Se tutto bene: ne imposto i dati a video
322800031024if  2c                   if        Wnfv    <> *blanks
322900031022     c                   eval      S3Dfpp  =  FVVfpp
323000031022     c                   move      DSdata6       S3Ddfv
323100031024e   2c                   endif
323200031111      * e rieseguo il controllo sul numero di F.V. x autotrasportatore
323300031111      *   (se ne ho selezionato uno da ricerca)
323400031111if  2c                   if        S1Cast  =  '?'
323500031111     c                   clear                   S1Cast
323600031111     c                   exsr      CtrlAutotr3
323700031111     c                   eval      S1Cast =  Wast
323800031111e   2c                   endif
323900040312x   1c                   else
324000040312     c                   clear                   S3Cnfv
324100040312e   1c                   endif
324200031022      *
324300060110     c                   ENDSR
324400031007      *
324500031007      *---------------------------------------------------------------*
324600031106      * Controlli per singolo F.V. del subfile S03 o S04              *
324700031007      *---------------------------------------------------------------*
324800031106     c     Ctrl_1FV      BEGSR
324900040621      *
325000040621     c                   reset                   Wdfv
325100031007      *
325200031007      * Controllo numero foglio viaggio
325300031007      * - se *blanks lo considero *zeros (sarebbe numerico)
325400031106if  1c                   if        Wnfv    =  *blanks
325500031106     c                   eval      Wnfv    =  *zeros
325600031007e   1c                   endif
325700031007      * - ricerca
325800031022     c     '?'           scan      Wnfv                                   10
325900031106if  1c                   if        *in10   =  *on
326000031106     c                   eval      Wnfv    =  *zeros
326100031106sel 2c                   select
326200031106w   2c                   when          $VDL    =  *off
326300031106     c                             and wNFVseq =  1
326400031106     c                   eval      Wcampo  =  'S1CNFV    '
326500031106w   2c                   when          $VDL    =  *off
326600031106     c                             and wNFVseq =  2
326700031106     c                   eval      Wcampo  =  'S3CNFV    '
326800031106w   2c                   when          $VDL    =  *on
326900031106     c                             and wNFVseq =  1
327000031106     c                   eval      Wcampo  =  'S2CNFV    '
327100031106w   2c                   when          $VDL    =  *on
327200031106     c                             and wNFVseq =  2
327300031106     c                   eval      Wcampo  =  'S4CNFV    '
327400031106e   2c                   endsl
327500031007     c                   exsr      srSEARCH
327600031022      * - - se non selezionato nulla sul 2° F.V. -> esco
327700031022if  2c                   if            wNFVseq =  2
327800031022     c                             and Wnfv    =  *zeros
327900031106     c                   eval      Wnfv    =  *blanks
328000060110     c                   leavesr
328100031022e   2c                   endif
328200031111      * - - se selezionato: memorizzo che dovrò ri-controllare se
328300031111      *     padroncino con altri f.v. (non visualizzati)
328400031111     c                   eval      S1Cast =  '?'
328500031007e   1c                   endif
328600031007      * - controlli:
328700031007      * - - numericità
328800031022     c     DigitN        check     Wnfv                                   10
328900031106if  1c                   if        *in10   =  *on
329000031106if  2c                   if        wNFVseq =  2
329100040309     c                   eval      *in56   =  *on
329200031022x   2c                   else
329300040309     c                   eval      *in54   =  *on
329400031022e   2c                   endif
329500031022     c                   seton                                          2890
329600040309     c                   eval      V1Dmsg  =  $Msg(20)
329700060110     c                   leavesr
329800031007e   1c                   endif
329900031007      *
330000031010      * - - effettiva immissione
330100031106if  1c                   if        Wnfv    =  *zeros
330200031106      *
330300031106     c                   clear                   FVVdfv
330400031106     c                   clear                   FVVfpp
330500031106      *
330600080129sel 2c                   select
330700080129      * - - - F.V.?non?più obbligatorio per fil. DPD
330800080129      *       (visualizzato solo un msg di avvertimento)
330900080129w   2c                   when          $VDL    =  *on
331000080129     c                             and $POdpd  =  *on
331100080129     c                             and $POerr  =  *off
331200080129     c                   eval      $POerr  = *on
331300080129     c                   seton                                        542890
331400080129     c                   eval      V1Dmsg  =  $Msg(50)
331500080129      * - - - F.V. obbligatorio per postazione regolare
331600080129w   2c                   when      $POdpd  =  *off
331700080129if  3c                   if        wNFVseq =  2
331800040309     c                   eval      *in56   =  *on
331900080129x   3c                   else
332000040309     c                   eval      *in54   =  *on
332100080129e   3c                   endif
332200031022     c                   seton                                          2890
332300080129if  3c                   if        $IMA    =  *off
332400040312     c                   eval      V1Dmsg  =  $Msg(7)
332500080129x   3c                   else
332600040309     c                   eval      V1Dmsg  =  $Msg(30)
332700080129e   3c                   endif
332800080129e   2c                   endsl
332900080129     c                   if        *in90
333000060110     c                   leavesr
333100080129     c                   endif
333200031106      *
333300031010x   1c                   else
333400031106      *
333500040611      * - - univocità per postazione
333600031030sel 2c                   select
333700031106w   2c                   when      wNFVseq =  1
333800031106     c                   eval      SAVnfv  =  Wnfv
333900040610      * - - - ammesso un solo F.V. per postazione "IMA"
334000031106w   2c                   when          wNFVseq =  2
334100040408     c                             and $IMA    =  *on
334200031106     c                   seton                                        562890
334300040309     c                   eval      V1Dmsg  =  $Msg(37)
334400040611      * - - - ammesso un solo F.V. per postazione P.O. DPD
334500040610w   2c                   when          wNFVseq =  2
334600040611     c                             and $POdpd  =  *on
334700040610     c                   seton                                        562890
334800040610     c                   eval      V1Dmsg  =  $Msg(42)
334900040610      * - - - non ammessi 2 F.V. identici
335000031106w   2c                   when          wNFVseq =  2
335100031106     c                             and SAVnfv  =  Wnfv
335200040408     c                             and $IMA    =  *off
335300031022     c                   seton                                        562890
335400040309     c                   eval      V1Dmsg  =  $Msg(35)
335500031106e   2c                   endsl
335600060110     c                   if        *in90
335700060110     c                   leavesr
335800060110     c                   endif
335900031106      *
336000031030      * - - univocità per foglio arrivi
336100040607if  2c                   if            $IMA    =  *off
336200040607     c                             and $PO     =  *off
336300031030     c                   exsr      ContaNFV
336400031106if  3c                   if        $NFVnr(xx)  >  1
336500031106if  4c                   if        wNFVseq     =  2
336600040309     c                   eval      *in56   =  *on
336700031030x   4c                   else
336800040309     c                   eval      *in54   =  *on
336900031030e   4c                   endif
337000031030     c                   seton                                          2890
337100040518     c                   eval      V1Dmsg  =  %replace(dsBPpos:
337200031106     c                                        $Msg(39):
337300040312     c                                        %scan('&PS':$Msg(39)))
337400060110     c                   leavesr
337500031030e   3c                   endif
337600031030e   2c                   endif
337700031007      * - - esistenza in FNFVV
337800040615if  2c                   if        $PO     =  *off
337900031106     c                   eval      FVVfgs  =  V1Cpog
338000040615if  3c                   if        $IMA    =  *on
338100031106     c                   eval      FVVnpg  =  3
338200040615x   3c                   else
338300031106     c                   eval      FVVnpg  =  4
338400040615e   3c                   endif
338500031022     c                   move      Wnfv          FVVnfv
338600031007     c     K03FNFVV      chain     FNFVV000
338700040615if  3c                   if        NOT %found(FNFVV03L)
338800031106     c*** (già da vl)              or  FVVatb  <> *blanks
338900031106     c*** (già da vl)              or  FVVfcf  <> *blanks
339000040414     c                             or ($IMA    =  *on
339100040408     c                             and FVVspg  <> 'A')
339200040615if  4c                   if        wNFVseq =  2
339300040309     c                   eval      *in56   =  *on
339400040615x   4c                   else
339500040309     c                   eval      *in54   =  *on
339600040615e   4c                   endif
339700031022     c                   seton                                          2890
339800040309     c                   eval      V1Dmsg  =  $Msg(21)
339900060110     c                   leavesr
340000040615e   3c                   endif
340100040621     c                   eval      Wdfv    =  FVVdfv
340200050614      * - - - distinta Messaggeria per piano di smistamento
340300050701if  3c                   if            §OGpkm  =  'S'
340400050701     c                             and $VDL    =  *on
340500050701     c                             and $IMA    =  *off
340600050701     c                             and $PO     =  *off
340700050701     c                             and S2Cbai  <> *blanks
340800050614     c                             and S2Cbai  <> C_PostazNull
340900050629     c                   movel     FVVspn        dFVVspn
341000050614     c                   exsr      X_Messaggeria
341100050614e   3c                   endif
341200040615      * - - esistenza in FNFGV
341300040615x   2c                   else
341400040615     c                   exsr      CtrlFVxPO
341500060110     c                   if        *in90
341600060110     c                   leavesr
341700060110     c                   endif
341800040615e   2c                   endif
341900031007      * - - univocità F.V. IMA nello stesso piano
342000040408if  2c                   if        $IMA    =  *on
342100031106if  3c                   if        SAVnfvIMA   =  *zeros
342200031022     c                   move      Wnfv          SAVnfvIMA
342300031007e   3c                   endif
342400031106if  3c                   if        Wnfv    <> SAVnfvIMA
342500040615if  4c                   if        wNFVseq =  2
342600040309     c                   eval      *in56   =  *on
342700031022x   4c                   else
342800040309     c                   eval      *in54   =  *on
342900031022e   4c                   endif
343000031022     c                   seton                                          2890
343100040518     c                   eval      V1Dmsg  =  %replace(SAVnfvIMA:
343200031106     c                                        $Msg(22):
343300040408     c                                        %scan('&FIMA':$Msg(22)))
343400060110     c                   leavesr
343500031007e   3c                   endif
343600040408      * - - - pulisco l'eventuale cod. autotrasportatore se F.V. IMA
343700040423      *       e ne imposto la decodifica da tab. "MPT"
343800040408if  3c                   if        $VDL    =  *off
343900040408     c                   clear                   S1Cpdc
344000040423     c*** già decodif.:  clear                   S3Dpdc
344100040408x   3c                   else
344200040408     c                   clear                   S2Cpdc
344300040423     c*** già decodif.:  clear                   S4Dpdc
344400040408e   3c                   endif
344500031007e   2c                   endif
344600040615      * - - controllo F.V. non IMA né per P.O. DPD
344700040408if  2c                   if        $IMA    =  *off
344800040615     c                             and $PO =  *off
344900031022      * - - - F.V. di data antecedente a quella del F.V. arrivi
345000031106if  3c                   if        FVVdfv  <  SAVdfva
345100031106if  4c                   if        wNFVseq =  2
345200040309     c                   eval      *in56   =  *on
345300031022x   4c                   else
345400040309     c                   eval      *in54   =  *on
345500031022e   4c                   endif
345600031022     c                   seton                                          2890
345700040309     c                   eval      V1Dmsg  =  $Msg(38)
345800060110     c                   leavesr
345900031022e   3c                   endif
346000031022      * - - - F.V. non IMA assegnato ad altro autotrasportatore
346100040519      *       o ad autotrasportatore già impegnato in altra baia/post.
346200031014     c                   exsr      CtrlAutotr1
346300060110     c                   if        *in90
346400060110     c                   leavesr
346500060110     c                   endif
346600040519      * - - - Controllo/decodifica codice autotrasportatore
346700031106if  3c                   if        wNFVseq <  2
346800031106     c                   eval      APDpdr  =  DSTpdr
346900031020     c                   exsr      CtrlAutotr2
347000060110     c                   if        *in90
347100060110     c                   leavesr
347200060110     c                   endif
347300031022e   3c                   endif
347400031024e   2c                   endif
347500031024e   1c                   endif
347600031007      *
347700031007      * Se OK imposto la data
347800031106     c                   reset                   WLBda8
347900031022     c                   reset                   DSdata8
348000031022     c                   reset                   DSdata6
348100031106if  1c                   if        Wnfv    <> *zeros
348200040621     c                   eval      G08inv  =  Wdfv
348300031007     c                   call      'XSRDA8'
348400031007     c                   parm                    WLBda8
348500031021     c                   move      G08dat        DSdata8
348600031021     c                   z-add     DSdt8gg       DSdt6gg
348700031021     c                   z-add     DSdt8mm       DSdt6mm
348800031021     c                   z-add     DSdt8aa       DSdt6aa
348900031106e   1c                   endif
349000031014      * Se *zeros lo reimposto a *blanks (numerico "editato")
349100031106if  1c                   if        Wnfv    =  *zeros
349200031106     c                   eval      Wnfv    =  *blanks
349300031014e   1c                   endif
349400031111      * memorizzo il F.V. tra quelli immessi nel SFL
349500031111if  1c                   if        Wnfv    >  *zeros
349600031111     c                   eval      xx      =  1
349700031111     c                   eval      *in10   =  *off
349800031111     c     *zeros        lookup    $FV(xx)                                10
349900031111if  2c                   if        *in10   =  *on
350000031111     c                   move      Wnfv          $FV(xx)
350100031111e   2c                   endif
350200031111e   1c                   endif
350300031007      *
350400060110     c                   ENDSR
350500040615      *
350600040615      *---------------------------------------------------------------*
350700040621      * Controlli per singolo foglio viaggio per punto operativo      *
350800040615      *---------------------------------------------------------------*
350900040615     c     CtrlFVxPO     BEGSR
351000040615      *
351100040621      * - Verifica esistenza tra i f.v. già memorizzati
351200040621      *   (con data >= a quella del f.v. V1CNFV)
351300040621     c                   clear                   Wnfv7
351400040621     c                   move      Wnfv          Wnfv7
351500040615     c                   eval      xx      =  1
351600040615     c                   eval      *in10   =  *off
351700040621     c     Wnfv7         lookup    $FVxPOn(xx)                            10
351800070521      * - verifica se F.V. assegnabile a più filiali di "passaggio"
351900070521if  1c                   dow           *in10       =  *on
352000070521     c                             and $FVxPOa(xx) <> Wpost
352100070521     c                             and xx          <  %elem($FVxPOn)
352200070521     c                   eval      xx      =  xx + 1
352300070521     c                   eval      *in10   =  *off
352400070521     c     Wnfv7         lookup    $FVxPOn(xx)                            10
352500070521e   1c                   enddo
352600040621sel 1c                   select
352700040621      * - - ok
352800040621w   1c                   when          *in10       =  *on
352900040621     c                             and $FVxPOa(xx) =  Wpost
353000040621     c                   eval      Wdfv    =  $FVxPOd(xx)
353100060110     c                   leavesr
353200040621      * - - assegnato a P.O. diverso da quello indicato come postazione
353300070521w   1c                   when          *in10       =  *on
353400070521     c                             and $FVxPOa(xx) <> Wpost
353500070521if  2c                   if        wNFVseq =  2
353600070521     c                   eval      *in56   =  *on
353700070521x   2c                   else
353800070521     c                   eval      *in54   =  *on
353900070521e   2c                   endif
354000070521     c                   seton                                          2890
354100070521     c                   eval      V1Dmsg  =  %replace($FVxPOa(xx):
354200070521     c                                        $Msg(43):
354300070521     c                                        %scan('&PS':$Msg(43)))
354400070521     c                   leavesr
354500040621e   1c                   endsl
354600040621      *
354700040621      * - Verifica esistenza tra i f.v. NON memorizzati
354800040621     c                   move      Wnfv          FGVnfv
354900040621     c                   move      S2Cbai        FGVlnp
355000040621     c     K02FGV01      chain     FNFGV01
355100040621sel 1c                   select
355200040621w   1c                   when      NOT %found(FNFGV01L)
355300040615     c                             or  %found(FNFGV01L)
355400040615     c                             and (FGVatb <> *blanks
355500040615     c                              or  FGVfcf <> *blanks)
355600040621if  2c                   if        wNFVseq =  2
355700040615     c                   eval      *in56   =  *on
355800040621x   2c                   else
355900040615     c                   eval      *in54   =  *on
356000040621e   2c                   endif
356100040615     c                   seton                                          2890
356200040615     c                   eval      V1Dmsg  =  %replace(S2Cbai:
356300040615     c                                        $Msg(23):
356400040615     c                                        %scan('&PS':$Msg(23)))
356500060110     c                   leavesr
356600040621w   1c                   when      FGVdfv  <  SAVdfvA
356700040621if  2c                   if        wNFVseq =  2
356800040621     c                   eval      *in56   =  *on
356900040621x   2c                   else
357000040621     c                   eval      *in54   =  *on
357100040621e   2c                   endif
357200040621     c                   seton                                          2890
357300040621     c                   eval      V1Dmsg  =  $Msg(38)
357400040621e   1c                   endsl
357500060110     c                   if        *in90
357600060110     c                   leavesr
357700060110     c                   endif
357800040621     c                   eval      Wdfv    =  FGVdfv
357900040615      *
358000060110     c                   ENDSR
358100031024      *
358200031024      *---------------------------------------------------------------*
358300031024      * Impostazione campi descrittivi per l'autotrasportatore        *
358400031024      *---------------------------------------------------------------*
358500031024     c     DecS03pdr     BEGSR
358600031024      *
358700031024     c                   clear                   Wpdr
358800031024if  1c                   if        S1Cpdc >  *zeros
358900031024     c                   move      S1Cpdc        Wpdr
359000031024e   1c                   endif
359100031024      *
359200031024      * Visualizzazione "*" se più F.V. per il padroncino
359300031024     c                   clear                   S1Cast
359400031024if  1c                   if        S1Cpdc >  *zeros
359500031024     c                   exsr      CtrlAutotr3
359600031024     c                   eval      S1Cast =  Wast
359700031024e   1c                   endif
359800031024      *
359900031024      * Visualizzazione padroncino di default se variato
360000031024if  1c                   if        Wpdr   =  H1Cpdc
360100031024     c                   movel     H1Dpdc        S3Dpdc
360200031024x   1c                   else
360300040408if  2c                   if            $IMA   =  *off
360400040316     c                             and S1Cbai <> C_PostazNull
360500040423     c                             and S1Cpdc >  *zeros
360600031024     c                   move      APDpdr        S1Cpdc
360700031024     c                   movel     APDrsc        S3Dpdc
360800031024e   2c                   endif
360900031024     c                   eval      S1Kdft =  C_Default
361000031024if  2c                   if        H1Cpdc <> *zeros
361100040315     c                   eval      S1Katd =  C_Autotr
361200031024     c                   move      H1Cpdc        S1Catd
361300031024e   2c                   endif
361400031024     c                   eval      S1Datd =  H1Dpdc
361500031024e   1c                   endif
361600031024      *
361700031024     c                   ENDSR
361800031007      *
361900031007      *---------------------------------------------------------------*
362000031007      * Controllo dati immessi in videata S02 & S04                   *
362100031007      *---------------------------------------------------------------*
362200031010     c     CtrS02        BEGSR
362300061011      *
362400061011     c                   clear                   $PObau
362500061011     c                   clear                   $POpos
362600161212     c                   clear                   sk_Fil
362700161212     c                   clear                   sk_Rrn
362800090203     c                   clear                   Free_Rec_Sfl
362900031007      *
363000031007      * Ciclo di lettura del sfl
363100031007if  1c                   if        $Default = *on
363200031007     c                   readc     MS03S02
363300031007x   1c                   else
363400031007     c                   readc     MS03S04
363500031007e   1c                   endif
363600031020      *
363700031007do  1c                   DOW       NOT %eof(FIMS03D)
363800031007      *
363900031007     c                   movea     *zeros        *in(41)
364000031007     c                   z-add     S02nrr        C02rcd
364100031007      * - ctrl su singola riga
364200031014if  2c                   if            S2Cpep  <> *zeros
364300031014     c                             or  S2Cdes  <> *blanks
364400031014     c                             or  S2Cbnl  <> *blanks
364500040304     c                             or  S2Cbai  <> *blanks
364600161215     c                             or  S2Cznc  <> *blanks
364700031014     c                             or (S2Cpdc  <> *zeros
364800031014     c                             and S2Cpdc  <> *blanks)
364900031014     c                             or (S2Cnfv  <> *blanks
365000031014     c                             and S2Cnfv  <> *zeros)
365100031007     c                   exsr      CtrS02rec
365200031007if  3c                   if            $Default = *off
365300031007     c                             and *in90    = *off
365400040611     c                             and ($PO     = *off
365500040611     c                              or  $POdpd  = *on)
365600040518     c                             and S2Cbai  <> C_PostazNull
365700040309     c                             and S2Cbnl  <> 'S'
365800031007     c                   exsr      CtrS04rec
365900031007e   3c                   endif
366000031007x   2c                   else
366100040304     c                   clear                   S2Cbai
366200161213     c                   clear                   S2Cznc
366300031023     c                   clear                   S2Ddfv
366400031106     c                   clear                   S2Dfpp
366500050628     c                   clear                   S2Dmes
366600031106     c                   clear                   S4Ddfv
366700031106     c                   clear                   S4Dfpp
366800050628     c                   clear                   S4Dmes
366900031106     c                   clear                   S2Cast
367000031007     c                   clear                   S2Cpdc
367100031007     c                   clear                   S2Dpdc
367200031007     c                   clear                   S4Dpdc
367300031023     c                   clear                   S2Cdes
367400031023     c                   clear                   S2Kdft
367500040305     c                   clear                   S2Kbad
367600040305     c                   clear                   S2Cbad
367700031023     c                   clear                   S2Katd
367800031023     c                   clear                   S2Catd
367900031023     c                   clear                   S2Datd
368000090203      * - Memorizzazione primo rec. vuoto nel sfl
368100090203if  3c                   if        Free_Rec_Sfl = *zero
368200090203     c                   eval      Free_Rec_Sfl = S02nrr
368300090203e   3c                   endif
368400031007e   2c                   endif
368500031024      *
368600031024      * - reimposto indicatori x DSPATR condizionati
368700040702      *   se NON rilevati errori
368800040702if  2c                   if        NOT *in90
368900040702     c                             and $Default = *on
369000040702     c                             or  $Default = *off
369100060110     c                   exsr      AtrS02S04
369200040702e   2c                   endif
369300031007      *
369400031007      * - riattivo SFLNXTCHG (non solo se rilevato errore)
369500031007     c                   eval      *in32    = *on
369600031007      * - aggiorno il SFL
369700031007if  2c                   if        $Default = *on
369800031007     c                   update    MS03S02
369900031007x   2c                   else
370000031007     c                   update    MS03S04
370100031007e   2c                   endif
370200031007     c                   eval      *in32    = *off
370300031007      * - se rilevato errore esco dal ciclo di controllo
370400031007if  2c                   if        *in28    = *on
370500031106     c                   z-add     C02rcd        C02csr
370600090203     c                   leavesr
370700031007e   2c                   endif
370800031007      * - leggo il record successivo
370900031007if  2c                   if        $Default = *on
371000031007     c                   readc     MS03S02
371100031007x   2c                   else
371200031007     c                   readc     MS03S04
371300031007e   2c                   endif
371400031007      *
371500031007e   1c                   ENDDO
371600040312      *
371700040423      * Controlli finali "generali"
371800040312      *   (se NON ho già rilevato altri errori da segnalare)
371900161212      *
372000161212      /free
372100161212
372200161212         // -?Controllo inserimento Zona (cioè Turno) 99 - OBBLIGATORIA) -?
372300161212         //  ?per ogni Baia = Filiale?
372400161212         For  xx = 1  To  %elem(sk_Fil);
372500161212
372600161213           if  sk_Fil(xx) <= *zero;
372700161212             leave;
372800161212           endif;
372900161212
373000161212           If  sk_Rrn(xx) < *zero;
373100161212
373200161212             S02nrr = sk_Rrn(xx) * (-1);
373300161212             C02csr = S02nrr;
373400161212             if  $Video = '3';
373500161213               chain  S02nrr  MS03S02;
373600161212             else;
373700161213               chain  S02nrr  MS03S04;
373800161212             endif;
373900161212             *in58  = *on;
374000161212             *in28  = *on;
374100161212             *in90  = *on;
374200161213             V1Dmsg = %replace( sk_Fil(xx) : $Msg(58)
374300161212                              : %scan( '___' : $MSG(58) ) );
374400161212             if  %found(FIMS03D);
374500161213               *in32 = *on;
374600161212               if   $Video = '3';
374700161212                 update  MS03S02;
374800161212               else;
374900161212                 update  MS03S04;
375000161212               endif;
375100161212             endif;
375200161212             leavesr;
375300161212
375400161212           EndIf;
375500161212
375600161212         EndFor;
375700161212
375800161212      /end-free
375900090203      *
376000090203      * - Baia Non Letti obbligatoria
376100090203      *   (dovrebbe riscontrarsi solo nel piano di default)
376200090203if  1c                   if        %xfoot($BNLnr) = *zero
376300090203     c                   eval      S02nrr = Free_Rec_Sfl
376400090203     c                   eval      C02csr = S02nrr
376500090203if  2c                   if        $Video = '3'
376600090203     c     S02nrr        chain     MS03S02
376700090203x   2c                   else
376800090203     c     S02nrr        chain     MS03S04
376900090203e   2c                   endif
377000090203     c                   seton                                        572890
377100090203     c                   movel     $Msg(54)      V1Dmsg
377200090203if  2c                   if        %found(FIMS03D)
377300090203if  3c                   if        $Video = '3'
377400090203     c                   update    MS03S02
377500090203x   3c                   else
377600090203     c                   update    MS03S04
377700090203e   3c                   endif
377800090203e   2c                   endif
377900090203     c                   leavesr
378000090203e   1c                   endif
378100090203      *
378200090203      * - Controllo postazioni per baia
378300040312     c                   exsr      CtrBAIxPEP
378400031007      *
378500031007     c                   ENDSR
378600031007      *
378700031007      *---------------------------------------------------------------*
378800031007      * Controllo singolo record del subfile S02 & S04                *
378900031007      *---------------------------------------------------------------*
379000031007     c     CtrS02rec     BEGSR
379100031027      *
379200031027if  1c                   if        $Default = *off
379300031027     c                   clear                   S2Kdft
379400040304     c                   clear                   S2Kbad
379500040304     c                   clear                   S2Cbad
379600031027     c                   clear                   S2Katd
379700031027     c                   clear                   S2Catd
379800031027     c                   clear                   S2Datd
379900031027e   1c                   endif
380000040423      *
380100040423     c                   reset                   $IMA
380200040423     c                   reset                   $IMAdft
380300040607     c                   reset                   $PO
380400040611     c                   reset                   $POdpd
380500080129     c                   reset                   $POerr
380600040607     c                   reset                   $POdft
380700031007      *
380800031014      * Controllo baia di uscita
380900031007sel 1c                   select
381000031007w   1c                   when      S2Cpep = *zeros
381100031007     c                   seton                                        512890
381200031009     c                   movel     $Msg(24)      V1Dmsg
381300031007w   1c                   when      S2Cpep < 01  or  S2Cpep > 98
381400031007     c                   seton                                        512890
381500031009     c                   movel     $Msg(25)      V1Dmsg
381600031007x   1c                   other
381700031007     c                   exsr      ContaBAU
381800031007e   1c                   endsl
381900060110     c                   if        *in90
382000060110     c                   leavesr
382100060110     c                   endif
382200031007      *
382300031007      * Controllo descrizione
382400031014      * (non obbligatoria)
382500031007      *
382600031007      * Controllo flag baia non letti
382700031007if  1c                   if            S2Cbnl <> *blanks
382800031007     c                             and S2Cbnl <> 'S'
382900040304     c                   seton                                        572890
383000031009     c                   movel     $Msg(27)      V1Dmsg
383100031007x   1c                   else
383200040309if  2c                   if        S2Cbnl     =  'S'
383300031007     c                   exsr      ContaBNL
383400031007if  3c                   if        $BNLnr(xx) >  1
383500040304     c                   seton                                        572890
383600031009     c                   movel     $Msg(28)      V1Dmsg
383700031007e   3c                   endif
383800031007e   2c                   endif
383900031007e   1c                   endif
384000060110     c                   if        *in90
384100060110     c                   leavesr
384200060110     c                   endif
384300040305      *
384400040305      * Controllo codice postazione
384500040423      * - ricerca
384600040423     c     '?'           scan      S2Cbai                                 10
384700040423if  1c                   if        *in10  =  *on
384800040423     c                   eval      S2Cbai =  *blanks
384900040423     c                   eval      Wcampo =  'S2CBAI    '
385000040423     c                   exsr      srSEARCH
385100040423e   1c                   endif
385200040423      * - controllo
385300040423if  1c                   if        $Default = *off
385400040423     c                   eval      $Hidden  = *on
385500040423     c                   eval      Wpost    = H2Cbai
385600040423     c                   exsr      Ctrl_IMA
385700040423if  2c                   if        $IMAdft  = *on
385800040423     c                   eval      H2Dpdc   = $MPTdes(yy)
385900040423e   2c                   endif
386000040625     c                   exsr      Ctrl_PO
386100040625if  2c                   if        $POdft   = *on
386200040625     c                   eval      H2Dpdc   = $ORGdes(xy)
386300040625e   2c                   endif
386400040423e   1c                   endif
386500040423     c                   eval      $Hidden  = *off
386600040423     c                   eval      Wpost    = S2Cbai
386700040408     c                   exsr      Ctrl_IMA
386800040423if  1c                   if        $IMA     = *on
386900040423     c                   eval      S2Dpdc   = $MPTdes(yy)
387000040423     c                   eval      S4Dpdc   = $MPTdes(yy)
387100040423e   1c                   endif
387200040607     c                   exsr      Ctrl_PO
387300040607if  1c                   if        $PO      = *on
387400040611if  2c                   if        $POdpd   = *off
387500040611     c                   clear                   S2Cnfv
387600040611     c                   clear                   S2Dfpp
387700040611     c                   clear                   S2Ddfv
387800040611     c                   clear                   S4Cnfv
387900040611     c                   clear                   S4Dfpp
388000040611     c                   clear                   S4Ddfv
388100040611     c                   clear                   S2Cast
388200040611e   2c                   endif
388300061011     c                   clear                   S2Cpdc
388400040607     c                   eval      S2Dpdc   = $ORGdes(xy)
388500040607     c                   eval      S4Dpdc   = $ORGdes(xy)
388600040607e   1c                   endif
388700040423if  1c                   if            $Default = *off
388800040423     c                             and $IMAdft  = *on
388900040423     c                             and H2Cbai  <> S2Cbai
389000040423     c                   eval      S2Datd   = H2Dpdc
389100040423e   2c                   endif
389200080902if  1c                   if            $Default = *on
389300080902     c                             and S2Cbnl   = 'S'
389400080902     c                             and S2Cbai  <> *blanks
389500080902     c                   seton                                        532890
389600080902     c                   movel     $Msg(26)      V1Dmsg
389700080902     c                   leavesr
389800080902e   1c                   endif
389900040409if  1c                   if            $Default = *on
390000040409     c                             and S2Cbnl  <> 'S'
390100040409     c                             and S2Cbai   = *blanks
390200040409     c                   eval      S2Cbai   = C_PostazNull
390300040409e   1c                   endif
390400040305sel 1c                   select
390500040305w   1c                   when          $Default = *on
390600040305     c                             and S2Cbnl   = 'S'
390700040305     c                             and S2Cbai   = *blanks
390800040409w   1c                   when      S2Cbai   = C_PostazNull
390900040409if  2c                   if        $Default = *off
391000040305     c                   clear                   S2Cnfv
391100040305     c                   clear                   S2Ddfv
391200040305     c                   clear                   S2Dfpp
391300040305     c                   clear                   S4Cnfv
391400040305     c                   clear                   S4Ddfv
391500040305     c                   clear                   S4Dfpp
391600040305     c                   clear                   S4Dpdc
391700040409e   2c                   endif
391800161216     c                   clear                   S2Cznc
391900040409     c                   clear                   S2Cpdc
392000040305w   1c                   when          S2Cbai =  *blanks
392100040305     c                             and S2Cbnl <> 'S'
392200040305     c                   seton                                        532890
392300040305     c                   movel     $Msg(13)      V1Dmsg
392400040305w   1c                   when          S2Cbai <> *blanks
392500040305     c                             and S2Cbnl =  'S'
392600040305     c                   seton                                        532890
392700040305     c                   movel     $Msg(26)      V1Dmsg
392800040305w   1c                   when         (%subst(S2Cbai:1:1) >= 'A'
392900040305     c                             and %subst(S2Cbai:1:1) <= 'Z')
393000040305     c                             and %subst(S2Cbai:2:2) >= '01'
393100040305     c                             and %subst(S2Cbai:2:2) <= '99'
393200040305     c                   move      S2Cbai        w002a
393300040305     c     DigitN        check     w002a                                  10
393400040305if  2c                   if        *in10  =  *on
393500040305     c                   seton                                        532890
393600040305     c                   movel     $Msg(14)      V1Dmsg
393700040305x   2c                   else
393800040423     c                   eval      Wpost  =  S2Cbai
393900040305     c                   exsr      ContaBAI
394000040305if  3c                   if        $BAInr(xx) >  1
394100040305     c                   seton                                        532890
394200040305     c                   movel     $Msg(15)      V1Dmsg
394300040305e   3c                   endif
394400040305e   2c                   endif
394500040607w   1c                   when          S2Cbnl =  *blanks
394600040607     c                             and $IMA   =  *off
394700040607     c                             and $PO    =  *on
394800061010     c                             and $POdpd =  *on
394900040607if  3c                   if        $ORGnr(xy) >  1
395000040607     c                   seton                                        532890
395100040607     c                   movel     $Msg(33)      V1Dmsg
395200040607e   3c                   endif
395300040305w   1c                   when          S2Cbnl =  *blanks
395400040408     c                             and $IMA   =  *off
395500040607     c                             and $PO    =  *off
395600040305     c                             OR  S2Cbnl =  'S'
395700040305     c                             and S2Cbai <> *blanks
395800040305     c                   seton                                        532890
395900040305     c                   movel     $Msg(14)      V1Dmsg
396000040305e   1c                   endsl
396100060110     c                   if        *in90
396200060110     c                   leavesr
396300060110     c                   endif
396400040316      *
396500040316      * Controllo la postazione sia "compatibile" con le altre già
396600040316      *   immesse nella stessa baia
396700040316if  1c                   if        S2Cbai <> C_PostazNull
396800040409     c                             and
396900040409     c                             S2Cbai <> *blanks
397000040316     c                   eval      xx     =  1
397100040316     c                   eval      *in10  =  *off
397200040316     c     S2Cpep        lookup    $BAU(xx)                               10
397300040628sel 2c                   select
397400040628      * - Unica (o prima) postazione della baia in esame
397500040628w   2c                   when      NOT  *in10
397600040628      * - Postazione IMA/non-IMA o P.O./non-P.O. nella stessa baia
397700161110      *  ?Su richiesta di Nicola:?tolto controllo postaz. IMA/non-IMA
397800161110      *   (ora IMA e AUT possono stare nella stessa baia;
397900161110      *   sono le FILIALI che continuano a richiedere una loro baia)
398000161110      * - Postazione P.O./non-P.O. nella stessa baia
398100161110w   2c                   when           $BAUbai(xx) <> *blanks
398200161110     c                             and  $PosPO(xx)  <> $PO
398300040316     c                   seton                                        532890
398400161110     c*//                eval      V1Dmsg =  $Msg(40)
398500161110     c                   eval      V1Dmsg =  $Msg(57)
398600060110     c                   leavesr
398700040628      * - Postazione P.O. DPD non unico nella baia
398800040628w   2c                   when           $BAUbai(xx)    <> *blanks
398900040628     c                             and  $BAUbai(xx)    <> S2Cbai
399000040628     c                             and  $PosPOdpd(xx)  <> $POdpd
399100040628     c                   seton                                        532890
399200040628     c                   eval      V1Dmsg =  $Msg(44)
399300060110     c                   leavesr
399400040607w   2c***                when           *in10       =  *on
399500040607     c***                          and  $PosIMA(xx) =  $IMA
399600061011     c***                          and  $PosPO(xx)  =  $PO
399700040316      ***  Ok. Vediamo il prossimo...
399800040628e   2c                   endsl
399900040316e   1c                   endif
400000061011      *
400100061011      * Controllo l'eventuale utilizzo dei P.O. per baia
400200061011if  1c                   if             S2Cbnl  = *blanks
400300061011     c                             and  $IMA    = *off
400400061011     c                             and  $PO     = *on
400500061011     c                             and  $POdpd  = *off
400600061011      *
400700061011     c                   eval      WX     =  1
400800061011     c     S2Cbai        lookup    $POpos(wx)                             10
400900061011do  2c                   dow       *in10  =  *on
401000061011sel 3c                   select
401100061011w   3c                   when      S2Cpep =  $PObau(wx)
401200061011     c                   seton                                        532890
401300161212     c                   eval      V1Dmsg =  %trimR($Msg(33))
401400061012     c                                    +  ' nella stessa baia'
401500061011     c                   leavesr
401600061011w   3c                   when      wx     >= %elem($POpos)
401700061011     c                   leave
401800061011x   3c                   other
401900061011     c                   add       1             wx
402000061013     c     S2Cbai        lookup    $POpos(wx)                             10
402100061011e   3c                   endsl
402200061011e   2c                   enddo
402300061011      * Controllo progressivo per postazione = P.O. (FL3)
402400061011     c                   clear                   wFL3
402500061011do  2c                   do        S02nrr        wx
402600061012if  3c                   if        S2Cbai =  $POpos(wx)
402700061011     c                   add       1             wFL3
402800061012if  4c                   if        wFL3   >= 9
402900061011     c                   seton                                        532890
403000161212     c                   eval      V1Dmsg =  %trimR($Msg(33))
403100061012     c                                    +  ' in oltre 9 baie'
403200061011     c                   leavesr
403300061011e   4c                   endif
403400061011e   3c                   endif
403500061011e   2c                   enddo
403600061011e   1c                   endif
403700161212      *
403800161215      /free
403900161215
404000161215         // -?Controllo inserimento Zona (cioè Turno) solo se Baia = Filiale?
404100161215         //  ?(99 = generica)?
404200161215         Select;
404300161215           // - -?Non richiesta ma inserita?
404400161215           When  ( S2Cbnl <> *blank  or
404500161215                   $IMA    = *on     or
404600161215                   $PO     = *off )  and
404700161215                 S2Cznc   <> *blank;
404800161215             //*·clear  S2Cznc;
404900161215             *in58  =  *on;
405000161215             *in28  =  *on;
405100161215             *in90  =  *on;
405200161215             V1Dmsg =  $Msg(59);
405300161215             leavesr;
405400161215           // - -?Richiesta ma non inserita?
405500161215           When  S2Cbnl   =  *blank  and
405600161215                 $IMA     =  *off    and
405700161215                 $PO      =  *on     and
405800161215                 S2Cznc   =  *blank;
405900161215             //*·S2Cznc = 99;
406000161215             *in58  =  *on;
406100161215             *in28  =  *on;
406200161215             *in90  =  *on;
406300170117             //*·V1Dmsg =  %replace( sk_Fil(xx) : $Msg(60)
406400170117             V1Dmsg =  %replace( S2Cbai : $Msg(60)
406500161215                               : %scan( '___' : $Msg(60) ) );
406600161215             leavesr;
406700161215         EndSl;
406800161215         // - -?Numericità (se inserita)?
406900161215         If  S2Cznc <> *blank  and
407000161215             %check( DigitN : S2Cznc ) > *zero;
407100161215           *in58  =  *on;
407200161215           *in28  =  *on;
407300161215           *in90  =  *on;
407400161215           V1Dmsg =  $Msg(61);
407500161215           leavesr;
407600161215         EndIf;
407700161215
407800161215      /end-free
407900040316      *
408000040305      * ...e ne memorizzo gli utilizzi per baia
408100040409      *   (ammesse postazioni *blank per la "baia non letti")
408200040316if  1c                   if        S2Cbai <> C_PostazNull
408300040305     c                   exsr      MemBAIxPEP
408400040305e   1c                   endif
408500040312      *
408600040305      * Visualizzo la postazione di default se variata in piano
408700040305      *   giornaliero
408800161213if  1c                   if            $Default =  *off
408900161213     c                             and (S2Cbai  <> H2Cbai
409000161213     c                              or  S2Cznc  <> H2Cznc)
409100040423     c                   eval      S2Kdft  = C_Default
409200161213if  2c                   if        S2Cbai <> H2Cbai
409300040625     c                   eval      S2Kbad  = C_Postazione
409400040423     c                   eval      S2Cbad  = H2Cbai
409500161213if  3c                   if           $IMAdft = *on
409600040625     c                             or $POdft  = *on
409700040423     c                   eval      S2Datd  = H2Dpdc
409800161213e   3c                   endif
409900161213x   2c                   else
410000161213     c                   eval      S2Kbad  = 'Zona'
410100161215     c                   eval      S2Cbad  = H2Cznc
410200161213e   2c                   endif
410300040305e   1c                   endif
410400031007      *
410500031007      * Controllo codice autotrasportatore
410600031007      *   (in S03 e S04 è campo protetto)
410700031015      * Per il piano giornaliero farò appositi controlli nella subr.
410800031015      *   CTRS04REC
410900050614     c                   clear                   S2Dmes
411000050614     c                   clear                   S4Dmes
411100031007if  1c                   if        $Default = *on
411200040607if  2c                   if            $IMA = *off
411300040607     c                             and $PO  = *off
411400031007     c                   clear                   S2Dpdc
411500031009     c                   clear                   S4Dpdc
411600040423e   2c                   endif
411700031007      * - se *blanks lo considero *zeros (sarebbe numerico)
411800031007if  2c                   if        S2Cpdc =  *blanks
411900031007     c                   eval      S2Cpdc =  *zeros
412000031007e   2c                   endif
412100031007      * - ricerca
412200031007     c     '?'           scan      S2Cpdc                                 10
412300031007if  2c                   if        *in10  =  *on
412400031007     c                   eval      S2Cpdc =  *zeros
412500031007     c                   eval      Wcampo =  'S2CPDC    '
412600031007     c                   exsr      srSEARCH
412700031007e   2c                   endif
412800031007      * - controllo
412900031007sel 2c                   select
413000040305w   2c                   when          S2Cbnl =  *blanks
413100040409     c                             and ($IMA  =  *on
413200040607     c                              or $PO    =  *on
413300040409     c                              or S2Cbai =  C_PostazNull)
413400040305     c                             and S2Cpdc =  *zeros
413500031007w   2c                   when          S2Cbnl =  'S'
413600031007     c                             and S2Cpdc <> *zeros
413700031007     c                   seton                                        552890
413800031009     c                   movel     $Msg(29)      V1Dmsg
413900040408w   2c                   when          $IMA   =  *on
414000040305     c                             and S2Cpdc <> *zeros
414100040305     c                   seton                                        552890
414200040305     c                   movel     $Msg(16)      V1Dmsg
414300040607w   2c                   when          $PO    =  *on
414400040607     c                             and S2Cpdc <> *zeros
414500040607     c                   seton                                        552890
414600040607     c                   movel     $Msg(18)      V1Dmsg
414700040305w   2c                   when          S2Cbnl =  *blanks
414800040408     c                             and $IMA   =  *off
414900040607     c                             and $PO    =  *off
415000031021     c                   movel     V1Cpog        APDpdr
415100031021     c                   move      S2Cpdc        APDpdr
415200031014     c                   exsr      CtrlAutotr2
415300031007     c                   movel     APDrsc        S2Dpdc
415400040519if  3c                   if        NOT *in90
415500040519     c                   eval      Wpdr   =  APDpdr
415600040519     c                   exsr      CtrlAutotr4
415700040519e   3c                   endif
415800031007e   2c                   endsl
415900060110     c                   if        *in90
416000060110     c                   leavesr
416100060110     c                   endif
416200050629      * - imposto il flag per autotrasportoatore Messaggeria
416300050614if  3c                   if            $Default = *on
416400050614     c                             and §OGpkm   = 'S'
416500050629     c                             and $VDL     = *on
416600050614     c                             and $IMA     = *off
416700050614     c                             and $PO      = *off
416800050614     c                             and S2Cbai  <> C_PostazNull
416900050614     c                             and S2Cbai  <> *blanks
417000050701     c                   eval      wNFVseq = 1
417100050629     c                   movel     APDflr        dAPDflr
417200050614     c                   exsr      X_Messaggeria
417300050614e   3c                   endif
417400031007      * - se *zeros lo reimposto a *blanks (numerico "editato")
417500031014if  2c                   if        S2Cpdc =  *zeros
417600031014     c                   eval      S2Cpdc =  *blanks
417700031007e   2c                   endif
417800031007e   1c                   endif
417900040305      *
418000040305      * In caso di piano giornaliero controllo se è stata tolta la
418100040305      *   postazione ("XXX"), perchè in tal caso non verrà eseguita
418200040305      *   la subr. per i controlli specifici, pertanto non verrebbe
418300040305      *   evidenziato l'autotrasportatore pre-impostato
418400061011if  1c                   if             $Default = *off
418500061011     c                             and (S2Cbai   = C_PostazNull
418600061011     c                             or   $PO      = *on)
418700040305     c                   clear                   S2Cpdc
418800061011if  2c                   if        $PO    =  *off
418900040305     c                   clear                   S4Dpdc
419000061011e   2c                   endif
419100040305     c                   exsr      DecS04pdr
419200040305e   1c                   endif
419300031007      *
419400060110     c                   ENDSR
419500031007      *
419600031007      *---------------------------------------------------------------*
419700031007      * Controlli aggiuntivi per singolo record del subfile S04       *
419800031007      *---------------------------------------------------------------*
419900031007     c     CtrS04rec     BEGSR
420000031023      *
420100031106     c                   reset                   SAVnfv
420200031106      *
420300031106      * Se impostato solo il 2° numero F.V. lo sposto al 1°
420400031106if  1c                   if            (S2Cnfv  =  *blanks
420500031106     c                             or   S2Cnfv  =  *zeros)
420600031106     c                             and (S4Cnfv  <> *blanks
420700031106     c                             and  S4Cnfv  <> *zeros)
420800031106     c                   eval      S2Cnfv  =  S4Cnfv
420900031106     c                   clear                   S4Cnfv
421000031106     c                   clear                   S4Ddfv
421100031106     c                   clear                   S4Dfpp
421200050628     c                   clear                   S4Dmes
421300031106e   1c                   endif
421400031106      *
421500031106      * Controllo 1° numero foglio viaggio
421600031007     c                   clear                   S2Ddfv
421700031027     c                   clear                   S2Dfpp
421800050628     c                   clear                   S2Dmes
421900031106     c                   eval      wNFVseq =  1
422000031106     c                   eval      Wnfv    =  S2Cnfv
422100031106     c                   exsr      Ctrl_1FV
422200031106     c                   eval      S2Cnfv  =  Wnfv
422300080129if  1c                   if            $PO     =  *on
422400080129     c                             and $POdpd  =  *on
422500080130     c                   eval      $ORGerr(xy) =  (Wnfv = *blanks   or
422600080130     c                                             Wnfv = *zeros)
422700080129e   1c                   endif
422800060110     c                   if        *in90
422900060110     c                   leavesr
423000060110     c                   endif
423100031106      * Se tutto bene: ne imposto i dati a video
423200031106if  1c                   if        Wnfv    <> *blanks
423300040621if  2c                   if        $PO     =  *off
423400031106     c                   eval      S2Dfpp  =  FVVfpp
423500031106     c                   move      DSdata6       S2Ddfv
423600040621x   2c                   else
423700040621     c***                clear                   S2Dfpp
423800040621     c                   move      DSdata6       S2Ddfv
423900040621e   2c                   endif
424000031106e   1c                   endif
424100031106      *
424200031106      * Imposto flag e decodifico variazioni relative all'autotrasp.
424300031106      *   (che deve essere comune ad entrambi i F.V.)
424400040610if  1c                   if            $IMA    =  *off
424500040610     c                             and $PO     =  *off
424600040610if  2c                   if        S2Cnfv  <> *blanks
424700031106     c                   move      APDpdr        S2Cpdc
424800031106     c                   movel     APDrsc        S4Dpdc
424900040610x   2c                   else
425000040305     c                   clear                   S2Cpdc
425100040305     c                   clear                   S4Dpdc
425200040423e   2c                   endif
425300031106e   1c                   endif
425400031106     c                   exsr      DecS04pdr
425500040615      *
425600040615      * Imposto flag relativo alla postazione = P.O. DPD con più F.V.
425700040615if  1c                   if        $POdpd   =  *on
425800040621     c*** decod. P.O.    clear                   S2Cpdc
425900040621     c*** decod. P.O.    clear                   S4Dpdc
426000040621     c                   eval      Wpost    = S2Cbai
426100040615     c                   exsr      AsterFVxPO
426200040615     c                   eval      S2Cast   = Wast
426300040615e   1c                   endif
426400031106      *
426500031106      * Controllo 2° numero foglio viaggio (se immesso)
426600031106     c                   clear                   S4Ddfv
426700031106     c                   clear                   S4Dfpp
426800050628     c                   clear                   S4Dmes
426900031106if  1c                   if            S4Cnfv <> *blanks
427000031106     c                             and S4Cnfv <> *zeros
427100031106     c                   eval      wNFVseq =  2
427200031106     c                   eval      Wnfv    =  S4Cnfv
427300031106     c                   exsr      Ctrl_1FV
427400031106     c                   eval      S4Cnfv  =  Wnfv
427500060110     c                   if        *in90
427600060110     c                   leavesr
427700060110     c                   endif
427800031106      * Se tutto bene: ne imposto i dati a video
427900031106if  2c                   if        Wnfv    <> *blanks
428000031106     c                   eval      S4Dfpp  =  FVVfpp
428100031106     c                   move      DSdata6       S4Ddfv
428200031106e   2c                   endif
428300040305      * e rieseguo il controllo sul numero di F.V. x autotrasportatore
428400040305      *   (se ne ho selezionato uno da ricerca)
428500040305if  2c                   if        S2Cast  =  '?'
428600040305     c                   clear                   S2Cast
428700040305     c                   exsr      CtrlAutotr3
428800040305     c                   eval      S2Cast =  Wast
428900040305e   2c                   endif
429000031106x   1c                   else
429100031106     c                   clear                   S4Cnfv
429200031106e   1c                   endif
429300031106      *
429400060110     c                   ENDSR
429500031024      *
429600031024      *---------------------------------------------------------------*
429700031024      * Impostazione campi descrittivi per l'autotrasportatore        *
429800031024      *---------------------------------------------------------------*
429900031024     c     DecS04pdr     BEGSR
430000031024      *
430100031024     c                   clear                   Wpdr
430200031024if  1c                   if        S2Cpdc >  *zeros
430300031024     c                   move      S2Cpdc        Wpdr
430400031024e   1c                   endif
430500031024      *
430600031024      * Visualizzazione "*" se più F.V. per il padroncino
430700031024     c                   clear                   S2Cast
430800031024if  1c                   if        S2Cpdc >  *zeros
430900031024     c                   exsr      CtrlAutotr3
431000031024     c                   eval      S2Cast =  Wast
431100031024e   1c                   endif
431200031024      *
431300031024      * Visualizzazione padroncino di default se variato
431400031024if  1c                   if        Wpdr   =  H2Cpdc
431500040625if  2c                   if            $IMA   =  *off
431600040625     c                             and $PO    =  *off
431700040625     c                             and S2Cbai <> C_PostazNull
431800031024     c                   movel     H2Dpdc        S4Dpdc
431900040423e   2c                   endif
432000031024x   1c                   else
432100040305if  2c                   if            S2Cbnl =  *blanks
432200040408     c                             and $IMA   =  *off
432300040625     c                             and $PO    =  *off
432400040316     c                             and S2Cbai <> C_PostazNull
432500040305if  3c                   if        S2Cpdc >  *zeros
432600031024     c                   move      APDpdr        S2Cpdc
432700031024     c                   movel     APDrsc        S4Dpdc
432800040305e   3c                   endif
432900031024e   2c                   endif
433000031024     c                   eval      S2Kdft =  C_Default
433100031024if  2c                   if        H2Cpdc <> *zeros
433200040315     c                   eval      S2Katd =  C_Autotr
433300031024     c                   move      H2Cpdc        S2Catd
433400031024e   2c                   endif
433500031024     c                   eval      S2Datd =  H2Dpdc
433600031024e   1c                   endif
433700031024      *
433800031024     c                   ENDSR
433900060110      *
434000060110      *---------------------------------------------------------------*
434100060110      * Impostazione settaggi attributi di visualizzazione x S01/S03  *
434200060110      *---------------------------------------------------------------*
434300060110     c     AtrS01S03     BEGSR
434400060110      *
434500060110      * Autotrasportatore Messaggeria
434600060110     c                   eval      *in04 = (§OGpkm = 'S')
434700060110      * Baia non letti
434800060110     c                   eval      *in80 = (S2Cbnl = 'S')
434900060110      * Padroncino e/o Postazione variati
435000060110     c                   clear                   Wpdr
435100060110if  1c                   if            S1Cpdc <> *blanks
435200060110     c                             and S1Cpdc <> *zeros
435300060110     c                   movel     V1Cpog        Wpdr
435400060110     c                   move      S1Cpdc        Wpdr
435500060110e   1c                   endif
435600060110     c                   eval      *in85 = (Wpdr   <> H1Cpdc   or
435700060110     c                                      S1Cbai <> H1Cbai)
435800060110      * Postazione = "IMx" o P.O.
435900060110     c                   eval      *in86 = ($IMA   =  *on      or
436000060110     c                                      $PO    =  *on)
436100060110      *
436200060110     c                   ENDSR
436300060110      *
436400060110      *---------------------------------------------------------------*
436500060110      * Impostazione settaggi attributi di visualizzazione x S02/S04  *
436600060110      *---------------------------------------------------------------*
436700060110     c     AtrS02S04     BEGSR
436800060110      *
436900060110      * Autotrasportatore Messaggeria
437000060110     c                   eval      *in04 = (§OGpkm = 'S')
437100060110      * Baia non letti
437200060110     c                   eval      *in80 = (S2Cbnl = 'S')
437300060110      * Padroncino e/o Postazione variati
437400060110     c                   clear                   Wpdr
437500060110if  1c                   if            S2Cpdc <> *blanks
437600060110     c                             and S2Cpdc <> *zeros
437700060110     c                   movel     V1Cpog        Wpdr
437800060110     c                   move      S2Cpdc        Wpdr
437900060110e   1c                   endif
438000060110     c                   eval      *in85 = (Wpdr   <> H2Cpdc   or
438100161213     c                                      S2Cbai <> H2Cbai   or
438200161213     c                                      S2Cznc <> H2Cznc)
438300060110      * Postazione = "IMx" o P.O.
438400060110     c                   eval      *in86 = ($IMA   =  *on      or
438500060110     c                                      $PO    =  *on)
438600060110      * Autotrasportatore Messaggeria
438700060110      * - 1° F.V. o Default Messaggeria
438800060110     c                   eval      *in87 = (S2Dmes <> *blanks)
438900060110      * - 2° F.V. Messaggeria
439000060110     c                   if        $Default    = *off
439100060110     c                   eval      *in88 = (S4Dmes <> *blanks)
439200060110     c                   endif
439300060110      *
439400060110     c                   ENDSR
439500031014      *
439600031014      *---------------------------------------------------------------*
439700031030      * Controllo codice autotrasportatore per singolo F.V. / baia    *
439800031014      *---------------------------------------------------------------*
439900031014     c     CtrlAutotr1   BEGSR
440000031014      *
440100031014      * Reperisco il padroncino memorizzato nel F.V. di default
440200031014     c                   clear                   DS_FV
440300031014     c                   eval      dsFVdfv    =  FVVdfv
440400031014     c                   eval      dsFVnfv    =  FVVnfv
440500031022     c                   move      DS_FV         w0150
440600031014     c                   eval      xx    = 1
440700031014     c                   eval      *in10 = *off
440800031111     c     w0150         lookup    $MEMfv1(xx)                            10
440900031111if  1c                   if        *in10 = *off
441000031111     c     w0150         lookup    $MEMfv2(xx)                            10
441100031111e   1c                   endif
441200031014if  1c                   if        *in10 = *on
441300031014     c                   eval      DSTpdr     =  $MEMat(xx)
441400031014x   1c                   else
441500031111      * - Se non reperito il F.V. tra quelli automaticamente proposti
441600031014      *   (per es. durante la variazione di un piano giornaliero)
441700031014      *   aggancio il file Fogli Distinte Consegne & memorizzo il F.V.
441800120613     c                   eval      DSTnpg     =  FVVnpg
441900120613     c                   eval      DSTnfv     =  FVVnfv
442000120613     c                   eval      DSTfgs     =  FVVfgs
442100120613     c     K03FIDST      chain     FIDST000
442200120613if  2c                   if        NOT %found(FIDST01L)
442300031022if  3c                   if        wNFVseq    =  2
442400031022     c                   movel     *on           *in56
442500031022x   3c                   else
442600031022     c                   movel     *on           *in54
442700031022e   3c                   endif
442800031022     c                   seton                                          2890
442900031014     c                   eval      V1Dmsg = $Msg(21)
443000060110     c                   leavesr
443100031014x   2c                   else
443200031111      * - - controllo se padroncino già memorizzato
443300031111      *     (se non lo è ancora  : lo memorizzo ora;
443400031111      *        se non ho più spazio: esco dal ciclo senza memorizzare)
443500031014     c                   eval      xx    = 1
443600031014     c                   eval      *in10 = *off
443700031111do  3c                   DOW       *in10 = *off
443800031111     c     DSTpdr        lookup    $MEMat(xx)                             10
443900031111if  4c                   if        *in10 = *off
444000031111     c     *zeros        lookup    $MEMat(xx)                             10
444100031111if  5c                   if        *in10 = *on
444200031111     c                   eval      $MEMat(xx)  =  DSTpdr
444300031111x   5c                   else
444400031111     c                   leave
444500031111e   5c                   endif
444600031111e   4c                   endif
444700031111      * - - per il padroncino cerco il primo elemento libero tra le
444800031111      *     schiere $MEMfv1 o $MEMfv2;
444900031111      *     se non lo trovo: torno sopra per ri-memorizzare il codice
445000031111      *       padroncino e questo nuovo f.v.
445100050701     c                   movel     FVVspn        dFVVspn
445200031111sel 4c                   select
445300031111w   4c                   when      *in10       = *on    and
445400031111     c                             $MEMfv1(xx) = *loval
445500031111     c                   eval      dsFVdfv     =  FVVdfv
445600031111     c                   eval      dsFVnfv     =  FVVnfv
445700031111     c                   move      DS_FV         w0150
445800031111     c                   eval      $MEMfv1(xx) =  w0150
445900031111     c                   eval      $MEMmp1(xx) =  FVVfpp
446000050701     c                   eval      $MEMpm1(xx) =  §FVVtpm
446100031111w   4c                   when      *in10       = *on    and
446200031111     c                             $MEMfv2(xx) = *loval
446300031111     c                   eval      dsFVdfv     =  FVVdfv
446400031111     c                   eval      dsFVnfv     =  FVVnfv
446500031111     c                   move      DS_FV         w0150
446600031111     c                   eval      $MEMfv2(xx) =  w0150
446700031111     c                   eval      $MEMmp2(xx) =  FVVfpp
446800050701     c                   eval      $MEMpm2(xx) =  §FVVtpm
446900031111x   4c                   other
447000031111if  5c                   if        xx    < %elem($MEMat)
447100031111     c                   eval      xx    = xx + 1
447200031111     c                   eval      *in10 = *off
447300031111x   5c                   else
447400031111     c                   leave
447500031111e   5c                   endif
447600031111e   4c                   endsl
447700031111e   3c                   enddo
447800031014e   2c                   endif
447900031014e   1c                   endif
448000031022      *
448100031106      * Se piano giornaliero (per P.O. con o senza VDL):
448200031022if  1c                   if        $Default   =  *off
448300040519      *?Solo per VdL (almeno per ora):?
448400040519      * Controllo che il codice autotrasportatore NON sia già impostato
448500040519      *   per altra baia/postazione
448600040519      *   (se variato - perchè, altrimenti, risulta GIA' controllato
448700040519      *   nella subr. "CtrS01rec")
448800040519if  2c                   if            $VDL    =  *on
448900040519     c                             and wNFVseq =  1
449000040519     c                   eval      Wpdr   =  DSTpdr
449100040519     c                   exsr      CtrlAutotr4
449200060110     c                   if        *in90
449300060110     c                   leavesr
449400060110     c                   endif
449500040519e   2c                   endif
449600040519      * Confronto autotrasportatori dei 2 F.V. x baia
449700031022sel 2c                   select
449800040519w   2c                   when      wNFVseq     =  1
449900040519      * - memorizzo l'autotrasportatore del 1° F.V. per confrontarlo
450000031022      *   con quello del 2° F.V. (per baia)
450100040519     c                   eval      SAVpdr      =  DSTpdr
450200040519w   2c                   when          wNFVseq =  2
450300040519     c                             and DSTpdr  <> SAVpdr
450400031022      * - l'autotrasportatore del 2° F.V. deve essere uguale a quello
450500031022      *   del 1° F.V. inserito (per baia)
450600031022     c                   seton                                        562890
450700031022     c                   eval      V1Dmsg = %replace(
450800031022     c                                      %editw(DSTpdr:'     0 '):
450900031022     c                                      $Msg(36):
451000031022     c                                      %scan('&PADR  ':$Msg(36)))
451100060110     c                   leavesr
451200031022e   2c                   endsl
451300031022e   1c                   endif
451400031014      *
451500060110     c                   ENDSR
451600031014      *
451700031014      *---------------------------------------------------------------*
451800031014      * Controllo codice autotrasportatore                            *
451900031014      *---------------------------------------------------------------*
452000031022     c     CtrlAutotr2   BEGSR
452100031014      *
452200031014     c     K02FIAPD      chain     FIAPD000
452300031014      *
452400031014if  1c                   if        NOT %found(FIAPD01L)
452500031014     c                             or  APDatb <> *blanks
452600031014     c                   clear                   APDrsc
452700031014     c                   seton                                        552890
452800031014     c                   movel     $Msg(17)      V1Dmsg
452900031014e   1c                   endif
453000031014      *
453100031014     c                   ENDSR
453200031024      *
453300031024      *---------------------------------------------------------------*
453400031024      * Evidenzia autotrasportatore con più F.V.                      *
453500031024      *---------------------------------------------------------------*
453600031024     c     CtrlAutotr3   BEGSR
453700031024      *
453800031024     c                   clear                   Wast
453900031024      * Default o giornaliero, evidenzio più F.V. per il padroncino
454000031024     c                   movel     V1Cpog        Wpdr
454100031024     c                   eval      xx     =  1
454200031024     c                   eval      *in10  =  *off
454300031024     c     Wpdr          lookup    $MEMat(xx)                             10
454400031114if  1c                   if            *in10      = *on
454500031114     c                             and $MEMxf(xx) > 1
454600031024     c                   eval      Wast   =  '*'
454700031024e   1c                   endif
454800031024      *
454900031024     c                   ENDSR
455000040519      *
455100040519      *---------------------------------------------------------------*
455200040519      * Controllo codice autotrasportatore per singola baia/postaz.   *
455300040519      *---------------------------------------------------------------*
455400040519     c     CtrlAutotr4   BEGSR
455500040519      *
455600040519     c                   clear                   ds_BaiaPost
455700040519if  1c                   if        $VDL   =  *off
455800040519     c                   movel     S1Cpep        dsBPbai
455900040519     c                   movel     S1Cbai        dsBPpos
456000040519x   1c                   else
456100040519     c                   movel     S2Cpep        dsBPbai
456200040519     c                   movel     S2Cbai        dsBPpos
456300040519e   1c                   endif
456400040519      *
456500040519     c                   eval      xx     = 1
456600040519     c                   eval      *in10  = *off
456700040519     c     Wpdr          lookup    $MEMat(xx)                             10
456800040519      *
456900040519sel 1c                   select
457000040519      * autotrasportatore non ancora memorizzato
457100040519w   1c                   when      NOT *in10
457200040519     c     *zeros        lookup    $MEMat(xx)                             10
457300040519if  2c                   if        *in10
457400040519     c                   eval      $MEMat(xx) =  Wpdr
457500040519     c                   eval      $MEMbp(xx) =  ds_BaiaPost
457600040519e   2c                   endif
457700040519      * 1° utilizzo dell'autotrasportatore: ne memorizzo baia/postaz.
457800040519w   1c                   when      $MEMbp(xx) =  *blanks
457900040519     c                   movel     ds_BaiaPost   $MEMbp(xx)
458000040519      * autotrasportatore già utilizzato in altre baie/postazioni
458100040519w   1c                   when      $MEMbp(xx) <> ds_BaiaPost
458200040519     c                   movel     $MEMbp(xx)    ds_BaiaPost
458300040519if  2c                   if        $Default   =  *on
458400040519     c                   eval      *in55  =  *on
458500040519x   2c                   else
458600040519     c                   eval      *in54  =  *on
458700040519e   2c                   endif
458800040519     c                   seton                                          2890
458900040519     c                   eval      V1Dmsg = %replace(
459000040519     c                                      %editw(Wpdr:'     0 '):
459100040519     c                                      $Msg(34):
459200040519     c                                      %scan('&PADR  ':$Msg(34)))
459300040519     c                   eval      V1Dmsg = %replace(dsBPpos:V1Dmsg:
459400040519     c                                      %scan('&PS':V1Dmsg))
459500040519     c                   eval      V1Dmsg = %replace(dsBPbai:V1Dmsg:
459600040519     c                                      %scan('&B        ':V1Dmsg))
459700060110     c                   leavesr
459800040519      * ... "beccata" baia/postazione in esame
459900040519w   1c*** ok:            when      $MEMbp(xx) =  ds_BaiaPost
460000040519e   1c                   endsl
460100040519      *
460200060110     c                   ENDSR
460300040615      *
460400040615      *---------------------------------------------------------------*
460500040615      * Evidenzia P.O. con più F.V.                                   *
460600040615      *---------------------------------------------------------------*
460700040615     c     AsterFVxPO    BEGSR
460800040615      *
460900040615     c                   clear                   Wast
461000040615      *
461100040615     c                   eval      xx     =  1
461200040615     c                   eval      *in10  =  *off
461300040615     c     Wpost         lookup    $FVxPOa(xx)                            10
461400040615      *
461500040615if  1c                   if        *in10  =  *on
461600040615      *
461700040615     c                   eval      xx     =  xx + 1
461800040615     c                   eval      *in10  =  *off
461900040615     c     Wpost         lookup    $FVxPOa(xx)                            10
462000040615      *
462100040615if  2c                   if        *in10  =  *on
462200040615     c                   eval      Wast   =  '*'
462300040615e   2c                   endif
462400040615      *
462500040615e   1c                   endif
462600040615      *
462700040615     c                   ENDSR
462800031021      *
462900031021      *---------------------------------------------------------------*
463000040408      * Memorizzo (l'ultima) postazione immessa per ogni codice baia  *
463100031021      *---------------------------------------------------------------*
463200031021     c     MemBAIxPEP    BEGSR
463300031021      *
463400031021     c                   eval      xx     = 1
463500031021     c                   eval      *in10  = *off
463600040305      *
463700040305if  1c                   if        $VDL   = *off
463800040305      *
463900040305     c     S1Cpep        lookup    $PEP(xx)                               10
464000040305      * ...lo trovo sicuro, perchè li ho già memorizzati nelle subr.
464100040305      *   "ContaPEP"
464200040305if  2c                   if            *in10       =  *on
464300040409     c                             and $PEPbai(xx) =  *blanks
464400040305     c                   eval      $PEPbai(xx) = S1Cbai
464500040305e   2c                   endif
464600040305      *
464700040305x   1c                   else
464800040305      *
464900040305     c     S2Cpep        lookup    $BAU(xx)                               10
465000040305      * ...lo trovo sicuro, perchè li ho già memorizzati nelle subr.
465100040305      *   "ContaBAU"
465200040305if  2c                   if            *in10       =  *on
465300040409     c                             and $BAUbai(xx) =  *blanks
465400040309if  3c                   if        S2Cbnl      = 'S'
465500040309     c                   eval      $BAUbai(xx) = S2Cbnl
465600040309x   3c                   else
465700040305     c                   eval      $BAUbai(xx) = S2Cbai
465800040309e   3c                   endif
465900040628     c                   eval      $PosPO(xx)      =  $PO
466000040628     c                   eval      $PosPOdpd(xx)   =  $POdpd
466100040305e   2c                   endif
466200040305      *
466300040305e   1c                   endif
466400040409      *
466500040409      * Memorizzo le postazioni "IMA" immesse per controllare la
466600040409      *  effettiva immissione di quelle obbligatorie
466700161115      *?N.B.?-?Ora le postazioni IMA possono essere nella stessa?
466800161115      *       ?baia con altre postazioni NON IMA.?
466900040409if  1c                   if            *in10       =  *on
467000161115     c                             and $IMA        =  *on
467100040409     c                   eval      xx     = 1
467200040409     c                   eval      *in10  = *off
467300040409if  2c                   if        $VDL   = *off
467400040409     c     S1Cbai        lookup    $MPT(xx)                               10
467500040409x   2c                   else
467600040409     c     S2Cbai        lookup    $MPT(xx)                               10
467700040409e   2c                   endif
467800040409      * - la trova sicuramente !
467900040409if  2c                   if        *in10  = *on
468000040409     c                   add       1             $MPTnr(xx)
468100040409e   2c                   endif
468200040409e   1c                   endif
468300061011      *
468400061011      * Aggiorno anche le postazioni utilizzate per baia
468500061011      *   (per ora utile solo al controllo dell'utilizzo dei P.O.)
468600061011if  1c                   if        $VDL   = *on
468700061011     c                   eval      $PObau(S02nrr)  =  S2Cpep
468800061011     c                   eval      $POpos(S02nrr)  =  S2Cbai
468900061011e   1c                   endif
469000161212      *
469100161212      /free
469200161212
469300161212         // -?Memorizzazione SflRrn per Postazione = Filiale per il?
469400161212         //  ?controllo della Zona (cioè del Turno)?
469500161212         If    $VDL   =  *on    and
469600161212             ( $PO    =  *on  or
469700161212               $POdpd =  *on );
469800161212
469900161212           clear  xx;
470000161212           xx = %lookup ( S2Cbai : sk_Fil );
470100161212
470200161212           if  xx = *zero;
470300161213             xx = %lookup( *blank : sk_Fil );
470400161212             if  xx > *zero;
470500161212               sk_Fil(xx) = S2Cbai;
470600161212             endif;
470700161212           endif;
470800161212
470900161212           if  xx > *zero;
471000161213             select;
471100161213               // -?Memorizzazione zona generica (99)?
471200161215               when  S2Cznc  = '99';
471300161213                 sk_Rrn(xx)  = S02nrr;
471400161213               // -?Memorizzazione zona specifica SE non già?
471500161213               //  ?memorizzata una zona generica (99)?
471600161213               when  sk_Rrn(xx) <= *zero;
471700161213                 sk_Rrn(xx)  = S02nrr * (-1);
471800161213             endsl;
471900161212           endif;
472000161212
472100161212         EndIf;
472200161212
472300161212      /end-free
472400031021      *
472500031021     c                   ENDSR
472600031021      *
472700031021      *---------------------------------------------------------------*
472800040312      * Controllo delle postazioni immesse (per baia)                 *
472900031021      *---------------------------------------------------------------*
473000031021     c     CtrBAIxPEP    BEGSR
473100031021      *
473200040408      * Controllo se immessa ALMENO una postazione "IMA"
473300040408      *                   ed ALMENO una postazione "IM0"
473400040408     c                   clear                   xx
473500040408do  1c                   dou       xx   >= %elem($MPTobbl)
473600040408     c                   eval      xx    = xx + 1
473700040409if  2c                   if            $MPTobbl(xx) = *on
473800040409     c                             and $MPTnr(xx)   = *zeros
473900040408     c                   seton                                          2890
474000040519     c                   eval      V1Dmsg  =  %replace($MPT(xx):
474100040409     c                                        $Msg(31):
474200040409     c                                        %scan('IMA':$Msg(31)))
474300060110     c                   leavesr
474400040408e   2c                   endif
474500040408e   1c                   enddo
474600040312      *
474700040309      * Controllo se immessa almeno una postazione per ogni baia
474800040409      *   (ora SOLO per NO VdL)
474900060110if  1c                   if        $VDL   =  *on
475000060110     c                   leavesr
475100060110e   1c                   endif
475200060110      *
475300031021do  1c                   DO        C_MaxRecSf    xx
475400031021      *
475500040305if  2c                   if            $VDL     =  *off
475600040305     c                             and $PEP(xx) =  *blanks
475700040305     c                             OR  $VDL     =  *on
475800040305     c                             and $BAU(xx) =  *zeros
475900031021     c                   leave
476000031021e   2c                   endif
476100031021      *
476200040305if  2c                   if            $VDL        =  *off
476300040305     c                             and $PEPbai(xx) =  *blanks
476400040305     c                             OR  $VDL        =  *on
476500040305     c                             and $BAUbai(xx) =  *blanks
476600031021     c                   seton                                          2890
476700040315if  2c                   if        $VDL   =  *off
476800040315     c                   eval      C01csr = $CsrRRN(xx)
476900040519     c                   eval      V1Dmsg = %replace($PEP(xx):
477000031021     c                                      $Msg(32):
477100031021     c                                      %scan('&PEP':$Msg(32)))
477200040315x   2c                   else
477300040315     c                   eval      C02csr = $CsrRRN(xx)
477400040315     c                   eval      V1Dmsg = %replace(
477500040315     c                                      %editw($BAU(xx):'0  ')+' ':
477600040315     c                                      $Msg(32):
477700040315     c                                      %scan('&PEP':$Msg(32)))
477800040315e   2c                   endif
477900031021     c                   leave
478000040305e   2c                   endif
478100031021      *
478200031021e   1c                   ENDDO
478300031021      *
478400060110     c                   ENDSR
478500031007      *
478600031007      *---------------------------------------------------------------*
478700040628      * Controllo immissione codice baia                              *
478800031007      *---------------------------------------------------------------*
478900031007     c     ContaPEP      BegSr
479000031007      *
479100031007     c                   eval      xx       =  1
479200031007     c                   eval      *in10    =  *off
479300031007      * Controllo se già immesso
479400031010     c     S1Cpep        lookup    $PEP(xx)                               10
479500031007      * Se NON già immesso: memorizzo
479600031007if  1c                   if        *in10    =  *off
479700031007     c     *blanks       lookup    $PEP(xx)                               10
479800031007if  2c                   if        *in10
479900031007     c                   eval      $PEP(xx) =  S1Cpep
480000031007x   2c                   else
480100031007     c                   eval      xx       =  %elem($PEP)
480200031007     c                   eval      $PEP(xx) =  *hival
480300031007e   2c                   endif
480400040315     c                   eval      $CsrRRN(xx) = S01nrr
480500031007e   1c                   endif
480600031007      *
480700031007     c                   ENDSR
480800031007      *
480900031007      *---------------------------------------------------------------*
481000031007      * Controllo immissione codice baia                              *
481100031007      *---------------------------------------------------------------*
481200031007     c     ContaBAI      BegSr
481300031007      *
481400031007     c                   eval      xx       =  1
481500031007     c                   eval      *in10    =  *off
481600031007      * Controllo se già immesso
481700040423     c     Wpost         lookup    $BAI(xx)                               10
481800031007      * Se NON già immesso: memorizzo
481900031007if  1c                   if        *in10    =  *off
482000031007     c     *blanks       lookup    $BAI(xx)                               10
482100031007if  2c                   if        *in10
482200040423     c                   eval      $BAI(xx) =  Wpost
482300040628x   2c                   else
482400040628     c                   eval      xx       =  %elem($BAI)
482500040628     c                   eval      $BAI(xx) =  *hival
482600031007e   2c                   endif
482700031007e   1c                   endif
482800031007      * Aggiorno conteggio x codice
482900031007     c                   add       1             $BAInr(xx)
483000050629      *
483100050629      *?Se picking su VdL x Messaggeria:?
483200050629      * consento l'ulizzo della stessa postazione più volte,
483300050629      * ma solo nella stessa baia
483400050629if  1c                   if             §OGpkm      =  'S'
483500050629     c                             and  $VDL        =  *on
483600050629     c                             and  $IMA        =  *off
483700050629     c                             and  $PO         =  *off
483800050629      *
483900050629if  2c                   if        $BAIbau(xx) =  *zeros
484000050629     c                   eval      $BAIbau(xx) =  S2Cpep
484100050629e   2c                   endif
484200050629      * - impostazione messaggio di avvertimento
484300050629if  2c                   if             $BAInr(xx)  >  1
484400050629     c                             and  $BAIbau(xx) =  S2Cpep
484500050629     c                   eval      $BAInr(xx)  =  1
484600050629if  3c                   if        $MsgBAIbau  =  *off
484700050629     c                   seton                                        532890
484800050629     c                   eval      V1Dmsg      =  $Msg(49)
484900050629     c                   eval      $MsgBAIbau  =  *on
485000050629     c***                leavesr
485100050629e   3c                   endif
485200050629e   2c                   endif
485300050629      *
485400050629e   1c                   endif
485500031007      *
485600031007     c                   ENDSR
485700031007      *
485800031007      *---------------------------------------------------------------*
485900031007      * Controllo immissione codice baia di uscita                    *
486000031007      *---------------------------------------------------------------*
486100031007     c     ContaBAU      BegSr
486200031007      *
486300031007     c                   eval      xx       =  1
486400031007     c                   eval      *in10    =  *off
486500031007      * Controllo se già immesso
486600031007     c     S2Cpep        lookup    $BAU(xx)                               10
486700031007      * Se NON già immesso: memorizzo
486800031007if  1c                   if        *in10    =  *off
486900031007     c     *zeros        lookup    $BAU(xx)                               10
487000031007if  2c                   if        *in10
487100031007     c                   eval      $BAU(xx) =  S2Cpep
487200040628x   2c                   else
487300040628     c                   eval      xx       =  %elem($BAU)
487400040628     c                   eval      $BAU(xx) =  *hival
487500031007e   2c                   endif
487600040315     c                   eval      $CsrRRN(xx) = S02nrr
487700031007e   1c                   endif
487800031007      *
487900031007     c                   ENDSR
488000031030      *
488100031030      *---------------------------------------------------------------*
488200031030      * Controllo immissione numero foglio viaggio                    *
488300031030      *---------------------------------------------------------------*
488400031030     c     ContaNFV      BegSr
488500031030      *
488600031030     c                   eval      xx       =  1
488700031030     c                   eval      *in10    =  *off
488800031030      * Controllo se già immesso
488900031030     c                   clear                   Wnfv7
489000031030     c                   move      Wnfv          Wnfv7
489100031030     c     Wnfv7         lookup    $NFV(xx)                               10
489200031030      * Se NON già immesso: memorizzo
489300031030if  1c                   if        *in10    =  *off
489400031030     c     *zeros        lookup    $NFV(xx)                               10
489500031030if  2c                   if        *in10
489600031030     c                   eval      $NFV(xx) =  Wnfv7
489700031030x   2c                   else
489800031030     c                   eval      xx       =  %elem($NFV)
489900031030     c                   eval      $NFV(xx) =  *hival
490000031030e   2c                   endif
490100031030      * Se NON già immesso: memorizzo postazione/baia
490200040519     c                   clear                   ds_BaiaPost
490300031030if  2c                   if        $VDL     =  *off
490400040519     c                   move      S1Cpep        dsBPbai
490500040519     c                   movel     S1Cbai        dsBPpos
490600031030x   2c                   else
490700040519     c                   movel     S2Cpep        dsBPbai
490800040519     c                   movel     S2Cbai        dsBPpos
490900031030e   2c                   endif
491000040628     c                   movel     ds_BaiaPost   $NFVbp(xx)
491100040628      * Se già immesso: ne reperisco baia/postazione precedente
491200040628x   1c                   else
491300040628     c                   movel     $NFVbp(xx)    ds_BaiaPost
491400031030e   1c                   endif
491500031030      * Aggiorno conteggio x codice
491600031030     c                   add       1             $NFVnr(xx)
491700031030      *
491800031030     c                   ENDSR
491900031007      *
492000031007      *---------------------------------------------------------------*
492100031007      * Controllo immissione flag baia non letti                      *
492200031007      *---------------------------------------------------------------*
492300031007     c     ContaBNL      BegSr
492400031007      *
492500031007     c                   eval      xx       =  1
492600031007     c                   eval      *in10    =  *off
492700031007      * Controllo se già immesso
492800031007     c     S2Cbnl        lookup    $BNL(xx)                               10
492900031007      * Se NON già immesso: memorizzo
493000031007if  1c                   if        *in10    =  *off
493100031007     c     *blanks       lookup    $BNL(xx)                               10
493200031007if  2c                   if        *in10
493300031007     c                   eval      $BNL(xx) =  S2Cbnl
493400031007x   2c                   else
493500031007     c                   eval      xx       =  %elem($BNL)
493600031007     c                   eval      $BNL(xx) =  *hival
493700031007e   2c                   endif
493800031007e   1c                   endif
493900031007      * Aggiorno conteggio x codice
494000031007     c                   add       1             $BNLnr(xx)
494100031007      *
494200031007     c                   ENDSR
494300031007      *
494400031007      *---------------------------------------------------------------*
494500031007      * Ricerche                                                      *
494600031007      *---------------------------------------------------------------*
494700031007     c     srSEARCH      BEGSR
494800031007      *
494900031007sel 1c                   select
495000040423      *
495100040423      * Postazione (tab. MPT)
495200040423w   1c                   when          Wcampo   = 'S1CBAI    '
495300040423     c                             or  Wcampo   = 'S2CBAI    '
495400040423     c                   clear                   TIBS02ds
495500040423     c                   eval      T02mod = 'R'
495600040423     c                   eval      T02cod = 'MPT'
495700040423     c                   movel     KNSIF         T02sif
495800040423     c                   call      'TIBS02R'
495900040423     c                   parm                    KPJBA
496000040423     c                   parm                    TIBS02ds
496100040423if  1c                   if        T02err = *blanks
496200040423if  2c                   if        Wcampo = 'S1CBAI    '
496300040423     c                   movel     T02ke1        S1Cbai
496400040423x   2c                   else
496500040423     c                   movel     T02ke1        S2Cbai
496600040423e   2c                   endif
496700040423     c                   movel     T02uni        dMFP
496800040423e   1c                   endif
496900031007      *
497000031007      * Numero foglio viaggio
497100031013w   1c                   when          Wcampo   = 'V1CNFV    '
497200031013     c                             or (Wcampo   = 'S1CNFV    '
497300031022     c                              or Wcampo   = 'S2CNFV    '
497400031106     c                              or Wcampo   = 'S3CNFV    '
497500031106     c                              or Wcampo   = 'S4CNFV    ')
497600031007     c                             and $Default = *off
497700040611     c                             and $PO      = *off
497800031111     c                             or  *inKG    = *on
497900031015     c                   reset                   DSLR01
498000031013sel 2c                   select
498100031013w   2c                   when      Wcampo = 'V1CNFV    '
498200031013     c                   eval      DLRnpg = V1Cnpg                              Categoria foglio
498300040408w   2c                   when         (Wcampo = 'S1CNFV    '
498400040408     c                              or Wcampo = 'S3CNFV    '
498500040408     c                              or Wcampo = 'S2CNFV    '
498600040408     c                              or Wcampo = 'S4CNFV    ')
498700040408     c                             and $IMA   = *on
498800031013     c                   eval      DLRnpg = 3                                   Categoria foglio
498900031013e   2c                   endsl
499000031111if  2c                   if        Wcampo <> 'V1CNFV    '
499100031111     c                   z-add     V1Ddfv        DLRgda                         Dal gg/mm/aaaa
499200031111     c                   z-add     SAVdfva       DLRada                         Dal aaaa/mm/gg
499300031111e   2c                   endif
499400031007     c                   z-add     V1Cpog        DLRfgs                         P.O. gestione
499500031007     c                   movel     SDSpgm        DLRpgm                         Nome programma
499600031015     c                   movel(p)  DSLR01        KPJBU
499700031111if  2c                   if        *inKG  = *off
499800031007     c                   call      'FNLR02R'
499900031007     c                   parm                    KPJBA
500000031111x   2c                   else
500100031111     c                   call      'FNLR02R'
500200031111     c                   parm                    KPJBA
500300031111     c                   parm                    $FV
500400031111e   2c                   endif
500500031015     c                   movel     KPJBU         DSLR01
500600031013sel 2c                   select
500700031013w   2c                   when      Wcampo = 'V1CNFV    '
500800031013     c                   move      DLRnfv        V1Cnfv
500900031013w   2c                   when      Wcampo = 'S1CNFV    '
501000031007     c                   move      DLRnfv        S1Cnfv
501100031106     c                   move      DLRnfv        Wnfv
501200031106w   2c                   when      Wcampo = 'S2CNFV    '
501300031106     c                   move      DLRnfv        S2Cnfv
501400031106     c                   move      DLRnfv        Wnfv
501500031022w   2c                   when      Wcampo = 'S3CNFV    '
501600031022     c                   move      DLRnfv        S3Cnfv
501700031022     c                   move      DLRnfv        Wnfv
501800031106w   2c                   when      Wcampo = 'S4CNFV    '
501900031106     c                   move      DLRnfv        S4Cnfv
502000031106     c                   move      DLRnfv        Wnfv
502100031013e   2c                   endsl
502200040611      *
502300040611      * Numero foglio viaggio x P.O. DPD
502400040630      * (Con p.o. 195 si interrogano ANCHE f.v. per P.O. 330)
502500080118      * (...Anche con fil. 196 o 197)
502600040611w   1c                   when         (Wcampo   = 'S2CNFV    '
502700040611     c                              or Wcampo   = 'S4CNFV    ')
502800040611     c                             and $Default = *off
502900040611     c                             and $POdpd   = *on
503000130506     c                   reset                   fnlst3ds
503100040611     c                   eval      DLSdti =  V1Ddfv
503200040621     c                   eval      DLSdtf =  31129999
503300130506     c                   movel(p)  fnlst3ds      KPJBU
503400040611     c                   move      S2Cbai        DLSpo
503500040630     c                   clear                   DLSpo2
503600070521if  2c                   if            S2Cbai   = '195'
503700080118     c                             or  S2Cbai   = '196'
503800070521     c                             or  S2Cbai   = '197'
503900040630     c                   eval      DLSpo2 =  330
504000040630e   2c                   endif
504100130506     c                   call      'FNLST3R'
504200040611     c                   parm                    KPJBA
504300040611     c                   parm                    DLSpo
504400040630     c                   parm                    DLSpo2
504500130506     c                   movel     KPJBU         fnlst3ds
504600040611sel 2c                   select
504700040611      * - non selezionato alcun f.v.
504800040611w   2c                   when      DLSnfv = *zeros
504900040611      * - selezionato il 1° f.v.
505000040621w   2c                   when      Wcampo = 'S2CNFV    '
505100040611     c                   move      DLSnfv        S2Cnfv
505200040611     c                   move      DLSnfv        Wnfv
505300040611      * - selezionato il 2° f.v. (sarebbe proibito!!!)
505400040611w   2c                   when      Wcampo = 'S4CNFV    '
505500040611     c                   move      DLSnfv        S4Cnfv
505600040611     c                   move      DLSnfv        Wnfv
505700040611e   2c                   endsl
505800031007      *
505900031007      * Autotrasportatore
506000031007w   1c                   when          Wcampo   = 'S1CPDC    '
506100031007     c                             or  Wcampo   = 'S2CPDC    '
506200031007     c                   reset                   FNLV24ds
506300031007     c                   z-add     V1Cpog        D24fil
506400031007     c                   movel(p)  FNLV24ds      KPJBU
506500031007     c                   call      'FNLV24R'
506600031007     c                   parm                    KPJBA
506700031007     c                   movel     KPJBU         FNLV24ds
506800031007if  2c                   if        D24pdr <> *zeros
506900031007sel 3c                   select
507000031007w   3c                   when          Wcampo   = 'S1CPDC    '
507100031007     c                             and $Default = *on
507200031007     c                   move      D24pdr        S1Cpdc
507300031007     c                   movel     D24rsc        S1Dpdc
507400031007w   3c                   when          Wcampo   = 'S2CPDC    '
507500031007     c                             and $Default = *on
507600031007     c                   move      D24pdr        S2Cpdc
507700031007     c                   movel     D24rsc        S2Dpdc
507800031007w   3c                   when          Wcampo   = 'S1CPDC    '
507900031007     c                             and $Default = *off
508000031007     c                   move      D24pdr        S1Cpdc
508100031007     c                   movel     D24rsc        S3Dpdc
508200031007w   3c                   when          Wcampo   = 'S2CPDC    '
508300031007     c                             and $Default = *off
508400031007     c                   move      D24pdr        S2Cpdc
508500031007     c                   movel     D24rsc        S4Dpdc
508600031007e   3c                   endsl
508700031007e   2c                   endif
508800031007      *
508900031007e   1c                   endsl
509000031007      *
509100031007     c                   ENDSR
509200031112      *
509300031112      *---------------------------------------------------------------*
509400031112      * Controllo esistenza di distinte aperte non inserite nel piano *
509500031112      *---------------------------------------------------------------*
509600031112     c     CtrlDistAp    BEGSR
509700031112      *
509800031112      * Memorizzo il tasto funzionale F7=Salvataggio e Stampa
509900031112     c                   movel     *inKH         WinKH
510000031112      *
510100031112      * Richiamo pgm. FNLR02R per fare il controllo
510200031112     c                   reset                   DSLR01
510300031112     c                   movel     '3'           DLRtfv
510400031112     c                   z-add     V1Ddfv        DLRgda                         Dal gg/mm/aaaa
510500031112     c                   z-add     SAVdfva       DLRada                         Dal aaaa/mm/gg
510600031112     c                   z-add     V1Cpog        DLRfgs                         P.O. gestione
510700031112     c                   movel     SDSpgm        DLRpgm                         Nome programma
510800031112     c                   movel(p)  DSLR01        KPJBU
510900031112     c                   call      'FNLR02R'
511000031112     c                   parm                    KPJBA
511100031112     c                   parm                    $FV
511200031112     c                   movel     KPJBU         DSLR01
511300031112      *
511400031112      * Emetto (eventualmente) segnalazione a video
511500031112if  1c                   if        DLRfs1 = *on
511600031112     c                   clear                   MS03W01
511700031112     c                   movel     $Msg(41)      W1Dms2
511800031112     c                   eval      W1Dtfa = 'F6=Conferma   '
511900031112     c                                    + 'F12=Ritorno'
512000031112do  2c                   dou       *inKF   or   *inKL
512100031112     c                   exfmt     MS03W01
512200031112e   2c                   enddo
512300031112if  2c                   if        *inKL
512400031112     c                   movel     *off          WinKH
512500031112e   2c                   endif
512600031112e   1c                   endif
512700031112      *
512800031112     c                   ENDSR
512900080129      *
513000080129      *---------------------------------------------------------------*
513100080129      * Controllo mancaato inserimento di F.V. per filiali DPD        *
513200080129      *---------------------------------------------------------------*
513300080129     c     CtrlFVdpd     BEGSR
513400080129      *
513500080129      * Memorizzo il tasto funzionale F7=Salvataggio e Stampa
513600080129     c                   movel     *inKH         WinKH
513700080129      *
513800080129      * Conteggio delle segnalazioni di filiali DPD senza F.V.
513900080129     c                   clear                   wx
514000080129     c                   clear                   wy
514100080129do  1c                   DOW       wx < %elem($ORGerr)
514200080129     c                   add       1             wx
514300080129if  2c                   if        $ORGerr(wx) =  *on
514400080129     c                   eval      wy = wy + 1
514500080129e   2c                   endif
514600080129e   1c                   ENDDO
514700080129if  1c                   if        wy = *zeros
514800080129     c                   leavesr
514900080129e   1c                   endif
515000080129      *
515100080129      * Avviso del totale di tali segnalazioni già effettuate
515200080129     c                   clear                   MS03W01
515300080129     c                   movel     $Msg(51)      W1Dms1
515400080129     c                   eval      W1Dms1  =  %replace(
515500080129     c                                          %editc(wy:'Z'):
515600080129     c                                          $Msg(51):
515700080129     c                                          %scan( '&FV':$Msg(51) ) )
515800080129     c                   eval      W1Dms2  = $Msg(52)
515900080129     c                   eval      W1Dms3  = $Msg(53)
516000080129     c                   eval      W1Dtfa  = 'Enter=Conferma'
516100080129     c                                     + '   '
516200080129     c                                     + 'F12=Ritorno'
516300080129do  1c                   dou           $Tasto = C_Enter
516400080129     c                             or  $Tasto = C_F12
516500080129     c                   exfmt     MS03W01
516600080129e   1c                   enddo
516700080129      *
516800080129sel 1c                   select
516900080129w   1c                   when      $Tasto  = C_F12
517000080129     c                   eval      WinKH   = *off
517100080129w   1c                   when      WinKH   = *off
517200080129     c                   eval      *inKF   = *on
517300080211x   1c                   other
517400080211     c                   eval      *inKH   = WinKH
517500080129e   1c                   endsl
517600080129      *
517700080129     c                   ENDSR
517800031007      *
517900031007      *---------------------------------------------------------------*
518000031007      * Aggiornamento dati nel file FLTR900F                          *
518100031007      *---------------------------------------------------------------*
518200031007     c     Upd_FLTR9     BEGSR
518300031007      *
518400031007      * Cancellazione dei record esistenti
518500031007     c                   exsr      Dlt_FLTR9
518600031007      *
518700031007      * Aggiunta dei record impostati a video nel sfl
518800031107     c                   reset                   SAVpep
518900031107     c                   reset                   SAVbau
519000031107     c                   reset                   SAVpdr
519100031007     c                   clear                   S01nrr
519200031007     c                   clear                   S02nrr
519300031107      *
519400031009do  1c                   DO        MAXnrr
519500031007      *
519600031007sel 2c                   select
519700031007w   2c                   when      $Video = '2'
519800031007     c                   add       1             S01nrr
519900031007     c     S01nrr        chain     MS03S01
520000031007w   2c                   when      $Video = '3'
520100031007     c                   add       1             S02nrr
520200031007     c     S02nrr        chain     MS03S02
520300031007w   2c                   when      $Video = '4'
520400031007     c                   add       1             S01nrr
520500031007     c     S01nrr        chain     MS03S03
520600031007w   2c                   when      $Video = '5'
520700031007     c                   add       1             S02nrr
520800031007     c     S02nrr        chain     MS03S04
520900031007e   2c                   endsl
521000031007      *
521100031007if  2c                   if        NOT %found(FIMS03D)
521200031007     c                   leave
521300031007e   2c                   endif
521400031007      *
521500040409if  2c                   if             $VDL     = *off
521600040409     c                             and (S1Cpep   = *blanks
521700040409     c                             and  S1Cdes   = *blanks
521800040409     c                             and  S1Cbai   = *blanks
521900040409     c                             and (S1Cnfv   = *blanks
522000040409     c                              or  S1Cnfv   = *zeros)
522100040409     c                             and (S1Cpdc   = *blanks
522200040409     c                              or  S1Cpdc   = *zeros))
522300040409     c                             OR   $VDL     = *on
522400040409     c                             and (S2Cpep   = *zeros
522500040409     c                             and  S2Cdes   = *blanks
522600040409     c                             and  S2Cbnl   = *blanks
522700040409     c                             and  S2Cbai   = *blanks
522800161215     c                             and  S2Cznc   = *blanks
522900040409     c                             and (S2Cnfv   = *blanks
523000040409     c                              or  S2Cnfv   = *zeros)
523100040409     c                             and (S2Cpdc   = *blanks
523200040409     c                              or  S2Cpdc   = *zeros))
523300031007     c                   iter
523400031007e   2c                   endif
523500031007      *
523600031007     c                   exsr      Wrt_FLTR9
523700031007      *
523800031007e   1c                   ENDDO
523900031007      *
524000031007     c                   ENDSR
524100031007      *
524200031007      *---------------------------------------------------------------*
524300031007      * Delete da FLTR900F dei dati esistenti                         *
524400031007      *---------------------------------------------------------------*
524500031007     c     Dlt_FLTR9     BEGSR
524600031007      *
524700031007     c                   eval      TR9fgs   = V1Cpog
524800040415if  1c                   if        V1Cnfv   = *zeros
524900040415     c                   clear                   TR9npga
525000040415     c                   clear                   TR9nfva
525100040415     c                   else
525200040415     c                   move      V1Cnpg        TR9npga
525300031013     c                   move      V1Cnfv        TR9nfva
525400040415e   1c                   endif
525500031007     c     K03FLTR9      setll     FLTR9000
525600031007      *
525700031023     c                   dou       NOT %found(FLTR901L)
525800031007     c     K03FLTR9      delete    FLTR9000
525900031007     c                   enddo
526000031007      *
526100031007     c                   ENDSR
526200031007      *
526300031007      *---------------------------------------------------------------*
526400031007      * Write  in FLTR900F del singolo record impostato nel subfile   *
526500031007      *---------------------------------------------------------------*
526600031007     c     Wrt_FLTR9     BEGSR
526700040305      *
526800060113      * Calcolo del progressivo per baia in caso di immissione
526900060113      *   postazione in default
527000060113      *   (nel piano giornaliero NON è possibile inserire altre baie o
527100060113      *   postazioni...)
527200040305sel 1c                   select
527300060111      *
527400060113w   1c                   when           $Default = *on
527500060113     c                             and  $VDL     = *off
527600060113     c                             and  S1Cpep  <> SAVpep
527700060111     c                   eval      *in10    = *off
527800060111     c                   eval      xx       = 1
527900060111     c     S1Cpep        lookup    $PEP(xx)                               10
528000060111if  2c                   if        NOT *in10
528100060111     c     *blanks       lookup    $PEP(xx)                               10
528200060111if  3c                   if        *in10
528300060113     c                   eval      $PEP(xx) =  S1Cpep
528400060111e   3c                   endif
528500060111e   2c                   endif
528600060113     c                   eval      SAVpep   = S1Cpep
528700060111      *
528800060113w   1c                   when           $Default = *on
528900060113     c                             and  $VDL     = *on
529000060113     c                             and  S2Cpep  <> SAVbau
529100060111     c                   eval      *in10    = *off
529200060111     c                   eval      xx       = 1
529300060111     c     S2Cpep        lookup    $BAU(xx)                               10
529400060111if  2c                   if        NOT *in10
529500060111     c     *zeros        lookup    $BAU(xx)                               10
529600060111if  3c                   if        *in10
529700060111     c                   eval      $BAU(xx) = S2Cpep
529800060111e   3c                   endif
529900060111e   2c                   endif
530000040305     c                   eval      SAVbau   = S2Cpep
530100060111      *
530200040305e   1c                   endsl
530300060111      *
530400040621      * incremento progressivo per baia (anche per "XXX")
530500060113sel 1c                   select
530600060113      * - Baia "XXX" in piano giornaliero
530700060113if  1c                   when           $VDL     = *off
530800040621     c                             and  $Default = *off
530900040621     c                             and  S1Cbai   = C_PostazNull
531000040621     c                             OR   $VDL     = *on
531100040621     c                             and  $Default = *off
531200040621     c                             and  S2Cbai   = C_PostazNull
531300060113     c                   add       2             SAVprg
531400040621     c                   leavesr
531500060113      * - Piano giornaliero VDL
531600060113w   1c                   when           $VDL     = *on
531700060113     c                             and  $Default = *off
531800060113     c                   eval      SAVprg   = H2Cprg
531900161110      * - Piano giornaliero NON VDL
532000060113w   1c                   when           $VDL     = *off
532100060113     c                             and  $Default = *off
532200060113     c                   eval      SAVprg   = H1Cprg
532300060113      * - Piano di default
532400060113w   1c                   when           $Default = *on
532500060113     c                   add       1             SAVprg
532600060113e   1c                   endsl
532700061011      *
532800061011      * Incremento progressivo per postazione = P.O.
532900061011     c                   clear                   wFL3
533000061011if  1c                   if        $VDL     = *on
533100061011do  2c                   do        S02nrr        wx
533200061011if  3c                   if        S2Cbai   = $POpos(wx)
533300061011     c                   add       1             wFL3
533400061011e   3c                   endif
533500061011e   2c                   enddo
533600061011      * (almeno il record in esame è già stato conteggiato
533700061011      *  SE NON piando di default)
533800061011if  2c                   if        wFL3     = *zeros
533900061011     c                   eval      wFL3     = 1
534000061011e   2c                   endif
534100061011e   1c                   endif
534200031007      *
534300031007     c                   clear                   FLTR9000
534400031007      *
534500031007     c                   eval      TR9fgs   = V1Cpog
534600040408if  1c                   if        V1Cnfv   > *zeros
534700031007     c                   eval      TR9npga  = V1Cnpg
534800031013     c                   move      V1Cnfv        TR9nfva
534900040415e   1c                   endif
535000040305     c                   move      SAVprg        TR9prg
535100031007      *
535200031007      * campi x NO VDL
535300031007if  1c                   if        $VDL     = *off
535400031007      *
535500031010     c                   move      S1Cpep        TR9pep
535600031007     c                   eval      TR9des   = S1Cdes
535700171106if  2c*//*               eval      %subst( TR9des : 28 : 1 ) = V1Ctps
535800171106if  2c                   eval      %subst( TR9des : 28 : 1 ) = 'R'
535900031030     c*** (solo VDL)     eval      TR9bnl   = *blanks
536000031007     c                   eval      TR9bai   = S1Cbai
536100031013if  2c                   if            S1Cpdc <> *blanks
536200031013     c                             and S1Cpdc <> *zeros
536300031021     c                   movel     V1Cpog        TR9pdc
536400031007     c                   move      S1Cpdc        TR9pdc
536500031010e   2c                   endif
536600031007if  2c                   if        $Default = *off
536700040423     c                   eval      $Hidden  = *off
536800040423     c                   eval      Wpost    = S1Cbai
536900040408     c                   exsr      Ctrl_IMA
537000040408if  3c                   if        $IMA     = *on
537100031007     c                   eval      TR9npg   = 3
537200031007x   3c                   else
537300031007     c                   eval      TR9npg   = 4
537400031007e   3c                   endif
537500031007     c                   move      S1Cnfv        TR9nfv
537600031007     c                   clear                   WLBda8
537700031007     c                   eval      G08dat   = S1Ddfv
537800031007     c                   eval      G08err   = '0'
537900031007     c                   call      'XSRDA8'
538000031007     c                   parm                    WLBda8
538100031021     c                   eval      TR9dfv   = G08inv
538200031022     c                   eval      TR9fl1   = S1Dfpp
538300031007e   2c                   endif
538400031007      *
538500031007      * campi x SÌ VDL
538600031007x   1c                   else
538700031007      *
538800031007     c                   movel(p)  S2Cpep        TR9pep
538900031007     c                   eval      TR9des   = S2Cdes
539000171106if  2c*//*               eval      %subst( TR9des : 28 : 1 ) = V1Ctps
539100171106if  2c                   eval      %subst( TR9des : 28 : 1 ) = 'V'
539200170214if  2c*//?NON impostato:?if        $PO      = *off         ?($PO non è ancora impostato)?
539300170214if  2c                   if        S2Cznc  <> *blank
539400161215     c                   eval      %subst( TR9des : 29 ) = S2Cznc
539500161213e   2c                   endif
539600031007     c                   eval      TR9bnl   = S2Cbnl
539700040305     c                   eval      TR9bai   = S2Cbai
539800031013if  2c                   if            S2Cpdc <> *blanks
539900031013     c                             and S2Cpdc <> *zeros
540000031021     c                   movel     V1Cpog        TR9pdc
540100031007     c                   move      S2Cpdc        TR9pdc
540200031010e   2c                   endif
540300031007if  2c                   if        $Default = *off
540400031007if  3c                   if        S2Cnfv   > *zeros
540500040423     c                   eval      $Hidden  = *off
540600040423     c                   eval      Wpost    = S2Cbai
540700040408     c                   exsr      Ctrl_IMA
540800040615     c                   exsr      Ctrl_PO
540900040615sel 4c                   select
541000161213w   4c                   when      $PO      = *on
541100040615     c                   eval      TR9npg   = 1
541200040615w   4c                   when      $IMA     = *on
541300040309     c                   eval      TR9npg   = 3
541400040615x   4c                   other
541500040309     c                   eval      TR9npg   = 4
541600040615e   4c                   endsl
541700040309e   3c                   endif
541800031007     c                   move      S2Cnfv        TR9nfv
541900031007     c                   clear                   WLBda8
542000031007     c                   eval      G08dat   = S2Ddfv
542100031007     c                   eval      G08err   = '0'
542200031007     c                   call      'XSRDA8'
542300031007     c                   parm                    WLBda8
542400031021     c                   eval      TR9dfv   = G08inv
542500031029     c                   eval      TR9fl1   = S2Dfpp
542600031007e   2c                   endif
542700061011     c                   move      wFL3          TR9fl3
542800031007      *
542900031007e   1c                   endif
543000040702      *
543100040702      * campi "comuni"
543200040702if  1c                   if        V1Cdpd   = 'S'
543300040702     c                   move      'D'           TR9fl2
543400040702e   1c                   endif
543500040702      *                  __________________
543600031007     c                   WRITE     FLTR9000
543700040702      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
543800031022      *
543900031022      * EVENTUALE 2° record per 2° F.V.
544000031022      *
544100040621if  1c                   if           $Default = *off
544200040621     c                   add       1             SAVprg
544300040621e   1c                   endif
544400040415if  1c                   if           $Default = *off
544500040415     c                             and $VDL    = *off
544600031022     c                             and S3Cnfv <> *blanks
544700031022     c                             and S3Cnfv <> *zeros
544800040415     c                             OR $Default = *off
544900040415     c                             and $VDL    = *on
545000031106     c                             and S4Cnfv <> *blanks
545100031106     c                             and S4Cnfv <> *zeros
545200031022      *
545300040621     c*** già sopra:     add       1             SAVprg
545400040305     c                   eval      TR9prg   = SAVprg
545500031106     c                   clear                   WLBda8
545600031106     c                   eval      G08err   = '0'
545700031106if  2c                   if        $VDL     = *off
545800031022     c                   move      S3Cnfv        TR9nfv
545900031022     c                   eval      G08dat   = S3Ddfv
546000031022     c                   call      'XSRDA8'
546100031022     c                   parm                    WLBda8
546200031022     c                   eval      TR9dfv   = G08inv
546300031022     c                   eval      TR9fl1   = S3Dfpp
546400031106x   2c                   else
546500031106     c                   move      S4Cnfv        TR9nfv
546600031106     c                   eval      G08dat   = S4Ddfv
546700031106     c                   call      'XSRDA8'
546800031106     c                   parm                    WLBda8
546900031106     c                   eval      TR9dfv   = G08inv
547000031106     c                   eval      TR9fl1   = S4Dfpp
547100031106e   2c                   endif
547200040702      *                  __________________
547300031022     c                   WRITE     FLTR9000
547400040702      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
547500031022e   1c                   endif
547600031007      *
547700031007     c                   ENDSR
547800031007
547900031007      *---------------------------------------------------------------*
548000031007      * SCHIERE A TEMPO DI COMPILAZIONE                               *
548100031007      *---------------------------------------------------------------*
548200151106** -- $MSG ------------------------------------------------------------------*
548300070530Immettere la filiale gestione                                                     1
548400070530Filiale gestione errata                                                           2
548500070530Filiale non abilitata al picking diretto                                          3
548600031007Categoria foglio obbligatoria                                                     4
548700031007Categoria foglio errata                                                           5
548800070530Filiale gestione non abilitata all'utilizzo della categoria foglio immessa        6
548900040312Immettere il numero Foglio Viaggio                                                7
549000031010Immettere un numero foglio arrivi esistente e non annullato                       8
549100031009             Mancano ANCHE i dati di default...                                   9
549200031211Immettere il codice baia                                                         10
549300031211Codice baia errato                                                               11
549400070530Filiale non abilitata al piano DPD                                               12
549500031211Immettere il codice postazione                                                   13
549600031211Codice postazione errato                                                         14
549700031211Codice postazione già inserito                                                   15
549800040629Impossibile specificare codice AUT per codice postazione "IMA"                   16
549900031009Codice autotrasportatore errato                                                  17
550000070530Impossibile specificare codice AUT per codice postazione "Filiale"               18
550100040315         CANCELLARE il piano giornaliero in esame ?                              19
550200031009Numero Foglio Viaggio errato                                                     20
550300040702Immettere un numero Foglio Viaggio valido                                        21
550400040408F.V. IMA diverso da quello già impostato in questo piano (&FIMA)                 22
550500070530Immettere un numero F.V. valido e con filiale arrivo &PS                         23
550600031009Immettere la baia di uscita                                                      24
550700031009Baia di uscita errata (deve essere compresa tra 01 e 98)                         25
550800040305Impossibile specificare codice postazione per "baia non letti"                   26
550900031009Flag "Baia Non Letti" errato: 'S' = sì / ' ' = no                                27
551000031009Flag "Baia Non Letti" già specificato per un'altra baia di uscita                28
551100031009Impossibile specificare codice autotrasportatore per "baia non letti"            29
551200031009Immettere il numero foglio IMA                                                   30
551300031211Immettere almeno una postazione "IMA"                                            31
551400031211Immettere almeno una postazione valida per la baia &PEP                          32
551500070530Filiale già specificata come postazione                                          33
551600040519Autotrasportatore &PADR   già impegnato in postazione &PS di baia &B             34
551700031022Immettere un 2° F.V. diverso dal 1° F.V.                                         35
5518000310222° F.V. assegnato ad autotrasportatore (&PADR  ) diverso da quello del 1° F.V.   36
551900040629Immettere SOLO un F.V. per postazione "IMA"                                      37
552000031022F.V. di data antecedente a quella del F.V. arrivi                                38
552100040312Foglio Viaggio già inserito per la postazione &PS                                39
552200161110Immesse postazioni IMA e/o per autotrasportatori e/o filiali nella stessa baia   40 - LIBERO
552300031112ATTENZIONE: esistono distinte aperte non inserite nel piano.                     41
552400070530Immettere SOLO un F.V. per postazione "Filiale DPD"                              42
552500070530Foglio Viaggio con filiale di arrivo &PS                                         43
552600070530La postazione "Filiale DPD"  deve essere univoca per baia di uscita              44
552700060110?         Esiste un piano smistamento per picking.          |                   ?45
552800060110   Premere F6 per cancellarlo e proseguire con piano DPD,   |                    46
552900060110   altrimenti premere F12.                                  |                    47
553000070530     Mancano postazioni "Filiale DPD" nel piano di default.                      48
553100050629Esistono postazioni impostate più volte nella stessa baia - ENTER per forzare    49
553200080130Filiale DPD senza F.V. - ENTER per forzare                                       50
553300080129?           Lasciate &FV filiali DPD senza F.V.             |                   ?51
553400080130   Premere Invio per confermare; altrimenti premere F12.    |                    52
553500080130                                                            |                    53
553600090203Immettere una "Baia Non Letti" (obbligatoria)                                    54
553700151109Esiste già il Piano Smistamento Picking _         per il giorno __/__/____       55
553800151106Rilevato errore nell'estrazione dei dati del F.V. arrivi: avvisare CED           56
553900161110La stessa baia può contenere SOLO postaz. IMA/per autotrasp. o SOLO "Filiali"    57
554000161212Per la linea di defluenza ___ è obbligatoria la zona 99 in almeno una baia       58
554100161214La ZONA è inseribile solo per linee di defluenza                                 59
554200161214Immettere una Zona per la linea di defluenza ___                                 60
554300161215Zona errata                                                                      61
