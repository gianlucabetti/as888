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
038400031007      * 10    - Comodo                                                *
038500031007      * 28    - Emette il messaggio di errore a video                 *
038600031007      * 30    - Ripulisce il subfile                                  *
038700031007      * 31    - NON emette il subfile                                 *
038800031007      * 32    - Record modificato nel subfile (sflnxtchg)             *
038900031007      * 33    - Fine dati nel subfile         (sflend)                *
039000040702      * 39    - Codice P.O. in gestione   protetto     (PR)           *
039100040702      *        ?se?livello P.O. utente = '2'                          *
039200031007      * 41    - P.O. in gestione errato                               *
039300031007      * 42    - Categoria foglio errato                               *
039400031007      * 43    - Numero F.V. errato                                    *
039500040705      * 44    - P.O. non abilitato al piano smistamento DPD           *
039600031007      * 51    - Codice postazione / baia-uscita  errato               *
039700031022      * 52    - Descrizione mancante (NON usato)                      *
039800031007      * 53    - Codice baia  errato                                   *
039900031022      * 54    - Numero foglio viaggio  errato                         *
040000031007      * 55    - Codice autotrasportatore  errato                      *
040100031022      * 56    - 2° Numero foglio viaggio  errato                      *
040200040304      * 57    - Flag "baia non letti"  errato                         *
040300161212      * 58    - Zona (cioè Turno) per Baia errata                     *
040400040623      * 80    - Tutti i campi I/O del sfl protetti     (PR)           *
040500040623      *         descr. autotrasportatore  sottolineato (UL)           *
040600040309      *         (baia "non letti" in piano giornaliero per VdL)       *
040700040623      * 85    - Codice autotrasportatore  in evidenza  (RI)           *
040800031023      *         (variato in piano giornaliero rispetto al default)    *
040900040628      *         (variata postazione in piano giornaliero rispetto al  *
041000040628      *         default)                                              *
041100040623      * 86    - Descr. autotrasportatore  in evidenza  (HI)           *
041200040628      *         (postazione IMx o P.O.)                               *
041300050629      * 87    - Flag Messaggeria attivo                (RI)           *
041400050629      *         (Autotrasportatore Messaggeria - in default;          *
041500050629      *          1°  F.V.          Messaggeria - in smistamento)      *
041600050614      * 88    - Flag Messaggerie attivo                (RI)           *
041700050629      *         (2°  F.V.          Messaggeria - in smistamento)      *
041800031007      * 90    - Generico di errore                                    *
041900031007      *---------------------------------------------------------------*
042000031007
042100031007     c     *Entry        plist
042200031007     c                   parm                    KPJBA
042300031007      *
042400031007      * Operazioni Iniziali
042500040705     c                   exsr      RoutInz
042600031007      *
042700031007      * Gestione Video
042800031007do  1c                   DOW       $Fine = *off
042900031007      * - Filtro di lancio
043000031007cas 2c     $Video        caseq     '1'           GesD01
043100031007      * - Window con messaggio
043200031007cas 2c     $Video        caseq     'W'           GesW01
043300031106      * - SubFile (Default o Giornaliero, con VDL o senza VDL)
043400031106cas 2c                   cas                     GesSFL
043500031007e   2c                   endcs
043600031007e   1c                   ENDDO
043700031007      *
043800031007      * Fine
043900040623     c                   close     FLTR901X
044000040623     c                   eval      Qcmd = 'DLTOVR  FILE(FLTR901X)'
044100040623     c                   call      'QCMDEXC'
044200040623     c                   parm                    Qcmd
044300040623     c                   parm      50            Qlen
044400040623      *
044500031010     c                   clear                   TRUL06ds
044600031010     c                   eval      D06tla = 'C'
044700031010     c                   movel(p)  TRUL06ds      KPJBU
044800031010     c                   call      'TRUL06R'
044900031010     c                   parm                    KPJBA
045000031010     c                   clear                   FNLV55ds
045100031010     c                   eval      D55tla = 'C'
045200031010     c                   call      'FNLV55R'
045300031010     c                   parm                    FNLV55ds
045400031010     c                   reset                   TIBS02ds
045500031010     c                   eval      T02tla = 'C'
045600031010     c                   call      'TIBS02R'
045700031010     c                   parm                    KPJBA
045800031010     c                   parm                    TIBS02ds
045900031007     c                   movel     *on           *inLR
046000031007      *
046100031007      *---------------------------------------------------------------*
046200031007      * Operazioni Iniziali                                           *
046300031007      *---------------------------------------------------------------*
046400040705     c     RoutInz       BEGSR
046500031007      *
046600031007      * Definizione Key-List
046700031023      * - FLTR901L
046800031007     c     K03FLTR9      klist
046900031009     c                   kfld                    TR9fgs
047000031009     c                   kfld                    TR9npgA
047100031009     c                   kfld                    TR9nfvA
047200040621     c     K05TR901      klist
047300040623     c                   kfld                    TR9fgs
047400040623     c                   kfld                    XR9npgA
047500040623     c                   kfld                    XR9nfvA
047600040623     c                   kfld                    TR9pep
047700040621     c                   kfld                    XR9prg
047800040621     c     K06TR901      klist
047900040623     c                   kfld                    TR9fgs
048000040623     c                   kfld                    XR9npgA
048100040623     c                   kfld                    XR9nfvA
048200040623     c                   kfld                    TR9pep
048300040621     c                   kfld                    XR9prg
048400040621     c                   kfld                    TR9bai
048500031007      * - FIAPD01L
048600031007     c     K02FIAPD      klist
048700031007     c                   kfld                    APDtip
048800031007     c                   kfld                    APDpdr
048900031007     c                   eval      APDtip = 'A'
049000031007      * - FNFVV03L
049100031007     c     K03FNFVV      klist
049200031007     c                   kfld                    FVVfgs
049300031007     c                   kfld                    FVVnpg
049400031007     c                   kfld                    FVVnfv
049500031007     c     K02FNFVV      klist
049600031007     c                   kfld                    FVVfgs
049700031007     c                   kfld                    FVVnpg
049800031009      * - FNFVV08L
049900031009     c     K03FVV08      klist
050000031010     c                   kfld                    FV8fgs
050100031010     c                   kfld                    FV8npg
050200031010     c                   kfld                    FV8dfv
050300031010     c     K02FVV08      klist
050400031010     c                   kfld                    FV8fgs
050500031010     c                   kfld                    FV8npg
050600120613      * - FIDST01L
050700120613     c     K03FIDST      klist
050800120613     c                   kfld                    DSTnpg
050900120613     c                   kfld                    DSTnfv
051000120613     c                   kfld                    DSTfgs
051100040621      * - FNFGV02L
051200040621     c     K03FGV02      klist
051300040621     c                   kfld                    FGVlnp
051400040621     c                   kfld                    FGVdfv
051500040621     c                   kfld                    FGVnfv
051600040621      * - FNFGV01L
051700040621     c     K02FGV01      klist
051800040621     c                   kfld                    FGVnfv
051900040621     c                   kfld                    FGVlnp
052000031007      * - TNTBE01L
052100031007     c     K02TNTBE      klist
052200031007     c                   kfld                    TBEcod
052300031007     c                   kfld                    TBEke1
052400040302      * - TABEL00F
052500040227     c     KTAB          klist
052600040227     c                   kfld                    TBLkut
052700040227     c                   kfld                    TBLcod
052800040227     c                   kfld                    TBLkey
052900040227     c                   eval      TBLkut = 1
053000031007      *
053100031007      * Reperisco dati job
053200031007     c                   exsr      DatiJob
053300031008     c                   movel     SDSpgm        V1Tpgm
053400031007      *
053500031007      * Reperisco la data corrente
053600031007     c                   time                    n14                            *Ora (6)+ Data (8)
053700031007     c                   move      n14           n08                            *Data (8) in g/m/a
053800031106     c                   clear                   WLBda8
053900031007     c                   z-add     n08           G08dat
054000031106     c                   eval      G08err = '0'
054100031007     c                   call      'XSRDA8'
054200031007     c                   parm                    WLBda8
054300031007     c                   z-add     G08inv        Wdate                          *Data corrente a/m/g
054400031010      *
054500031010      * Carico schiera P.O. gestiti (£1)
054600031010     c                   clear                   TRUL06ds
054700031010     c                   movel     '£1'          D06cod
054800040701     c                   movel     DUTpou        D06key
054900031010     c                   movel(p)  TRUL06ds      KPJBU
055000031010     c                   call      'TRUL06R'
055100031010     c                   parm                    KPJBA
055200031010     c                   movel     KPJBU         TRUL06ds
055300040701     c                   movea     Lin           £1
055400040607      *
055500040607      * Carico schiera di tutti i P.O.
055600040607     c                   clear                   xx
055700040607     c                   clear                   $ORGfil
055800040607     c                   clear                   $ORGdes
055900040607     c*** qui non serve: clear                   $ORGnr
056000080129     c*** qui non serve: reset                   $ORGerr
056100040607     c     *loval        setll     AZORG
056200040607     c                   read      AZORG
056300040607do  1c                   dow       NOT %eof(AZORG01L)
056400040607if  2c                   if        ORGfva = *blanks
056500040607     c                   add       1             xx
056600040607     c                   move      ORGfil        $ORGfil(xx)
056700040607     c                   movel     ORGdes        $ORGdes(xx)
056800040611     c                   movel     ORGde3        Og143
056900040611     c                   movel     §OGntw        $ORGntw(xx)
057000040607e   2c                   endif
057100040607     c                   read      AZORG
057200040607e   1c                   enddo
057300040623      *
057400040623      * Override
057500040623     c                   eval      Qcmd = 'OVRDBF  FILE(FLTR901X)'
057600040623     c                                  +      ' TOFILE(FLTR901L)'
057700040623     c                   call      'QCMDEXC'
057800040623     c                   parm                    Qcmd             50
057900040623     c                   parm      50            Qlen             15 5
058000040623     c                   open      FLTR901X
058100031007      *
058200031007     c                   ENDSR
058300031007      *
058400031007      *---------------------------------------------------------------*
058500031007      * Reperimento Dati del job (Utente/Operativi)                   *
058600031007      *---------------------------------------------------------------*
058700031007     c     DatiJob       BEGSR
058800031007      *
058900031007     c     *dtaara       define    §azute        azuteds
059000031007     c     *dtaara       define    §datiute      ddatiute
059100031007      *
059200031007     c                   in(E)     *dtaara
059300031007     c                   IF        %ERROR or RSUT = *blanks
059400031007     c                   clear                   Tibs34Ds
059500031007     c                   call      'TIBS34R'
059600031007     c                   parm                    Tibs34Ds
059700031007     c                   in        *dtaara
059800031007     c                   ENDIF
059900031007      *
060000031007     c                   ENDSR
060100031007      *
060200031007      *---------------------------------------------------------------*
060300031007      * Gestione videata D01                                          *
060400031007      *---------------------------------------------------------------*
060500031007     c     GesD01        BEGSR
060600031007      *
060700031007      * Inizializzo la videata
060800031007if  1c                   if        $InzD01  = *on
060900031007     c                   exsr      InzD01
061000031007     c                   movel     *off          $InzD01
061100031007e   1c                   endif
061200031014      * Reimposto il numero F.V. a *blank se *zero (numerico editato)
061300031014if  2c                   if        V1Cnfv =  *zeros
061400031014     c                   eval      V1Cnfv =  *blanks
061500031014e   2c                   endif
061600031007      *
061700031007      * Scrivo la testata e la riga tasti funzionali abilitati
061800170117if  1c*//                if        NOT *in28
061900031007     c                   write     MS03T01
062000031007     c                   write     MS03Z01
062100170117e   1c*//                endif
062200031007      * Emetto la videata
062300031007     c                   exfmt     MS03D01
062400031007     c                   setoff                                       28  90
062500031007     c                   clear                   V1Dmsg
062600031007      *
062700031007sel 1c                   select
062800031007      * F3=Fine
062900031007w   1c                   when      *inKC
063000031007     c                   exsr      F03D01
063100060110     c                   leavesr
063200031007      *
063300031007e   1c                   endsl
063400031007      *
063500031007      * Controllo dati immessi a video
063600031007     c                   exsr      CtrD01
063700060110     c                   if        *in90
063800060110     c                   leavesr
063900060110     c                   endif
064000031009      *
064100031009      * Inizializzazione schiere, flag, variabili, ecc.
064200031009     c                   exsr      InzALL_1
064300031106     c                   eval      $Default = (V1Cnfv = *zeros)
064400040705      *
064500040705      * Testo eventuali sostituzioni tra piano std e piano dpd
064600040705if  1c                   if            $Default = *off
064700040705     c                             and $NewFVa  = *off
064800040705     c                   exsr      Check_PS
064900040705if  2c                   if        *inKC  or  *inKL
065000060110     c                   leavesr
065100040705e   2c                   endif
065200040705e   1c                   endif
065300031009      *
065400031009      * Caricamento dati da tab. MSL
065500031009     c                   exsr      Tab_MSL
065600031007      *
065700031007      * Controllo se P.O. in gestione ha VDL
065800031007     c                   exsr      Ctrl_VDL
065900031009      *
066000031110      * Se piano giornaliero:
066100031106      * Intabellamento dati di default   &
066200031106      * Memorizzazione dati per il reperimento dell'ultimo foglio
066300031106      *   consegne per autotrasportatore
066400040309if  1c                   if        $Default = *off
066500031020     c                   exsr      Memo_DFT
066600031009e   1c                   endif
066700031010      *
066800031106      * Passaggio alla videata successiva
066900031010if  1c                   if        $Default = *on
067000031010     c                   movea     '10'          *in(01)
067100031010x   1c                   else
067200031010     c                   movea     '01'          *in(01)
067300031010e   1c                   endif
067400031007      *
067500031007if  1c                   if        $VDL     = *off
067600031007      *
067700031007     c                   eval      $InzS01  = *on
067800031007if  2c                   if        $Default = *on
067900031007     c                   eval      $Video   = '2'
068000031007x   2c                   else
068100031007     c                   eval      $Video   = '4'
068200031007e   2c                   endif
068300031007      *
068400031007x   1c                   else
068500031007      *
068600031007     c                   eval      $InzS02  = *on
068700031007if  2c                   if        $Default = *on
068800031007     c                   eval      $Video   = '3'
068900031007x   2c                   else
069000031007     c                   eval      $Video   = '5'
069100031007e   2c                   endif
069200031007      *
069300031007e   1c                   endif
069400031007      *
069500060110     c                   ENDSR
069600031007      *
069700031007      *---------------------------------------------------------------*
069800031007      * Inizializzazione videata D01                                  *
069900031007      *---------------------------------------------------------------*
070000031007     c     InzD01        BEGSR
070100031007      *
070200031007      * Imposto i dati di default
070300031010if  1c                   if        V1Cpog = *zeros
070400031007     c                   eval      V1Cpog = DUTpou
070500031007     c                   eval      V1Cnpg = 2
070600031010e   1c                   endif
070700031010     c                   clear                   V1Cnfv
070800031014     c                   clear                   V1Dnfv
070900031010     c                   clear                   V1Ddfv
071000031007      *
071100031007     c                   clear                   V1Dmsg
071200031007     c                   setoff                                       28  90
071300031007     c                   movea     *zeros        *in(41)
071400031010      *
071500031010      * Posiziono il cursore sul campo "numero F.V."
071600031010     c                   eval      *in43  = *on
071700040702      *
071800040702      * Proteggo il campo "P.O. in gestione" se P.O. utente di 2° livello
071900040705     c                   eval      *in39  = (DUTlpo   = '2')
072000040705      *
072100040705      * Controllo se P.O. abilitato a piano DPD
072200040705     c                   eval      *in03  = *off
072300040705     c                   eval      xx     = *zeros
072400040705do  1c                   dow       xx     < %elem(£1)
072500040705     c                   eval      xx     = xx + 1
072600040705if  2c                   if        £1(xx) = *zeros
072700040705     c                   leave
072800040705e   2c                   endif
072900040705     c                   eval      yy     = 1
073000040705     c                   eval      *in10  = *off
073100040705     c                   move      £1(xx)        w003a
073200040705     c     w003a         lookup    $ORGfil(yy)                            10
073300040705if  2c                   if            *in10       =  *on
073400040705     c                             and $ORGntw(yy) =  'DPD'
073500040705     c                   eval      *in03  = *on
073600040705     c                   leave
073700040705e   2c                   endif
073800040705e   1c                   enddo
073900070530if  1c                   if            V1Cpog      = 049
074000070530     c                             and *in03       = *on
074100070530     c                   eval      V1Cdpd = 'S'
074200070530e   1c                   endif
074300031007      *
074400031007     c                   ENDSR
074500031007      *
074600031007      *---------------------------------------------------------------*
074700031007      * Gestione tasto funzionale F03 da videata D01                  *
074800031007      *---------------------------------------------------------------*
074900031007     c     F03D01        BEGSR
075000031007      *
075100031007      * Chiudo il programma
075200031007     c                   eval      $Fine = *on
075300031007      *
075400031007     c                   ENDSR
075500031007      *
075600031007      *---------------------------------------------------------------*
075700031007      * Controllo dati immessi in videata D01                         *
075800031007      *---------------------------------------------------------------*
075900031007     c     CtrD01        BEGSR
076000031007      *
076100031113     c                   reset                   $NewFVa
076200040702     c                   reset                   $DltPS
076300031007     c                   movea     *zeros        *in(41)
076400151109      * -?Spegnimento indicatore di errore SQL?
076500151109     c                   eval      *in34 = *off
076600031007      *
076700031007      * Controllo P.O. in gestione
076800031007if  1c                   if        V1Cpog <> SAVpog
076900031007     c                   exsr      DecodPOG
077000031009if  2c                   if        *in90   = *off
077100031009     c                   eval      SAVpog  = V1Cpog
077200031007x   2c                   else
077300031007     c                   reset                   SAVpog
077400031007e   2c                   endif
077500031007e   1c                   endif
077600060110     c                   if        *in90
077700060110     c                   leavesr
077800060110     c                   endif
077900031007      *
078000031007      * Controllo categoria (tipo)
078100031007sel 1c                   select
078200031007w   1c                   when          V1Cnpg =  *zeros
078300031013     c                             and V1Cnfv <> *zeros
078400031013     c                             and V1Cnfv <> *blanks
078500031007     c                   seton                                        422890
078600031007     c                   movel     $Msg(4)       V1Dmsg
078700031007w   1c                   when          V1Cnpg <> *zeros
078800031007     c                             and V1Cnpg <> 2
078900031007     c                   seton                                        422890
079000031007     c                   movel     $Msg(5)       V1Dmsg
079100031007e   1c                   endsl
079200060110     c                   if        *in90
079300060110     c                   leavesr
079400060110     c                   endif
079500040302      *
079600040302      * Controllo categoria da TABEL (se NON default)
079700040302if  1c                   if            V1Cnpg <> *zeros
079800040302     c                             or (V1Cnfv <> *zeros
079900040302     c                             and V1Cnfv <> *blanks)
080000040302      *
080100040227     c                   movel     '7N'          TBLcod
080200040312     c                   movel(p)  V1Cnpg        TBLkey
080300040227     c     KTAB          chain     TABEL
080400040227      * - inesistente
080500040227if  2c                   if        NOT %found(TABEL00F)
080600040227     c                             or  TBLflg <> *blanks
080700040227     c                   seton                                        422890
080800040227     c                   movel     $Msg(5)       V1Dmsg
080900060110     c                   leavesr
081000040227e   2c                   endif
081100040227     c                   movel     TBLuni        ds7N
081200040227      * - non utilizzabile dal P.O. in gestione
081300040227if  2c                   if        §7Nuti = '9'
081400040227     c                   seton                                        422890
081500040227     c                   movel     $Msg(5)       V1Dmsg
081600060110     c                   leavesr
081700040227e   2c                   endif
081800040227      * - utilizzabile solo da P.O. di diverso livello
081900040227if  2c                   if           (§7Nuti =  '1'
082000040227     c                             and V1Cpog <> D55tfp)
082100040227     c                             OR (§7Nuti =  '2'
082200040227     c                             and V1Cpog =  D55tfp)
082300040227     c                   seton                                        422890
082400040227     c                   movel     $Msg(6)       V1Dmsg
082500060110     c                   leavesr
082600040227e   2c                   endif
082700040302      *
082800040302e   1c                   endif
082900040302      *
083000031007      * Controllo numero foglio viaggio
083100031013      * - se *blanks lo considero *zeros (sarebbe numerico)
083200031013if  1c                   if        V1Cnfv =  *blanks
083300031013     c                   eval      V1Cnfv =  *zeros
083400031013e   1c                   endif
083500031013      * - ricerca
083600031013     c     '?'           scan      V1Cnfv                                 10
083700031013if  1c                   if        *in10  =  *on
083800040415      * - - per interrogare il foglio è obbligatorio inserirne il tipo
083900040415      *     => ora fisso a "2"
084000040415if  2c                   if        V1Cnpg =  *zeros
084100040415     c                   seton                                        422890
084200040415     c                   movel     $Msg(5)       V1Dmsg
084300060110     c                   leavesr
084400040415e   2c                   endif
084500031013     c                   eval      V1Cnfv =  *zeros
084600031013     c                   eval      Wcampo =  'V1CNFV    '
084700031013     c                   exsr      srSEARCH
084800040705     c                   eval      *in90  =  *on
084900031013e   1c                   endif
085000031013      * - controlli:
085100031013      * - - numericità
085200031013     c     DigitN        check     V1Cnfv                                 10
085300031013if  1c                   if        *in10  =  *on
085400031013     c                   seton                                        542890
085500031013     c                   movel     $Msg(20)      V1Dmsg
085600060110     c                   leavesr
085700031013e   1c                   endif
085800040415      * - - effettiva immissione
085900040415if  1c                   if            V1Cnpg <> *zeros
086000040415     c                             and V1Cnfv =  *zeros
086100040415     c                   seton                                        432890
086200040415     c                   movel     $Msg(7)       V1Dmsg
086300060110     c                   leavesr
086400040415e   1c                   endif
086500031009      *
086600040701     c*** (già fatto)    reset                   $NewFVa
086700031009if  1c                   IF        V1Cnfv <> *zeros
086800031013      * - - inserimento in FLTR9
086900031009     c                   eval      TR9fgs  = V1Cpog
087000031009     c                   eval      TR9npga = V1Cnpg
087100031013     c                   move      V1Cnfv        TR9nfva
087200031009     c     K03FLTR9      setll     FLTR9000
087300031023if  2c                   if        NOT %equal(FLTR901L)
087400031009     c                   eval      $NewFVa = *on
087500031013      * - - esistenza in FNFVV
087600031009     c                   eval      FVVfgs  = V1Cpog
087700031009     c                   eval      FVVnpg  = V1Cnpg
087800031013     c                   move      V1Cnfv        FVVnfv
087900031009     c     K03FNFVV      chain     FNFVV000
088000031009if  3c                   if        NOT %found(FNFVV03L)
088100031009     c*** (già da vl)              or  FVVatb <> *blanks
088200031009     c*** (già da vl)              or  FVVfcf <> *blanks
088300151106     c                   seton                                        432890
088400031009     c                   movel     $Msg(8)       V1Dmsg
088500060110     c                   leavesr
088600031010x   3c                   else
088700031106     c                   reset                   WLBda8
088800040705     c                   eval      G08inv  = FVVdfv
088900031010     c                   call      'XSRDA8'
089000031010     c                   parm                    WLBda8
089100040705     c                   eval      V1Ddfv  = G08dat
089200040705     c                   eval      SAVdfva = FVVdfv
089300031009e   3c                   endif
089400031013x   2c                   else
089500031013      * - - già esistente in FLTR9 (=> reperisco la data del F.V.)
089600040705     c                   eval      FVVfgs  = V1Cpog
089700040705     c                   eval      FVVnpg  = V1Cnpg
089800031013     c                   move      V1Cnfv        FVVnfv
089900031013     c     K03FNFVV      chain     FNFVV000
090000031013if  3c                   if            %found(FNFVV03L)
090100031013     c*** (già da vl)              and FVVatb <> *blanks
090200031013     c*** (già da vl)              and FVVfcf <> *blanks
090300031106     c                   reset                   WLBda8
090400040705     c                   eval      G08inv  = FVVdfv
090500031013     c                   call      'XSRDA8'
090600031013     c                   parm                    WLBda8
090700040705     c                   eval      V1Ddfv  = G08dat
090800040705     c                   eval      SAVdfva = FVVdfv
090900031013e   3c                   endif
091000031009e   2c                   endif
091100151106      /free
091200151106
091300151106           // -?Verifica esistenza altri F.V. per la stessa data?
091400151109           //  ?(NON se Piano di Default)?
091500151109           //If  $NewFVA = *on;
091600151109           If  V1Cnpg <> *zero;
091700151106
091800151106             exsr  sr_OpenCursor;
091900151106
092000151106             If  Not *in34;
092100151106
092200151106               exsr  sr_ReadCursor;
092300151106               exsr  sr_CloseCursor;
092400151106
092500151109               //if  ds_NFVxDFV.TR9nfva > *zeros;
092600151109               if  ds_NFVxDFV.TR9nfva > *zero
092700151109                   and        $NewFVA = *on;
092800151106
092900151109                 wDate_Eur = %date( SavDFVa : *ISO );
093000151106                 *in43  = *on;
093100151106                 *in28  = *on;
093200151106                 *in90  = *on;
093300151106                 V1Dmsg = $Msg(55);
093400151109                 V1Dmsg = %replace( %trim( %editc(
093500151109                                    ds_NFVxDFV.TR9nfva : '2' ) )
093600151106                                  : V1Dmsg
093700151109                                  : %scan( '_      ' : V1Dmsg ) );
093800151109                 V1Dmsg = %replace( %editc( %dec( wDate_Eur ) : 'Y' )
093900151106                                  : V1Dmsg
094000151106                                  : %scan( '__/__/____' : V1Dmsg ) );
094100151106                 leavesr;
094200151106
094300151106               endif;
094400151106
094500151106             Else;
094600151106
094700151106               *in28  = *on;
094800151106               *in90  = *on;
094900151106               V1Dmsg = $Msg(56);
095000151106               leavesr;
095100151106
095200151106             EndIf;
095300151106
095400151109           EndIf;
095500151106
095600151106      /end-free
095700031009e   1c                   ENDIF
095800040705      * Anche se ho solo eseguito una ricerca ("?") del F.V.
095900040705      *   riemetto questa videata prima di proseguire con i
096000040705      *   controlli relativi al flag S/N DPD.
096100040705if  1c                   if        *in90   = *on
096200040705     c                   eval      *in44   =*on
096300060110     c                   leavesr
096400040705e   1c                   endif
096500040705      *
096600040705      * Se richiesto un piano di default, ma impostato piano DPD:
096700040705      *   vince il piano di default
096800040705if  1c                   if            V1Cnpg =  *zeros
096900040705     c                             and V1Cnfv =  *zeros
097000040705     c                   clear                   V1Cdpd
097100040705e   1c                   endif
097200040705      *
097300040705      * Controllo se P.O. abilitato a piano DPD
097400040705if  1c                   if        V1Cdpd  = 'S'
097500040705     c                   eval      *in90   = *on
097600040705     c                   eval      xx      = *zeros
097700040705do  2c                   dow       xx      < %elem(£1pog)
097800040705     c                   eval      xx      = xx + 1
097900040705if  3c                   if        £1pog(xx) <= *zeros
098000040705     c                   leave
098100040705e   3c                   endif
098200040705     c                   eval      yy      = 1
098300040705     c                   eval      *in10   = *off
098400040705     c     £1pog(xx)     lookup    $ORGfil(yy)                            10
098500040705if  3c                   if            *in10       =  *on
098600040705     c                             and $ORGntw(yy) =  'DPD'
098700040705     c                   eval      *in90   = *off
098800040705     c                   leave
098900040705e   3c                   endif
099000040705e   2c                   enddo
099100040705if  2c                   if        *in90   = *on
099200040705     c                   seton                                        442890
099300040705     c                   movel     $Msg(12)      V1Dmsg
099400060110     c                   leavesr
099500040705e   2c                   endif
099600040705e   1c                   endif
099700040705      *
099800040705      * Controllo se esistono P.O. DPD nel piano di default
099900040705if  1c                   if        V1Cdpd  = 'S'
100000040705     c                   eval      *in90   = *on
100100040705     c                   eval      TR9fgs  = V1Cpog
100200040705     c                   clear                   TR9npga
100300040705     c                   clear                   TR9nfva
100400040705     c     K03FLTR9      setll     FLTR9000
100500040705     c     K03FLTR9      reade(n)  FLTR9000
100600040705do  2c                   dow       NOT %eof(FLTR901L)
100700040705     c                   eval      xx      = 1
100800050615     c                   eval      *in10   = *off
100900040705     c     TR9bai        lookup    $ORGfil(xx)                            10
101000040705if  3c                   if            *in10       = *on
101100040705     c                             and $ORGntw(xx) = 'DPD'
101200040705     c                   eval      *in90   = *off
101300040705     c                   leave
101400040705e   3c                   endif
101500040705     c     K03FLTR9      reade(n)  FLTR9000
101600040705e   2c                   enddo
101700040705      * - Se non ne trovo: emetto l'apposito messaggio (xx) in window
101800040705if  2c                   if        *in90   = *on
101900040705     c                   eval      $Video  = 'W'
102000040705     c                   eval      xx      = 48
102100060110     c                   leavesr
102200040705e   2c                   endif
102300040705e   1c                   endif
102400031014      *
102500031014      * imposto numero F.V. in campo numerico editato a video
102600031014     c                   move      V1Cnfv        V1Dnfv
102700031007      *
102800060110     c                   ENDSR
102900031010      *
103000031010      *---------------------------------------------------------------*
103100031010      * Decodifica P.O. in gestione                                   *
103200031010      *---------------------------------------------------------------*
103300031010     c     DecodPOG      BegSr
103400031010      *
103500031010if  1c                   if        V1Cpog =  *zeros
103600031010     c                   clear                   V1Dpog
103700031010     c                   clear                   OG150
103800031010     c                   seton                                        412890
103900031010     c                   movel     $Msg(1)       V1Dmsg
104000060110     c                   leavesr
104100031010e   1c                   endif
104200031010      *
104300031010     c     V1Cpog        chain     AZORG
104400031010      *
104500031010if  1c                   if        not %found(AZORG01L)
104600031010     c                             or  ORGfva = '*'
104700031010      *
104800031010     c                   movel     *all'? '      V1Dpog
104900031010     c                   clear                   OG150
105000031010     c                   seton                                        412890
105100031010     c                   movel     $Msg(2)       V1Dmsg
105200060110     c                   leavesr
105300031010      *
105400031010x   1c                   else
105500031010      *
105600031010     c                   movel     ORGdes        V1Dpog
105700031010      *
105800031010     c                   eval      *in10  =  *off
105900040701     c     V1Cpog        lookup    £1                                     10
106000031010if  2c                   if        NOT *in10
106100031010     c                   seton                                        412890
106200031010     c                   movel     $Msg(2)       V1Dmsg
106300060110     c                   leavesr
106400031010e   2c                   endif
106500031010      *
106600031010     c                   movel     ORGdf0        OG150
106700031013if  2c                   if        §OGpck <> 'S'
106800040427     c                             and    %subst(KNMUS:1:3) <> 'EDP'
106900031010     c                   seton                                        412890
107000031010     c                   movel     $Msg(3)       V1Dmsg
107100060110     c                   leavesr
107200031010e   2c                   endif
107300031010      *
107400031010      * Se tutto ok reperisco il terminal per verificarne il livello
107500031010     c                   clear                   FNLV55ds
107600031010     c                   eval      D55lin = V1Cpog
107700031010     c                   eval      D55drf = Wdate
107800031010     c                   call      'FNLV55R'
107900031010     c                   parm                    FNLV55ds
108000040705      *
108100040705     c                   clear                   TRUL06ds
108200040705     c                   movel     '£1'          D06cod
108300040705     c                   movel     V1Cpog        D06key
108400040705     c                   movel(p)  TRUL06ds      KPJBU
108500040705     c                   call      'TRUL06R'
108600040705     c                   parm                    KPJBA
108700040705     c                   movel     KPJBU         TRUL06ds
108800040705     c                   movea     D06lia        £1pog
108900031010      *
109000031010e   1c                   endif
109100031010      *
109200060110     c                   ENDSR
109300031009      *
109400031009      *---------------------------------------------------------------*
109500031009      * Inizializzazione schiere, flag, variabili, ecc                *
109600031009      *---------------------------------------------------------------*
109700031009     c     InzALL_1      BEGSR
109800031009      *
109900031009     c                   reset                   $Fine
110000031009     c                   reset                   $InzS01
110100031009     c                   reset                   $InzS02
110200031023     c                   reset                   $EoF
110300031009     c                   reset                   $EndSFL
110400031009     c                   reset                   $Default
110500040701     c*** già impostato: reset                   $NewFVa
110600040701     c*** già impostato: reset                   $DltPS
110700031009     c                   reset                   $VDL
110800040409     c                   reset                   $IMA
110900040423     c                   reset                   $IMAdft
111000040607     c                   reset                   $PO
111100040611     c                   reset                   $POdpd
111200080129     c*** non qui:       reset                   $POerr
111300040607     c                   reset                   $POdft
111400040423     c                   reset                   $Hidden
111500031023     c                   reset                   $Found
111600050629     c                   reset                   $MsgBAIbau
111700031009      *
111800031009     c                   reset                   $MEMat
111900040519     c*** non qui:       reset                   $MEMbp
112000031111     c                   reset                   $MEMfv1
112100031111     c                   reset                   $MEMmp1
112200050701     c                   reset                   $MEMpm1
112300031111     c                   reset                   $MEMfv2
112400031111     c                   reset                   $MEMmp2
112500050701     c                   reset                   $MEMpm2
112600031009     c                   reset                   $MEMxf
112700040615     c                   reset                   $FVxPOa
112800040615     c                   reset                   $FVxPOn
112900040615     c                   reset                   $FVxPOd
113000031009      *
113100031009     c                   reset                   S01nrr
113200031009     c                   reset                   S02nrr
113300031009     c                   reset                   MAXnrr
113400031030     c                   reset                   SAVpep
113500031107     c                   reset                   SAVbau
113600040309      *
113700040423     c                   reset                   Wpost
113800031009      *
113900031009     c                   ENDSR
114000031009      *
114100031021      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
114200031021      *
114300031009     c     InzALL_2      BEGSR
114400031112      *
114500031111     c                   reset                   $FV
114600040315     c                   reset                   $CsrRRN
114700031009     c                   reset                   $PEP
114800031030     c                   reset                   $PEPbai
114900031009     c                   reset                   $BAI
115000031009     c                   reset                   $BAInr
115100050629     c                   reset                   $BAIbau
115200031009     c                   reset                   $BAU
115300040305     c                   reset                   $BAUbai
115400031009     c                   reset                   $BNL
115500031009     c                   reset                   $BNLnr
115600031030     c                   reset                   $NFV
115700040628     c                   reset                   $NFVbp
115800031030     c                   reset                   $NFVnr
115900040519     c                   reset                   $MEMbp
116000040409     c                   reset                   $MPTnr
116100040607     c                   reset                   $ORGnr
116200040607     c                   reset                   $PosPO
116300040628     c                   reset                   $PosPOdpd
116400031009      *
116500031009     c                   reset                   XX
116600031022     c                   reset                   SAVnfvIMA
116700031030     c                   reset                   SAVnfv
116800031022     c                   reset                   SAVpdr
116900040305     c                   reset                   SAVprg
117000031009     c                   reset                   Wpdr
117100031022     c                   reset                   Wnfv
117200031111     c                   reset                   Wnfv7
117300040423     c                   reset                   Wpost
117400031024     c                   reset                   Wast
117500031009      *
117600031009     c                   ENDSR
117700031009      *
117800031009      *---------------------------------------------------------------*
117900031009      * Caricamento dati da tab. MSL                                  *
118000031009      *---------------------------------------------------------------*
118100031009     c     Tab_MSL       BEGSR
118200031009      *
118300031009      * Carico schiera tab. MSL
118400031009     c                   clear                   XX
118500031009     c                   clear                   $MSLpep
118600031009     c                   movel     'MSL'         TBEcod
118700031009     c                   movel(p)  V1Cpog        TBEke1
118800031009     c     K02TNTBE      setll     TNTBE000
118900031009     c     K02TNTBE      reade     TNTBE000
119000031009do  1c                   dow       NOT %eof(TNTBE01L)
119100031009if  2c                   if        TBEatb = *blanks
119200031009     c                   add       1             xx
119300031009     c                   movel     TBEuni        dMSL
119400031009     c                   movel     §MSLpep       $MSLpep(xx)
119500031009e   2c                   endif
119600031009     c     K02TNTBE      reade     TNTBE000
119700031009e   1c                   enddo
119800031009      *
119900031009     c                   ENDSR
120000040408      *
120100040408      *---------------------------------------------------------------*
120200040408      * Caricamento tab. MPT                                          *
120300040408      *---------------------------------------------------------------*
120400040408     c     Tab_MPT       BEGSR
120500040408      *
120600040408      * Carico schiera tab. MPT
120700040408     c                   clear                   XX
120800040408     c                   clear                   $MPT
120900040423     c                   clear                   $MPTdes
121000040409     c                   reset                   $MPTobbl
121100040423     c*** qui non serve: clear                   $MPTnr
121200040408     c                   movel     'MPT'         TBEcod
121300040408     c     TBEcod        setll     TNTBE000
121400040408     c     TBEcod        reade     TNTBE000
121500040408do  1c                   dow       NOT %eof(TNTBE01L)
121600040408if  2c                   if        TBEatb  = *blanks
121700040408     c                   movel     TBEuni        dMPT
121800040409if  3c                   if            $VDL    =  *on
121900040409     c                             and §MPTsiv <> 'N'
122000040409     c                             OR  $VDL    =  *off
122100040409     c                             and §MPTnov <> 'N'
122200040408     c                   add       1             xx
122300040408     c                   movel     TBEke1        $MPT(xx)
122400040423     c                   movel     §MPTdes       $MPTdes(xx)
122500040409if  4c                   if            $VDL    =  *on
122600040409     c                             and §MPTsiv =  'S'
122700040409     c                             OR  $VDL    =  *off
122800040409     c                             and §MPTnov =  'S'
122900040409     c                   movel     *on           $MPTobbl(xx)
123000040409e   4c                   endif
123100040408e   3c                   endif
123200040408e   2c                   endif
123300040408     c     TBEcod        reade     TNTBE000
123400040408e   1c                   enddo
123500040408      *
123600040408      * La postazione "IMA" (generica) la considero sempre valida
123700040408      *   ma dò per scontato che sia inserita in tab. "MPT" !!!
123800040408if  1c***                if        xx      = *zeros
123900040408     c***                eval      $MPT(1) = 'IMA'
124000040408e   1c***                endif
124100040408      *
124200040408     c                   ENDSR
124300031009      *
124400031009      *---------------------------------------------------------------*
124500031009      * Controllo se P.O. in gestione con VDL                         *
124600031009      *---------------------------------------------------------------*
124700031009     c     Ctrl_VDL      BEGSR
124800031009      *
124900031009     c                   reset                   $VDL
125000031009     c                   clear                   dMFP
125100031009      *
125200031009     c                   reset                   TIBS02ds
125300031009     c                   movel     KNSIF         T02sif
125400031009     c                   movel     V1Cpog        T02ke1
125500031009     c                   call      'TIBS02R'
125600031009     c                   parm                    KPJBA
125700031009     c                   parm                    TIBS02ds
125800080121if  1c                   if        T02err  = *blanks
125900031009     c                   movel     T02uni        dMFP
126000031009e   1c                   endif
126100031009      *
126200040630      * Test se presente VdL o Cml per piano
126300060214      * - NON per i P.O. "elencati" in C_PO_...
126400161110      * => CML non è più usato!
126500060214if  1c                   if        V1Cpog <> C_PO_a
126600060214      *
126700031009     c     'V'           lookup    §MFPap                                 10
126800161110     ***c  n10'C'           lookup    §MFPap                                 10
126900031009     c                   movel     *in10         $VDL
127000060214      *
127100060214e   1c                   endif
127200040408      *
127300040408      * Caricamento postazioni "IMA" da tab. MPT
127400040408     c                   exsr      Tab_MPT
127500031009      *
127600031009     c                   ENDSR
127700031009      *
127800031009      *---------------------------------------------------------------*
127900031020      * Intabellamento dati dal default                               *
128000031009      *---------------------------------------------------------------*
128100031020     c     Memo_DFT      BEGSR
128200031009      *
128300031009     c                   clear                   TR9npga
128400031009     c                   clear                   TR9nfva
128500031009     c     K03FLTR9      setll     FLTR9000
128600040624     c     K03FLTR9      reade(n)  FLTR9000
128700031009      *
128800040315do  1c                   DOW       NOT %eof(FLTR901L)
128900031020      *
129000031023      * Intabellamento autotrasportatori di default:
129100040701      * serve a "dare priorità" ad essi quando (dopo, qui sotto), si
129200040701      * memorizzeranno i F.V. più recenti di tutti gli autotrasportato-
129300040701      * ri (memorizzati e non): potrebbe andare "buca" all'ultimo auto-
129400040701      * trasportatore del default - se sono già stati memorizzati tanti
129500040701      * autotrasportatori quanti sono gli elementi della schiera $MEMat...
129600040315if  2c                   IF        TR9bnl <> 'S'
129700031023      *
129800031009      * - controllo se cod. autotrasportatore già memorizzato
129900040315if  3c                   if        TR9pdc <> *zeros
130000040615     c                   eval      xx      = 1
130100040615     c                   eval      *in10   = *off
130200031009     c     TR9pdc        lookup    $MEMat(xx)                             10
130300040615if  4c                   if        *in10   = *off
130400031009      * - - memorizzo nuovo cod. autotrasportatore
130500040615     c                   eval      xx      = 1
130600031009     c     *zeros        lookup    $MEMat(xx)                             10
130700040615if  5c                   if        *in10   = *on
130800040615     c                   eval      $MEMat(xx)  = TR9pdc
130900040315x   5c                   else
131000031009      * - - - se già memorizzati il n° massimo di autotrasportatori
131100031009      *       esco dal ciclo di intabellamento
131200031009     c                   leave
131300040315e   5c                   endif
131400040315e   4c                   endif
131500040315e   3c                   endif
131600040309      *
131700040315e   2c                   ENDIF
131800031020      *
131900040624     c     K03FLTR9      reade(n)  FLTR9000
132000040315e   1c                   ENDDO
132100040315      *
132200031023      *
132300031009      * Intabellamento F.V. più recente per ogni autotrasportatore
132400031110      *   prima intabellato (dal default) e NON (basta che abbia un
132500031110      *   f.v. valido per la data del f.v.arrivi impostato)
132600031023      *
132700040615     c                   eval      FV8fgs  = V1Cpog
132800040615     c                   eval      FV8npg  = 4
132900040615     c                   eval      FV8dfv  = FVVdfv
133000031009     c     K03FVV08      setll     FNFVV08
133100031010     c     K02FVV08      reade     FNFVV08
133200031009      *
133300031009do  1c                   DOW       NOT %eof(FNFVV08L)
133400040615if  2c                   if            FV8atb  = *blanks
133500040615     c                             and FV8fcf  = *blanks
133600120613     c                   eval      DSTnpg  = FV8npg
133700120613     c                   eval      DSTnfv  = FV8nfv
133800120613     c                   eval      DSTfgs  = V1Cpog
133900120613     c     K03FIDST      chain     FIDST000
134000120613if  3c                   if        %found(FIDST01L)
134100031009     c                   clear                   DS_FV
134200040615     c                   eval      dsFVdfv     = FV8dfv
134300040615     c                   eval      dsFVnfv     = FV8nfv
134400031022     c                   movel     DS_FV         w0150
134500031009      * - controllo se cod. autotrasportatore già memorizzato
134600040615     c                   eval      xx      = 1
134700040615     c                   eval      *in10   = *off
134800031009     c     DSTpdr        lookup    $MEMat(xx)                             10
134900040615if  4c                   if        *in10   = *off
135000031110     c     *zeros        lookup    $MEMat(xx)                             10
135100040615if  5c                   if        *in10   = *on
135200040615     c                   eval      $MEMat(xx)  = DSTpdr
135300031110e   5c                   endif
135400031110e   4c                   endif
135500031111      * - - memorizzo data e numero distinte più recenti
135600040615if  4c                   if        *in10   = *on
135700050701     c                   movel     FV8spn        dFVVspn
135800031111     c                   add       1             $MEMxf(xx)
135900031111sel 5c                   select
136000031111      * - - - 1° f.v.
136100040615w   5c                   when      $MEMfv1(xx) < w0150
136200040615if  6c                   if        $MEMfv1(xx) > $MEMfv2(xx)
136300040615     c                   eval      $MEMfv2(xx) = $MEMfv1(xx)
136400040615     c                   eval      $MEMmp2(xx) = $MEMmp1(xx)
136500050701     c                   eval      $MEMpm2(xx) = $MEMpm1(xx)
136600031111e   6c                   endif
136700040615     c                   eval      $MEMfv1(xx) = w0150
136800040615     c                   eval      $MEMmp1(xx) = FV8fpp
136900050701if  2c                   eval      $MEMpm1(xx) = §FVVtpm
137000031111      * - - - 2° f.v.
137100040615w   5c                   when      $MEMfv2(xx) < w0150
137200040615     c                   eval      $MEMfv2(xx) = w0150
137300040615     c                   eval      $MEMmp2(xx) = FV8fpp
137400050701if  2c                   eval      $MEMpm2(xx) = §FVVtpm
137500031111e   5c                   endsl
137600031009e   4c                   endif
137700031009e   3c                   endif
137800031009e   2c                   endif
137900031010     c     K02FVV08      reade     FNFVV08
138000031009e   1c                   ENDDO
138100040615      *
138200040615      *
138300040615      * Intabellamento F.V. più recente per ogni P.O. DPD
138400040615      *   prima intabellato (dal default) e NON (basta che abbia un
138500040615      *   f.v. valido per la data del f.v.arrivi impostato)
138600040615      *
138700040615     c                   clear                   xx
138800040615     c                   eval      FGVlnp  = V1Cpog
138900040615     c                   eval      FGVdfv  = SAVdfva
139000040615     c                   eval      FGVnfv  = *loval
139100040621     c     K03FGV02      setll     FNFGV000
139200040615     c     FGVlnp        reade     FNFGV000
139300040615      *
139400040615do  1c                   DOW       NOT %eof(FNFGV02L)
139500040615      *
139600040615if  2c                   IF            FGVatb  = *blanks
139700040615     c                             and FGVfcf  = *blanks
139800040705      *
139900040705     c                   clear                   Wnfv7
140000040705      * - 1° controllo con P.O. arrivo
140100040615     c                   eval      xy      = 1
140200040615     c                   eval      *in10   = *off
140300040615     c                   move      FGVlna        Wpost
140400040615     c     Wpost         lookup    $ORGfil(xy)                            10
140500040705      * - - imposto numero distinta
140600070530if  3c                   IF            *in10       = *on
140700040705     c                             and $ORGntw(xy) = 'DPD'
140800040705     c                   eval      Wnfv7   = FGVnfv
140900070530     c                   exsr      Memo_NFV
141000070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
141100070530      *     memorizzabile
141200080121if  4c                   if        xx      = %elem($FVxPOa)
141300070530     c                   leavesr
141400070530e   4c                   endif
141500070530e   3c                   ENDIF
141600040705      *
141700040705      * - 2° controllo con altri P.O. (1) del F.V.
141800040630     c                   clear                   xFGVffv
141900070530if  3c                   IF        FGVttr <> 'L'
142000040630     c                   movea     FGVffv        xFGVffv
142100040630     c                   clear                   wx
142200070530do  4c                   DoW       wx      < %elem(xFGVffv)
142300040630     c                   add       1             wx
142400040630if  5c                   if        xFGVffv(wx)     =  *blanks
142500040630     c                   leave
142600040630e   5c                   endif
142700040630     c                   eval      xy      = 1
142800040630     c                   eval      *in10   = *off
142900040630     c     xFGVffv(wx)   lookup    $ORGfil(xy)                            10
143000040705      * - - imposto numero distinta - negativo
143100040705      *     per memorizzare che il p.o. DPD è tra "altri p.o." del f.v.
143200040705      *     (e non è il p.o. finale di arrivo)
143300040630if  5c                   if            *in10       =  *on
143400040630     c                             and $ORGntw(xy) =  'DPD'
143500040630     c                   move      xFGVffv(wx)   Wpost
143600040705     c                   eval      Wnfv7   = FGVnfv * (-1)
143700070521     c                   exsr      Memo_NFV
143800070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
143900070530      *     memorizzabile
144000080121if  6c                   if        xx      = %elem($FVxPOa)
144100070530     c                   leavesr
144200070530e   6c                   endif
144300040630e   5c                   endif
144400070530e   4c                   EndDo
144500070530e   3c                   ENDIF
144600040705      *
144700040705      * - 3° controllo con altri P.O. (2) del F.V.
144800070530if  3c                   IF        FGVttr <> 'L'
144900040630     c                   clear                   xFGVffv
145000040630     c                   movea     FGVff2        xFGVffv
145100040630     c                   clear                   wx
145200070530do  4c                   DoW       wx      < %elem(xFGVffv)
145300040630     c                   add       1             wx
145400040630if  5c                   if        xFGVffv(wx)     =  *blanks
145500040630     c                   leave
145600040630e   5c                   endif
145700040630     c                   eval      xy      = 1
145800040630     c                   eval      *in10   = *off
145900040630     c     xFGVffv(wx)   lookup    $ORGfil(xy)                            10
146000040705      * - - imposto numero distinta - negativo
146100040705      *     per memorizzare che il p.o. DPD è tra "altri p.o." del f.v.
146200040705      *     (e non è il p.o. finale di arrivo)
146300040630if  5c                   if            *in10       =  *on
146400040630     c                             and $ORGntw(xy) =  'DPD'
146500040630     c                   move      xFGVffv(wx)   Wpost
146600040705     c                   eval      Wnfv7   = FGVnfv * (-1)
146700070521     c                   exsr      Memo_NFV
146800070530      * - - Esco dal ciclo di memorizzazione se ho reperito l'ultimo
146900070530      *     memorizzabile
147000080121if  6c                   if        xx      = %elem($FVxPOa)
147100070530     c                   leavesr
147200070530e   6c                   endif
147300040630e   5c                   endif
147400070530e   4c                   EndDo
147500070530e   3c                   ENDIF
147600040705      *
147700040705e   2c                   ENDIF
147800040615      *
147900040615     c     FGVlnp        reade     FNFGV000
148000040615      *
148100040615e   1c                   ENDDO
148200031009      *
148300031009     c                   ENDSR
148400070521      *
148500070521      *---------------------------------------------------------------*
148600070521      * Intabellamento F.V. disponibili x filiale                     *
148700070521      *---------------------------------------------------------------*
148800070521     c     Memo_NFV      BEGSR
148900070521      *
149000070521      * - Memorizzo data e numero distinte più recenti
149100070521     c                   eval      xx          = xx + 1
149200070521     c                   eval      $FVxPOa(xx) = Wpost
149300070521     c                   eval      $FVxPOn(xx) = Wnfv7
149400070521     c                   eval      $FVxPOd(xx) = FGVdfv
149500070521      *
149600070521     c                   ENDSR
149700031007      *
149800031007      *---------------------------------------------------------------*
149900031007      * Gestione videata W01                                          *
150000031007      *---------------------------------------------------------------*
150100031007     c     GesW01        BEGSR
150200031007      *
150300031007      * Preparazione videata con messaggio
150400031007     c                   clear                   MS03W01
150500040705     c                   movel     $Msg(xx)      W1Dms2
150600031009     c                   eval      W1Dtfa  = 'F12=Ritorno'
150700031007      *
150800031007      * Emissione messaggio
150900031009do  2c                   dou       *inKL   = *on
151000031007     c                   exfmt     MS03W01
151100031007e   2c                   enddo
151200031007      *
151300031007      * Ritorno alla videata iniziale
151400031007     c                   reset                   $Video
151500040705     c                   eval      *in90   = *off
151600031007      *
151700031007     c                   ENDSR
151800031007      *
151900031007      *---------------------------------------------------------------*
152000031106      * Gestione videata C01/S01 & C03/S03 o C02/S02 & C04/S04        *
152100031007      *---------------------------------------------------------------*
152200031106     c     GesSFL        BEGSR
152300031106      *
152400031106      * Inizializzo la videata
152500031106if  1c                   if            $VDL    = *off
152600031106     c                             and $InzS01 = *on
152700031106     c                             OR  $VDL    = *on
152800031106     c                             and $InzS02 = *on
152900031106     c                   exsr      InzSFL
153000031106     c                   eval      $InzS01     = *off
153100031106     c                   eval      $InzS02     = *off
153200031106e   1c                   endif
153300031106      *
153400031106      * Se non sono stati trovati record NEANCHE con il piano di
153500031106      *   default emetto un messaggio e torno alla 1ª videata
153600031106if  1c                   if        MAXnrr  = *zeros
153700031106     c                   eval      $Video  = 'W'
153800040705     c                   eval      xx      = 9
153900060110     c                   leavesr
154000031106x   1c                   else
154100031106     c                   eval      *in30   = *off
154200031106e   1c                   endif
154300031106      *
154400031106      * Posiziono il cursore
154500031106sel 1c                   select
154600031106w   1c                   when          $VDL   = *off
154700031106     c                             and C01csr > *zeros
154800031106     c                   z-add     C01csr        C01rcd
154900031106w   1c                   when          $VDL   = *on
155000031106     c                             and C02csr > *zeros
155100031106     c                   z-add     C02csr        C02rcd
155200031106w   1c                   when          $VDL   = *off
155300031106     c                             and C01csr = *zeros
155400031106     c                   z-add     1             C01rcd
155500031106w   1c                   when          $VDL   = *on
155600031106     c                             and C02csr = *zeros
155700031106     c                   z-add     1             C02rcd
155800031106e   1c                   endsl
155900031106      *
156000031106      * Scrivo la testata e la riga tasti funzionali abilitati
156100031106if  1c                   if        NOT *in90
156200031106     c                   write     MS03T01
156300031106if  2c                   if        $Default = *on
156400031106     c                   write     MS03Z02
156500031106x   2c                   else
156600031106     c                   write     MS03Z03
156700031106e   2c                   endif
156800031106e   1c                   endif
156900151109      *
157000151109      *?Altro Piano Smistamento già esistente per lo stesso giorno?
157100151109     c                   if        ds_NFVxDFV.TR9nfva > *zero
157200151109     c                             and        $NewFVA = *off
157300151109     c                             and        V1Cnpg <> *zero
157400151109     c                   eval      wDate_Eur = %date( SavDFVa : *ISO )
157500151109     c                   eval      *in28  = *on
157600151109     c                   eval      *in90  = *on
157700151109     c                   eval      V1Dmsg = $Msg(55)
157800151109     c                   eval      V1Dmsg = %replace( %trim( %editc(
157900151109     c                                        ds_NFVxDFV.TR9nfva : '2' ) )
158000151109     c                                      : V1Dmsg
158100151109     c                                      : %scan( '_      ' : V1Dmsg ) )
158200151109     c                   eval      V1Dmsg = %replace(
158300151109     c                                      %editc( %dec( wDate_Eur ) : 'Y' )
158400151109     c                                      : V1Dmsg
158500151109     c                                      : %scan( '__/__/____' : V1Dmsg ) )
158600151109     c                   endif
158700031106      *
158800031106      * Emetto la videata
158900060110     c     §OGpkm        comp      'S'                                    04
159000031106sel 1c                   select
159100031106w   1c                   when      $Video = '2'
159200031106     c                   exfmt     MS03C01
159300031106w   1c                   when      $Video = '3'
159400031106     c                   exfmt     MS03C02
159500031106w   1c                   when      $Video = '4'
159600031106     c                   exfmt     MS03C03
159700031106w   1c                   when      $Video = '5'
159800031106     c                   exfmt     MS03C04
159900031106e   1c                   endsl
160000031106      *
160100031106     c                   setoff                                       28  90
160200031106     c                   clear                   V1Dmsg
160300031106      *
160400031106sel 1c                   select
160500031106      * F3=Fine
160600031106w   1c                   when      *inKC
160700031106     c                   exsr      F03D01
160800060110     c                   leavesr
160900031106      * F12=Ritorno
161000031106w   1c                   when      *inKL
161100031106     c                   exsr      F12SFL
161200060110     c                   leavesr
161300040315      * F16=Cancellazione piano
161400040315w   1c                   when      *inKQ
161500040315     c                   exsr      F16SFL
161600060110     c                   leavesr
161700031106      *
161800031106e   1c                   endsl
161900031106      *
162000151109      *?Altro Piano Smistamento già esistente per lo stesso giorno?
162100151109     c                   if        ds_NFVxDFV.TR9nfva > *zero
162200151109     c                             and        $NewFVA = *off
162300151109     c                   leavesr
162400151109     c                   endif
162500151109      *
162600031106     c                   exsr      InzALL_2
162700031106      *
162800031106      * Controllo dati immessi a video
162900031106if  1c                   if        $VDL = *off
163000031106     c                   exsr      CtrS01
163100031106x   1c                   else
163200031106     c                   exsr      CtrS02
163300031106e   1c                   endif
163400060110     c                   if        *in90
163500060110     c                   leavesr
163600060110     c                   endif
163700031111      *
163800031111      * F7=Interrogazione F.V. non inseriti
163900031111if  1c                   if        *inKG
164000031111     c                   clear                   Wcampo
164100031111     c                   exsr      srSEARCH
164200060110     c                   leavesr
164300031111e   1c                   endif
164400031112      *
164500031112      * Alla conferma (F6 o F8): segnalo se esistono distinte aperte
164600031112      * ------------------------ NON inserite nel piano giornaliero
164700080129      *   e/o rimembra delle filiali DPD lasciate SENZA F.V.
164800040311     c                   reset                   WinKH
164900031112if  1c                   if        (*inKF  or  *inKH)
165000031112     c                             and $Default = *off
165100080129     c                   exsr      CtrlFVdpd
165200080129if  2c                   if        *inKL
165300080129     c                   leavesr
165400080129e   2c                   endif
165500031112     c                   exsr      CtrlDistAp
165600031112e   1c                   endif
165700031106      *
165800031106      * F6=Aggiornamento
165900031106      * F8=Aggiornamento & Stampa
166000031112if  1c                   if        *inKF   or  *inKH
166100031112     c                                     or  WinKH = *on
166200031106     c                   exsr      Upd_FLTR9
166300031112if  2c                   if        *inKH   or  WinKH = *on
166400031106     c                   exsr      F08SFL
166500031106e   2c                   endif
166600031106     c                   reset                   $Video
166700031106     c                   reset                   $InzD01
166800031106e   1c                   endif
166900031106      *
167000060110     c                   ENDSR
167100031007      *
167200031007      *---------------------------------------------------------------*
167300031106      * Inizializzazione videata S01 & S03 o S02 & S04                *
167400031007      *---------------------------------------------------------------*
167500031106     c     InzSFL        BEGSR
167600031007      *
167700031007      * Pulizia subfile
167800031007     c                   seton                                        3031
167900031106sel 1c                   select
168000031106w   1c                   when      $Video = '2'
168100031007     c                   write     MS03C01
168200031106w   1c                   when      $Video = '3'
168300031106     c                   write     MS03C02
168400031106w   1c                   when      $Video = '4'
168500031007     c                   write     MS03C03
168600031106w   1c                   when      $Video = '5'
168700031106     c                   write     MS03C04
168800031106e   1c                   endsl
168900031009     c                   setoff                                         31
169000031007      *
169100031009     c                   clear                   C01rcd
169200031009     c                   clear                   C01csr
169300031009     c                   clear                   S01nrr
169400031106     c                   clear                   C02rcd
169500031106     c                   clear                   C02csr
169600031106     c                   clear                   S02nrr
169700031007     c                   clear                   V1Dmsg
169800031007     c                   setoff                                       28  90
169900031007     c                   movea     *zeros        *in(41)
170000080129      *
170100080129     c                   reset                   $ORGerr
170200031007      *
170300031009      * Caricamento dei dati da presentare nel sfl (max 90 rec)
170400031023      * se è piano di DEFAULT leggo con key *zeros
170500031023      * se è NUOVO piano GIORNALIERO propongo quelli di default
170600040315      * se è VECCHIO piano GIORNALIERO rifaccio la lista per baie /
170700040315      *   postazioni dal piano di default e vi abbino i dati già memo-
170800040315      *   rizzati (questo per mantenere l'elenco delle baie/postazioni
170900040315      *   comprensivo di quelle eventualmente non registrate, visua-
171000040315      *   lizzate con "XXX").
171100031106     c                   eval      TR9fgs  = V1Cpog
171200031106     c                   clear                   TR9npga
171300031106     c                   clear                   TR9nfva
171400040623     c                   eval      XR9npga = V1Cnpg
171500040623     c                   move      V1Cnfv        XR9nfva
171600031106      *
171700031007     c                   reset                   $EndSFL
171800031007do  1c                   DOW       $EndSFL = *off
171900031007     c                   exsr      Carica_SFL
172000031007e   1c                   ENDDO
172100031007      *
172200031106if  1c                   if        $VDL    = *off
172300031007     c                   eval      MAXnrr  = S01nrr
172400031106x   1c                   else
172500031106     c                   eval      MAXnrr  = S02nrr
172600031106e   1c                   endif
172700031009     c                   eval      *in33   = ($EndSFL = *on)
172800031007      *
172900031007     c                   ENDSR
173000151104      *
173100151106      /free
173200151106
173300151106       //--------------------------------------------------------------
173400151106       //?Preparaz. stringa SQL per estrazione altri piani smistamento ?
173500151106       //?  picking per lo stesso giorno.                              ?
173600151106       //?Preparazione cursore.                                        ?
173700151106       //?Dichiarazione cursore.                                       ?
173800151106       //?Apertura cursore.                                            ?
173900151106       //--------------------------------------------------------------
174000151106       BEGSR  sr_OpenCursor;
174100151109
174200151106         // -?Impostazione opzioni per SQL?
174300151106         exec sql   set  option  DynUsrPrf = *Owner,
174400151106                                 CloSqlCsr = *EndMod;
174500151106
174600151106         // -?Preparazione stringa SQL per estrarre i dati?
174700151106         wSQL = 'with NFV_x_DFV as +
174800151109                              ( select TR9fgs, TR9npga, TR9nfva, TR9fl2, +
174900151109                                       Fvvdfv +
175000151106                                  from FLTR900F inner join FNFVV00F +
175100151106                                    on TR9fgs  = FVVfgs +
175200151106                                   and TR9npga = FVVnpg +
175300151106                                   and TR9nfva = FVVnfv +
175400151106                                 where FVVfcf  = '' '' +
175500151109                              group by TR9fgs, TR9npga, TR9nfva, TR9fl2, +
175600151109                                       FVVdfv +
175700151109                              order by TR9fgs, TR9npga, TR9nfva, TR9fl2, +
175800151109                                       FVVdfv ), +
175900151106
176000151106                      CountNFV  as +
176100151109                              ( select TR9fgs, TR9npga, TR9fl2, FVVdfv, +
176200151106                                       count(*) as wNFVxDFV +
176300151106                                  from NFV_x_DFV +
176400151109                              group by TR9fgs, TR9npga, TR9fl2, FVVdfv ) +
176500151106
176600151106                 select NFV_x_DFV.*, CountNFV.wNFVxDFV +
176700151106
176800151106                   from NFV_x_DFV inner join CountNFV +
176900151106                     on NFV_x_DFV.TR9fgs  = CountNFV.TR9fgs +
177000151106                    and NFV_x_DFV.TR9npgA = CountNFV.TR9npgA +
177100151109                    and NFV_x_DFV.TR9fl2  = CountNFV.TR9fl2 +
177200151106                    and NFV_x_DFV.FVVdfv  = CountNFV.FVVdfv +
177300151106
177400151109                 where NFV_x_DFV.TR9fgs  = ' +
177500151109                       %trim( %editc( V1Cpog : '3' ) ) +
177600151109                 ' and NFV_x_DFV.TR9nfvA <> ' +
177700151109                       %trim( %editc( TR9nfva : '3' ) ) +
177800151109                 ' and NFV_x_DFV.FVVdfv  = ' +
177900151106                       %trim( %editc( SavDFVa : '3' ) );
178000151106
178100151109         if  V1Cdpd = 'S';
178200151109           wSQL +=
178300151109                 ' and NFV_x_DFV.TR9fl2  = ''D''';
178400151109         else;
178500151109           wSQL +=
178600151109                 ' and NFV_x_DFV.TR9fl2  = '' ''';
178700151109         endif;
178800151109
178900151106         // -?Dichiarazione cursore?
179000151106         exec sql   prepare S1   from :wSQL;
179100151106         exec sql   declare C1   cursor for S1;
179200151106
179300151106         if  SQLcode < *zero;
179400151106           *in34 = *on;
179500151109           exsr  sr_CloseCursor;
179600151106           exsr  sr_PrintERR;
179700151106           leavesr;
179800151106         endif;
179900151106
180000151106         // -?Apertura del cursore?
180100151106         exec sql   open C1;
180200151106
180300151106         if  SQLcode < *zero;
180400151106           *in34 = *on;
180500151106           exsr  sr_CloseCursor;
180600151106           exsr  sr_PrintERR;
180700151106           leavesr;
180800151106         endif;
180900151106
181000151106       ENDSR;
181100151106
181200151106       //--------------------------------------------------------------
181300151106       //?Lettura cursore.                                             ?
181400151106       //--------------------------------------------------------------
181500151106       BEGSR  sr_ReadCursor;
181600151106
181700151106         clear  ds_NFVxDFV;
181800151106
181900151106         exec sql   fetch next   from C1   into :ds_NFVxDFV;
182000151106
182100151106         select;
182200151106           when  SQLcode = 100;
182300151106             *in34 = *on;
182400151106             exsr  sr_CloseCursor;
182500151106           when  SQLcode < *zero;
182600151106             *in34 = *on;
182700151106             exsr  sr_CloseCursor;
182800151106             exsr  sr_PrintERR;
182900151106         endsl;
183000151106
183100151106       ENDSR;
183200151106
183300151106       //--------------------------------------------------------------
183400151106       //?Chiusura cursore.                                            ?
183500151106       //--------------------------------------------------------------
183600151106       BEGSR  sr_CloseCursor;
183700151106
183800151106         // -?Chiusura del cursore?
183900151109         exec sql   close C1;
184000151106
184100151106       ENDSR;
184200151106
184300151106       //--------------------------------------------------------------
184400151106       //?Stampa segnalazione dell'errore rilevato via SQL             ?
184500151106       //--------------------------------------------------------------
184600151106       BEGSR  sr_PrintERR;
184700151106
184800151106         // -?Stampa del Dump?
184900151106         Dump(A);
185000151106
185100151106         // -?Stampa del Job-Log?
185200151106         Qcmd = 'DSPJOBLOG job(*) output(*print)';
185300151106         exsr  sr_ExecCmd;
185400151106
185500151106       ENDSR;
185600151106
185700151106       //--------------------------------------------------------------
185800151106       //?Esecuzione del comando (già impostato).                      ?
185900151106       //--------------------------------------------------------------
186000151106       BEGSR  sr_ExecCmd;
186100151106
186200151106         clear Qcap0100;
186300151106         Qcabcsdh = *off;
186400151106         Qcapa    = *off;
186500151106         Qcacmdss = *off;
186600151106         Qcaerved = *allX'00';
186700151106
186800151106         clear Qusec;
186900151106         Qusbprv  = %size(Qusec);
187000151106
187100151106         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
187200151106                           %size(Qcap0100) : 'CPOP0100' : *omit :
187300151106                           0 : 0 : Qusec);
187400151106
187500151106         //if  Qusei <> *blank;
187600151106         //  ...;
187700151106         //endif;
187800151106
187900151106       ENDSR;
188000151106
188100151106      /end-free
188200031029      *
188300031029      *---------------------------------------------------------------*
188400031029      * Gestione tasto funzionale F8  da videata S01/S02/S03/S04      *
188500031029      *---------------------------------------------------------------*
188600031106     c     F08SFL        BEGSR
188700031029      *
188800031029      * Richiamo pgm. di stampa per piano di smistamento in esame
188900031029     c                   clear                   FIMS04ds
189000031029     c                   z-add     V1Cpog        D04pog
189100040408if  1c                   if        V1Cnfv > *zeros
189200031029     c                   z-add     V1Cnpg        D04npga
189300040408e   1c                   endif
189400031029     c                   move      V1Cnfv        D04nfva
189500031029      *
189600031029     c                   movel(p)  FIMS04ds      KPJBU
189700031029     c                   call      'FIMS04R'
189800031029     c                   parm                    KPJBA
189900031029      *
190000031029     c                   ENDSR
190100031007      *
190200031007      *---------------------------------------------------------------*
190300031007      * Gestione tasto funzionale F12 da videata S01/S02/S03/S04      *
190400031007      *---------------------------------------------------------------*
190500031106     c     F12SFL        BEGSR
190600031007      *
190700031007      * Torno alla 1ª videata D01
190800031007     c                   reset                   $Video
190900031010     c                   movea     '00'          *in(01)
191000031007      *
191100031007     c                   ENDSR
191200040315      *
191300040315      *---------------------------------------------------------------*
191400040315      * Gestione tasto funzionale F16 da videata S03/S04              *
191500040315      *---------------------------------------------------------------*
191600040315     c     F16SFL        BEGSR
191700040315      *
191800040315      * Cancella record esistenti (se?NON?immissione)
191900040702if  1c                   if            $NewFVa = *off
192000040702     c                             and $DltPS  = *off
192100040315      *
192200040315      * - Richiesta conferma
192300040315     c                   clear                   MS03W01
192400040315     c                   movel     $Msg(19)      W1Dms2
192500040315     c                   eval      W1Dtfa = 'F6=Conferma   '
192600040315     c                                    + 'F12=Ritorno'
192700040315do  2c                   dou       *inKF   or   *inKL
192800040315     c                   exfmt     MS03W01
192900040315e   2c                   enddo
193000060110      * F12-Ritorno
193100060110     c                   if        *inKL
193200060110     c                   leavesr
193300060110     c                   endif
193400060110      * F6-Conferma
193500040315     c                   exsr      Dlt_FLTR9
193600040315      *
193700040315e   1c                   endif
193800040315      *
193900040315      * Torno alla 1ª videata D01
194000040315     c                   reset                   $Video
194100040315     c                   movea     '00'          *in(01)
194200040315      *
194300060110     c                   ENDSR
194400040701      *
194500040701      *---------------------------------------------------------------*
194600040701      * Controllo esistenza piano smistamento STD se richiesto DPD    *
194700040701      *---------------------------------------------------------------*
194800040701     c     Check_PS      BEGSR
194900040701      *
195000040701     c*** (già fatto)    reset                   $DltPS
195100040705     c                   eval      TR9fgs  = V1Cpog
195200040705     c                   eval      TR9npga = V1Cnpg
195300040705     c                   move      V1Cnfv        TR9nfva
195400040701      *
195500040705     c     K03FLTR9      setll     FLTR9000
195600040705     c     K03FLTR9      reade(n)  FLTR9000
195700040701      *
195800040701sel 1c                   select
195900040701      **** - nuovo piano (IMPOSSIBILE qui)
196000040705w   1c***                when      %eof(FLTR901L)
196100040701     c***                eval      $NewFVa = *on
196200060110     c***                leavesr
196300040701      * - richiesto piano std, esiste piano std: modificabile
196400040701w   1c                   when          V1Cdpd =  *blanks
196500040701     c                             and TR9fl2 =  *blanks
196600060110     c                   leavesr
196700040701      * - richiesto piano std, esiste piano dpd: da sostituire
196800040701w   1c                   when          V1Cdpd =  *blanks
196900040701     c                             and TR9fl2 =  'D'
197000040701     c                   eval      $DltPS  = *on
197100060110     c                   leavesr
197200040701      * - richiesto piano dpd, esiste piano dpd: modificabile
197300040701w   1c                   when          V1Cdpd =  'S'
197400040701     c                             and TR9fl2 =  'D'
197500060110     c                   leavesr
197600040701      **** - richiesto piano dpd, esiste piano std: da sostituire
197700040701w   1c***                when          V1Cdpd =  'S'
197800040701     c***                          and TR9fl2 =  *blanks
197900040701     c*** (sotto)        ...
198000040701e   1c                   endsl
198100040701      *
198200040701      * Richiesto piano dpd, esiste piano std: da sostituire
198300040701      * Richiesta conferma per annullamento piano smistamento picking
198400040701     c                   clear                   MS03W01
198500040701     c                   movel     $Msg(45)      W1Dms1
198600040701     c                   movel     $Msg(46)      W1Dms2
198700040701     c                   movel     $Msg(47)      W1Dms3
198800040701     c                   eval      W1Dtfa  = 'F6=Conferma'
198900040701     c                                     + ' (nuovo piano DPD)   '
199000040701     c                                     + 'F12=Ritorno'
199100040701do  1c                   dou       *inKF   or   *inKL
199200040701     c                   exfmt     MS03W01
199300040701e   1c                   enddo
199400040701      *
199500040701sel 1c                   select
199600040701      * - F6 = cancella
199700040701w   1c                   when      *inKF   = *on
199800040701     c*** non adesso:    exsr      Dlt_FLTR9
199900040701     c                   eval      $DltPS  = *on
200000040701      * - F12 = ritorno
200100040701w   1c                   when      *inKL   = *on
200200040701     c                   reset                   $Video
200300040701     c                   movea     '00'          *in(01)
200400040701e   1c                   endsl
200500040701      *
200600060110     c                   ENDSR
200700031106      *
200800031106      *---------------------------------------------------------------*
200900031106      * Caricamento subfile da file FLTR900F                          *
201000031106      *---------------------------------------------------------------*
201100040705     c     Carica_SFL    BEGSR
201200031106      *
201300031106      * Solo la prima volta:
201400031106      *   posizionamento iniziale + lettura 1° record
201500031106if  1c                   if            S01nrr =  *zeros
201600031106     c                             and S02nrr =  *zeros
201700040705     c                   clear                   TR9npga
201800040705     c                   clear                   TR9nfva
201900031106     c     K03FLTR9      setll     FLTR9000
202000040624     c     K03FLTR9      reade(n)  FLTR9000
202100031106if  2c                   if        %eof(FLTR901L)
202200031106     c                   eval      $EOF   = *on
202300031106e   2c                   endif
202400031106e   1c                   endif
202500031106      *
202600031106      * Pulizia iniziale del record nel sfl in esame
202700031106sel 1c                   select
202800031106w   1c                   when      $Video = '2'
202900031106     c                   clear                   MS03S01
203000031106w   1c                   when      $Video = '3'
203100031106     c                   clear                   MS03S02
203200031106e   1c                   when      $Video = '4'
203300031106     c                   clear                   MS03S03
203400031106w   1c                   when      $Video = '5'
203500031106     c                   clear                   MS03S04
203600031106e   1c                   endsl
203700031106      *
203800031106      * Se NON già rilevato %EOF:
203900031106      *   impostazione campi sfl con quelli dell'ultimo rec. letto
204000031106      *   e lettura singolo record successivo
204100031106if  1c                   IF        $EOF   = *off
204200031106      *
204300040309     c                   reset                   $Found
204400040629     c                   reset                   $IMA
204500040629     c                   reset                   $IMAdft
204600040629     c                   reset                   $PO
204700040629     c                   reset                   $POdpd
204800080129     c                   reset                   $POerr
204900040629     c                   reset                   $POdft
205000040309      *
205100040702      * - Per PIANO GIORNALIERO già immesso:
205200031106      *   controllo la corrispondenza dei dati di default (che sto
205300031106      *   leggendo) con quelli effettivamente immessi
205400040315if  2c                   if            $Default = *off
205500040315     c                             and $NewFVa  = *off
205600040702     c                             and $DltPS   = *off
205700031106     c                   exsr      RepFVAeff
205800040702      * - Per PIANO DI DEFAULT  &
205900040702      *   Per PIANO GIORNALIERO nuovo:
206000040623      *   imposto i campi del file di comodo (per il piano giornaliero)
206100040623      *   come quelli del file in aggiornamento (per i dati di default)
206200040623x   2c                   else
206300040623     c                   movel     FLTR9ds       FLXR9ds
206400040623     c                   eval      $Found   = *on
206500031106e   2c                   endif
206600040701      *
206700040701      * - Controllo se postazione IMA o P.O.
206800080118if  2c                   if            $Default = *off
206900040702     c                             and TR9bnl  <> 'S'
207000040702     c                   eval      $Hidden  = *on
207100040702     c                   eval      Wpost    = TR9bai
207200040702     c                   exsr      Ctrl_IMA
207300040702     c                   exsr      Ctrl_PO
207400040705e   2c                   endif
207500040705if  2c                   if        $Found   = *on
207600040701     c                   eval      $Hidden  = *off
207700040701     c                   eval      Wpost    = XR9bai
207800040701     c                   exsr      Ctrl_IMA
207900040701     c                   exsr      Ctrl_PO
208000040705e   2c                   endif
208100040701      *
208200040702      * - Selezione per piano giornaliero DPD:
208300040705if  2c                   if            $Default = *off
208400040702     c*** non solo new:            and $NewFVa  = *off
208500040702     c                             and V1Cdpd   = 'S'
208600040702      *   - baia non letti
208700040702     c                             and TR9bnl  <> 'S'
208800040705      *   - postazioni IMx
208900040705     c                             and $IMA     = *off
209000040702      *   - postazioni PO-dpd
209100040702     c                             and $POdpd   = *off
209200040702     c                   goto      Read_Next
209300040705e   2c                   endif
209400040705      *
209500040705      *   - postazioni IMx obbligatorie
209600040705if  2c                   if            $Default = *off
209700040705     c*** non solo new:            and $NewFVa  = *off
209800040705     c                             and V1Cdpd   = 'S'
209900040705     c                             and $IMA     = *on
210000040705     c                   eval      *in10    = *off
210100040705     c                   eval      xx       = 1
210200040705     c     Wpost         lookup    $MPT(xx)                               10
210300040705     c   10$MPTobbl(xx)  cabeq     *off          Read_Next
210400040705e   2c                   endif
210500031106      *
210600031106      * - Default (comuni)
210700040309      * > > P.O. senza VDL
210800040701if  2c                   if        $VDL     = *off
210900040701     c                   exsr      CarNoVDL_1
211000040309      * > > P.O. con VDL
211100040701x   2c                   else
211200040701     c                   exsr      CarSiVDL_1
211300040701e   2c                   endif
211400031106      *
211500031106      * - Impostazione dei F.V. in piano giornaliero
211600031106if  2c                   IF        $Default = *off
211700031106      *
211800031106      * - - da default
211900040702if  3c                   IF            $NewFVa  = *on
212000040702     c                             or  $DltPS   = *on
212100031106      *
212200040309sel 4c                   select
212300040309w   4c                   when      TR9bnl = 'S'
212400040615w   4c                   when      $PO    = *on
212500040705if  5c                   if        $POdpd = *on
212600040705     c                   exsr      PostPOdpd
212700040705e   5c                   endif
212800040309w   4c                   when      $IMA   = *on
212900040615     c                   exsr      PostIMA
213000040309x   4c                   other
213100040615     c                   exsr      PostNoIMA
213200040309e   4c                   endsl
213300031106      *
213400031106      * - - da record già immessi
213500031106x   3c                   ELSE
213600031106      *
213700040621if  4c                   if        $Found   = *on
213800031106     c                   reset                   WLBda8
213900031106     c                   reset                   DSdata8
214000031106     c                   reset                   DSdata6
214100040621if  5c                   if        XR9dfv   > *zeros
214200040623     c                   eval      G08inv   = XR9dfv
214300031106     c                   call      'XSRDA8'
214400031106     c                   parm                    WLBda8
214500031106     c                   move      G08dat        DSdata8
214600031106     c                   z-add     DSdt8gg       DSdt6gg
214700031106     c                   z-add     DSdt8mm       DSdt6mm
214800031106     c                   z-add     DSdt8aa       DSdt6aa
214900040621e   5c                   endif
215000040621e   4c                   endif
215100040309      * > > > P.O. senza VDL
215200031106if  4c                   if        $VDL     = *off
215300040701     c                   exsr      CarNoVDL_2
215400040309      * > > > P.O. con VDL
215500031106x   4c                   else
215600040701     c                   exsr      CarSiVDL_2
215700031106e   4c                   endif
215800031106      *
215900031106e   3c                   ENDIF
216000040705      *
216100040705      * Reimposto il valore assoluto tra i F.V. memorizzati x P.O. DPD
216200040705     c                   eval      $FVxPOn  =  %abs($FVxPOn)
216300031106      *
216400031106      * Per piano giornaliero imposto "*" se più F.V. per il padroncino
216500040615      *   o se più F.V. per la postazione = P.O. DPD (solo VdL)
216600040701     c                   clear                   Wast
216700031106     c                   clear                   Wpdr
216800040615     c                   clear                   Wpost
216900040701sel 3c                   select
217000040701w   3c                   when      XR9pdc   > *zeros
217100040621     c                   eval      Wpdr     = XR9pdc
217200031106     c                   exsr      CtrlAutotr3
217300040701w   3c                   when      $POdpd   = *on
217400040621     c                   eval      Wpost    = XR9bai
217500040615     c                   exsr      AsterFVxPO
217600040701e   3c                   endsl
217700040701if  3c                   if        $VDL     = *off
217800040701     c                   eval      S1Cast   = Wast
217900040701x   3c                   else
218000040701     c                   eval      S2Cast   = Wast
218100040701e   3c                   endIf
218200031106      *
218300031106e   2c                   ENDIF
218400031106      *
218500031106      * Se *zeros li reimposto a *blanks (numerici "editati")
218600031106if  2c                   if        S1Cnfv   = *zeros
218700031106     c                   clear                   S1Cnfv
218800031106e   2c                   endif
218900031106if  2c                   if        S3Cnfv   = *zeros
219000031106     c                   clear                   S1Cnfv
219100031106e   2c                   endif
219200031106if  2c                   if        S2Cnfv   = *zeros
219300031106     c                   clear                   S2Cnfv
219400031106e   2c                   endif
219500040304if  2c                   if        S4Cnfv   = *zeros
219600040304     c                   clear                   S4Cnfv
219700040304e   2c                   endif
219800031106      *
219900090203x   1c***                ELSE
220000031106      *
220100031106      * Se già rilevato %EOF:
220200031106      * - impostazione campi vuoti nei restanti record del sfl
220300031106      *   (se "default")
220400031106      *
220500031106e   1c                   ENDIF
220600040701      *
220700040701      * Scrivo il singolo record nel subfile
220800040701     c                   exsr      WrtSflRec
220900040701      *
221000040701     c     Read_Next     tag
221100031106      *
221200031106      * Leggo il record successivo da caricare
221300031106if  1c                   if        $EOF     = *off
221400040624     c     K03FLTR9      reade(n)  FLTR9000
221500031106if  2c                   if        %eof(FLTR901L)
221600031106     c                   eval      $EoF     = *on
221700031106e   2c                   endif
221800031106e   1c                   endif
221900031106      *
222000031106      * Imposto l'uscita SE:
222100031106      * - ho già scritto il numero massimo di rec. previsti nel sfl
222200031106if  1c                   if            S01nrr  >= C_MaxRecSf
222300031106     c                             or  S02nrr  >= C_MaxRecSf
222400031106      * - è stato rilevato EOF in "Piano Giornaliero" (NON di default)
222500040702      *   non emetto records vuoti (come per il default)
222600031106     c                             or ($Default = *off
222700040702     c                             and $EoF     = *on)
222800031106     c                   eval      $EndSFL  = *on
222900031106e   1c                   endif
223000031106      *
223100031106     c                   ENDSR
223200040701      *
223300040701      *---------------------------------------------------------------*
223400050614      * Scrittura singolo record nel subfile                          *
223500040701      *---------------------------------------------------------------*
223600040701     c     WrtSflRec     BEGSR
223700040701      *
223800040701      * - reimposto indicatori x DSPATR condizionati
223900040701if  1c                   IF        $VDL = *off
224000060110     c                   exsr      AtrS01S03
224100040701x   1c                   ELSE
224200060110     c                   exsr      AtrS02S04
224300040701e   1c                   ENDIF
224400040701      *
224500040701      * Scrittura del singolo record nel sfl in esame
224600040702if  1c                   if            $Default = *on
224700040702     c                             OR  $Default = *off
224800040702     c                             and $EoF     = *off
224900040701     c                   eval      *in32    = *on
225000040701sel 2c                   select
225100040701w   2c                   when      $Video   = '2'
225200040701     c                   add       1             S01nrr
225300040701     c                   write     MS03S01
225400040701w   2c                   when      $Video   = '3'
225500040701     c                   add       1             S02nrr
225600040701     c                   write     MS03S02
225700040701e   2c                   when      $Video   = '4'
225800060110     c                   eval      *in54    = (*in80  = *off)
225900040701     c                   add       1             S01nrr
226000040701     c                   write     MS03S03
226100040701w   2c                   when      $Video   = '5'
226200060110     c                   eval      *in54    = (*in80  = *off)
226300040701     c                   add       1             S02nrr
226400040701     c                   write     MS03S04
226500040701e   2c                   endsl
226600040701e   1c                   endif
226700040701      *
226800040701     c                   ENDSR
226900031106      *
227000031106      *---------------------------------------------------------------*
227100031106      * Reperimento dei dati effettivamente immessi per postazione    *
227200031106      *---------------------------------------------------------------*
227300031106     c     RepFVAeff     BEGSR
227400031106      *
227500031106      * Ctrl se esiste il rec. corrispondente nel piano giornaliero
227600031106      *   già immesso
227700040623     c                   eval      XR9prg = (TR9prg * 2) - 1
227800040621     c     K05TR901      chain     FLTR901
227900040621     c                   eval      $Found = (%found(FLTR901X))
228000040621      *
228100040621      * Se NON lo trovo: segnalo il rec. di dft come omesso ("XXX")
228200040623if  1c                   if        $Found  = *off
228300040623     c*** troppo:        clear                   FLXR9ds
228400040623     c                   clear                   XR9bai
228500161213     c                   clear                   XR9des
228600040623     c                   clear                   XR9pdc
228700040623     c                   clear                   XR9npg
228800040623     c                   clear                   XR9nfv
228900040623     c                   clear                   XR9dfv
229000040623     c                   clear                   XR9fl1
229100040623     c                   clear                   XR9fl2
229200040623     c                   clear                   XR9fl3
229300040623     c                   eval      XR9bai  = C_PostazNull
229400040621e   1c                   endif
229500031106      *
229600040315     c                   ENDSR
229700050614      *
229800050614      *---------------------------------------------------------------*
229900050614      * Verifica se:                                                  *
230000050614      * - autotrasportatore MESSAGGERIA per piano di default          *
230100050614      * - distinta MESSAGGERIA per piano di smistamento               *
230200050614      *---------------------------------------------------------------*
230300050614     c     X_Messaggeria BEGSR
230400050614      *
230500050614if  1c                   if        $Default = *on
230600050614      *
230700050614     c                   clear                   S2Dmes
230800050614      * in piano di default: i dati dell'autotrasportatore sono appena
230900050614      *   stati reperiti
231000050614     c*** (già fatto)    movel     APDflr        dAPDflr
231100050614if  2c                   if        §APDtpm  = 'M'
231200050629     c                   eval      S2Dmes   = C_Messaggeria
231300050614e   2c                   endif
231400050614      *
231500050614x   1c                   else
231600050614      *
231700050614if  2c                   if        wNFVseq  = 1
231800050614     c                   clear                   S2Dmes
231900050614x   2c                   else
232000050614     c                   clear                   S4Dmes
232100050614e   2c                   endif
232200050614      * in piano di smistamento: i dati del F.V. sono appena
232300050614      *   stati reperiti da FNFVV
232400050701     c*** (già fatto)    movel     FVVspn        dFVVspn
232500050614if  2c                   if        §FVVtpm  = 'M'
232600050614if  3c                   if        wNFVseq  = 1
232700050629     c                   eval      S2Dmes   = C_Messaggeria
232800050614x   3c                   else
232900050629     c                   eval      S4Dmes   = C_Messaggeria
233000050614e   3c                   endif
233100050614e   2c                   endif
233200050614      *
233300050614e   1c                   endif
233400050614      *
233500050614     c                   ENDSR
233600040701      *
233700040701      *---------------------------------------------------------------*
233800040701      * Caricamento dati di default in F.V. per P.O. senza VDL        *
233900040701      *---------------------------------------------------------------*
234000040701     c     CarNoVDL_1    BEGSR
234100040701      *
234200040701     c                   move      TR9pep        S1Cpep
234300040701     c                   eval      S1Cdes   = TR9des
234400040701     c                   eval      S1Cbai   = XR9bai
234500040701if  1c                   if        $Found   = *on
234600040701sel 2c                   select
234700040701w   2c                   when      $IMA     = *on
234800040701     c                   movel     $MPTdes(yy)   S1Dpdc
234900040701     c                   movel     $MPTdes(yy)   S3Dpdc
235000040701w   2c*** solo x VDL:    when      $PO      = *on
235100040701w   2c                   when      XR9pdc  <> *zeros
235200040701     c                   move      XR9pdc        S1Cpdc
235300040701     c                   eval      APDpdr   = XR9pdc
235400040701     c                   exsr      CtrlAutotr2
235500040701     c                   movel     APDrsc        S1Dpdc
235600040701     c                   movel     APDrsc        S3Dpdc
235700040701     c                   setoff                                       552890
235800040701     c                   clear                   V1Dmsg
235900040701e   2c                   endsl
236000040701e   1c                   endif
236100040701      * - - - Campi hidden
236200040701if  1c                   if        $Default = *off
236300060113if  2c                   if            $NewFVa = *off
236400060113     c                             and $DltPS  = *off
236500060113     c                             and $Found  = *on
236600060113     c                   eval      H1Cprg   = XR9prg
236700060113x   2c                   else
236800060113     c                   eval      H1Cprg   = (TR9prg * 2) - 1
236900060113e   2c                   endif
237000040701     c                   eval      H1Cbai   = TR9bai
237100040701     c                   eval      H1Cpdc   = TR9pdc
237200040701if  2c                   if            $IMAdft = *off
237300040701     c                             and H1Cpdc <> APDpdr
237400040701     c                   eval      APDpdr   = H1Cpdc
237500040701     c                   exsr      CtrlAutotr2
237600040701     c                   setoff                                       552890
237700040701     c                   clear                   V1Dmsg
237800040701e   2c                   endif
237900040701if  2c                   if        $IMAdft  = *off
238000040701     c                   eval      H1Dpdc   = APDrsc
238100040701x   2c                   else
238200040701     c                   eval      H1Dpdc   = $MPTdes(yy)
238300040701e   2c                   endif
238400040701e   1c                   endif
238500040701      *
238600040701     c                   ENDSR
238700040701      *
238800040701      *---------------------------------------------------------------*
238900040701      * Impostazione dei F.V. in piano giornaliero per P.O. senza VDL *
239000040701      *---------------------------------------------------------------*
239100040701     c     CarNoVDL_2    BEGSR
239200040701      *
239300040701if  1c                   if        $Found   = *off
239400040701      *
239500040701     c                   eval      S1Cbai   = C_PostazNull
239600040701     c                   eval      S1Kdft   = C_Default
239700040701     c                   eval      S1Kbad   = C_Postazione
239800040701     c                   eval      S1Cbad   = H1Cbai
239900040701if  2c                   if        H1Cpdc  <> *zeros
240000040701     c                   eval      S1Katd   = C_Autotr
240100040701     c                   move      H1Cpdc        S1Catd
240200040701e   2c                   endif
240300040701     c                   eval      S1Datd   = H1Dpdc
240400040701      *
240500040701x   1c                   else
240600040701      *
240700040701     c                   move      XR9nfv        S1Cnfv
240800040701     c                   move      DSdata6       S1Ddfv
240900040701     c                   eval      S1Dfpp   = XR9fl1
241000040701if  2c                   if        $IMA     = *off
241100040702     c                             and $PO  = *off
241200040701     c                   exsr      CercaIIfv
241300040701e   2c                   endif
241400040701      *
241500040701e   1c                   endif
241600040701      *
241700040701     c                   ENDSR
241800040701      *
241900040701      *---------------------------------------------------------------*
242000040701      * Caricamento dati di default in F.V. per P.O.  con  VDL        *
242100040701      *---------------------------------------------------------------*
242200040701     c     CarSiVDL_1    BEGSR
242300040701      *
242400040701     c                   movel     TR9pep        S2Cpep
242500040701     c                   eval      S2Cdes   = TR9des
242600040701     c                   eval      S2Cbnl   = TR9bnl
242700040701     c                   eval      S2Cbai   = XR9bai
242800161213     c                   if        $PO      = *on
242900161215     c                   eval      S2Cznc   = %subst( XR9des : 29 )
243000161213     c                   endif
243100040701      *
243200040701if  1c                   IF        $Found   = *on
243300040701      *
243400040701sel 2c                   SELECT
243500040701      *
243600040701w   2c                   WHEN      $IMA     = *on
243700040701     c                   movel     $MPTdes(yy)   S2Dpdc
243800040701     c                   movel     $MPTdes(yy)   S4Dpdc
243900040701      *
244000040701w   2c                   WHEN      $PO      = *on
244100040701     c                   movel     $ORGdes(xy)   S2Dpdc
244200040701     c                   movel     $ORGdes(xy)   S4Dpdc
244300040701      *
244400040701w   2c                   WHEN      XR9pdc  <> *zeros
244500040701     c                   move      XR9pdc        S2Cpdc
244600040701     c                   eval      APDpdr   = XR9pdc
244700040701     c                   exsr      CtrlAutotr2
244800040701     c                   movel     APDrsc        S2Dpdc
244900040701     c                   movel     APDrsc        S4Dpdc
245000050629      * - imposto il flag per autotrasportatore Messaggeria
245100050614if  3c                   if            $Default = *on
245200050614     c                             and §OGpkm   = 'S'
245300171012     c**************               and $VDL     = *on
245400050614     c                             and $IMA     = *off
245500050614     c                             and $PO      = *off
245600050701     c                   eval      wNFVseq  = 1
245700050614     c                   movel     APDflr        dAPDflr
245800050629     c                   exsr      X_Messaggeria
245900050614e   3c                   endif
246000040701     c                   setoff                                       552890
246100040701     c                   clear                   V1Dmsg
246200040701      *
246300040701w   2c                   WHEN      TR9bnl   = 'S'
246400040701     c                   eval      S4Dpdc   = C_BaiaNonLetti
246500040701      *
246600040701e   2c                   ENDSL
246700040701      *
246800040701e   1c                   ENDIF
246900040701      *
247000040701      * - - - Campi hidden
247100060113if  1c                   if        $Default = *off
247200060113if  2c                   if            $NewFVa = *off
247300060113     c                             and $DltPS  = *off
247400060113     c                             and $Found  = *on
247500060113     c                   eval      H2Cprg   = XR9prg
247600060113x   2c                   else
247700060113     c                   eval      H2Cprg   = (TR9prg * 2) - 1
247800060113e   2c                   endif
247900060113e   1c                   endif
248000040701if  1c                   IF            $Default = *off
248100040701     c                             and TR9bnl  <> 'S'
248200040701      *
248300040701     c                   eval      H2Cbai   = TR9bai
248400161213if  2c                   if        $POdft   = *on
248500161215     c                   eval      H2Cznc   = %subst( TR9des : 29 )
248600161213e   2c                   endif
248700040701     c                   eval      H2Cpdc   = TR9pdc
248800040701      *
248900040701if  2c                   if            $IMAdft = *off
249000040701     c                             and $POdft  = *off
249100040701     c                             and H2Cpdc <> APDpdr
249200040701     c                   eval      APDpdr   = H2Cpdc
249300040701     c                   exsr      CtrlAutotr2
249400040701     c                   setoff                                       552890
249500040701     c                   clear                   V1Dmsg
249600040701e   2c                   endif
249700040701      *
249800040701sel 2c                   Select
249900040701w   2c                   When      $IMAdft  = *off   and
250000040701     c                             $POdft   = *off
250100040701     c                   eval      H2Dpdc   = APDrsc
250200040701w   2c                   When      $IMAdft  = *on
250300040701     c                   eval      H2Dpdc   = $MPTdes(yy)
250400040701w   2c                   When      $POdft   = *on
250500040701     c                   eval      H2Dpdc   = $ORGdes(xy)
250600040701e   2c                   EndSl
250700040701      *
250800040701e   1c                   ENDIF
250900040701      *
251000040701     c                   ENDSR
251100040701      *
251200040701      *---------------------------------------------------------------*
251300040701      * Impostazione dei F.V. in piano giornaliero per P.O.  con  VDL *
251400040701      *---------------------------------------------------------------*
251500040701     c     CarSiVDL_2    BEGSR
251600040701      *
251700040701sel 1c                   Select
251800040701      *
251900040701w   1c                   When      TR9bnl   = 'S'
252000040701      *
252100040701w   1c                   When          $Found =  *off
252200040701     c                             and TR9bnl <> 'S'
252300040701     c                             and TR9bai <> C_PostazNull
252400040701     c                   eval      S2Cbai   = C_PostazNull
252500040701     c                   eval      S2Kdft   = C_Default
252600040701     c                   eval      S2Kbad   = C_Postazione
252700040701     c                   eval      S2Cbad   = H2Cbai
252800040701if  2c                   if        H2Cpdc  <> *zeros
252900040701     c                   eval      S2Katd   = C_Autotr
253000040701     c                   move      H2Cpdc        S2Catd
253100040701e   2c                   endif
253200040701     c                   eval      S2Datd   = H2Dpdc
253300040701      *
253400040701w   1c                   When          $Found =  *on
253500040701     c                             and XR9nfv <> *zeros
253600040701     c                   move      XR9nfv        S2Cnfv
253700040701     c                   move      DSdata6       S2Ddfv
253800040701     c                   eval      S2Dfpp   = XR9fl1
253900050629      * - reperisco il flag per autotrasportatore Messaggeria
254000050629if  2c                   if            $Default = *off
254100050629     c                             and §OGpkm   = 'S'
254200171012     c************                 and $VDL     = *on
254300050629     c                             and $IMA     = *off
254400050629     c                             and $PO      = *off
254500050701     c                   eval      FVVfgs   = V1Cpog
254600050701     c                   eval      FVVnpg   = 4
254700050629     c                   move      XR9nfv        FVVnfv
254800050629     c     K03FNFVV      chain     FNFVV000
254900050629if  3c                   if            %found(FNFVV03L)
255000050701     c                   eval      wNFVseq  = 1
255100050629     c                   movel     FVVspn        dFVVspn
255200050629     c                   exsr      X_Messaggeria
255300050629e   3c                   endif
255400050629e   2c                   endif
255500040701if  2c                   if        $IMA     = *off
255600040702     c                             and $PO  = *off
255700040701     c                   exsr      CercaIIfv
255800050629      * - reperisco il flag per autotrasportatore Messaggeria
255900050629if  3c                   if            $Default = *off
256000050629     c                             and §OGpkm   = 'S'
256100171012     c************                 and $VDL     = *on
256200050629     c                             and $IMA     = *off
256300050629     c                             and $PO      = *off
256400050629     c                             and %found(FLTR901X)
256500050629     c                             and XR9nfv   > *zeros
256600050701     c                   eval      FVVfgs   = V1Cpog
256700050701     c                   eval      FVVnpg   = 4
256800050629     c                   move      XR9nfv        FVVnfv
256900050629     c     K03FNFVV      chain     FNFVV000
257000050629if  4c                   if            %found(FNFVV03L)
257100050701     c                   eval      wNFVseq  = 2
257200050629     c                   movel     FVVspn        dFVVspn
257300050629     c                   exsr      X_Messaggeria
257400050629e   4c                   endif
257500050629e   3c                   endif
257600040701e   2c                   endif
257700040701      *
257800040701e   1c                   EndSl
257900040701      *
258000040701     c                   ENDSR
258100040607      *
258200040607      *---------------------------------------------------------------*
258300040615      * Verifica se Postazione = P.O. (...magari DPD) !!!             *
258400040607      *---------------------------------------------------------------*
258500040607     c     Ctrl_PO       BEGSR
258600040607      *
258700040607if  1c                   if        $Hidden  = *off
258800040607     c                   reset                   $PO
258900040621     c                   reset                   $POdpd
259000080129     c                   reset                   $POerr
259100040607x   1c                   else
259200040607     c                   reset                   $POdft
259300040607e   1c                   endif
259400040607      *
259500040607if  1c                   if            Wpost  <> *blanks
259600040607     c                             and Wpost  <> C_PostazNull
259700040607     c                             and $IMA   =  *off
259800040607     c                             and %subst(Wpost:1:1) >= '0'
259900040607     c                             and %subst(Wpost:1:1) <= '9'
260000040607      *
260100040607     c                   eval      xy       = 1
260200040607     c                   eval      *in10    = *off
260300040607     c     Wpost         lookup    $ORGfil(xy)                            10
260400040607if  2c                   if        $Hidden  = *off
260500040607     c                   eval      $PO      = *in10
260600040607x   2c                   else
260700040607     c                   eval      $POdft   = *in10
260800040607e   2c                   endif
260900040607      *
261000040607if  2c                   if        $Hidden  = *off
261100040607     c                             and $PO  = *on
261200040611     c                   eval      $POdpd   = ($ORGntw(xy) = 'DPD')
261300080129     c                   eval      $POerr   = ($ORGerr(xy) = *on)
261400040607     c                   add       1             $ORGnr(xy)
261500040607e   2c                   endif
261600040607      *
261700040607e   1c                   endif
261800040607      *
261900040607     c                   ENDSR
262000040408      *
262100040408      *---------------------------------------------------------------*
262200040408      * Verifica se postazione "IMA"                                  *
262300040408      *---------------------------------------------------------------*
262400040408     c     Ctrl_IMA      BEGSR
262500040408      *
262600040423if  1c                   if        $Hidden  = *off
262700040408     c                   reset                   $IMA
262800040423x   1c                   else
262900040423     c                   reset                   $IMAdft
263000040423e   1c                   endif
263100040408      *
263200040423if  1c                   if            Wpost  <> *blanks
263300040423     c                             and Wpost  <> C_PostazNull
263400040408      *
263500040423     c                   eval      yy       = 1
263600040408     c                   eval      *in10    = *off
263700040423     c     Wpost         lookup    $MPT(yy)                               10
263800040423if  2c                   if        $Hidden  = *off
263900040408     c                   eval      $IMA     = *in10
264000040423x   2c                   else
264100040423     c                   eval      $IMAdft  = *in10
264200040423e   2c                   endif
264300040423      *
264400040408e   1c                   endif
264500040408      *
264600040408     c                   ENDSR
264700040705      *
264800040705      *---------------------------------------------------------------*
264900040705      * Impostazione F.V. da default per postazione = P.O. DPD        *
265000040705      *---------------------------------------------------------------*
265100040705     c     PostPOdpd     BEGSR
265200040705      *
265300040705     c                   eval      xx     = 1
265400040705     c                   eval      *in10  = *off
265500040705     c                   reset                   Wpost
265600040705if  1c                   if        $VDL   = *off
265700040705     c                   move      S1Cbai        Wpost
265800040705x   1c                   else
265900040705     c                   move      S2Cbai        Wpost
266000040705e   1c                   endif
266100040705      *
266200040705do  1c                   DOW       xx    <= %elem($FVxPOa)
266300040705      *
266400040705     c     Wpost         lookup    $FVxPOa(xx)                            10
266500040705      *
266600040705      * Nessun f.v. reperito per il DPD in esame
266700040705if  2c                   if        NOT *in10
266800040705     c                   leave
266900040705e   2c                   endif
267000040705      *
267100040705      * Scarto i f.v. con p.o. DPD tra "altri P.O." del f.v.
267200040705if  2c                   if            *in10       = *on
267300040705     c                             and $FVxPOn(xx) < *zeros
267400040705     c                   eval      xx     = xx + 1
267500040705     c                   iter
267600040705e   2c                   endif
267700040705      *
267800040705      * Propongo il 1° f.v. con LNA = p.o. DPD
267900040705if  2c                   IF        *in10  = *on
268000040705      *
268100040705     c                   reset                   WLBda8
268200040705     c                   reset                   DSdata8
268300040705     c                   reset                   DSdata6
268400040705     c                   eval      G08inv = $FVxPOd(xx)
268500040705     c                   call      'XSRDA8'
268600040705     c                   parm                    WLBda8
268700040705     c                   move      G08dat        DSdata8
268800040705     c                   z-add     DSdt8gg       DSdt6gg
268900040705     c                   z-add     DSdt8mm       DSdt6mm
269000040705     c                   z-add     DSdt8aa       DSdt6aa
269100040705if  3c                   if        $VDL   = *off
269200040705     c                   move      $FVxPOn(xx)   S1Cnfv
269300040705     c                   move      DSdata6       S1Ddfv
269400040705x   3c                   else
269500040705     c                   move      $FVxPOn(xx)   S2Cnfv
269600040705     c                   move      DSdata6       S2Ddfv
269700040705e   3c                   endif
269800040705     c                   leave
269900040705      *
270000040705e   2c                   ENDIF
270100040705      *
270200040705e   1c                   ENDDO
270300040705      *
270400040705     c                   ENDSR
270500031106      *
270600031106      *---------------------------------------------------------------*
270700040615      * Impostazione F.V. da default per postazione IMA               *
270800031106      *---------------------------------------------------------------*
270900040615     c     PostIMA       BEGSR
271000031106      *
271100031106     c                   eval      FVVfgs = V1Cpog
271200031106     c                   eval      FVVnpg = 3
271300031106     c                   eval      FVVnfv = *hival
271400031106     c     K03FNFVV      setgt     FNFVV000
271500031106     c     K02FNFVV      readpe    FNFVV000
271600031106      *
271700031106do  1c                   DOW       NOT %eof(FNFVV03L)
271800031106      *
271900031106if  2c                   IF            FVVatb = *blanks
272000031106     c                             and FVVfcf = *blanks
272100031106     c                             and FVVspg = 'A'
272200031106     c                   reset                   WLBda8
272300031106     c                   reset                   DSdata8
272400031106     c                   reset                   DSdata6
272500031106     c                   eval      G08inv = FVVdfv
272600031106     c                   call      'XSRDA8'
272700031106     c                   parm                    WLBda8
272800031106     c                   move      G08dat        DSdata8
272900031106     c                   z-add     DSdt8gg       DSdt6gg
273000031106     c                   z-add     DSdt8mm       DSdt6mm
273100031106     c                   z-add     DSdt8aa       DSdt6aa
273200031106if  3c                   if        $VDL   = *off
273300031106     c                   move      *zeros        S1Cnfv
273400031106     c                   move      FVVnfv        S1Cnfv
273500031106     c                   move      DSdata6       S1Ddfv
273600031106     c                   eval      S1Dfpp = FVVfpp
273700031106x   3c                   else
273800031106     c                   move      *zeros        S2Cnfv
273900031106     c                   move      FVVnfv        S2Cnfv
274000031106     c                   move      DSdata6       S2Ddfv
274100031106     c                   eval      S2Dfpp = FVVfpp
274200031106e   3c                   endif
274300031106     c                   leave
274400031106e   2c                   ENDIF
274500031106      *
274600031106     c     K02FNFVV      readpe    FNFVV000
274700031106e   1c                   ENDDO
274800031106      *
274900031106     c                   ENDSR
275000031106      *
275100031106      *---------------------------------------------------------------*
275200040615      * Impostazione F.V. da default per postazione non IMA           *
275300031106      *---------------------------------------------------------------*
275400040615     c     PostNoIMA     BEGSR
275500031106      *
275600031106     c                   eval      xx     = 1
275700031106     c                   eval      *in10  = *off
275800031106     c                   movel     V1Cpog        Wpdr
275900031106if  1c                   if        $VDL   = *off
276000031106     c                   move      S1Cpdc        Wpdr
276100031106x   1c                   else
276200031106     c                   move      S2Cpdc        Wpdr
276300031106e   1c                   endif
276400031106     c     Wpdr          lookup    $MEMat(xx)                             10
276500050701      *
276600031111if  1c                   IF        *in10  = *on
276700050701      *
276800050701     c                   clear                   dFVVspn
276900031111      * - 1° f.v.
277000031111if  2c                   IF        $MEMfv1(xx) > *zeros
277100031106      *
277200031111     c                   move      $MEMfv1(xx)   DS_FV
277300031106     c                   reset                   WLBda8
277400031106     c                   reset                   DSdata8
277500031106     c                   reset                   DSdata6
277600031106     c                   eval      G08inv = dsFVdfv
277700031106     c                   call      'XSRDA8'
277800031106     c                   parm                    WLBda8
277900031106     c                   move      G08dat        DSdata8
278000031106     c                   z-add     DSdt8gg       DSdt6gg
278100031106     c                   z-add     DSdt8mm       DSdt6mm
278200031106     c                   z-add     DSdt8aa       DSdt6aa
278300031111if  3c                   if        $VDL   = *off
278400031106     c                   move      dsFVnfv       S1Cnfv
278500031106     c                   move      DSdata6       S1Ddfv
278600031111     c                   eval      S1Dfpp = $MEMmp1(xx)
278700031111x   3c                   else
278800031106     c                   move      dsFVnfv       S2Cnfv
278900031106     c                   move      DSdata6       S2Ddfv
279000031111     c                   eval      S2Dfpp = $MEMmp1(xx)
279100050629      *   - imposto il flag per autotrasportatore Messaggeria
279200050614if  4c                   if            $Default = *off
279300050614     c                             and §OGpkm   = 'S'
279400171012     c***********                  and $VDL     = *on
279500050614     c                             and $IMA     = *off
279600050614     c                             and $PO      = *off
279700050701     c                   eval      wNFVseq = 1
279800050701     c                   eval      §FVVtpm = $MEMpm1(xx)
279900050614     c                   exsr      X_Messaggeria
280000050614e   4c                   endif
280100031111e   3c                   endif
280200031111      *
280300031111e   2c                   ENDIF
280400031111      * - 2° f.v.
280500031111if  2c                   IF        $MEMfv2(xx) > *zeros
280600031111      *
280700031111     c                   move      $MEMfv2(xx)   DS_FV
280800031111     c                   reset                   WLBda8
280900031111     c                   reset                   DSdata8
281000031111     c                   reset                   DSdata6
281100031111     c                   eval      G08inv = dsFVdfv
281200031111     c                   call      'XSRDA8'
281300031111     c                   parm                    WLBda8
281400031111     c                   move      G08dat        DSdata8
281500031111     c                   z-add     DSdt8gg       DSdt6gg
281600031111     c                   z-add     DSdt8mm       DSdt6mm
281700031111     c                   z-add     DSdt8aa       DSdt6aa
281800031111if  3c                   if        $VDL   = *off
281900031111     c                   move      dsFVnfv       S3Cnfv
282000031111     c                   move      DSdata6       S3Ddfv
282100031111     c                   eval      S3Dfpp = $MEMmp2(xx)
282200031111x   3c                   else
282300031111     c                   move      dsFVnfv       S4Cnfv
282400031111     c                   move      DSdata6       S4Ddfv
282500031111     c                   eval      S4Dfpp = $MEMmp2(xx)
282600050629      *   - imposto il flag per autotrasportatore Messaggeria
282700050614if  4c                   if            $Default = *off
282800050614     c                             and §OGpkm   = 'S'
282900171012     c************                 and $VDL     = *on
283000050614     c                             and $IMA     = *off
283100050614     c                             and $PO      = *off
283200050701     c                   eval      wNFVseq = 2
283300050701     c                   eval      §FVVtpm = $MEMpm2(xx)
283400050614     c                   exsr      X_Messaggeria
283500050614e   4c                   endif
283600031111e   3c                   endif
283700031111      *
283800031111e   2c                   ENDIF
283900031106      *
284000031106e   1c                   ENDIF
284100031106      *
284200031106     c                   ENDSR
284300031106      *
284400031106      *---------------------------------------------------------------*
284500031106      * Reperimento eventuale secondo F.V. per postazione/baia        *
284600031106      *---------------------------------------------------------------*
284700031106     c     CercaIIfv     BegSr
284800031106      *
284900040621      * (Se modifica del "piano giornaliero" per P.O. con/senza VDL)
285000040621      *
285100040621     c                   eval      XR9prg = XR9prg + 1
285200040621     c     K06TR901      chain     FLTR901
285300031106      *
285400040621      * Se esiste: ne visualizzo i dati del f.v.
285500040621if  1c                   if        %found(FLTR901X)
285600040309     c                   reset                   WLBda8
285700040309     c                   reset                   DSdata8
285800040309     c                   reset                   DSdata6
285900040621if  2c                   if        XR9dfv   > *zeros
286000040621     c                   eval      G08inv   = XR9dfv
286100040309     c                   call      'XSRDA8'
286200040309     c                   parm                    WLBda8
286300040309     c                   move      G08dat        DSdata8
286400040309     c                   z-add     DSdt8gg       DSdt6gg
286500040309     c                   z-add     DSdt8mm       DSdt6mm
286600040309     c                   z-add     DSdt8aa       DSdt6aa
286700040309e   2c                   endif
286800040315if  2c                   if        $VDL   = *off
286900040621     c                   move      XR9nfv        S3Cnfv
287000040621     c                   eval      S3Dfpp   = XR9fl1
287100040621if  3c                   if        XR9dfv   > *zeros
287200031106     c                   move      DSdata6       S3Ddfv
287300040309e   3c                   endif
287400040315x   2c                   else
287500040621     c                   move      XR9nfv        S4Cnfv
287600040621     c                   eval      S4Dfpp   = XR9fl1
287700040621if  3c                   if        XR9dfv   > *zeros
287800040315     c                   move      DSdata6       S4Ddfv
287900040315e   3c                   endif
288000040315e   2c                   endif
288100031106e   1c                   endif
288200031106      *
288300031106     c                   ENDSR
288400031007      *
288500031007      *---------------------------------------------------------------*
288600031007      * Controllo dati immessi in videata S01 & S03                   *
288700031007      *---------------------------------------------------------------*
288800031007     c     CtrS01        BEGSR
288900031007      *
289000031007      * Ciclo di lettura del sfl
289100031007if  1c                   if        $Default = *on
289200031007     c                   readc     MS03S01
289300031007x   1c                   else
289400031007     c                   readc     MS03S03
289500031007e   1c                   endif
289600031020      *
289700031007do  1c                   DOW       NOT %eof(FIMS03D)
289800031007      *
289900031007     c                   movea     *zeros        *in(41)
290000031007     c                   z-add     S01nrr        C01rcd
290100031007      * - ctrl su singola riga
290200031014if  2c                   if            S1Cpep  <> *blanks
290300031014     c                             or  S1Cdes  <> *blanks
290400031014     c                             or  S1Cbai  <> *blanks
290500031014     c                             or (S1Cpdc  <> *zeros
290600031014     c                             and S1Cpdc  <> *blanks)
290700031014     c                             or (S1Cnfv  <> *blanks
290800031014     c                             and S1Cnfv  <> *zeros)
290900031007     c                   exsr      CtrS01rec
291000031020if  3c                   if            $Default = *off
291100031007     c                             and *in90    = *off
291200040518     c                             and S1Cbai  <> C_PostazNull
291300031007     c                   exsr      CtrS03rec
291400031007e   3c                   endif
291500031007x   2c                   else
291600031007     c                   clear                   S1Ddfv
291700031021     c                   clear                   S1Dfpp
291800031022     c                   clear                   S3Ddfv
291900031022     c                   clear                   S3Dfpp
292000031023     c                   clear                   S1Cast
292100031007     c                   clear                   S1Cpdc
292200031007     c                   clear                   S1Dpdc
292300031007     c                   clear                   S3Dpdc
292400031023     c                   clear                   S1Cdes
292500031023     c                   clear                   S1Kdft
292600031023     c                   clear                   S1Kbad
292700031023     c                   clear                   S1Cbad
292800031023     c                   clear                   S1Katd
292900031023     c                   clear                   S1Catd
293000031023     c                   clear                   S1Datd
293100031007e   2c                   endif
293200031010      *
293300031010      * - reimposto indicatori x DSPATR condizionati
293400040702      *   se NON rilevati errori
293500040702if  2c                   if        NOT *in90
293600040702     c                             and $Default = *on
293700040702     c                             or  $Default = *off
293800060110     c                   exsr      AtrS01S03
293900040702e   2c                   endif
294000031007      *
294100031007      * - riattivo SFLNXTCHG (non solo se rilevato errore)
294200031007     c                   eval      *in32    = *on
294300031007      * - aggiorno il SFL
294400031007if  2c                   if        $Default = *on
294500031007     c                   update    MS03S01
294600031007x   2c                   else
294700031007     c                   update    MS03S03
294800031007e   2c                   endif
294900031007     c                   eval      *in32    = *off
295000031007      * - se rilevato errore esco dal ciclo di controllo
295100031007if  2c                   if        *in28    = *on
295200031104     c                   z-add     C01rcd        C01csr
295300031007     c                   leave
295400031007e   2c                   endif
295500031007      * - leggo il record successivo
295600031007if  2c                   if        $Default = *on
295700031007     c                   readc     MS03S01
295800031007x   2c                   else
295900031007     c                   readc     MS03S03
296000031007e   2c                   endif
296100031007      *
296200031007e   1c                   ENDDO
296300031020      *
296400040423      * Controlli finali "generali"
296500031021      *   (se NON ho già rilevato altri errori da segnalare)
296600040423if  1c                   if        *in28    = *off
296700031021     c                   exsr      CtrBAIxPEP
296800031020e   1c                   endif
296900031007      *
297000031007     c                   ENDSR
297100031007      *
297200031007      *---------------------------------------------------------------*
297300031007      * Controllo singolo record del subfile S01 & S03                *
297400031007      *---------------------------------------------------------------*
297500031007     c     CtrS01rec     BEGSR
297600031023      *
297700031023if  1c                   if        $Default = *off
297800031023     c                   clear                   S1Kdft
297900031023     c                   clear                   S1Kbad
298000031023     c                   clear                   S1Cbad
298100031023     c                   clear                   S1Katd
298200031023     c                   clear                   S1Catd
298300031023     c                   clear                   S1Datd
298400031023e   1c                   endif
298500040423      *
298600040423     c                   reset                   $IMA
298700040423     c                   reset                   $IMAdft
298800031007      *
298900040305      * Controllo codice baia
299000031007if  1c                   if        S1Cpep = *blanks
299100031007     c                   seton                                        512890
299200031009     c                   movel     $Msg(10)      V1Dmsg
299300031007x   1c                   else
299400031010     c                   move(p)   S1Cpep        w014a
299500031010     c     w014a         lookup    $MSLpep                                10
299600031007if  2c                   if        NOT *in10
299700031007     c                   seton                                        512890
299800031009     c                   movel     $Msg(11)      V1Dmsg
299900060110     c                   leavesr
300000031007e   2c                   endif
300100040226     c                   exsr      ContaPEP
300200031007e   1c                   endif
300300060110     c                   if        *in90
300400060110     c                   leavesr
300500060110     c                   endif
300600031007      *
300700031007      * Controllo descrizione
300800031014      * (non obbligatoria)
300900031007      *
301000040305      * Controllo codice postazione
301100040423      * - ricerca
301200040423     c     '?'           scan      S1Cbai                                 10
301300040423if  1c                   if        *in10  =  *on
301400040423     c                   eval      S1Cbai =  *blanks
301500040423     c                   eval      Wcampo =  'S1CBAI    '
301600040423     c                   exsr      srSEARCH
301700040423e   1c                   endif
301800040423      * - controllo
301900040423if  1c                   if        $Default = *off
302000040423     c                   eval      $Hidden  = *on
302100040423     c                   eval      Wpost    = H1Cbai
302200040423     c                   exsr      Ctrl_IMA
302300040423if  2c                   if        $IMAdft  = *on
302400040423     c                   eval      H1Dpdc   = $MPTdes(yy)
302500040423e   2c                   endif
302600040423e   1c                   endif
302700040423     c                   eval      $Hidden  = *off
302800040423     c                   eval      Wpost    = S1Cbai
302900040408     c                   exsr      Ctrl_IMA
303000040423if  1c                   if        $IMA     = *on
303100040423     c                   eval      S1Dpdc   = $MPTdes(yy)
303200040423     c                   eval      S3Dpdc   = $MPTdes(yy)
303300040423e   1c                   endif
303400040423if  1c                   if            $Default = *off
303500040423     c                             and $IMAdft  = *on
303600040423     c                             and H1Cbai  <> S1Cbai
303700040423     c                   eval      S1Datd   = H1Dpdc
303800040423e   2c                   endif
303900031007sel 1c                   select
304000031020w   1c                   when          $Default = *off
304100040316     c                             and S1Cbai   = C_PostazNull
304200031020     c                   clear                   S1Cnfv
304300031020     c                   clear                   S1Ddfv
304400031021     c                   clear                   S1Dfpp
304500031211     c                   clear                   S3Cnfv
304600031211     c                   clear                   S3Ddfv
304700031211     c                   clear                   S3Dfpp
304800031020     c                   clear                   S3Dpdc
304900040408     c                   clear                   S1Cpdc
305000031007w   1c                   when      S1Cbai   =  *blanks
305100031007     c                   seton                                        532890
305200031009     c                   movel     $Msg(13)      V1Dmsg
305300070411w   1c                   when         (%subst(S1Cbai:1:1) >= 'A'
305400070411     c                             and %subst(S1Cbai:1:1) <= 'Z')
305500031010     c                             and %subst(S1Cbai:2:2) >= '01'
305600031010     c                             and %subst(S1Cbai:2:2) <= '99'
305700031007     c                   move      S1Cbai        w002a
305800031007     c     DigitN        check     w002a                                  10
305900031009if  2c                   if        *in10  =  *on
306000031007     c                   seton                                        532890
306100031009     c                   movel     $Msg(14)      V1Dmsg
306200031007x   2c                   else
306300040423     c                   eval      Wpost  =  S1Cbai
306400031007     c                   exsr      ContaBAI
306500031007if  3c                   if        $BAInr(xx) >  1
306600031007     c                   seton                                        532890
306700031009     c                   movel     $Msg(15)      V1Dmsg
306800031007e   3c                   endif
306900031007e   2c                   endif
307000040408w   1c                   when      $IMA   =  *off
307100031007     c                   seton                                        532890
307200031009     c                   movel     $Msg(14)      V1Dmsg
307300031007e   1c                   endsl
307400060110     c                   if        *in90
307500060110     c                   leavesr
307600060110     c                   endif
307700040316      *
307800040316      * Controllo la postazione sia "compatibile" con le altre già
307900040316      *   immesse nella stessa baia
308000161110      * -?Su richiesta di Nicola:?tolto controllo postaz. IMA/non-IMA
308100161110      *   (ora IMA e AUT possono stare nella stessa baia;
308200161110      *   sono le FILIALI che continuano a richiedere una loro baia -
308300161110      *   - MA SONO INSERIBILI SOLO NEI PIANI VDL, non in questi)
308400040316      *
308500040305      * ...e ne memorizzo gli utilizzi per baia
308600040316if  1c                   if        S1Cbai <> C_PostazNull
308700031021     c                   exsr      MemBAIxPEP
308800031020e   1c                   endif
308900040315      *
309000040305      * Visualizzo la postazione di default se variata in piano
309100040305      *   giornaliero
309200031023if  1c                   if            $Default = *off
309300031023     c                             and S1Cbai  <> H1Cbai
309400040423     c                   eval      S1Kdft  = C_Default
309500040625     c                   eval      S1Kbad  = C_Postazione
309600040423     c                   eval      S1Cbad  = H1Cbai
309700040423if  2c                   if        $IMAdft = *on
309800040423     c                   eval      S1Datd  = H1Dpdc
309900040423e   2c                   endif
310000031023e   1c                   endif
310100031007      *
310200031007      * Controllo codice autotrasportatore
310300031007      *   (in S03 e S04 è campo protetto)
310400031015      * Per il piano giornaliero farò appositi controlli nella subr.
310500031015      *   CTRS03REC
310600031007if  1c                   if        $Default = *on
310700040423if  2c                   if        $IMA     = *off
310800031007     c                   clear                   S1Dpdc
310900031009     c                   clear                   S3Dpdc
311000040423e   2c                   endif
311100031007      * - se *blanks lo considero *zeros (sarebbe numerico)
311200031007if  2c                   if        S1Cpdc =  *blanks
311300031007     c                   eval      S1Cpdc =  *zeros
311400031007e   2c                   endif
311500031007      * - ricerca
311600031007     c     '?'           scan      S1Cpdc                                 10
311700031007if  2c                   if        *in10  =  *on
311800031007     c                   eval      S1Cpdc =  *zeros
311900031007     c                   eval      Wcampo =  'S1CPDC    '
312000031007     c                   exsr      srSEARCH
312100031007e   2c                   endif
312200031007      * - controllo
312300031007sel 2c                   select
312400040409w   2c                   when         ($IMA   =  *on
312500040409     c                              or S1Cbai =  C_PostazNull)
312600031010     c                             and S1Cpdc =  *zeros
312700040409w   2c                   when          $IMA   =  *on
312800031007     c                             and S1Cpdc <> *zeros
312900031007     c                   seton                                        552890
313000031009     c                   movel     $Msg(16)      V1Dmsg
313100031015x   2c                   other
313200031021     c                   movel     V1Cpog        APDpdr
313300031021     c                   move      S1Cpdc        APDpdr
313400031014     c                   exsr      CtrlAutotr2
313500031007     c                   movel     APDrsc        S1Dpdc
313600031009     c                   movel     APDrsc        S3Dpdc
313700031007e   2c                   endsl
313800060110     c                   if        *in90
313900060110     c                   leavesr
314000060110     c                   endif
314100031007      * - se *zeros lo reimposto a *blanks (numerico "editato")
314200031007if  2c                   if        S1Cpdc =  *zeros
314300031007     c                   eval      S1Cpdc =  *blanks
314400031007e   2c                   endif
314500031007e   1c                   endif
314600031027      *
314700040305      * In caso di piano giornaliero controllo se è stata tolta la
314800040305      *   postazione ("XXX"), perchè in tal caso non verrà eseguita
314900040305      *   la subr. per i controlli specifici, pertanto non verrebbe
315000040409      *   evidenziato l'autotrasportatore pre-impostato nel default
315100031027if  1c                   if            $Default = *off
315200040316     c                             and S1Cbai   = C_PostazNull
315300031027     c                   clear                   S1Cpdc
315400031027     c                   clear                   S3Dpdc
315500031027     c                   exsr      DecS03pdr
315600031027e   1c                   endif
315700031007      *
315800060110     c                   ENDSR
315900031022      *
316000031022      *---------------------------------------------------------------*
316100031022      * Controlli aggiuntivi per singolo record del subfile S03       *
316200031022      *---------------------------------------------------------------*
316300031022     c     CtrS03rec     BEGSR
316400031022      *
316500031022     c                   reset                   SAVnfv
316600031022      *
316700031022      * Se impostato solo il 2° numero F.V. lo sposto al 1°
316800031022if  1c                   if            (S1Cnfv  =  *blanks
316900031022     c                             or   S1Cnfv  =  *zeros)
317000031022     c                             and (S3Cnfv  <> *blanks
317100031022     c                             and  S3Cnfv  <> *zeros)
317200031022     c                   eval      S1Cnfv  =  S3Cnfv
317300031022     c                   clear                   S3Cnfv
317400031022     c                   clear                   S3Ddfv
317500031022     c                   clear                   S3Dfpp
317600031022e   1c                   endif
317700031022      *
317800031022      * Controllo 1° numero foglio viaggio
317900031022     c                   clear                   S1Ddfv
318000031022     c                   clear                   S1Dfpp
318100031022     c                   eval      wNFVseq =  1
318200031022     c                   eval      Wnfv    =  S1Cnfv
318300031106     c                   exsr      Ctrl_1FV
318400031022     c                   eval      S1Cnfv  =  Wnfv
318500060110     c                   if        *in90
318600060110     c                   leavesr
318700060110     c                   endif
318800031024      * Se tutto bene: ne imposto i dati a video
318900040305if  1c                   if        Wnfv    <> *blanks
319000031022     c                   eval      S1Dfpp  =  FVVfpp
319100031022     c                   move      DSdata6       S1Ddfv
319200040305e   1c                   endif
319300031024      *
319400031024      * Imposto flag e decodifico variazioni relative all'autotrasp.
319500031024      *   (che deve essere comune ad entrambi i F.V.)
319600040312if  1c                   if            S1Cnfv  <> *blanks
319700040408     c                             and $IMA    =  *off
319800031024     c                   move      APDpdr        S1Cpdc
319900031024     c                   movel     APDrsc        S3Dpdc
320000031024x   1c                   else
320100031024     c                   clear                   S1Cpdc
320200040423if  2c                   if        $IMA     = *off
320300031024     c                   clear                   S3Dpdc
320400040423e   2c                   endif
320500031024e   1c                   endif
320600031024     c                   exsr      DecS03pdr
320700031022      *
320800040312      * Controllo 2° numero foglio viaggio (se immesso)
320900031022     c                   clear                   S3Ddfv
321000031022     c                   clear                   S3Dfpp
321100040312if  1c                   if            S3Cnfv <> *blanks
321200040312     c                             and S3Cnfv <> *zeros
321300031022     c                   eval      wNFVseq =  2
321400031022     c                   eval      Wnfv    =  S3Cnfv
321500031106     c                   exsr      Ctrl_1FV
321600031022     c                   eval      S3Cnfv  =  Wnfv
321700060110     c                   if        *in90
321800060110     c                   leavesr
321900060110     c                   endif
322000031024      * Se tutto bene: ne imposto i dati a video
322100031024if  2c                   if        Wnfv    <> *blanks
322200031022     c                   eval      S3Dfpp  =  FVVfpp
322300031022     c                   move      DSdata6       S3Ddfv
322400031024e   2c                   endif
322500031111      * e rieseguo il controllo sul numero di F.V. x autotrasportatore
322600031111      *   (se ne ho selezionato uno da ricerca)
322700031111if  2c                   if        S1Cast  =  '?'
322800031111     c                   clear                   S1Cast
322900031111     c                   exsr      CtrlAutotr3
323000031111     c                   eval      S1Cast =  Wast
323100031111e   2c                   endif
323200040312x   1c                   else
323300040312     c                   clear                   S3Cnfv
323400040312e   1c                   endif
323500031022      *
323600060110     c                   ENDSR
323700031007      *
323800031007      *---------------------------------------------------------------*
323900031106      * Controlli per singolo F.V. del subfile S03 o S04              *
324000031007      *---------------------------------------------------------------*
324100031106     c     Ctrl_1FV      BEGSR
324200040621      *
324300040621     c                   reset                   Wdfv
324400031007      *
324500031007      * Controllo numero foglio viaggio
324600031007      * - se *blanks lo considero *zeros (sarebbe numerico)
324700031106if  1c                   if        Wnfv    =  *blanks
324800031106     c                   eval      Wnfv    =  *zeros
324900031007e   1c                   endif
325000031007      * - ricerca
325100031022     c     '?'           scan      Wnfv                                   10
325200031106if  1c                   if        *in10   =  *on
325300031106     c                   eval      Wnfv    =  *zeros
325400031106sel 2c                   select
325500031106w   2c                   when          $VDL    =  *off
325600031106     c                             and wNFVseq =  1
325700031106     c                   eval      Wcampo  =  'S1CNFV    '
325800031106w   2c                   when          $VDL    =  *off
325900031106     c                             and wNFVseq =  2
326000031106     c                   eval      Wcampo  =  'S3CNFV    '
326100031106w   2c                   when          $VDL    =  *on
326200031106     c                             and wNFVseq =  1
326300031106     c                   eval      Wcampo  =  'S2CNFV    '
326400031106w   2c                   when          $VDL    =  *on
326500031106     c                             and wNFVseq =  2
326600031106     c                   eval      Wcampo  =  'S4CNFV    '
326700031106e   2c                   endsl
326800031007     c                   exsr      srSEARCH
326900031022      * - - se non selezionato nulla sul 2° F.V. -> esco
327000031022if  2c                   if            wNFVseq =  2
327100031022     c                             and Wnfv    =  *zeros
327200031106     c                   eval      Wnfv    =  *blanks
327300060110     c                   leavesr
327400031022e   2c                   endif
327500031111      * - - se selezionato: memorizzo che dovrò ri-controllare se
327600031111      *     padroncino con altri f.v. (non visualizzati)
327700031111     c                   eval      S1Cast =  '?'
327800031007e   1c                   endif
327900031007      * - controlli:
328000031007      * - - numericità
328100031022     c     DigitN        check     Wnfv                                   10
328200031106if  1c                   if        *in10   =  *on
328300031106if  2c                   if        wNFVseq =  2
328400040309     c                   eval      *in56   =  *on
328500031022x   2c                   else
328600040309     c                   eval      *in54   =  *on
328700031022e   2c                   endif
328800031022     c                   seton                                          2890
328900040309     c                   eval      V1Dmsg  =  $Msg(20)
329000060110     c                   leavesr
329100031007e   1c                   endif
329200031007      *
329300031010      * - - effettiva immissione
329400031106if  1c                   if        Wnfv    =  *zeros
329500031106      *
329600031106     c                   clear                   FVVdfv
329700031106     c                   clear                   FVVfpp
329800031106      *
329900080129sel 2c                   select
330000080129      * - - - F.V.?non?più obbligatorio per fil. DPD
330100080129      *       (visualizzato solo un msg di avvertimento)
330200080129w   2c                   when          $VDL    =  *on
330300080129     c                             and $POdpd  =  *on
330400080129     c                             and $POerr  =  *off
330500080129     c                   eval      $POerr  = *on
330600080129     c                   seton                                        542890
330700080129     c                   eval      V1Dmsg  =  $Msg(50)
330800080129      * - - - F.V. obbligatorio per postazione regolare
330900080129w   2c                   when      $POdpd  =  *off
331000080129if  3c                   if        wNFVseq =  2
331100040309     c                   eval      *in56   =  *on
331200080129x   3c                   else
331300040309     c                   eval      *in54   =  *on
331400080129e   3c                   endif
331500031022     c                   seton                                          2890
331600080129if  3c                   if        $IMA    =  *off
331700040312     c                   eval      V1Dmsg  =  $Msg(7)
331800080129x   3c                   else
331900040309     c                   eval      V1Dmsg  =  $Msg(30)
332000080129e   3c                   endif
332100080129e   2c                   endsl
332200080129     c                   if        *in90
332300060110     c                   leavesr
332400080129     c                   endif
332500031106      *
332600031010x   1c                   else
332700031106      *
332800040611      * - - univocità per postazione
332900031030sel 2c                   select
333000031106w   2c                   when      wNFVseq =  1
333100031106     c                   eval      SAVnfv  =  Wnfv
333200040610      * - - - ammesso un solo F.V. per postazione "IMA"
333300031106w   2c                   when          wNFVseq =  2
333400040408     c                             and $IMA    =  *on
333500031106     c                   seton                                        562890
333600040309     c                   eval      V1Dmsg  =  $Msg(37)
333700040611      * - - - ammesso un solo F.V. per postazione P.O. DPD
333800040610w   2c                   when          wNFVseq =  2
333900040611     c                             and $POdpd  =  *on
334000040610     c                   seton                                        562890
334100040610     c                   eval      V1Dmsg  =  $Msg(42)
334200040610      * - - - non ammessi 2 F.V. identici
334300031106w   2c                   when          wNFVseq =  2
334400031106     c                             and SAVnfv  =  Wnfv
334500040408     c                             and $IMA    =  *off
334600031022     c                   seton                                        562890
334700040309     c                   eval      V1Dmsg  =  $Msg(35)
334800031106e   2c                   endsl
334900060110     c                   if        *in90
335000060110     c                   leavesr
335100060110     c                   endif
335200031106      *
335300031030      * - - univocità per foglio arrivi
335400040607if  2c                   if            $IMA    =  *off
335500040607     c                             and $PO     =  *off
335600031030     c                   exsr      ContaNFV
335700031106if  3c                   if        $NFVnr(xx)  >  1
335800031106if  4c                   if        wNFVseq     =  2
335900040309     c                   eval      *in56   =  *on
336000031030x   4c                   else
336100040309     c                   eval      *in54   =  *on
336200031030e   4c                   endif
336300031030     c                   seton                                          2890
336400040518     c                   eval      V1Dmsg  =  %replace(dsBPpos:
336500031106     c                                        $Msg(39):
336600040312     c                                        %scan('&PS':$Msg(39)))
336700060110     c                   leavesr
336800031030e   3c                   endif
336900031030e   2c                   endif
337000031007      * - - esistenza in FNFVV
337100040615if  2c                   if        $PO     =  *off
337200031106     c                   eval      FVVfgs  =  V1Cpog
337300040615if  3c                   if        $IMA    =  *on
337400031106     c                   eval      FVVnpg  =  3
337500040615x   3c                   else
337600031106     c                   eval      FVVnpg  =  4
337700040615e   3c                   endif
337800031022     c                   move      Wnfv          FVVnfv
337900031007     c     K03FNFVV      chain     FNFVV000
338000040615if  3c                   if        NOT %found(FNFVV03L)
338100031106     c*** (già da vl)              or  FVVatb  <> *blanks
338200031106     c*** (già da vl)              or  FVVfcf  <> *blanks
338300040414     c                             or ($IMA    =  *on
338400040408     c                             and FVVspg  <> 'A')
338500040615if  4c                   if        wNFVseq =  2
338600040309     c                   eval      *in56   =  *on
338700040615x   4c                   else
338800040309     c                   eval      *in54   =  *on
338900040615e   4c                   endif
339000031022     c                   seton                                          2890
339100040309     c                   eval      V1Dmsg  =  $Msg(21)
339200060110     c                   leavesr
339300040615e   3c                   endif
339400040621     c                   eval      Wdfv    =  FVVdfv
339500050614      * - - - distinta Messaggeria per piano di smistamento
339600050701if  3c                   if            §OGpkm  =  'S'
339700171012     c**************               and $VDL    =  *on
339800050701     c                             and $IMA    =  *off
339900050701     c                             and $PO     =  *off
340000050701     c                             and S2Cbai  <> *blanks
340100050614     c                             and S2Cbai  <> C_PostazNull
340200050629     c                   movel     FVVspn        dFVVspn
340300050614     c                   exsr      X_Messaggeria
340400050614e   3c                   endif
340500040615      * - - esistenza in FNFGV
340600040615x   2c                   else
340700040615     c                   exsr      CtrlFVxPO
340800060110     c                   if        *in90
340900060110     c                   leavesr
341000060110     c                   endif
341100040615e   2c                   endif
341200031007      * - - univocità F.V. IMA nello stesso piano
341300040408if  2c                   if        $IMA    =  *on
341400031106if  3c                   if        SAVnfvIMA   =  *zeros
341500031022     c                   move      Wnfv          SAVnfvIMA
341600031007e   3c                   endif
341700031106if  3c                   if        Wnfv    <> SAVnfvIMA
341800040615if  4c                   if        wNFVseq =  2
341900040309     c                   eval      *in56   =  *on
342000031022x   4c                   else
342100040309     c                   eval      *in54   =  *on
342200031022e   4c                   endif
342300031022     c                   seton                                          2890
342400040518     c                   eval      V1Dmsg  =  %replace(SAVnfvIMA:
342500031106     c                                        $Msg(22):
342600040408     c                                        %scan('&FIMA':$Msg(22)))
342700060110     c                   leavesr
342800031007e   3c                   endif
342900040408      * - - - pulisco l'eventuale cod. autotrasportatore se F.V. IMA
343000040423      *       e ne imposto la decodifica da tab. "MPT"
343100040408if  3c                   if        $VDL    =  *off
343200040408     c                   clear                   S1Cpdc
343300040423     c*** già decodif.:  clear                   S3Dpdc
343400040408x   3c                   else
343500040408     c                   clear                   S2Cpdc
343600040423     c*** già decodif.:  clear                   S4Dpdc
343700040408e   3c                   endif
343800031007e   2c                   endif
343900040615      * - - controllo F.V. non IMA né per P.O. DPD
344000040408if  2c                   if        $IMA    =  *off
344100040615     c                             and $PO =  *off
344200031022      * - - - F.V. di data antecedente a quella del F.V. arrivi
344300031106if  3c                   if        FVVdfv  <  SAVdfva
344400031106if  4c                   if        wNFVseq =  2
344500040309     c                   eval      *in56   =  *on
344600031022x   4c                   else
344700040309     c                   eval      *in54   =  *on
344800031022e   4c                   endif
344900031022     c                   seton                                          2890
345000040309     c                   eval      V1Dmsg  =  $Msg(38)
345100060110     c                   leavesr
345200031022e   3c                   endif
345300031022      * - - - F.V. non IMA assegnato ad altro autotrasportatore
345400040519      *       o ad autotrasportatore già impegnato in altra baia/post.
345500031014     c                   exsr      CtrlAutotr1
345600060110     c                   if        *in90
345700060110     c                   leavesr
345800060110     c                   endif
345900040519      * - - - Controllo/decodifica codice autotrasportatore
346000031106if  3c                   if        wNFVseq <  2
346100031106     c                   eval      APDpdr  =  DSTpdr
346200031020     c                   exsr      CtrlAutotr2
346300060110     c                   if        *in90
346400060110     c                   leavesr
346500060110     c                   endif
346600031022e   3c                   endif
346700031024e   2c                   endif
346800031024e   1c                   endif
346900031007      *
347000031007      * Se OK imposto la data
347100031106     c                   reset                   WLBda8
347200031022     c                   reset                   DSdata8
347300031022     c                   reset                   DSdata6
347400031106if  1c                   if        Wnfv    <> *zeros
347500040621     c                   eval      G08inv  =  Wdfv
347600031007     c                   call      'XSRDA8'
347700031007     c                   parm                    WLBda8
347800031021     c                   move      G08dat        DSdata8
347900031021     c                   z-add     DSdt8gg       DSdt6gg
348000031021     c                   z-add     DSdt8mm       DSdt6mm
348100031021     c                   z-add     DSdt8aa       DSdt6aa
348200031106e   1c                   endif
348300031014      * Se *zeros lo reimposto a *blanks (numerico "editato")
348400031106if  1c                   if        Wnfv    =  *zeros
348500031106     c                   eval      Wnfv    =  *blanks
348600031014e   1c                   endif
348700031111      * memorizzo il F.V. tra quelli immessi nel SFL
348800031111if  1c                   if        Wnfv    >  *zeros
348900031111     c                   eval      xx      =  1
349000031111     c                   eval      *in10   =  *off
349100031111     c     *zeros        lookup    $FV(xx)                                10
349200031111if  2c                   if        *in10   =  *on
349300031111     c                   move      Wnfv          $FV(xx)
349400031111e   2c                   endif
349500031111e   1c                   endif
349600031007      *
349700060110     c                   ENDSR
349800040615      *
349900040615      *---------------------------------------------------------------*
350000040621      * Controlli per singolo foglio viaggio per punto operativo      *
350100040615      *---------------------------------------------------------------*
350200040615     c     CtrlFVxPO     BEGSR
350300040615      *
350400040621      * - Verifica esistenza tra i f.v. già memorizzati
350500040621      *   (con data >= a quella del f.v. V1CNFV)
350600040621     c                   clear                   Wnfv7
350700040621     c                   move      Wnfv          Wnfv7
350800040615     c                   eval      xx      =  1
350900040615     c                   eval      *in10   =  *off
351000040621     c     Wnfv7         lookup    $FVxPOn(xx)                            10
351100070521      * - verifica se F.V. assegnabile a più filiali di "passaggio"
351200070521if  1c                   dow           *in10       =  *on
351300070521     c                             and $FVxPOa(xx) <> Wpost
351400070521     c                             and xx          <  %elem($FVxPOn)
351500070521     c                   eval      xx      =  xx + 1
351600070521     c                   eval      *in10   =  *off
351700070521     c     Wnfv7         lookup    $FVxPOn(xx)                            10
351800070521e   1c                   enddo
351900040621sel 1c                   select
352000040621      * - - ok
352100040621w   1c                   when          *in10       =  *on
352200040621     c                             and $FVxPOa(xx) =  Wpost
352300040621     c                   eval      Wdfv    =  $FVxPOd(xx)
352400060110     c                   leavesr
352500040621      * - - assegnato a P.O. diverso da quello indicato come postazione
352600070521w   1c                   when          *in10       =  *on
352700070521     c                             and $FVxPOa(xx) <> Wpost
352800070521if  2c                   if        wNFVseq =  2
352900070521     c                   eval      *in56   =  *on
353000070521x   2c                   else
353100070521     c                   eval      *in54   =  *on
353200070521e   2c                   endif
353300070521     c                   seton                                          2890
353400070521     c                   eval      V1Dmsg  =  %replace($FVxPOa(xx):
353500070521     c                                        $Msg(43):
353600070521     c                                        %scan('&PS':$Msg(43)))
353700070521     c                   leavesr
353800040621e   1c                   endsl
353900040621      *
354000040621      * - Verifica esistenza tra i f.v. NON memorizzati
354100040621     c                   move      Wnfv          FGVnfv
354200040621     c                   move      S2Cbai        FGVlnp
354300040621     c     K02FGV01      chain     FNFGV01
354400040621sel 1c                   select
354500040621w   1c                   when      NOT %found(FNFGV01L)
354600040615     c                             or  %found(FNFGV01L)
354700040615     c                             and (FGVatb <> *blanks
354800040615     c                              or  FGVfcf <> *blanks)
354900040621if  2c                   if        wNFVseq =  2
355000040615     c                   eval      *in56   =  *on
355100040621x   2c                   else
355200040615     c                   eval      *in54   =  *on
355300040621e   2c                   endif
355400040615     c                   seton                                          2890
355500040615     c                   eval      V1Dmsg  =  %replace(S2Cbai:
355600040615     c                                        $Msg(23):
355700040615     c                                        %scan('&PS':$Msg(23)))
355800060110     c                   leavesr
355900040621w   1c                   when      FGVdfv  <  SAVdfvA
356000040621if  2c                   if        wNFVseq =  2
356100040621     c                   eval      *in56   =  *on
356200040621x   2c                   else
356300040621     c                   eval      *in54   =  *on
356400040621e   2c                   endif
356500040621     c                   seton                                          2890
356600040621     c                   eval      V1Dmsg  =  $Msg(38)
356700040621e   1c                   endsl
356800060110     c                   if        *in90
356900060110     c                   leavesr
357000060110     c                   endif
357100040621     c                   eval      Wdfv    =  FGVdfv
357200040615      *
357300060110     c                   ENDSR
357400031024      *
357500031024      *---------------------------------------------------------------*
357600031024      * Impostazione campi descrittivi per l'autotrasportatore        *
357700031024      *---------------------------------------------------------------*
357800031024     c     DecS03pdr     BEGSR
357900031024      *
358000031024     c                   clear                   Wpdr
358100031024if  1c                   if        S1Cpdc >  *zeros
358200031024     c                   move      S1Cpdc        Wpdr
358300031024e   1c                   endif
358400031024      *
358500031024      * Visualizzazione "*" se più F.V. per il padroncino
358600031024     c                   clear                   S1Cast
358700031024if  1c                   if        S1Cpdc >  *zeros
358800031024     c                   exsr      CtrlAutotr3
358900031024     c                   eval      S1Cast =  Wast
359000031024e   1c                   endif
359100031024      *
359200031024      * Visualizzazione padroncino di default se variato
359300031024if  1c                   if        Wpdr   =  H1Cpdc
359400031024     c                   movel     H1Dpdc        S3Dpdc
359500031024x   1c                   else
359600040408if  2c                   if            $IMA   =  *off
359700040316     c                             and S1Cbai <> C_PostazNull
359800040423     c                             and S1Cpdc >  *zeros
359900031024     c                   move      APDpdr        S1Cpdc
360000031024     c                   movel     APDrsc        S3Dpdc
360100031024e   2c                   endif
360200031024     c                   eval      S1Kdft =  C_Default
360300031024if  2c                   if        H1Cpdc <> *zeros
360400040315     c                   eval      S1Katd =  C_Autotr
360500031024     c                   move      H1Cpdc        S1Catd
360600031024e   2c                   endif
360700031024     c                   eval      S1Datd =  H1Dpdc
360800031024e   1c                   endif
360900031024      *
361000031024     c                   ENDSR
361100031007      *
361200031007      *---------------------------------------------------------------*
361300031007      * Controllo dati immessi in videata S02 & S04                   *
361400031007      *---------------------------------------------------------------*
361500031010     c     CtrS02        BEGSR
361600061011      *
361700061011     c                   clear                   $PObau
361800061011     c                   clear                   $POpos
361900161212     c                   clear                   sk_Fil
362000161212     c                   clear                   sk_Rrn
362100090203     c                   clear                   Free_Rec_Sfl
362200031007      *
362300031007      * Ciclo di lettura del sfl
362400031007if  1c                   if        $Default = *on
362500031007     c                   readc     MS03S02
362600031007x   1c                   else
362700031007     c                   readc     MS03S04
362800031007e   1c                   endif
362900031020      *
363000031007do  1c                   DOW       NOT %eof(FIMS03D)
363100031007      *
363200031007     c                   movea     *zeros        *in(41)
363300031007     c                   z-add     S02nrr        C02rcd
363400031007      * - ctrl su singola riga
363500031014if  2c                   if            S2Cpep  <> *zeros
363600031014     c                             or  S2Cdes  <> *blanks
363700031014     c                             or  S2Cbnl  <> *blanks
363800040304     c                             or  S2Cbai  <> *blanks
363900161215     c                             or  S2Cznc  <> *blanks
364000031014     c                             or (S2Cpdc  <> *zeros
364100031014     c                             and S2Cpdc  <> *blanks)
364200031014     c                             or (S2Cnfv  <> *blanks
364300031014     c                             and S2Cnfv  <> *zeros)
364400031007     c                   exsr      CtrS02rec
364500031007if  3c                   if            $Default = *off
364600031007     c                             and *in90    = *off
364700040611     c                             and ($PO     = *off
364800040611     c                              or  $POdpd  = *on)
364900040518     c                             and S2Cbai  <> C_PostazNull
365000040309     c                             and S2Cbnl  <> 'S'
365100031007     c                   exsr      CtrS04rec
365200031007e   3c                   endif
365300031007x   2c                   else
365400040304     c                   clear                   S2Cbai
365500161213     c                   clear                   S2Cznc
365600031023     c                   clear                   S2Ddfv
365700031106     c                   clear                   S2Dfpp
365800050628     c                   clear                   S2Dmes
365900031106     c                   clear                   S4Ddfv
366000031106     c                   clear                   S4Dfpp
366100050628     c                   clear                   S4Dmes
366200031106     c                   clear                   S2Cast
366300031007     c                   clear                   S2Cpdc
366400031007     c                   clear                   S2Dpdc
366500031007     c                   clear                   S4Dpdc
366600031023     c                   clear                   S2Cdes
366700031023     c                   clear                   S2Kdft
366800040305     c                   clear                   S2Kbad
366900040305     c                   clear                   S2Cbad
367000031023     c                   clear                   S2Katd
367100031023     c                   clear                   S2Catd
367200031023     c                   clear                   S2Datd
367300090203      * - Memorizzazione primo rec. vuoto nel sfl
367400090203if  3c                   if        Free_Rec_Sfl = *zero
367500090203     c                   eval      Free_Rec_Sfl = S02nrr
367600090203e   3c                   endif
367700031007e   2c                   endif
367800031024      *
367900031024      * - reimposto indicatori x DSPATR condizionati
368000040702      *   se NON rilevati errori
368100040702if  2c                   if        NOT *in90
368200040702     c                             and $Default = *on
368300040702     c                             or  $Default = *off
368400060110     c                   exsr      AtrS02S04
368500040702e   2c                   endif
368600031007      *
368700031007      * - riattivo SFLNXTCHG (non solo se rilevato errore)
368800031007     c                   eval      *in32    = *on
368900031007      * - aggiorno il SFL
369000031007if  2c                   if        $Default = *on
369100031007     c                   update    MS03S02
369200031007x   2c                   else
369300031007     c                   update    MS03S04
369400031007e   2c                   endif
369500031007     c                   eval      *in32    = *off
369600031007      * - se rilevato errore esco dal ciclo di controllo
369700031007if  2c                   if        *in28    = *on
369800031106     c                   z-add     C02rcd        C02csr
369900090203     c                   leavesr
370000031007e   2c                   endif
370100031007      * - leggo il record successivo
370200031007if  2c                   if        $Default = *on
370300031007     c                   readc     MS03S02
370400031007x   2c                   else
370500031007     c                   readc     MS03S04
370600031007e   2c                   endif
370700031007      *
370800031007e   1c                   ENDDO
370900040312      *
371000040423      * Controlli finali "generali"
371100040312      *   (se NON ho già rilevato altri errori da segnalare)
371200161212      *
371300161212      /free
371400161212
371500161212         // -?Controllo inserimento Zona (cioè Turno) 99 - OBBLIGATORIA) -?
371600161212         //  ?per ogni Baia = Filiale?
371700161212         For  xx = 1  To  %elem(sk_Fil);
371800161212
371900161213           if  sk_Fil(xx) <= *zero;
372000161212             leave;
372100161212           endif;
372200161212
372300161212           If  sk_Rrn(xx) < *zero;
372400161212
372500161212             S02nrr = sk_Rrn(xx) * (-1);
372600161212             C02csr = S02nrr;
372700161212             if  $Video = '3';
372800161213               chain  S02nrr  MS03S02;
372900161212             else;
373000161213               chain  S02nrr  MS03S04;
373100161212             endif;
373200161212             *in58  = *on;
373300161212             *in28  = *on;
373400161212             *in90  = *on;
373500161213             V1Dmsg = %replace( sk_Fil(xx) : $Msg(58)
373600161212                              : %scan( '___' : $MSG(58) ) );
373700161212             if  %found(FIMS03D);
373800161213               *in32 = *on;
373900161212               if   $Video = '3';
374000161212                 update  MS03S02;
374100161212               else;
374200161212                 update  MS03S04;
374300161212               endif;
374400161212             endif;
374500161212             leavesr;
374600161212
374700161212           EndIf;
374800161212
374900161212         EndFor;
375000161212
375100161212      /end-free
375200090203      *
375300090203      * - Baia Non Letti obbligatoria
375400090203      *   (dovrebbe riscontrarsi solo nel piano di default)
375500090203if  1c                   if        %xfoot($BNLnr) = *zero
375600090203     c                   eval      S02nrr = Free_Rec_Sfl
375700090203     c                   eval      C02csr = S02nrr
375800090203if  2c                   if        $Video = '3'
375900090203     c     S02nrr        chain     MS03S02
376000090203x   2c                   else
376100090203     c     S02nrr        chain     MS03S04
376200090203e   2c                   endif
376300090203     c                   seton                                        572890
376400090203     c                   movel     $Msg(54)      V1Dmsg
376500090203if  2c                   if        %found(FIMS03D)
376600090203if  3c                   if        $Video = '3'
376700090203     c                   update    MS03S02
376800090203x   3c                   else
376900090203     c                   update    MS03S04
377000090203e   3c                   endif
377100090203e   2c                   endif
377200090203     c                   leavesr
377300090203e   1c                   endif
377400090203      *
377500090203      * - Controllo postazioni per baia
377600040312     c                   exsr      CtrBAIxPEP
377700031007      *
377800031007     c                   ENDSR
377900031007      *
378000031007      *---------------------------------------------------------------*
378100031007      * Controllo singolo record del subfile S02 & S04                *
378200031007      *---------------------------------------------------------------*
378300031007     c     CtrS02rec     BEGSR
378400031027      *
378500031027if  1c                   if        $Default = *off
378600031027     c                   clear                   S2Kdft
378700040304     c                   clear                   S2Kbad
378800040304     c                   clear                   S2Cbad
378900031027     c                   clear                   S2Katd
379000031027     c                   clear                   S2Catd
379100031027     c                   clear                   S2Datd
379200031027e   1c                   endif
379300040423      *
379400040423     c                   reset                   $IMA
379500040423     c                   reset                   $IMAdft
379600040607     c                   reset                   $PO
379700040611     c                   reset                   $POdpd
379800080129     c                   reset                   $POerr
379900040607     c                   reset                   $POdft
380000031007      *
380100031014      * Controllo baia di uscita
380200031007sel 1c                   select
380300031007w   1c                   when      S2Cpep = *zeros
380400031007     c                   seton                                        512890
380500031009     c                   movel     $Msg(24)      V1Dmsg
380600031007w   1c                   when      S2Cpep < 01  or  S2Cpep > 98
380700031007     c                   seton                                        512890
380800031009     c                   movel     $Msg(25)      V1Dmsg
380900031007x   1c                   other
381000031007     c                   exsr      ContaBAU
381100031007e   1c                   endsl
381200060110     c                   if        *in90
381300060110     c                   leavesr
381400060110     c                   endif
381500031007      *
381600031007      * Controllo descrizione
381700031014      * (non obbligatoria)
381800031007      *
381900031007      * Controllo flag baia non letti
382000031007if  1c                   if            S2Cbnl <> *blanks
382100031007     c                             and S2Cbnl <> 'S'
382200040304     c                   seton                                        572890
382300031009     c                   movel     $Msg(27)      V1Dmsg
382400031007x   1c                   else
382500040309if  2c                   if        S2Cbnl     =  'S'
382600031007     c                   exsr      ContaBNL
382700031007if  3c                   if        $BNLnr(xx) >  1
382800040304     c                   seton                                        572890
382900031009     c                   movel     $Msg(28)      V1Dmsg
383000031007e   3c                   endif
383100031007e   2c                   endif
383200031007e   1c                   endif
383300060110     c                   if        *in90
383400060110     c                   leavesr
383500060110     c                   endif
383600040305      *
383700040305      * Controllo codice postazione
383800040423      * - ricerca
383900040423     c     '?'           scan      S2Cbai                                 10
384000040423if  1c                   if        *in10  =  *on
384100040423     c                   eval      S2Cbai =  *blanks
384200040423     c                   eval      Wcampo =  'S2CBAI    '
384300040423     c                   exsr      srSEARCH
384400040423e   1c                   endif
384500040423      * - controllo
384600040423if  1c                   if        $Default = *off
384700040423     c                   eval      $Hidden  = *on
384800040423     c                   eval      Wpost    = H2Cbai
384900040423     c                   exsr      Ctrl_IMA
385000040423if  2c                   if        $IMAdft  = *on
385100040423     c                   eval      H2Dpdc   = $MPTdes(yy)
385200040423e   2c                   endif
385300040625     c                   exsr      Ctrl_PO
385400040625if  2c                   if        $POdft   = *on
385500040625     c                   eval      H2Dpdc   = $ORGdes(xy)
385600040625e   2c                   endif
385700040423e   1c                   endif
385800040423     c                   eval      $Hidden  = *off
385900040423     c                   eval      Wpost    = S2Cbai
386000040408     c                   exsr      Ctrl_IMA
386100040423if  1c                   if        $IMA     = *on
386200040423     c                   eval      S2Dpdc   = $MPTdes(yy)
386300040423     c                   eval      S4Dpdc   = $MPTdes(yy)
386400040423e   1c                   endif
386500040607     c                   exsr      Ctrl_PO
386600040607if  1c                   if        $PO      = *on
386700040611if  2c                   if        $POdpd   = *off
386800040611     c                   clear                   S2Cnfv
386900040611     c                   clear                   S2Dfpp
387000040611     c                   clear                   S2Ddfv
387100040611     c                   clear                   S4Cnfv
387200040611     c                   clear                   S4Dfpp
387300040611     c                   clear                   S4Ddfv
387400040611     c                   clear                   S2Cast
387500040611e   2c                   endif
387600061011     c                   clear                   S2Cpdc
387700040607     c                   eval      S2Dpdc   = $ORGdes(xy)
387800040607     c                   eval      S4Dpdc   = $ORGdes(xy)
387900040607e   1c                   endif
388000040423if  1c                   if            $Default = *off
388100040423     c                             and $IMAdft  = *on
388200040423     c                             and H2Cbai  <> S2Cbai
388300040423     c                   eval      S2Datd   = H2Dpdc
388400040423e   2c                   endif
388500080902if  1c                   if            $Default = *on
388600080902     c                             and S2Cbnl   = 'S'
388700080902     c                             and S2Cbai  <> *blanks
388800080902     c                   seton                                        532890
388900080902     c                   movel     $Msg(26)      V1Dmsg
389000080902     c                   leavesr
389100080902e   1c                   endif
389200040409if  1c                   if            $Default = *on
389300040409     c                             and S2Cbnl  <> 'S'
389400040409     c                             and S2Cbai   = *blanks
389500040409     c                   eval      S2Cbai   = C_PostazNull
389600040409e   1c                   endif
389700040305sel 1c                   select
389800040305w   1c                   when          $Default = *on
389900040305     c                             and S2Cbnl   = 'S'
390000040305     c                             and S2Cbai   = *blanks
390100040409w   1c                   when      S2Cbai   = C_PostazNull
390200040409if  2c                   if        $Default = *off
390300040305     c                   clear                   S2Cnfv
390400040305     c                   clear                   S2Ddfv
390500040305     c                   clear                   S2Dfpp
390600040305     c                   clear                   S4Cnfv
390700040305     c                   clear                   S4Ddfv
390800040305     c                   clear                   S4Dfpp
390900040305     c                   clear                   S4Dpdc
391000040409e   2c                   endif
391100161216     c                   clear                   S2Cznc
391200040409     c                   clear                   S2Cpdc
391300040305w   1c                   when          S2Cbai =  *blanks
391400040305     c                             and S2Cbnl <> 'S'
391500040305     c                   seton                                        532890
391600040305     c                   movel     $Msg(13)      V1Dmsg
391700040305w   1c                   when          S2Cbai <> *blanks
391800040305     c                             and S2Cbnl =  'S'
391900040305     c                   seton                                        532890
392000040305     c                   movel     $Msg(26)      V1Dmsg
392100040305w   1c                   when         (%subst(S2Cbai:1:1) >= 'A'
392200040305     c                             and %subst(S2Cbai:1:1) <= 'Z')
392300040305     c                             and %subst(S2Cbai:2:2) >= '01'
392400040305     c                             and %subst(S2Cbai:2:2) <= '99'
392500040305     c                   move      S2Cbai        w002a
392600040305     c     DigitN        check     w002a                                  10
392700040305if  2c                   if        *in10  =  *on
392800040305     c                   seton                                        532890
392900040305     c                   movel     $Msg(14)      V1Dmsg
393000040305x   2c                   else
393100040423     c                   eval      Wpost  =  S2Cbai
393200040305     c                   exsr      ContaBAI
393300040305if  3c                   if        $BAInr(xx) >  1
393400040305     c                   seton                                        532890
393500040305     c                   movel     $Msg(15)      V1Dmsg
393600040305e   3c                   endif
393700040305e   2c                   endif
393800040607w   1c                   when          S2Cbnl =  *blanks
393900040607     c                             and $IMA   =  *off
394000040607     c                             and $PO    =  *on
394100061010     c                             and $POdpd =  *on
394200040607if  3c                   if        $ORGnr(xy) >  1
394300040607     c                   seton                                        532890
394400040607     c                   movel     $Msg(33)      V1Dmsg
394500040607e   3c                   endif
394600040305w   1c                   when          S2Cbnl =  *blanks
394700040408     c                             and $IMA   =  *off
394800040607     c                             and $PO    =  *off
394900040305     c                             OR  S2Cbnl =  'S'
395000040305     c                             and S2Cbai <> *blanks
395100040305     c                   seton                                        532890
395200040305     c                   movel     $Msg(14)      V1Dmsg
395300040305e   1c                   endsl
395400060110     c                   if        *in90
395500060110     c                   leavesr
395600060110     c                   endif
395700040316      *
395800040316      * Controllo la postazione sia "compatibile" con le altre già
395900040316      *   immesse nella stessa baia
396000040316if  1c                   if        S2Cbai <> C_PostazNull
396100040409     c                             and
396200040409     c                             S2Cbai <> *blanks
396300040316     c                   eval      xx     =  1
396400040316     c                   eval      *in10  =  *off
396500040316     c     S2Cpep        lookup    $BAU(xx)                               10
396600040628sel 2c                   select
396700040628      * - Unica (o prima) postazione della baia in esame
396800040628w   2c                   when      NOT  *in10
396900040628      * - Postazione IMA/non-IMA o P.O./non-P.O. nella stessa baia
397000161110      *  ?Su richiesta di Nicola:?tolto controllo postaz. IMA/non-IMA
397100161110      *   (ora IMA e AUT possono stare nella stessa baia;
397200161110      *   sono le FILIALI che continuano a richiedere una loro baia)
397300161110      * - Postazione P.O./non-P.O. nella stessa baia
397400161110w   2c                   when           $BAUbai(xx) <> *blanks
397500161110     c                             and  $PosPO(xx)  <> $PO
397600040316     c                   seton                                        532890
397700161110     c*//                eval      V1Dmsg =  $Msg(40)
397800161110     c                   eval      V1Dmsg =  $Msg(57)
397900060110     c                   leavesr
398000040628      * - Postazione P.O. DPD non unico nella baia
398100040628w   2c                   when           $BAUbai(xx)    <> *blanks
398200040628     c                             and  $BAUbai(xx)    <> S2Cbai
398300040628     c                             and  $PosPOdpd(xx)  <> $POdpd
398400040628     c                   seton                                        532890
398500040628     c                   eval      V1Dmsg =  $Msg(44)
398600060110     c                   leavesr
398700040607w   2c***                when           *in10       =  *on
398800040607     c***                          and  $PosIMA(xx) =  $IMA
398900061011     c***                          and  $PosPO(xx)  =  $PO
399000040316      ***  Ok. Vediamo il prossimo...
399100040628e   2c                   endsl
399200040316e   1c                   endif
399300061011      *
399400061011      * Controllo l'eventuale utilizzo dei P.O. per baia
399500061011if  1c                   if             S2Cbnl  = *blanks
399600061011     c                             and  $IMA    = *off
399700061011     c                             and  $PO     = *on
399800061011     c                             and  $POdpd  = *off
399900061011      *
400000061011     c                   eval      WX     =  1
400100061011     c     S2Cbai        lookup    $POpos(wx)                             10
400200061011do  2c                   dow       *in10  =  *on
400300061011sel 3c                   select
400400061011w   3c                   when      S2Cpep =  $PObau(wx)
400500061011     c                   seton                                        532890
400600161212     c                   eval      V1Dmsg =  %trimR($Msg(33))
400700061012     c                                    +  ' nella stessa baia'
400800061011     c                   leavesr
400900061011w   3c                   when      wx     >= %elem($POpos)
401000061011     c                   leave
401100061011x   3c                   other
401200061011     c                   add       1             wx
401300061013     c     S2Cbai        lookup    $POpos(wx)                             10
401400061011e   3c                   endsl
401500061011e   2c                   enddo
401600061011      * Controllo progressivo per postazione = P.O. (FL3)
401700061011     c                   clear                   wFL3
401800061011do  2c                   do        S02nrr        wx
401900061012if  3c                   if        S2Cbai =  $POpos(wx)
402000061011     c                   add       1             wFL3
402100061012if  4c                   if        wFL3   >= 9
402200061011     c                   seton                                        532890
402300161212     c                   eval      V1Dmsg =  %trimR($Msg(33))
402400061012     c                                    +  ' in oltre 9 baie'
402500061011     c                   leavesr
402600061011e   4c                   endif
402700061011e   3c                   endif
402800061011e   2c                   enddo
402900061011e   1c                   endif
403000161212      *
403100161215      /free
403200161215
403300161215         // -?Controllo inserimento Zona (cioè Turno) solo se Baia = Filiale?
403400161215         //  ?(99 = generica)?
403500161215         Select;
403600161215           // - -?Non richiesta ma inserita?
403700161215           When  ( S2Cbnl <> *blank  or
403800161215                   $IMA    = *on     or
403900161215                   $PO     = *off )  and
404000161215                 S2Cznc   <> *blank;
404100161215             //*·clear  S2Cznc;
404200161215             *in58  =  *on;
404300161215             *in28  =  *on;
404400161215             *in90  =  *on;
404500161215             V1Dmsg =  $Msg(59);
404600161215             leavesr;
404700161215           // - -?Richiesta ma non inserita?
404800161215           When  S2Cbnl   =  *blank  and
404900161215                 $IMA     =  *off    and
405000161215                 $PO      =  *on     and
405100161215                 S2Cznc   =  *blank;
405200161215             //*·S2Cznc = 99;
405300161215             *in58  =  *on;
405400161215             *in28  =  *on;
405500161215             *in90  =  *on;
405600170117             //*·V1Dmsg =  %replace( sk_Fil(xx) : $Msg(60)
405700170117             V1Dmsg =  %replace( S2Cbai : $Msg(60)
405800161215                               : %scan( '___' : $Msg(60) ) );
405900161215             leavesr;
406000161215         EndSl;
406100161215         // - -?Numericità (se inserita)?
406200161215         If  S2Cznc <> *blank  and
406300161215             %check( DigitN : S2Cznc ) > *zero;
406400161215           *in58  =  *on;
406500161215           *in28  =  *on;
406600161215           *in90  =  *on;
406700161215           V1Dmsg =  $Msg(61);
406800161215           leavesr;
406900161215         EndIf;
407000161215
407100161215      /end-free
407200040316      *
407300040305      * ...e ne memorizzo gli utilizzi per baia
407400040409      *   (ammesse postazioni *blank per la "baia non letti")
407500040316if  1c                   if        S2Cbai <> C_PostazNull
407600040305     c                   exsr      MemBAIxPEP
407700040305e   1c                   endif
407800040312      *
407900040305      * Visualizzo la postazione di default se variata in piano
408000040305      *   giornaliero
408100161213if  1c                   if            $Default =  *off
408200161213     c                             and (S2Cbai  <> H2Cbai
408300161213     c                              or  S2Cznc  <> H2Cznc)
408400040423     c                   eval      S2Kdft  = C_Default
408500161213if  2c                   if        S2Cbai <> H2Cbai
408600040625     c                   eval      S2Kbad  = C_Postazione
408700040423     c                   eval      S2Cbad  = H2Cbai
408800161213if  3c                   if           $IMAdft = *on
408900040625     c                             or $POdft  = *on
409000040423     c                   eval      S2Datd  = H2Dpdc
409100161213e   3c                   endif
409200161213x   2c                   else
409300161213     c                   eval      S2Kbad  = 'Zona'
409400161215     c                   eval      S2Cbad  = H2Cznc
409500161213e   2c                   endif
409600040305e   1c                   endif
409700031007      *
409800031007      * Controllo codice autotrasportatore
409900031007      *   (in S03 e S04 è campo protetto)
410000031015      * Per il piano giornaliero farò appositi controlli nella subr.
410100031015      *   CTRS04REC
410200050614     c                   clear                   S2Dmes
410300050614     c                   clear                   S4Dmes
410400031007if  1c                   if        $Default = *on
410500040607if  2c                   if            $IMA = *off
410600040607     c                             and $PO  = *off
410700031007     c                   clear                   S2Dpdc
410800031009     c                   clear                   S4Dpdc
410900040423e   2c                   endif
411000031007      * - se *blanks lo considero *zeros (sarebbe numerico)
411100031007if  2c                   if        S2Cpdc =  *blanks
411200031007     c                   eval      S2Cpdc =  *zeros
411300031007e   2c                   endif
411400031007      * - ricerca
411500031007     c     '?'           scan      S2Cpdc                                 10
411600031007if  2c                   if        *in10  =  *on
411700031007     c                   eval      S2Cpdc =  *zeros
411800031007     c                   eval      Wcampo =  'S2CPDC    '
411900031007     c                   exsr      srSEARCH
412000031007e   2c                   endif
412100031007      * - controllo
412200031007sel 2c                   select
412300040305w   2c                   when          S2Cbnl =  *blanks
412400040409     c                             and ($IMA  =  *on
412500040607     c                              or $PO    =  *on
412600040409     c                              or S2Cbai =  C_PostazNull)
412700040305     c                             and S2Cpdc =  *zeros
412800031007w   2c                   when          S2Cbnl =  'S'
412900031007     c                             and S2Cpdc <> *zeros
413000031007     c                   seton                                        552890
413100031009     c                   movel     $Msg(29)      V1Dmsg
413200040408w   2c                   when          $IMA   =  *on
413300040305     c                             and S2Cpdc <> *zeros
413400040305     c                   seton                                        552890
413500040305     c                   movel     $Msg(16)      V1Dmsg
413600040607w   2c                   when          $PO    =  *on
413700040607     c                             and S2Cpdc <> *zeros
413800040607     c                   seton                                        552890
413900040607     c                   movel     $Msg(18)      V1Dmsg
414000040305w   2c                   when          S2Cbnl =  *blanks
414100040408     c                             and $IMA   =  *off
414200040607     c                             and $PO    =  *off
414300031021     c                   movel     V1Cpog        APDpdr
414400031021     c                   move      S2Cpdc        APDpdr
414500031014     c                   exsr      CtrlAutotr2
414600031007     c                   movel     APDrsc        S2Dpdc
414700040519if  3c                   if        NOT *in90
414800040519     c                   eval      Wpdr   =  APDpdr
414900040519     c                   exsr      CtrlAutotr4
415000040519e   3c                   endif
415100031007e   2c                   endsl
415200060110     c                   if        *in90
415300060110     c                   leavesr
415400060110     c                   endif
415500050629      * - imposto il flag per autotrasportoatore Messaggeria
415600050614if  3c                   if            $Default = *on
415700050614     c                             and §OGpkm   = 'S'
415800171012     c***************              and $VDL     = *on
415900050614     c                             and $IMA     = *off
416000050614     c                             and $PO      = *off
416100050614     c                             and S2Cbai  <> C_PostazNull
416200050614     c                             and S2Cbai  <> *blanks
416300050701     c                   eval      wNFVseq = 1
416400050629     c                   movel     APDflr        dAPDflr
416500050614     c                   exsr      X_Messaggeria
416600050614e   3c                   endif
416700031007      * - se *zeros lo reimposto a *blanks (numerico "editato")
416800031014if  2c                   if        S2Cpdc =  *zeros
416900031014     c                   eval      S2Cpdc =  *blanks
417000031007e   2c                   endif
417100031007e   1c                   endif
417200040305      *
417300040305      * In caso di piano giornaliero controllo se è stata tolta la
417400040305      *   postazione ("XXX"), perchè in tal caso non verrà eseguita
417500040305      *   la subr. per i controlli specifici, pertanto non verrebbe
417600040305      *   evidenziato l'autotrasportatore pre-impostato
417700061011if  1c                   if             $Default = *off
417800061011     c                             and (S2Cbai   = C_PostazNull
417900061011     c                             or   $PO      = *on)
418000040305     c                   clear                   S2Cpdc
418100061011if  2c                   if        $PO    =  *off
418200040305     c                   clear                   S4Dpdc
418300061011e   2c                   endif
418400040305     c                   exsr      DecS04pdr
418500040305e   1c                   endif
418600031007      *
418700060110     c                   ENDSR
418800031007      *
418900031007      *---------------------------------------------------------------*
419000031007      * Controlli aggiuntivi per singolo record del subfile S04       *
419100031007      *---------------------------------------------------------------*
419200031007     c     CtrS04rec     BEGSR
419300031023      *
419400031106     c                   reset                   SAVnfv
419500031106      *
419600031106      * Se impostato solo il 2° numero F.V. lo sposto al 1°
419700031106if  1c                   if            (S2Cnfv  =  *blanks
419800031106     c                             or   S2Cnfv  =  *zeros)
419900031106     c                             and (S4Cnfv  <> *blanks
420000031106     c                             and  S4Cnfv  <> *zeros)
420100031106     c                   eval      S2Cnfv  =  S4Cnfv
420200031106     c                   clear                   S4Cnfv
420300031106     c                   clear                   S4Ddfv
420400031106     c                   clear                   S4Dfpp
420500050628     c                   clear                   S4Dmes
420600031106e   1c                   endif
420700031106      *
420800031106      * Controllo 1° numero foglio viaggio
420900031007     c                   clear                   S2Ddfv
421000031027     c                   clear                   S2Dfpp
421100050628     c                   clear                   S2Dmes
421200031106     c                   eval      wNFVseq =  1
421300031106     c                   eval      Wnfv    =  S2Cnfv
421400031106     c                   exsr      Ctrl_1FV
421500031106     c                   eval      S2Cnfv  =  Wnfv
421600080129if  1c                   if            $PO     =  *on
421700080129     c                             and $POdpd  =  *on
421800080130     c                   eval      $ORGerr(xy) =  (Wnfv = *blanks   or
421900080130     c                                             Wnfv = *zeros)
422000080129e   1c                   endif
422100060110     c                   if        *in90
422200060110     c                   leavesr
422300060110     c                   endif
422400031106      * Se tutto bene: ne imposto i dati a video
422500031106if  1c                   if        Wnfv    <> *blanks
422600040621if  2c                   if        $PO     =  *off
422700031106     c                   eval      S2Dfpp  =  FVVfpp
422800031106     c                   move      DSdata6       S2Ddfv
422900040621x   2c                   else
423000040621     c***                clear                   S2Dfpp
423100040621     c                   move      DSdata6       S2Ddfv
423200040621e   2c                   endif
423300031106e   1c                   endif
423400031106      *
423500031106      * Imposto flag e decodifico variazioni relative all'autotrasp.
423600031106      *   (che deve essere comune ad entrambi i F.V.)
423700040610if  1c                   if            $IMA    =  *off
423800040610     c                             and $PO     =  *off
423900040610if  2c                   if        S2Cnfv  <> *blanks
424000031106     c                   move      APDpdr        S2Cpdc
424100031106     c                   movel     APDrsc        S4Dpdc
424200040610x   2c                   else
424300040305     c                   clear                   S2Cpdc
424400040305     c                   clear                   S4Dpdc
424500040423e   2c                   endif
424600031106e   1c                   endif
424700031106     c                   exsr      DecS04pdr
424800040615      *
424900040615      * Imposto flag relativo alla postazione = P.O. DPD con più F.V.
425000040615if  1c                   if        $POdpd   =  *on
425100040621     c*** decod. P.O.    clear                   S2Cpdc
425200040621     c*** decod. P.O.    clear                   S4Dpdc
425300040621     c                   eval      Wpost    = S2Cbai
425400040615     c                   exsr      AsterFVxPO
425500040615     c                   eval      S2Cast   = Wast
425600040615e   1c                   endif
425700031106      *
425800031106      * Controllo 2° numero foglio viaggio (se immesso)
425900031106     c                   clear                   S4Ddfv
426000031106     c                   clear                   S4Dfpp
426100050628     c                   clear                   S4Dmes
426200031106if  1c                   if            S4Cnfv <> *blanks
426300031106     c                             and S4Cnfv <> *zeros
426400031106     c                   eval      wNFVseq =  2
426500031106     c                   eval      Wnfv    =  S4Cnfv
426600031106     c                   exsr      Ctrl_1FV
426700031106     c                   eval      S4Cnfv  =  Wnfv
426800060110     c                   if        *in90
426900060110     c                   leavesr
427000060110     c                   endif
427100031106      * Se tutto bene: ne imposto i dati a video
427200031106if  2c                   if        Wnfv    <> *blanks
427300031106     c                   eval      S4Dfpp  =  FVVfpp
427400031106     c                   move      DSdata6       S4Ddfv
427500031106e   2c                   endif
427600040305      * e rieseguo il controllo sul numero di F.V. x autotrasportatore
427700040305      *   (se ne ho selezionato uno da ricerca)
427800040305if  2c                   if        S2Cast  =  '?'
427900040305     c                   clear                   S2Cast
428000040305     c                   exsr      CtrlAutotr3
428100040305     c                   eval      S2Cast =  Wast
428200040305e   2c                   endif
428300031106x   1c                   else
428400031106     c                   clear                   S4Cnfv
428500031106e   1c                   endif
428600031106      *
428700060110     c                   ENDSR
428800031024      *
428900031024      *---------------------------------------------------------------*
429000031024      * Impostazione campi descrittivi per l'autotrasportatore        *
429100031024      *---------------------------------------------------------------*
429200031024     c     DecS04pdr     BEGSR
429300031024      *
429400031024     c                   clear                   Wpdr
429500031024if  1c                   if        S2Cpdc >  *zeros
429600031024     c                   move      S2Cpdc        Wpdr
429700031024e   1c                   endif
429800031024      *
429900031024      * Visualizzazione "*" se più F.V. per il padroncino
430000031024     c                   clear                   S2Cast
430100031024if  1c                   if        S2Cpdc >  *zeros
430200031024     c                   exsr      CtrlAutotr3
430300031024     c                   eval      S2Cast =  Wast
430400031024e   1c                   endif
430500031024      *
430600031024      * Visualizzazione padroncino di default se variato
430700031024if  1c                   if        Wpdr   =  H2Cpdc
430800040625if  2c                   if            $IMA   =  *off
430900040625     c                             and $PO    =  *off
431000040625     c                             and S2Cbai <> C_PostazNull
431100031024     c                   movel     H2Dpdc        S4Dpdc
431200040423e   2c                   endif
431300031024x   1c                   else
431400040305if  2c                   if            S2Cbnl =  *blanks
431500040408     c                             and $IMA   =  *off
431600040625     c                             and $PO    =  *off
431700040316     c                             and S2Cbai <> C_PostazNull
431800040305if  3c                   if        S2Cpdc >  *zeros
431900031024     c                   move      APDpdr        S2Cpdc
432000031024     c                   movel     APDrsc        S4Dpdc
432100040305e   3c                   endif
432200031024e   2c                   endif
432300031024     c                   eval      S2Kdft =  C_Default
432400031024if  2c                   if        H2Cpdc <> *zeros
432500040315     c                   eval      S2Katd =  C_Autotr
432600031024     c                   move      H2Cpdc        S2Catd
432700031024e   2c                   endif
432800031024     c                   eval      S2Datd =  H2Dpdc
432900031024e   1c                   endif
433000031024      *
433100031024     c                   ENDSR
433200060110      *
433300060110      *---------------------------------------------------------------*
433400060110      * Impostazione settaggi attributi di visualizzazione x S01/S03  *
433500060110      *---------------------------------------------------------------*
433600060110     c     AtrS01S03     BEGSR
433700060110      *
433800060110      * Autotrasportatore Messaggeria
433900060110     c                   eval      *in04 = (§OGpkm = 'S')
434000060110      * Baia non letti
434100060110     c                   eval      *in80 = (S2Cbnl = 'S')
434200060110      * Padroncino e/o Postazione variati
434300060110     c                   clear                   Wpdr
434400060110if  1c                   if            S1Cpdc <> *blanks
434500060110     c                             and S1Cpdc <> *zeros
434600060110     c                   movel     V1Cpog        Wpdr
434700060110     c                   move      S1Cpdc        Wpdr
434800060110e   1c                   endif
434900060110     c                   eval      *in85 = (Wpdr   <> H1Cpdc   or
435000060110     c                                      S1Cbai <> H1Cbai)
435100060110      * Postazione = "IMx" o P.O.
435200060110     c                   eval      *in86 = ($IMA   =  *on      or
435300060110     c                                      $PO    =  *on)
435400060110      *
435500060110     c                   ENDSR
435600060110      *
435700060110      *---------------------------------------------------------------*
435800060110      * Impostazione settaggi attributi di visualizzazione x S02/S04  *
435900060110      *---------------------------------------------------------------*
436000060110     c     AtrS02S04     BEGSR
436100060110      *
436200060110      * Autotrasportatore Messaggeria
436300060110     c                   eval      *in04 = (§OGpkm = 'S')
436400060110      * Baia non letti
436500060110     c                   eval      *in80 = (S2Cbnl = 'S')
436600060110      * Padroncino e/o Postazione variati
436700060110     c                   clear                   Wpdr
436800060110if  1c                   if            S2Cpdc <> *blanks
436900060110     c                             and S2Cpdc <> *zeros
437000060110     c                   movel     V1Cpog        Wpdr
437100060110     c                   move      S2Cpdc        Wpdr
437200060110e   1c                   endif
437300060110     c                   eval      *in85 = (Wpdr   <> H2Cpdc   or
437400161213     c                                      S2Cbai <> H2Cbai   or
437500161213     c                                      S2Cznc <> H2Cznc)
437600060110      * Postazione = "IMx" o P.O.
437700060110     c                   eval      *in86 = ($IMA   =  *on      or
437800060110     c                                      $PO    =  *on)
437900060110      * Autotrasportatore Messaggeria
438000060110      * - 1° F.V. o Default Messaggeria
438100060110     c                   eval      *in87 = (S2Dmes <> *blanks)
438200060110      * - 2° F.V. Messaggeria
438300060110     c                   if        $Default    = *off
438400060110     c                   eval      *in88 = (S4Dmes <> *blanks)
438500060110     c                   endif
438600060110      *
438700060110     c                   ENDSR
438800031014      *
438900031014      *---------------------------------------------------------------*
439000031030      * Controllo codice autotrasportatore per singolo F.V. / baia    *
439100031014      *---------------------------------------------------------------*
439200031014     c     CtrlAutotr1   BEGSR
439300031014      *
439400031014      * Reperisco il padroncino memorizzato nel F.V. di default
439500031014     c                   clear                   DS_FV
439600031014     c                   eval      dsFVdfv    =  FVVdfv
439700031014     c                   eval      dsFVnfv    =  FVVnfv
439800031022     c                   move      DS_FV         w0150
439900031014     c                   eval      xx    = 1
440000031014     c                   eval      *in10 = *off
440100031111     c     w0150         lookup    $MEMfv1(xx)                            10
440200031111if  1c                   if        *in10 = *off
440300031111     c     w0150         lookup    $MEMfv2(xx)                            10
440400031111e   1c                   endif
440500031014if  1c                   if        *in10 = *on
440600031014     c                   eval      DSTpdr     =  $MEMat(xx)
440700031014x   1c                   else
440800031111      * - Se non reperito il F.V. tra quelli automaticamente proposti
440900031014      *   (per es. durante la variazione di un piano giornaliero)
441000031014      *   aggancio il file Fogli Distinte Consegne & memorizzo il F.V.
441100120613     c                   eval      DSTnpg     =  FVVnpg
441200120613     c                   eval      DSTnfv     =  FVVnfv
441300120613     c                   eval      DSTfgs     =  FVVfgs
441400120613     c     K03FIDST      chain     FIDST000
441500120613if  2c                   if        NOT %found(FIDST01L)
441600031022if  3c                   if        wNFVseq    =  2
441700031022     c                   movel     *on           *in56
441800031022x   3c                   else
441900031022     c                   movel     *on           *in54
442000031022e   3c                   endif
442100031022     c                   seton                                          2890
442200031014     c                   eval      V1Dmsg = $Msg(21)
442300060110     c                   leavesr
442400031014x   2c                   else
442500031111      * - - controllo se padroncino già memorizzato
442600031111      *     (se non lo è ancora  : lo memorizzo ora;
442700031111      *        se non ho più spazio: esco dal ciclo senza memorizzare)
442800031014     c                   eval      xx    = 1
442900031014     c                   eval      *in10 = *off
443000031111do  3c                   DOW       *in10 = *off
443100031111     c     DSTpdr        lookup    $MEMat(xx)                             10
443200031111if  4c                   if        *in10 = *off
443300031111     c     *zeros        lookup    $MEMat(xx)                             10
443400031111if  5c                   if        *in10 = *on
443500031111     c                   eval      $MEMat(xx)  =  DSTpdr
443600031111x   5c                   else
443700031111     c                   leave
443800031111e   5c                   endif
443900031111e   4c                   endif
444000031111      * - - per il padroncino cerco il primo elemento libero tra le
444100031111      *     schiere $MEMfv1 o $MEMfv2;
444200031111      *     se non lo trovo: torno sopra per ri-memorizzare il codice
444300031111      *       padroncino e questo nuovo f.v.
444400050701     c                   movel     FVVspn        dFVVspn
444500031111sel 4c                   select
444600031111w   4c                   when      *in10       = *on    and
444700031111     c                             $MEMfv1(xx) = *loval
444800031111     c                   eval      dsFVdfv     =  FVVdfv
444900031111     c                   eval      dsFVnfv     =  FVVnfv
445000031111     c                   move      DS_FV         w0150
445100031111     c                   eval      $MEMfv1(xx) =  w0150
445200031111     c                   eval      $MEMmp1(xx) =  FVVfpp
445300050701     c                   eval      $MEMpm1(xx) =  §FVVtpm
445400031111w   4c                   when      *in10       = *on    and
445500031111     c                             $MEMfv2(xx) = *loval
445600031111     c                   eval      dsFVdfv     =  FVVdfv
445700031111     c                   eval      dsFVnfv     =  FVVnfv
445800031111     c                   move      DS_FV         w0150
445900031111     c                   eval      $MEMfv2(xx) =  w0150
446000031111     c                   eval      $MEMmp2(xx) =  FVVfpp
446100050701     c                   eval      $MEMpm2(xx) =  §FVVtpm
446200031111x   4c                   other
446300031111if  5c                   if        xx    < %elem($MEMat)
446400031111     c                   eval      xx    = xx + 1
446500031111     c                   eval      *in10 = *off
446600031111x   5c                   else
446700031111     c                   leave
446800031111e   5c                   endif
446900031111e   4c                   endsl
447000031111e   3c                   enddo
447100031014e   2c                   endif
447200031014e   1c                   endif
447300031022      *
447400031106      * Se piano giornaliero (per P.O. con o senza VDL):
447500031022if  1c                   if        $Default   =  *off
447600040519      *?Solo per VdL (almeno per ora):?
447700040519      * Controllo che il codice autotrasportatore NON sia già impostato
447800040519      *   per altra baia/postazione
447900040519      *   (se variato - perchè, altrimenti, risulta GIA' controllato
448000040519      *   nella subr. "CtrS01rec")
448100040519if  2c                   if            $VDL    =  *on
448200040519     c                             and wNFVseq =  1
448300040519     c                   eval      Wpdr   =  DSTpdr
448400040519     c                   exsr      CtrlAutotr4
448500060110     c                   if        *in90
448600060110     c                   leavesr
448700060110     c                   endif
448800040519e   2c                   endif
448900040519      * Confronto autotrasportatori dei 2 F.V. x baia
449000031022sel 2c                   select
449100040519w   2c                   when      wNFVseq     =  1
449200040519      * - memorizzo l'autotrasportatore del 1° F.V. per confrontarlo
449300031022      *   con quello del 2° F.V. (per baia)
449400040519     c                   eval      SAVpdr      =  DSTpdr
449500040519w   2c                   when          wNFVseq =  2
449600040519     c                             and DSTpdr  <> SAVpdr
449700031022      * - l'autotrasportatore del 2° F.V. deve essere uguale a quello
449800031022      *   del 1° F.V. inserito (per baia)
449900031022     c                   seton                                        562890
450000031022     c                   eval      V1Dmsg = %replace(
450100031022     c                                      %editw(DSTpdr:'     0 '):
450200031022     c                                      $Msg(36):
450300031022     c                                      %scan('&PADR  ':$Msg(36)))
450400060110     c                   leavesr
450500031022e   2c                   endsl
450600031022e   1c                   endif
450700031014      *
450800060110     c                   ENDSR
450900031014      *
451000031014      *---------------------------------------------------------------*
451100031014      * Controllo codice autotrasportatore                            *
451200031014      *---------------------------------------------------------------*
451300031022     c     CtrlAutotr2   BEGSR
451400031014      *
451500031014     c     K02FIAPD      chain     FIAPD000
451600031014      *
451700031014if  1c                   if        NOT %found(FIAPD01L)
451800031014     c                             or  APDatb <> *blanks
451900031014     c                   clear                   APDrsc
452000031014     c                   seton                                        552890
452100031014     c                   movel     $Msg(17)      V1Dmsg
452200031014e   1c                   endif
452300031014      *
452400031014     c                   ENDSR
452500031024      *
452600031024      *---------------------------------------------------------------*
452700031024      * Evidenzia autotrasportatore con più F.V.                      *
452800031024      *---------------------------------------------------------------*
452900031024     c     CtrlAutotr3   BEGSR
453000031024      *
453100031024     c                   clear                   Wast
453200031024      * Default o giornaliero, evidenzio più F.V. per il padroncino
453300031024     c                   movel     V1Cpog        Wpdr
453400031024     c                   eval      xx     =  1
453500031024     c                   eval      *in10  =  *off
453600031024     c     Wpdr          lookup    $MEMat(xx)                             10
453700031114if  1c                   if            *in10      = *on
453800031114     c                             and $MEMxf(xx) > 1
453900031024     c                   eval      Wast   =  '*'
454000031024e   1c                   endif
454100031024      *
454200031024     c                   ENDSR
454300040519      *
454400040519      *---------------------------------------------------------------*
454500040519      * Controllo codice autotrasportatore per singola baia/postaz.   *
454600040519      *---------------------------------------------------------------*
454700040519     c     CtrlAutotr4   BEGSR
454800040519      *
454900040519     c                   clear                   ds_BaiaPost
455000040519if  1c                   if        $VDL   =  *off
455100040519     c                   movel     S1Cpep        dsBPbai
455200040519     c                   movel     S1Cbai        dsBPpos
455300040519x   1c                   else
455400040519     c                   movel     S2Cpep        dsBPbai
455500040519     c                   movel     S2Cbai        dsBPpos
455600040519e   1c                   endif
455700040519      *
455800040519     c                   eval      xx     = 1
455900040519     c                   eval      *in10  = *off
456000040519     c     Wpdr          lookup    $MEMat(xx)                             10
456100040519      *
456200040519sel 1c                   select
456300040519      * autotrasportatore non ancora memorizzato
456400040519w   1c                   when      NOT *in10
456500040519     c     *zeros        lookup    $MEMat(xx)                             10
456600040519if  2c                   if        *in10
456700040519     c                   eval      $MEMat(xx) =  Wpdr
456800040519     c                   eval      $MEMbp(xx) =  ds_BaiaPost
456900040519e   2c                   endif
457000040519      * 1° utilizzo dell'autotrasportatore: ne memorizzo baia/postaz.
457100040519w   1c                   when      $MEMbp(xx) =  *blanks
457200040519     c                   movel     ds_BaiaPost   $MEMbp(xx)
457300040519      * autotrasportatore già utilizzato in altre baie/postazioni
457400040519w   1c                   when      $MEMbp(xx) <> ds_BaiaPost
457500040519     c                   movel     $MEMbp(xx)    ds_BaiaPost
457600040519if  2c                   if        $Default   =  *on
457700040519     c                   eval      *in55  =  *on
457800040519x   2c                   else
457900040519     c                   eval      *in54  =  *on
458000040519e   2c                   endif
458100040519     c                   seton                                          2890
458200040519     c                   eval      V1Dmsg = %replace(
458300040519     c                                      %editw(Wpdr:'     0 '):
458400040519     c                                      $Msg(34):
458500040519     c                                      %scan('&PADR  ':$Msg(34)))
458600040519     c                   eval      V1Dmsg = %replace(dsBPpos:V1Dmsg:
458700040519     c                                      %scan('&PS':V1Dmsg))
458800040519     c                   eval      V1Dmsg = %replace(dsBPbai:V1Dmsg:
458900040519     c                                      %scan('&B        ':V1Dmsg))
459000060110     c                   leavesr
459100040519      * ... "beccata" baia/postazione in esame
459200040519w   1c*** ok:            when      $MEMbp(xx) =  ds_BaiaPost
459300040519e   1c                   endsl
459400040519      *
459500060110     c                   ENDSR
459600040615      *
459700040615      *---------------------------------------------------------------*
459800040615      * Evidenzia P.O. con più F.V.                                   *
459900040615      *---------------------------------------------------------------*
460000040615     c     AsterFVxPO    BEGSR
460100040615      *
460200040615     c                   clear                   Wast
460300040615      *
460400040615     c                   eval      xx     =  1
460500040615     c                   eval      *in10  =  *off
460600040615     c     Wpost         lookup    $FVxPOa(xx)                            10
460700040615      *
460800040615if  1c                   if        *in10  =  *on
460900040615      *
461000040615     c                   eval      xx     =  xx + 1
461100040615     c                   eval      *in10  =  *off
461200040615     c     Wpost         lookup    $FVxPOa(xx)                            10
461300040615      *
461400040615if  2c                   if        *in10  =  *on
461500040615     c                   eval      Wast   =  '*'
461600040615e   2c                   endif
461700040615      *
461800040615e   1c                   endif
461900040615      *
462000040615     c                   ENDSR
462100031021      *
462200031021      *---------------------------------------------------------------*
462300040408      * Memorizzo (l'ultima) postazione immessa per ogni codice baia  *
462400031021      *---------------------------------------------------------------*
462500031021     c     MemBAIxPEP    BEGSR
462600031021      *
462700031021     c                   eval      xx     = 1
462800031021     c                   eval      *in10  = *off
462900040305      *
463000040305if  1c                   if        $VDL   = *off
463100040305      *
463200040305     c     S1Cpep        lookup    $PEP(xx)                               10
463300040305      * ...lo trovo sicuro, perchè li ho già memorizzati nelle subr.
463400040305      *   "ContaPEP"
463500040305if  2c                   if            *in10       =  *on
463600040409     c                             and $PEPbai(xx) =  *blanks
463700040305     c                   eval      $PEPbai(xx) = S1Cbai
463800040305e   2c                   endif
463900040305      *
464000040305x   1c                   else
464100040305      *
464200040305     c     S2Cpep        lookup    $BAU(xx)                               10
464300040305      * ...lo trovo sicuro, perchè li ho già memorizzati nelle subr.
464400040305      *   "ContaBAU"
464500040305if  2c                   if            *in10       =  *on
464600040409     c                             and $BAUbai(xx) =  *blanks
464700040309if  3c                   if        S2Cbnl      = 'S'
464800040309     c                   eval      $BAUbai(xx) = S2Cbnl
464900040309x   3c                   else
465000040305     c                   eval      $BAUbai(xx) = S2Cbai
465100040309e   3c                   endif
465200040628     c                   eval      $PosPO(xx)      =  $PO
465300040628     c                   eval      $PosPOdpd(xx)   =  $POdpd
465400040305e   2c                   endif
465500040305      *
465600040305e   1c                   endif
465700040409      *
465800040409      * Memorizzo le postazioni "IMA" immesse per controllare la
465900040409      *  effettiva immissione di quelle obbligatorie
466000161115      *?N.B.?-?Ora le postazioni IMA possono essere nella stessa?
466100161115      *       ?baia con altre postazioni NON IMA.?
466200040409if  1c                   if            *in10       =  *on
466300161115     c                             and $IMA        =  *on
466400040409     c                   eval      xx     = 1
466500040409     c                   eval      *in10  = *off
466600040409if  2c                   if        $VDL   = *off
466700040409     c     S1Cbai        lookup    $MPT(xx)                               10
466800040409x   2c                   else
466900040409     c     S2Cbai        lookup    $MPT(xx)                               10
467000040409e   2c                   endif
467100040409      * - la trova sicuramente !
467200040409if  2c                   if        *in10  = *on
467300040409     c                   add       1             $MPTnr(xx)
467400040409e   2c                   endif
467500040409e   1c                   endif
467600061011      *
467700061011      * Aggiorno anche le postazioni utilizzate per baia
467800061011      *   (per ora utile solo al controllo dell'utilizzo dei P.O.)
467900061011if  1c                   if        $VDL   = *on
468000061011     c                   eval      $PObau(S02nrr)  =  S2Cpep
468100061011     c                   eval      $POpos(S02nrr)  =  S2Cbai
468200061011e   1c                   endif
468300161212      *
468400161212      /free
468500161212
468600161212         // -?Memorizzazione SflRrn per Postazione = Filiale per il?
468700161212         //  ?controllo della Zona (cioè del Turno)?
468800161212         If    $VDL   =  *on    and
468900161212             ( $PO    =  *on  or
469000161212               $POdpd =  *on );
469100161212
469200161212           clear  xx;
469300161212           xx = %lookup ( S2Cbai : sk_Fil );
469400161212
469500161212           if  xx = *zero;
469600161213             xx = %lookup( *blank : sk_Fil );
469700161212             if  xx > *zero;
469800161212               sk_Fil(xx) = S2Cbai;
469900161212             endif;
470000161212           endif;
470100161212
470200161212           if  xx > *zero;
470300161213             select;
470400161213               // -?Memorizzazione zona generica (99)?
470500161215               when  S2Cznc  = '99';
470600161213                 sk_Rrn(xx)  = S02nrr;
470700161213               // -?Memorizzazione zona specifica SE non già?
470800161213               //  ?memorizzata una zona generica (99)?
470900161213               when  sk_Rrn(xx) <= *zero;
471000161213                 sk_Rrn(xx)  = S02nrr * (-1);
471100161213             endsl;
471200161212           endif;
471300161212
471400161212         EndIf;
471500161212
471600161212      /end-free
471700031021      *
471800031021     c                   ENDSR
471900031021      *
472000031021      *---------------------------------------------------------------*
472100040312      * Controllo delle postazioni immesse (per baia)                 *
472200031021      *---------------------------------------------------------------*
472300031021     c     CtrBAIxPEP    BEGSR
472400031021      *
472500040408      * Controllo se immessa ALMENO una postazione "IMA"
472600040408      *                   ed ALMENO una postazione "IM0"
472700040408     c                   clear                   xx
472800040408do  1c                   dou       xx   >= %elem($MPTobbl)
472900040408     c                   eval      xx    = xx + 1
473000040409if  2c                   if            $MPTobbl(xx) = *on
473100040409     c                             and $MPTnr(xx)   = *zeros
473200040408     c                   seton                                          2890
473300040519     c                   eval      V1Dmsg  =  %replace($MPT(xx):
473400040409     c                                        $Msg(31):
473500040409     c                                        %scan('IMA':$Msg(31)))
473600060110     c                   leavesr
473700040408e   2c                   endif
473800040408e   1c                   enddo
473900040312      *
474000040309      * Controllo se immessa almeno una postazione per ogni baia
474100040409      *   (ora SOLO per NO VdL)
474200060110if  1c                   if        $VDL   =  *on
474300060110     c                   leavesr
474400060110e   1c                   endif
474500060110      *
474600031021do  1c                   DO        C_MaxRecSf    xx
474700031021      *
474800040305if  2c                   if            $VDL     =  *off
474900040305     c                             and $PEP(xx) =  *blanks
475000040305     c                             OR  $VDL     =  *on
475100040305     c                             and $BAU(xx) =  *zeros
475200031021     c                   leave
475300031021e   2c                   endif
475400031021      *
475500040305if  2c                   if            $VDL        =  *off
475600040305     c                             and $PEPbai(xx) =  *blanks
475700040305     c                             OR  $VDL        =  *on
475800040305     c                             and $BAUbai(xx) =  *blanks
475900031021     c                   seton                                          2890
476000040315if  2c                   if        $VDL   =  *off
476100040315     c                   eval      C01csr = $CsrRRN(xx)
476200040519     c                   eval      V1Dmsg = %replace($PEP(xx):
476300031021     c                                      $Msg(32):
476400031021     c                                      %scan('&PEP':$Msg(32)))
476500040315x   2c                   else
476600040315     c                   eval      C02csr = $CsrRRN(xx)
476700040315     c                   eval      V1Dmsg = %replace(
476800040315     c                                      %editw($BAU(xx):'0  ')+' ':
476900040315     c                                      $Msg(32):
477000040315     c                                      %scan('&PEP':$Msg(32)))
477100040315e   2c                   endif
477200031021     c                   leave
477300040305e   2c                   endif
477400031021      *
477500031021e   1c                   ENDDO
477600031021      *
477700060110     c                   ENDSR
477800031007      *
477900031007      *---------------------------------------------------------------*
478000040628      * Controllo immissione codice baia                              *
478100031007      *---------------------------------------------------------------*
478200031007     c     ContaPEP      BegSr
478300031007      *
478400031007     c                   eval      xx       =  1
478500031007     c                   eval      *in10    =  *off
478600031007      * Controllo se già immesso
478700031010     c     S1Cpep        lookup    $PEP(xx)                               10
478800031007      * Se NON già immesso: memorizzo
478900031007if  1c                   if        *in10    =  *off
479000031007     c     *blanks       lookup    $PEP(xx)                               10
479100031007if  2c                   if        *in10
479200031007     c                   eval      $PEP(xx) =  S1Cpep
479300031007x   2c                   else
479400031007     c                   eval      xx       =  %elem($PEP)
479500031007     c                   eval      $PEP(xx) =  *hival
479600031007e   2c                   endif
479700040315     c                   eval      $CsrRRN(xx) = S01nrr
479800031007e   1c                   endif
479900031007      *
480000031007     c                   ENDSR
480100031007      *
480200031007      *---------------------------------------------------------------*
480300031007      * Controllo immissione codice baia                              *
480400031007      *---------------------------------------------------------------*
480500031007     c     ContaBAI      BegSr
480600031007      *
480700031007     c                   eval      xx       =  1
480800031007     c                   eval      *in10    =  *off
480900031007      * Controllo se già immesso
481000040423     c     Wpost         lookup    $BAI(xx)                               10
481100031007      * Se NON già immesso: memorizzo
481200031007if  1c                   if        *in10    =  *off
481300031007     c     *blanks       lookup    $BAI(xx)                               10
481400031007if  2c                   if        *in10
481500040423     c                   eval      $BAI(xx) =  Wpost
481600040628x   2c                   else
481700040628     c                   eval      xx       =  %elem($BAI)
481800040628     c                   eval      $BAI(xx) =  *hival
481900031007e   2c                   endif
482000031007e   1c                   endif
482100031007      * Aggiorno conteggio x codice
482200031007     c                   add       1             $BAInr(xx)
482300050629      *
482400050629      *?Se picking su VdL x Messaggeria:?
482500050629      * consento l'ulizzo della stessa postazione più volte,
482600050629      * ma solo nella stessa baia
482700050629if  1c                   if             §OGpkm      =  'S'
482800050629     c                             and  $VDL        =  *on
482900050629     c                             and  $IMA        =  *off
483000050629     c                             and  $PO         =  *off
483100050629      *
483200050629if  2c                   if        $BAIbau(xx) =  *zeros
483300050629     c                   eval      $BAIbau(xx) =  S2Cpep
483400050629e   2c                   endif
483500050629      * - impostazione messaggio di avvertimento
483600050629if  2c                   if             $BAInr(xx)  >  1
483700050629     c                             and  $BAIbau(xx) =  S2Cpep
483800050629     c                   eval      $BAInr(xx)  =  1
483900050629if  3c                   if        $MsgBAIbau  =  *off
484000050629     c                   seton                                        532890
484100050629     c                   eval      V1Dmsg      =  $Msg(49)
484200050629     c                   eval      $MsgBAIbau  =  *on
484300050629     c***                leavesr
484400050629e   3c                   endif
484500050629e   2c                   endif
484600050629      *
484700050629e   1c                   endif
484800031007      *
484900031007     c                   ENDSR
485000031007      *
485100031007      *---------------------------------------------------------------*
485200031007      * Controllo immissione codice baia di uscita                    *
485300031007      *---------------------------------------------------------------*
485400031007     c     ContaBAU      BegSr
485500031007      *
485600031007     c                   eval      xx       =  1
485700031007     c                   eval      *in10    =  *off
485800031007      * Controllo se già immesso
485900031007     c     S2Cpep        lookup    $BAU(xx)                               10
486000031007      * Se NON già immesso: memorizzo
486100031007if  1c                   if        *in10    =  *off
486200031007     c     *zeros        lookup    $BAU(xx)                               10
486300031007if  2c                   if        *in10
486400031007     c                   eval      $BAU(xx) =  S2Cpep
486500040628x   2c                   else
486600040628     c                   eval      xx       =  %elem($BAU)
486700040628     c                   eval      $BAU(xx) =  *hival
486800031007e   2c                   endif
486900040315     c                   eval      $CsrRRN(xx) = S02nrr
487000031007e   1c                   endif
487100031007      *
487200031007     c                   ENDSR
487300031030      *
487400031030      *---------------------------------------------------------------*
487500031030      * Controllo immissione numero foglio viaggio                    *
487600031030      *---------------------------------------------------------------*
487700031030     c     ContaNFV      BegSr
487800031030      *
487900031030     c                   eval      xx       =  1
488000031030     c                   eval      *in10    =  *off
488100031030      * Controllo se già immesso
488200031030     c                   clear                   Wnfv7
488300031030     c                   move      Wnfv          Wnfv7
488400031030     c     Wnfv7         lookup    $NFV(xx)                               10
488500031030      * Se NON già immesso: memorizzo
488600031030if  1c                   if        *in10    =  *off
488700031030     c     *zeros        lookup    $NFV(xx)                               10
488800031030if  2c                   if        *in10
488900031030     c                   eval      $NFV(xx) =  Wnfv7
489000031030x   2c                   else
489100031030     c                   eval      xx       =  %elem($NFV)
489200031030     c                   eval      $NFV(xx) =  *hival
489300031030e   2c                   endif
489400031030      * Se NON già immesso: memorizzo postazione/baia
489500040519     c                   clear                   ds_BaiaPost
489600031030if  2c                   if        $VDL     =  *off
489700040519     c                   move      S1Cpep        dsBPbai
489800040519     c                   movel     S1Cbai        dsBPpos
489900031030x   2c                   else
490000040519     c                   movel     S2Cpep        dsBPbai
490100040519     c                   movel     S2Cbai        dsBPpos
490200031030e   2c                   endif
490300040628     c                   movel     ds_BaiaPost   $NFVbp(xx)
490400040628      * Se già immesso: ne reperisco baia/postazione precedente
490500040628x   1c                   else
490600040628     c                   movel     $NFVbp(xx)    ds_BaiaPost
490700031030e   1c                   endif
490800031030      * Aggiorno conteggio x codice
490900031030     c                   add       1             $NFVnr(xx)
491000031030      *
491100031030     c                   ENDSR
491200031007      *
491300031007      *---------------------------------------------------------------*
491400031007      * Controllo immissione flag baia non letti                      *
491500031007      *---------------------------------------------------------------*
491600031007     c     ContaBNL      BegSr
491700031007      *
491800031007     c                   eval      xx       =  1
491900031007     c                   eval      *in10    =  *off
492000031007      * Controllo se già immesso
492100031007     c     S2Cbnl        lookup    $BNL(xx)                               10
492200031007      * Se NON già immesso: memorizzo
492300031007if  1c                   if        *in10    =  *off
492400031007     c     *blanks       lookup    $BNL(xx)                               10
492500031007if  2c                   if        *in10
492600031007     c                   eval      $BNL(xx) =  S2Cbnl
492700031007x   2c                   else
492800031007     c                   eval      xx       =  %elem($BNL)
492900031007     c                   eval      $BNL(xx) =  *hival
493000031007e   2c                   endif
493100031007e   1c                   endif
493200031007      * Aggiorno conteggio x codice
493300031007     c                   add       1             $BNLnr(xx)
493400031007      *
493500031007     c                   ENDSR
493600031007      *
493700031007      *---------------------------------------------------------------*
493800031007      * Ricerche                                                      *
493900031007      *---------------------------------------------------------------*
494000031007     c     srSEARCH      BEGSR
494100031007      *
494200031007sel 1c                   select
494300040423      *
494400040423      * Postazione (tab. MPT)
494500040423w   1c                   when          Wcampo   = 'S1CBAI    '
494600040423     c                             or  Wcampo   = 'S2CBAI    '
494700040423     c                   clear                   TIBS02ds
494800040423     c                   eval      T02mod = 'R'
494900040423     c                   eval      T02cod = 'MPT'
495000040423     c                   movel     KNSIF         T02sif
495100040423     c                   call      'TIBS02R'
495200040423     c                   parm                    KPJBA
495300040423     c                   parm                    TIBS02ds
495400040423if  1c                   if        T02err = *blanks
495500040423if  2c                   if        Wcampo = 'S1CBAI    '
495600040423     c                   movel     T02ke1        S1Cbai
495700040423x   2c                   else
495800040423     c                   movel     T02ke1        S2Cbai
495900040423e   2c                   endif
496000040423     c                   movel     T02uni        dMFP
496100040423e   1c                   endif
496200031007      *
496300031007      * Numero foglio viaggio
496400031013w   1c                   when          Wcampo   = 'V1CNFV    '
496500031013     c                             or (Wcampo   = 'S1CNFV    '
496600031022     c                              or Wcampo   = 'S2CNFV    '
496700031106     c                              or Wcampo   = 'S3CNFV    '
496800031106     c                              or Wcampo   = 'S4CNFV    ')
496900031007     c                             and $Default = *off
497000040611     c                             and $PO      = *off
497100031111     c                             or  *inKG    = *on
497200031015     c                   reset                   DSLR01
497300031013sel 2c                   select
497400031013w   2c                   when      Wcampo = 'V1CNFV    '
497500031013     c                   eval      DLRnpg = V1Cnpg                              Categoria foglio
497600040408w   2c                   when         (Wcampo = 'S1CNFV    '
497700040408     c                              or Wcampo = 'S3CNFV    '
497800040408     c                              or Wcampo = 'S2CNFV    '
497900040408     c                              or Wcampo = 'S4CNFV    ')
498000040408     c                             and $IMA   = *on
498100031013     c                   eval      DLRnpg = 3                                   Categoria foglio
498200031013e   2c                   endsl
498300031111if  2c                   if        Wcampo <> 'V1CNFV    '
498400031111     c                   z-add     V1Ddfv        DLRgda                         Dal gg/mm/aaaa
498500031111     c                   z-add     SAVdfva       DLRada                         Dal aaaa/mm/gg
498600031111e   2c                   endif
498700031007     c                   z-add     V1Cpog        DLRfgs                         P.O. gestione
498800031007     c                   movel     SDSpgm        DLRpgm                         Nome programma
498900031015     c                   movel(p)  DSLR01        KPJBU
499000031111if  2c                   if        *inKG  = *off
499100031007     c                   call      'FNLR02R'
499200031007     c                   parm                    KPJBA
499300031111x   2c                   else
499400031111     c                   call      'FNLR02R'
499500031111     c                   parm                    KPJBA
499600031111     c                   parm                    $FV
499700031111e   2c                   endif
499800031015     c                   movel     KPJBU         DSLR01
499900031013sel 2c                   select
500000031013w   2c                   when      Wcampo = 'V1CNFV    '
500100031013     c                   move      DLRnfv        V1Cnfv
500200031013w   2c                   when      Wcampo = 'S1CNFV    '
500300031007     c                   move      DLRnfv        S1Cnfv
500400031106     c                   move      DLRnfv        Wnfv
500500031106w   2c                   when      Wcampo = 'S2CNFV    '
500600031106     c                   move      DLRnfv        S2Cnfv
500700031106     c                   move      DLRnfv        Wnfv
500800031022w   2c                   when      Wcampo = 'S3CNFV    '
500900031022     c                   move      DLRnfv        S3Cnfv
501000031022     c                   move      DLRnfv        Wnfv
501100031106w   2c                   when      Wcampo = 'S4CNFV    '
501200031106     c                   move      DLRnfv        S4Cnfv
501300031106     c                   move      DLRnfv        Wnfv
501400031013e   2c                   endsl
501500040611      *
501600040611      * Numero foglio viaggio x P.O. DPD
501700040630      * (Con p.o. 195 si interrogano ANCHE f.v. per P.O. 330)
501800080118      * (...Anche con fil. 196 o 197)
501900040611w   1c                   when         (Wcampo   = 'S2CNFV    '
502000040611     c                              or Wcampo   = 'S4CNFV    ')
502100040611     c                             and $Default = *off
502200040611     c                             and $POdpd   = *on
502300130506     c                   reset                   fnlst3ds
502400040611     c                   eval      DLSdti =  V1Ddfv
502500040621     c                   eval      DLSdtf =  31129999
502600130506     c                   movel(p)  fnlst3ds      KPJBU
502700040611     c                   move      S2Cbai        DLSpo
502800040630     c                   clear                   DLSpo2
502900070521if  2c                   if            S2Cbai   = '195'
503000080118     c                             or  S2Cbai   = '196'
503100070521     c                             or  S2Cbai   = '197'
503200040630     c                   eval      DLSpo2 =  330
503300040630e   2c                   endif
503400130506     c                   call      'FNLST3R'
503500040611     c                   parm                    KPJBA
503600040611     c                   parm                    DLSpo
503700040630     c                   parm                    DLSpo2
503800130506     c                   movel     KPJBU         fnlst3ds
503900040611sel 2c                   select
504000040611      * - non selezionato alcun f.v.
504100040611w   2c                   when      DLSnfv = *zeros
504200040611      * - selezionato il 1° f.v.
504300040621w   2c                   when      Wcampo = 'S2CNFV    '
504400040611     c                   move      DLSnfv        S2Cnfv
504500040611     c                   move      DLSnfv        Wnfv
504600040611      * - selezionato il 2° f.v. (sarebbe proibito!!!)
504700040611w   2c                   when      Wcampo = 'S4CNFV    '
504800040611     c                   move      DLSnfv        S4Cnfv
504900040611     c                   move      DLSnfv        Wnfv
505000040611e   2c                   endsl
505100031007      *
505200031007      * Autotrasportatore
505300031007w   1c                   when          Wcampo   = 'S1CPDC    '
505400031007     c                             or  Wcampo   = 'S2CPDC    '
505500031007     c                   reset                   FNLV24ds
505600031007     c                   z-add     V1Cpog        D24fil
505700031007     c                   movel(p)  FNLV24ds      KPJBU
505800031007     c                   call      'FNLV24R'
505900031007     c                   parm                    KPJBA
506000031007     c                   movel     KPJBU         FNLV24ds
506100031007if  2c                   if        D24pdr <> *zeros
506200031007sel 3c                   select
506300031007w   3c                   when          Wcampo   = 'S1CPDC    '
506400031007     c                             and $Default = *on
506500031007     c                   move      D24pdr        S1Cpdc
506600031007     c                   movel     D24rsc        S1Dpdc
506700031007w   3c                   when          Wcampo   = 'S2CPDC    '
506800031007     c                             and $Default = *on
506900031007     c                   move      D24pdr        S2Cpdc
507000031007     c                   movel     D24rsc        S2Dpdc
507100031007w   3c                   when          Wcampo   = 'S1CPDC    '
507200031007     c                             and $Default = *off
507300031007     c                   move      D24pdr        S1Cpdc
507400031007     c                   movel     D24rsc        S3Dpdc
507500031007w   3c                   when          Wcampo   = 'S2CPDC    '
507600031007     c                             and $Default = *off
507700031007     c                   move      D24pdr        S2Cpdc
507800031007     c                   movel     D24rsc        S4Dpdc
507900031007e   3c                   endsl
508000031007e   2c                   endif
508100031007      *
508200031007e   1c                   endsl
508300031007      *
508400031007     c                   ENDSR
508500031112      *
508600031112      *---------------------------------------------------------------*
508700031112      * Controllo esistenza di distinte aperte non inserite nel piano *
508800031112      *---------------------------------------------------------------*
508900031112     c     CtrlDistAp    BEGSR
509000031112      *
509100031112      * Memorizzo il tasto funzionale F7=Salvataggio e Stampa
509200031112     c                   movel     *inKH         WinKH
509300031112      *
509400031112      * Richiamo pgm. FNLR02R per fare il controllo
509500031112     c                   reset                   DSLR01
509600031112     c                   movel     '3'           DLRtfv
509700031112     c                   z-add     V1Ddfv        DLRgda                         Dal gg/mm/aaaa
509800031112     c                   z-add     SAVdfva       DLRada                         Dal aaaa/mm/gg
509900031112     c                   z-add     V1Cpog        DLRfgs                         P.O. gestione
510000031112     c                   movel     SDSpgm        DLRpgm                         Nome programma
510100031112     c                   movel(p)  DSLR01        KPJBU
510200031112     c                   call      'FNLR02R'
510300031112     c                   parm                    KPJBA
510400031112     c                   parm                    $FV
510500031112     c                   movel     KPJBU         DSLR01
510600031112      *
510700031112      * Emetto (eventualmente) segnalazione a video
510800031112if  1c                   if        DLRfs1 = *on
510900031112     c                   clear                   MS03W01
511000031112     c                   movel     $Msg(41)      W1Dms2
511100031112     c                   eval      W1Dtfa = 'F6=Conferma   '
511200031112     c                                    + 'F12=Ritorno'
511300031112do  2c                   dou       *inKF   or   *inKL
511400031112     c                   exfmt     MS03W01
511500031112e   2c                   enddo
511600031112if  2c                   if        *inKL
511700031112     c                   movel     *off          WinKH
511800031112e   2c                   endif
511900031112e   1c                   endif
512000031112      *
512100031112     c                   ENDSR
512200080129      *
512300080129      *---------------------------------------------------------------*
512400080129      * Controllo mancaato inserimento di F.V. per filiali DPD        *
512500080129      *---------------------------------------------------------------*
512600080129     c     CtrlFVdpd     BEGSR
512700080129      *
512800080129      * Memorizzo il tasto funzionale F7=Salvataggio e Stampa
512900080129     c                   movel     *inKH         WinKH
513000080129      *
513100080129      * Conteggio delle segnalazioni di filiali DPD senza F.V.
513200080129     c                   clear                   wx
513300080129     c                   clear                   wy
513400080129do  1c                   DOW       wx < %elem($ORGerr)
513500080129     c                   add       1             wx
513600080129if  2c                   if        $ORGerr(wx) =  *on
513700080129     c                   eval      wy = wy + 1
513800080129e   2c                   endif
513900080129e   1c                   ENDDO
514000080129if  1c                   if        wy = *zeros
514100080129     c                   leavesr
514200080129e   1c                   endif
514300080129      *
514400080129      * Avviso del totale di tali segnalazioni già effettuate
514500080129     c                   clear                   MS03W01
514600080129     c                   movel     $Msg(51)      W1Dms1
514700080129     c                   eval      W1Dms1  =  %replace(
514800080129     c                                          %editc(wy:'Z'):
514900080129     c                                          $Msg(51):
515000080129     c                                          %scan( '&FV':$Msg(51) ) )
515100080129     c                   eval      W1Dms2  = $Msg(52)
515200080129     c                   eval      W1Dms3  = $Msg(53)
515300080129     c                   eval      W1Dtfa  = 'Enter=Conferma'
515400080129     c                                     + '   '
515500080129     c                                     + 'F12=Ritorno'
515600080129do  1c                   dou           $Tasto = C_Enter
515700080129     c                             or  $Tasto = C_F12
515800080129     c                   exfmt     MS03W01
515900080129e   1c                   enddo
516000080129      *
516100080129sel 1c                   select
516200080129w   1c                   when      $Tasto  = C_F12
516300080129     c                   eval      WinKH   = *off
516400080129w   1c                   when      WinKH   = *off
516500080129     c                   eval      *inKF   = *on
516600080211x   1c                   other
516700080211     c                   eval      *inKH   = WinKH
516800080129e   1c                   endsl
516900080129      *
517000080129     c                   ENDSR
517100031007      *
517200031007      *---------------------------------------------------------------*
517300031007      * Aggiornamento dati nel file FLTR900F                          *
517400031007      *---------------------------------------------------------------*
517500031007     c     Upd_FLTR9     BEGSR
517600031007      *
517700031007      * Cancellazione dei record esistenti
517800031007     c                   exsr      Dlt_FLTR9
517900031007      *
518000031007      * Aggiunta dei record impostati a video nel sfl
518100031107     c                   reset                   SAVpep
518200031107     c                   reset                   SAVbau
518300031107     c                   reset                   SAVpdr
518400031007     c                   clear                   S01nrr
518500031007     c                   clear                   S02nrr
518600031107      *
518700031009do  1c                   DO        MAXnrr
518800031007      *
518900031007sel 2c                   select
519000031007w   2c                   when      $Video = '2'
519100031007     c                   add       1             S01nrr
519200031007     c     S01nrr        chain     MS03S01
519300031007w   2c                   when      $Video = '3'
519400031007     c                   add       1             S02nrr
519500031007     c     S02nrr        chain     MS03S02
519600031007w   2c                   when      $Video = '4'
519700031007     c                   add       1             S01nrr
519800031007     c     S01nrr        chain     MS03S03
519900031007w   2c                   when      $Video = '5'
520000031007     c                   add       1             S02nrr
520100031007     c     S02nrr        chain     MS03S04
520200031007e   2c                   endsl
520300031007      *
520400031007if  2c                   if        NOT %found(FIMS03D)
520500031007     c                   leave
520600031007e   2c                   endif
520700031007      *
520800040409if  2c                   if             $VDL     = *off
520900040409     c                             and (S1Cpep   = *blanks
521000040409     c                             and  S1Cdes   = *blanks
521100040409     c                             and  S1Cbai   = *blanks
521200040409     c                             and (S1Cnfv   = *blanks
521300040409     c                              or  S1Cnfv   = *zeros)
521400040409     c                             and (S1Cpdc   = *blanks
521500040409     c                              or  S1Cpdc   = *zeros))
521600040409     c                             OR   $VDL     = *on
521700040409     c                             and (S2Cpep   = *zeros
521800040409     c                             and  S2Cdes   = *blanks
521900040409     c                             and  S2Cbnl   = *blanks
522000040409     c                             and  S2Cbai   = *blanks
522100161215     c                             and  S2Cznc   = *blanks
522200040409     c                             and (S2Cnfv   = *blanks
522300040409     c                              or  S2Cnfv   = *zeros)
522400040409     c                             and (S2Cpdc   = *blanks
522500040409     c                              or  S2Cpdc   = *zeros))
522600031007     c                   iter
522700031007e   2c                   endif
522800031007      *
522900031007     c                   exsr      Wrt_FLTR9
523000031007      *
523100031007e   1c                   ENDDO
523200031007      *
523300031007     c                   ENDSR
523400031007      *
523500031007      *---------------------------------------------------------------*
523600031007      * Delete da FLTR900F dei dati esistenti                         *
523700031007      *---------------------------------------------------------------*
523800031007     c     Dlt_FLTR9     BEGSR
523900031007      *
524000031007     c                   eval      TR9fgs   = V1Cpog
524100040415if  1c                   if        V1Cnfv   = *zeros
524200040415     c                   clear                   TR9npga
524300040415     c                   clear                   TR9nfva
524400040415     c                   else
524500040415     c                   move      V1Cnpg        TR9npga
524600031013     c                   move      V1Cnfv        TR9nfva
524700040415e   1c                   endif
524800031007     c     K03FLTR9      setll     FLTR9000
524900031007      *
525000031023     c                   dou       NOT %found(FLTR901L)
525100031007     c     K03FLTR9      delete    FLTR9000
525200031007     c                   enddo
525300031007      *
525400031007     c                   ENDSR
525500031007      *
525600031007      *---------------------------------------------------------------*
525700031007      * Write  in FLTR900F del singolo record impostato nel subfile   *
525800031007      *---------------------------------------------------------------*
525900031007     c     Wrt_FLTR9     BEGSR
526000040305      *
526100060113      * Calcolo del progressivo per baia in caso di immissione
526200060113      *   postazione in default
526300060113      *   (nel piano giornaliero NON è possibile inserire altre baie o
526400060113      *   postazioni...)
526500040305sel 1c                   select
526600060111      *
526700060113w   1c                   when           $Default = *on
526800060113     c                             and  $VDL     = *off
526900060113     c                             and  S1Cpep  <> SAVpep
527000060111     c                   eval      *in10    = *off
527100060111     c                   eval      xx       = 1
527200060111     c     S1Cpep        lookup    $PEP(xx)                               10
527300060111if  2c                   if        NOT *in10
527400060111     c     *blanks       lookup    $PEP(xx)                               10
527500060111if  3c                   if        *in10
527600060113     c                   eval      $PEP(xx) =  S1Cpep
527700060111e   3c                   endif
527800060111e   2c                   endif
527900060113     c                   eval      SAVpep   = S1Cpep
528000060111      *
528100060113w   1c                   when           $Default = *on
528200060113     c                             and  $VDL     = *on
528300060113     c                             and  S2Cpep  <> SAVbau
528400060111     c                   eval      *in10    = *off
528500060111     c                   eval      xx       = 1
528600060111     c     S2Cpep        lookup    $BAU(xx)                               10
528700060111if  2c                   if        NOT *in10
528800060111     c     *zeros        lookup    $BAU(xx)                               10
528900060111if  3c                   if        *in10
529000060111     c                   eval      $BAU(xx) = S2Cpep
529100060111e   3c                   endif
529200060111e   2c                   endif
529300040305     c                   eval      SAVbau   = S2Cpep
529400060111      *
529500040305e   1c                   endsl
529600060111      *
529700040621      * incremento progressivo per baia (anche per "XXX")
529800060113sel 1c                   select
529900060113      * - Baia "XXX" in piano giornaliero
530000060113if  1c                   when           $VDL     = *off
530100040621     c                             and  $Default = *off
530200040621     c                             and  S1Cbai   = C_PostazNull
530300040621     c                             OR   $VDL     = *on
530400040621     c                             and  $Default = *off
530500040621     c                             and  S2Cbai   = C_PostazNull
530600060113     c                   add       2             SAVprg
530700040621     c                   leavesr
530800060113      * - Piano giornaliero VDL
530900060113w   1c                   when           $VDL     = *on
531000060113     c                             and  $Default = *off
531100060113     c                   eval      SAVprg   = H2Cprg
531200161110      * - Piano giornaliero NON VDL
531300060113w   1c                   when           $VDL     = *off
531400060113     c                             and  $Default = *off
531500060113     c                   eval      SAVprg   = H1Cprg
531600060113      * - Piano di default
531700060113w   1c                   when           $Default = *on
531800060113     c                   add       1             SAVprg
531900060113e   1c                   endsl
532000061011      *
532100061011      * Incremento progressivo per postazione = P.O.
532200061011     c                   clear                   wFL3
532300061011if  1c                   if        $VDL     = *on
532400061011do  2c                   do        S02nrr        wx
532500061011if  3c                   if        S2Cbai   = $POpos(wx)
532600061011     c                   add       1             wFL3
532700061011e   3c                   endif
532800061011e   2c                   enddo
532900061011      * (almeno il record in esame è già stato conteggiato
533000061011      *  SE NON piando di default)
533100061011if  2c                   if        wFL3     = *zeros
533200061011     c                   eval      wFL3     = 1
533300061011e   2c                   endif
533400061011e   1c                   endif
533500031007      *
533600031007     c                   clear                   FLTR9000
533700031007      *
533800031007     c                   eval      TR9fgs   = V1Cpog
533900040408if  1c                   if        V1Cnfv   > *zeros
534000031007     c                   eval      TR9npga  = V1Cnpg
534100031013     c                   move      V1Cnfv        TR9nfva
534200040415e   1c                   endif
534300040305     c                   move      SAVprg        TR9prg
534400031007      *
534500031007      * campi x NO VDL
534600031007if  1c                   if        $VDL     = *off
534700031007      *
534800031010     c                   move      S1Cpep        TR9pep
534900031007     c                   eval      TR9des   = S1Cdes
535000031030     c*** (solo VDL)     eval      TR9bnl   = *blanks
535100031007     c                   eval      TR9bai   = S1Cbai
535200031013if  2c                   if            S1Cpdc <> *blanks
535300031013     c                             and S1Cpdc <> *zeros
535400031021     c                   movel     V1Cpog        TR9pdc
535500031007     c                   move      S1Cpdc        TR9pdc
535600031010e   2c                   endif
535700031007if  2c                   if        $Default = *off
535800040423     c                   eval      $Hidden  = *off
535900040423     c                   eval      Wpost    = S1Cbai
536000040408     c                   exsr      Ctrl_IMA
536100040408if  3c                   if        $IMA     = *on
536200031007     c                   eval      TR9npg   = 3
536300031007x   3c                   else
536400031007     c                   eval      TR9npg   = 4
536500031007e   3c                   endif
536600031007     c                   move      S1Cnfv        TR9nfv
536700031007     c                   clear                   WLBda8
536800031007     c                   eval      G08dat   = S1Ddfv
536900031007     c                   eval      G08err   = '0'
537000031007     c                   call      'XSRDA8'
537100031007     c                   parm                    WLBda8
537200031021     c                   eval      TR9dfv   = G08inv
537300031022     c                   eval      TR9fl1   = S1Dfpp
537400031007e   2c                   endif
537500031007      *
537600031007      * campi x SÌ VDL
537700031007x   1c                   else
537800031007      *
537900031007     c                   movel(p)  S2Cpep        TR9pep
538000031007     c                   eval      TR9des   = S2Cdes
538100170214if  2c*//?NON impostato:?if        $PO      = *off         ?($PO non è ancora impostato)?
538200170214if  2c                   if        S2Cznc  <> *blank
538300161215     c                   eval      %subst( TR9des : 29 ) = S2Cznc
538400161213e   2c                   endif
538500031007     c                   eval      TR9bnl   = S2Cbnl
538600040305     c                   eval      TR9bai   = S2Cbai
538700031013if  2c                   if            S2Cpdc <> *blanks
538800031013     c                             and S2Cpdc <> *zeros
538900031021     c                   movel     V1Cpog        TR9pdc
539000031007     c                   move      S2Cpdc        TR9pdc
539100031010e   2c                   endif
539200031007if  2c                   if        $Default = *off
539300031007if  3c                   if        S2Cnfv   > *zeros
539400040423     c                   eval      $Hidden  = *off
539500040423     c                   eval      Wpost    = S2Cbai
539600040408     c                   exsr      Ctrl_IMA
539700040615     c                   exsr      Ctrl_PO
539800040615sel 4c                   select
539900161213w   4c                   when      $PO      = *on
540000040615     c                   eval      TR9npg   = 1
540100040615w   4c                   when      $IMA     = *on
540200040309     c                   eval      TR9npg   = 3
540300040615x   4c                   other
540400040309     c                   eval      TR9npg   = 4
540500040615e   4c                   endsl
540600040309e   3c                   endif
540700031007     c                   move      S2Cnfv        TR9nfv
540800031007     c                   clear                   WLBda8
540900031007     c                   eval      G08dat   = S2Ddfv
541000031007     c                   eval      G08err   = '0'
541100031007     c                   call      'XSRDA8'
541200031007     c                   parm                    WLBda8
541300031021     c                   eval      TR9dfv   = G08inv
541400031029     c                   eval      TR9fl1   = S2Dfpp
541500031007e   2c                   endif
541600061011     c                   move      wFL3          TR9fl3
541700031007      *
541800031007e   1c                   endif
541900040702      *
542000040702      * campi "comuni"
542100040702if  1c                   if        V1Cdpd   = 'S'
542200040702     c                   move      'D'           TR9fl2
542300040702e   1c                   endif
542400040702      *                  __________________
542500031007     c                   WRITE     FLTR9000
542600040702      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
542700031022      *
542800031022      * EVENTUALE 2° record per 2° F.V.
542900031022      *
543000040621if  1c                   if           $Default = *off
543100040621     c                   add       1             SAVprg
543200040621e   1c                   endif
543300040415if  1c                   if           $Default = *off
543400040415     c                             and $VDL    = *off
543500031022     c                             and S3Cnfv <> *blanks
543600031022     c                             and S3Cnfv <> *zeros
543700040415     c                             OR $Default = *off
543800040415     c                             and $VDL    = *on
543900031106     c                             and S4Cnfv <> *blanks
544000031106     c                             and S4Cnfv <> *zeros
544100031022      *
544200040621     c*** già sopra:     add       1             SAVprg
544300040305     c                   eval      TR9prg   = SAVprg
544400031106     c                   clear                   WLBda8
544500031106     c                   eval      G08err   = '0'
544600031106if  2c                   if        $VDL     = *off
544700031022     c                   move      S3Cnfv        TR9nfv
544800031022     c                   eval      G08dat   = S3Ddfv
544900031022     c                   call      'XSRDA8'
545000031022     c                   parm                    WLBda8
545100031022     c                   eval      TR9dfv   = G08inv
545200031022     c                   eval      TR9fl1   = S3Dfpp
545300031106x   2c                   else
545400031106     c                   move      S4Cnfv        TR9nfv
545500031106     c                   eval      G08dat   = S4Ddfv
545600031106     c                   call      'XSRDA8'
545700031106     c                   parm                    WLBda8
545800031106     c                   eval      TR9dfv   = G08inv
545900031106     c                   eval      TR9fl1   = S4Dfpp
546000031106e   2c                   endif
546100040702      *                  __________________
546200031022     c                   WRITE     FLTR9000
546300040702      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
546400031022e   1c                   endif
546500031007      *
546600031007     c                   ENDSR
546700031007
546800031007      *---------------------------------------------------------------*
546900031007      * SCHIERE A TEMPO DI COMPILAZIONE                               *
547000031007      *---------------------------------------------------------------*
547100151106** -- $MSG ------------------------------------------------------------------*
547200070530Immettere la filiale gestione                                                     1
547300070530Filiale gestione errata                                                           2
547400070530Filiale non abilitata al picking diretto                                          3
547500031007Categoria foglio obbligatoria                                                     4
547600031007Categoria foglio errata                                                           5
547700070530Filiale gestione non abilitata all'utilizzo della categoria foglio immessa        6
547800040312Immettere il numero Foglio Viaggio                                                7
547900031010Immettere un numero foglio arrivi esistente e non annullato                       8
548000031009             Mancano ANCHE i dati di default...                                   9
548100031211Immettere il codice baia                                                         10
548200031211Codice baia errato                                                               11
548300070530Filiale non abilitata al piano DPD                                               12
548400031211Immettere il codice postazione                                                   13
548500031211Codice postazione errato                                                         14
548600031211Codice postazione già inserito                                                   15
548700040629Impossibile specificare codice AUT per codice postazione "IMA"                   16
548800031009Codice autotrasportatore errato                                                  17
548900070530Impossibile specificare codice AUT per codice postazione "Filiale"               18
549000040315         CANCELLARE il piano giornaliero in esame ?                              19
549100031009Numero Foglio Viaggio errato                                                     20
549200040702Immettere un numero Foglio Viaggio valido                                        21
549300040408F.V. IMA diverso da quello già impostato in questo piano (&FIMA)                 22
549400070530Immettere un numero F.V. valido e con filiale arrivo &PS                         23
549500031009Immettere la baia di uscita                                                      24
549600031009Baia di uscita errata (deve essere compresa tra 01 e 98)                         25
549700040305Impossibile specificare codice postazione per "baia non letti"                   26
549800031009Flag "Baia Non Letti" errato: 'S' = sì / ' ' = no                                27
549900031009Flag "Baia Non Letti" già specificato per un'altra baia di uscita                28
550000031009Impossibile specificare codice autotrasportatore per "baia non letti"            29
550100031009Immettere il numero foglio IMA                                                   30
550200031211Immettere almeno una postazione "IMA"                                            31
550300031211Immettere almeno una postazione valida per la baia &PEP                          32
550400070530Filiale già specificata come postazione                                          33
550500040519Autotrasportatore &PADR   già impegnato in postazione &PS di baia &B             34
550600031022Immettere un 2° F.V. diverso dal 1° F.V.                                         35
5507000310222° F.V. assegnato ad autotrasportatore (&PADR  ) diverso da quello del 1° F.V.   36
550800040629Immettere SOLO un F.V. per postazione "IMA"                                      37
550900031022F.V. di data antecedente a quella del F.V. arrivi                                38
551000040312Foglio Viaggio già inserito per la postazione &PS                                39
551100161110Immesse postazioni IMA e/o per autotrasportatori e/o filiali nella stessa baia   40 - LIBERO
551200031112ATTENZIONE: esistono distinte aperte non inserite nel piano.                     41
551300070530Immettere SOLO un F.V. per postazione "Filiale DPD"                              42
551400070530Foglio Viaggio con filiale di arrivo &PS                                         43
551500070530La postazione "Filiale DPD"  deve essere univoca per baia di uscita              44
551600060110?         Esiste un piano smistamento per picking.          |                   ?45
551700060110   Premere F6 per cancellarlo e proseguire con piano DPD,   |                    46
551800060110   altrimenti premere F12.                                  |                    47
551900070530     Mancano postazioni "Filiale DPD" nel piano di default.                      48
552000050629Esistono postazioni impostate più volte nella stessa baia - ENTER per forzare    49
552100080130Filiale DPD senza F.V. - ENTER per forzare                                       50
552200080129?           Lasciate &FV filiali DPD senza F.V.             |                   ?51
552300080130   Premere Invio per confermare; altrimenti premere F12.    |                    52
552400080130                                                            |                    53
552500090203Immettere una "Baia Non Letti" (obbligatoria)                                    54
552600151109Esiste già il Piano Smistamento Picking _         per il giorno __/__/____       55
552700151106Rilevato errore nell'estrazione dei dati del F.V. arrivi: avvisare CED           56
552800161110La stessa baia può contenere SOLO postaz. IMA/per autotrasp. o SOLO "Filiali"    57
552900161212Per la linea di defluenza ___ è obbligatoria la zona 99 in almeno una baia       58
553000161214La ZONA è inseribile solo per linee di defluenza                                 59
553100161214Immettere una Zona per la linea di defluenza ___                                 60
553200161215Zona errata                                                                      61
