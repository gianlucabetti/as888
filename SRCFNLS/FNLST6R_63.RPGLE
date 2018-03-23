000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200130523      * FNLST6R *----------------------------------------------------*
000300130524      *         - Chiusura   da   I M P
000400020801      *--------------------------------------------------------------*
000500911010     FAZORG01L  IF   E           K DISK
000600130627     Ftntbe01l  IF   E           K DISK
000700130527     Fcnaco00f  IF   E           K DISK
000800130208     FFNFVV01L  iF   E           K DISK
000900960129     FFNFV201L  IF   E           K DISK
001000130424     FFNFV401L  IF   E           K DISK
001100020801     FFNFGV07L  IF   E           K DISK
001200020801     F                                     RENAME(FNFGV000:FNFGV007)
001300950107     FFNFGV01L  UF   E           K DISK
001400950107     F                                     RENAME(FNFGV000:FNFGV001)
001500000615     FFNFGW01L  IF   E           K DISK
001600130523     FFNLSt6D   CF   E             WORKSTN
001700130523     F                                     SFILE(LSt6DSF:NRR)
001800020801      *
001900020801      * S c h i e r e
002000020801      *
002100130524     D FS              S              8    DIM(299) inz
002200080228     D L1              S              3  0 DIM(30)
002300130524     D L6              S              3  0 DIM(30)
002400130527     D MSG             S             44    DIM(18) CTDATA PERRCD(1)
002500020801      *
002600020801      * D S   definite   i n t e r n a m e n t e
002700020801      *
002800020801      * Passaggio a Calcolo Volumi CML              -FNLSB7R-
002900960209     D PARAM1          DS
003000961008     D  PA1NUM                17     17  0 INZ
003100960209     D  PA1FEL               158    160  0
003200020801      *
003300130524      * Passaggio a Chiusura FV                     -FNLSU2R-
003400130524     D fnlsu2ds        DS
003500020801      * - Categoria foglio I.M.P.
003600940415     D  PA2NPG                 1      1  0
003700020801      * - Numero    foglio I.M.P.
003800950125     D  PA2NFV                 2      4P 0
003900950125     D  PA2DFV                 5      9P 0
004000020801      * - Flag di creazione Spunte da Organigramma DESCR 148
004100960209     D  PA2AGS                10     10
004200000703     D  VI2NFV                11     15
004300020801      * - Indicherà il numero di volte che è stata sottomessa di
004400020801      *   seguito la chiusura del FV: all'inizio è 0
004500931001     D  PA2NUM                17     17  0 INZ
004600020801      * - PA2FLG = "M" ---> richiamato dalla gestione F.Viaggio
004700960129     D  PA2FLG                19     19    INZ
004800080228     D  L1ela                 78    137P 0 INZ
004900911104     D                                     DIM(30)
005000130524     D  Pa2RSUT              138    157
005100930127     D  PA2FEL               158    160  0
005200950125     D  PA2FGS               164    166  0
005300170613     D* pa2CML non più usato
005400970423     D  PA2CML               167    167
005500170613     D* pa2AGP non più usato
005600010910     D  PA2AGP               168    168
005700040121     D  PA2spv               169    169
005800040121     D  PA2spp               170    170
005900130527     D  PA2ccm               171    177
006000130531     D  PA2barcode           178    178    inz
006100020801      *
006200130502     D fnlst4ds        DS
006300130502     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
006400130502     D*          B - DA BORDERO'
006500130502     D*          A - apertura foglio
006600130502     D*          C - DA CHIUSURA FV SINGOLA
006700130502     D*          I - DA CHIUSURA DA IMP
006800130502     D*          N - DA INTERROGAZIONE  FV
006900130502     D*          M - Manutenzione FGV
007000130502     D*          P - Manutenzione FGV con videata piombi
007100130502
007200130502     D* CAMPI INPUT OBBLIGATORI:
007300130502     D*  GES / FGS
007400130502     d
007500130502     D* CAMPI INPUT PER MANUTENZIONE :
007600130502     D*  NFV
007700130502     D*
007800130502     D* CAMPI INPUT SOLO PER APERTURA :
007900130502     d* DFV /OLD / TRN / NRRP / PDR
008000130502     D
008100130502     D  ilst4DFV               3     10
008200130502     D
008300130502     D  ilst4nfv              11     15
008400130502     D  IlsT4GES              16     16
008500130502     D* Se MSG pieno --> c'è stato ERRORE
008600130502     D  olst4msg              17     94
008700130502     d* se iolst4FOR è pieno --> premuto F6 per forzare foglio di traini doppio
008800130502     D  iolst4FOR             95     97
008900130502     D
009000130502     D  IlsT4OLD              98     98
009100130502     D* OLST4F12 = 'S'    --> premuto F12
009200130502     D  olst4F12             102    102
009300130502     d
009400130502     D  ilst4fgs             103    105
009500130502     D  ilst4TRN             106    112  0
009600130502     D  ilst4NRRP            113    125
009700130502     D* se non immessa il numero relativo record della proposta
009800130502     d*  si può immettere a modo vecchio il trazionista
009900130502     D  ilst4PDR             126    132
010000020801      *
010100070423     D PARAMa3         DS
010200070423     D  PAANFV                 1      5
010300070423     D* pa3tfp deve contenere il terminal di partenza per poter elaborare e
010400070423     d* trasmettere le bolle per terminal di partenza
010500070423     d* se non passato un valore > '000' il programma elabora e trasmette
010600070423     d* TUTTE le bolle presenti nel file fnblp46f (come faceva prima)
010700070423     D  PA3tfp                 6      8
010800111123     D  PA3num                 9      9    inz('0')
010900111123     d
011000130523      * Parametri per Interrogazione FV Partenze - FNLSt3ds-
011100130523     D fnlst3ds        DS
011200951108     D  PA7FLG                 1      1    INZ('R')
011300951108     D  PA7FL2                 2      2    INZ('F')
011400951108     D  PA7NFV                 3      5P 0 INZ
011500951108     D  PA7DIN                 6     10P 0 INZ
011600951108     D  PA7DFI                11     15P 0
011700951108     D  PA7TFV                16     16    INZ('2')
011800000615     D  PA7FGS               242    244  0
011900020801      * - Schiera usata per passare al pgm chiamante tutti i
012000020801      *   Fogli Viaggio selezionati
012100951108     D  FVA                   17    241P 0 INZ
012200951108     D                                     DIM(45)
012300020801      *
012400020801     D wPOFV           DS             8    inz
012500020801     D  wLNP                   1      3  0 inz
012600020801     D  wNFV                   4      8  0 inz
012700020801      *
012800020801      * Parametri per Controllo Data & Trasform in giorni - XSRDA8 -
012900950107     D WLBDAT          DS                  INZ
013000950107     D  G02DAT                 1      8  0
013100950107     D  G02INV                 9     16  0
013200950107     D  G02ERR                17     17
013300950107     D  G02TGI                18     22  0
013400020801      *
013500020801      * D S   definite   e s t e r n a m e n t e
013600020801      *
013700900517     D KPJBA         E DS
013800020801      * DS per Interrogazione Fogli Vari
013900950109     D DSLR01        E DS                  INZ
014000020801      * DS per TRUL06R - Caricamento £X
014100020801     D TRUL06DS      E DS
014200950107     D  LIN                    1     90  0
014300940221     D                                     DIM(30)
014400000615     D OG143         E DS
014500130524     D OG148         E DS
014600130524     D OG150         E DS
014700130502     D tibs02ds      E DS
014800130527     D dcli          E DS
014900130705     d
015000130705     D TRUL13DS      E DS
015100020801      *
015200020801      * V a r i a b i l i
015300020801      *
015400020801     D KPJBU$          s                   like(KPJBU)  inz
015500020801      * Campi chiave fissi
015600130524     d codut           s              1  0 inz(1)
015700130527     d kksc            s                   like(acoksc)
015800020801     D KEPA            s                   like(FV2EPA) inz('P')
015900020801     D KLAI            s                   like(FV2LAI) inz
016000130424     D Ktrc            s                   like(FV4trc) inz
016100020801     D KTDH            s                   like(FV2TDH) inz('T')
016200020801     D PARNPG          s                   like(FVVNPG) inz(3)
016300020801     D COMNFV          s                   like(FVVNFV) inz
016400020801     D CO2NFV          s                   like(FGVNFV) inz
016500130523     D KIMP            s                   like(FGVNFV) inz
016600130627     d kcod            s                   like(tbecod)
016700130524     d pardut          s             30
016800130524     d parrag          s             48
016900130524     d parkcc          s              4  0
017000130524     d parsta          s              1  0
017100130524     d parflr          s             90
017200130524     d pardit          s              3
017300130524     d parnum          s              2  0
017400130524     d parkcm          s             80
017500130524     d parksm          s            140
017600130524     d parkdm          s             60
017700130524     d paresci         s              1
017800130524     d parerr          s              2
017900130524     d pariva          s             16
018000130524     d parcdf          s             16
018100020801      * Indici di schiera
018200020801     D XB              s              3  0 inz
018300020801     D XI              s              4  0 inz
018400020801      * Data e Ora
018500020801     D UTime           s              6  0 inz
018600020801     D Ora             s              2  0 inz
018700020801     D W0140           s             14  0 inz
018800020801     D UDate8          s              8  0 inz
018900020801      *
019000020801     D POWORK          s              3  0 inz
019100020801      *
019200020801     D NRR             s              5  0 inz
019300130524     D unicoNRR        s              5  0 inz
019400020801     D SAVNRR          s              5  0 inz
019500020801     D SAVNFC          s              3  0 inz
019600020801     D RISUL           s              5  0 inz
019700020801     D RESTO           s              3  0 inz
019800020801     D NUM5            s              5  0 inz
019900130424     D wpmb            s             48    inz
020000130705     d Dataiso         s               d   datfmt(*iso)
020100130705     d Datadmy         s               d   datfmt(*dmy)
020200130705
020300130524     d TIBS34ds      e ds
020400130524     d dDatiUte      e ds
020500130524     d AZUTEds       e ds                  extname(AZUTE00F)
020600130705
020700130705     D fnlsu3ds        DS
020800130705     D  ilsu3flg               1      1
020900130705     D  ilsu3aas               2      5
021000130705     D  ilsu3lnp               6      8
021100130705     D  ilsu3nrs               9     10
021200130705     D  ilsu3nsp              11     17
021300130705     D  ilsu3DSP_B            18     25
021400130705     D  ilsu3DSP_S            26     33
021500130705     D  ilsu3nos              34     34
021600130705     d
021700130705     d  ilsu3tcr              35     35
021800130705     d  ilsu3dcr              36     43
021900130705     d  ilsu3hcr              44     47
022000130705     D  olsu3err             100    101
022100130705     d
022200130524
022300130527     d c_Digits        c                   const('0123456789')
022400130613
022500130613      //---------------------------------------------------------------
022600130613      /copy gaitrasrc/srcprotopr,tibs02r
022700020801
022800020801      *---------------------------------------------------------------*
022900020801      * INDICATORI
023000020801      *---------------------------------------------------------------*
023100020801      *  01    - GESTISCO POSIZIONAMENTO CURSORE
023200020801      *  02    - NON EMETTO IL CAMPO DELL'ORA PER RECORD NUOVI
023300040121      *  03    - SIMFEL utilizza volume e/o peso in spunta
023400020801      *  04    - FOGLIO GIA' ABBINATO
023500020801      *  10    - ROLL UP
023600020801      *  15    - FOGLIO GIA' CHIUSO
023700130527      *  16    - F9 in alta intensità
023800130627      *  25    - Quando emetto SFL non emetto richiesta dei fogli
023900130627      *  26    - esiste cliente abilitato alla borderizzazione
024000020801      *  30/34 - DI COMODO
024100020801      *  43/51 - ERRORI
024200020801      *  67    - SCAN ?
024300020801      *  72/73 - DI SUBFILE
024400020801      *  90    - ERRORE GENERICO
024500020801      *---------------------------------------------------------------*
024600020801
024700130527     C     Kaco          KLIST
024800130527     C                   KFLD                    codut
024900130527     C                   KFLD                    dutkci
025000130527     C                   KFLD                    kksc
025100020801     C     KFVV          KLIST
025200020801     C                   KFLD                    PARNPG
025300020801     C                   KFLD                    COMNFV
025400020801     C                   KFLD                    VIDFGS
025500020801     C     KFV2          KLIST
025600020801     C                   KFLD                    FGVLNP
025700020801     C                   KFLD                    FGVNFV
025800020801     C                   KFLD                    KEPA
025900020801     C                   KFLD                    KLAI
026000020801     C                   KFLD                    KTDH
026100130424     C     KFV4          KLIST
026200130424     C                   KFLD                    fgvlnp
026300130424     C                   KFLD                    fgvNFV
026400130424     C                   KFLD                    KEPA
026500130424     C                   KFLD                    Klai
026600130424     C                   KFLD                    KTRC
026700020801     C     K02FGW01      klist
026800020801     C                   kfld                    FGWnfv
026900020801     C                   kfld                    FGWlnp
027000020801     C     K02FGV01      klist
027100020801     C                   kfld                    CO2NFV
027200020801     C                   kfld                    SIMFEL
027300020801     C     K02FGV07      klist
027400021025     C                   kfld                    SIMfel
027500130523     C                   kfld                    kimp
027600020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
027700000000     C     *ENTRY        PLIST
027800130524     C                   PARM                    KPJBA
027900130524      *
028000130524      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
028100130524     c     *dtaara       define    §azute        azuteds
028200130524     c     *dtaara       define    §datiute      ddatiute
028300130524      *
028400130524     c                   clear                   AzUteDs
028500130524     c                   clear                   DDatiUte
028600130524     c                   clear                   Tibs34Ds
028700130524     c                   in(E)     *dtaara
028800130524if  1c                   if        %Error  or  RSUT = *blanks
028900130524     c                   call      'TIBS34R'
029000130524     c                   parm                    Tibs34Ds
029100130524     c                   in        *dtaara
029200130524e   1c                   endif
029300020801     C*
029400020801     C* Carico L1 e impostazioni iniziali
029500911104     C                   EXSR      CARTAB
029600020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
029700020801     C* Prima videata
029800900517     C*
029900130524     C                   MOVEL     '?'           VIDNFV
030000130524     c                   eval      vidsel='SI'
030100130711     c                   eval      vidaut='  '
030200130627     c                   seton                                        67
030300911008     C*
030400900509     C     FOR01         TAG
030500130524     c
030600130705     c                   clear                   viddsosp
030700130705
030800130524     c                   if        *in67
030900130524     C                   WRITE     lsT6D03
031000130524     c                   endif
031100130524     c
031200130524     C                   SETOFF                                       05
031300130523     C                   EXFMT     lsT6D01
031400020801     C* F03 = Fine Lavoro
031500130902     c                   if        *inkc
031600130613     C                   GOTO      FINE
031700130613     c                   endif
031800130710     c*
031900160125     c**                 if        *inkk=*on
032000160125     c**                 Clear                   tibs02ds
032100160125     c**                 Eval      T02Mod = 'R'
032200160125     c**                 Eval      T02Sif = Knsif
032300160125     c**                 Eval      T02Cod = 'BSP'
032400160125     c**                 Call      'TIBS02R'
032500160125     c**                 Parm                    Kpjba
032600160125     c**                 Parm                    tibs02ds
032700160125     c**                 seton                                        67
032800160125     C**                 GOTO      FOR01
032900160125    2c**                 endif
033000020801     C*
033100020801     C* Controllo 1.a videata
033200911008     C                   EXSR      CONTR1
033300130524     C   90              GOTO      FOR01
033400020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
033500020801     C* Seconda videata (SubFile)
033600950109     C                   SETON                                        05
033700911010     C     FORCT2        TAG
033800130523     C                   WRITE     lsT6D02
033900130523     C                   WRITE     lsT6D01
034000911008     C     FORCT         TAG
034100130523     C                   EXFMT     lsT6DCT
034200020801     C* F03 = Fine Lavoro
034300911030     C   KC              GOTO      FINE
034400020801     C* F12 = Ritorno alla videata precedente
034500130524     c                   if        *inkl
034600130524     c                   seton                                        67
034700130524     C                   GOTO      for01
034800130524     c                   endif
034900130524     c
035000020801     C* Roll Up (solo se l'ultimo record è pieno, cieè *in10 = *on)
035100020801if  1C                   IF        *IN10
035200911008     C                   Z-ADD     SAVNRR        NRR
035300130523     C     NRR           CHAIN     lsT6DSF                            30
035400020801if  2C                   IF        not *in30 and
035500020801     C                             VI2NFV < *ZEROS
035600920805     C                   SETON                                        43
035700020801x   2C                   ELSE
035800020801     C                   Z-ADD     1             XB
035900020801     C                   EXSR      PULSFL
036000020801e   2C                   ENDIF
036100920805     C                   GOTO      FORCT
036200020801e   1C                   ENDIF
036300020801     C*
036400020801     C* Controllo subfile
036500911008     C                   EXSR      CTRSFL
036600130527     c
036700130527     c                   if        *in90
036800130527     C                   GOTO      FORCT
036900130527     c                   endif
037000130524     C
037100130524     c* e ho più di un foglio da chiudere non posso attivare le parzializz
037200130524     C                   if        *inki or W4cccm>*zeros
037300130524     c                   if        contaFGV>1
037400130524     c                   seton                                        9053
037500130524     c                   goto      forct
037600130524     c                   endif
037700130527     c                   endif
037800130527     c
037900130527     c                   if        not *inkf and not *inki
038000130527     C                   GOTO      FORCT
038100130527     c                   endif
038200130524     c
038300130524     C* F09 - VISUALIZZO PARZIALIZZAZIONI
038400130524     C                   if        *inki
038500130524     c                   exsr      ctrParz
038600130527     c                   goto      forct2
038700130524     c                   endif
038800020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
038900020801     C* F06 = Elaborazione: Creo Spunte e sottometto Chiusura FV
039000931001     C                   EXSR      ELAB
039100911108     C*
039200000824     C                   CLEAR                   KQEBI
039300000824     C                   CLEAR                   KQIEB
039400950130     C*
039500020801     C* Invio File Trasmissione
039600950107     C                   MOVEL     'LSA3'        KCOAZ
039700070423     c                   clear                   parama3
039800070423     c                   move      simfel        pa3tfp
039900070423     c                   movel(P)  parama3       kpjbu
040000911108     C                   CALL      'BCH10'
040100911108     C                   PARM                    KPJBA
040200121204     c
040300020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
040400020801     C* Fine
040500930524     C*
040600920324     C     FINE          TAG
040700080228     C                   CLEAR                   TRUL06DS
040800080228     C                   MOVEL     'C'           D06TLA
040900080228     C                   MOVEL     TRUL06DS      KPJBU
041000080228     C                   CALL      'TRUL06R'
041100080228     C                   PARM                    KPJBA
041200911108     C*
041300020801     C                   MOVEL     *ON           *INLR
041400020801      *
041500020801      *--- CARICO TABELLE --------------------------------------------*
041600020801      *
041700911104     C     CARTAB        BEGSR
041800020801     C*
041900020801     C* Campi di passaggio
042000930127     C                   MOVEL     SIMFEL        PA2FEL
042100960209     C                   MOVEL     SIMFEL        PA1FEL
042200130524     C                   MOVEL     rsut          PA2rsut
042300921022     C*
042400020801     C                   TIME                    W0140
042500020801     C                   Z-ADD     W0140         UDATE8
042600940218     C                   MOVEL     3             PARNPG
042700940415     C                   MOVEL     3             PA2NPG
042800020801     C*
042900020801     C* Carico tabella Filiali Gestite £1
043000020801     C                   CLEAR                   TRUL06DS
043100950107     C                   MOVE      '£1'          D06COD
043200950107     C                   MOVEL     SIMFEL        D06KEY
043300020801     C                   MOVEL     TRUL06DS      KPJBU
043400950107     C                   CALL      'TRUL06R'
043500950107     C                   PARM                    KPJBA
043600020801     C                   MOVEL     KPJBU         TRUL06DS
043700950107     C                   MOVEA     LIN           L1
043800130627     c
043900130627     c
044000940415     C*
044100040121     C* Controllo se SIMFEL ha CML/VDL
044200930125     C     SIMFEL        CHAIN     AZORG                              31
044300960209     C   31              MOVEL     *BLANKS       ORGDE8
044400040121     C   31              MOVEL     *BLANKS       ORGDf0
044500040121     c                   eval      og150=orgdf0
044600960209     C*
044700040121     C                   TESTN                   §ogvol               31
044800930125     C*
044900120917     c* Sostituisco l'uso dei flag §ogspp e §ogspv con il test sulla
045000120917     c* data macchinone. Siccome il pgm di chiusura f.v. i campi pa2spv e pa2spp
045100120917     c* li utilizza, li continuo a passare ma valorizzandoli, come per il campo
045200120917      * PA2CML, in base alla data macchinone.
045300930125     C     *IN31         IFEQ      '1'
045400040121     C     §ogvol        ANDGT     *ZEROS
045500120914     C                   SETON                                        03
045600970423     C                   MOVEL     'S'           PA2CML
045700120914     C                   MOVEL     'S'           PA2spv
045800020801     C                   ENDIF
045900040121     C                   TESTN                   §ogpes               31
046000990917     C*
046100990917     C     *IN31         IFEQ      '1'
046200040121     C     §ogpes        ANDGT     *ZEROS
046300120914     C                   SETON                                        03
046400990917     C                   MOVEL     'P'           PA2CML
046500120914     C                   MOVEL     'S'           PA2spp
046600020801     C                   ENDIF
046700040121     c
046800120914     c***                eval      pa2spv=§ogspv
046900120914     c***                eval      pa2spp=§ogspp
047000120914     c***  pa2spv        ifne      *blanks
047100120914     c***  pa2spp        orne      *blanks
047200120914     c***                seton                                        03
047300120914     c***                endif
047400040121     c
047500020801     C* Dalla descrizione 148 vedo quale generazione spunte devo fare
047600020801     C*  in chiusura FV
047700960209     C                   MOVEL     ORGDE8        OG148
047800170613     C**   §OGAGS        IFEQ      'E'
047900170613     C**                 MOVEL     'S'           PA2AGS
048000170613     C**                 MOVEL     §OGAGP        PA2AGP
048100170613     C**                 ELSE
048200170613     C**                 CLEAR                   PA2AGS
048300170613     C**                 CLEAR                   PA2AGP
048400170613     C**                 ENDIF
048500170613     C                   eval      pa2ags=§ogags
048600911104     C*
048700020801     C* Controllo se sono Filiale di Primo o Secondo Livello
048800130523     C                   Z-ADD     SIMfel        VIDFGS
048900130524     c     simfel        chain     azorg01l
049000130524     c                   if        %found(azorg01l)
049100130524     c                   movel     orgdes        desfgs
049200130524     c                   endif
049300130613     C                   movel     vidfgs        alfafgs           3
049400000615     C*
049500911104     C                   ENDSR
049600020801      *
049700020801      *--- PULIZIA SUBFILE -------------------------------------------*
049800020801      *
049900911008     C     PULSFL        BEGSR
050000130523     C                   SETOFF                                       0102
050100130523     C                   CLEAR                   lsT6DSF
050200130524     c
050300130627     C     XB            DOWLE     16
050400130524     c                   if        primoV=' '
050500130524     c                   seton                                        60
050600130701     C     nrr           add       1             rec
050700130524     c                   eval      primoV='N'
050800130524     c                   endif
050900130524     c
051000020801     C                   ADD       1             NRR
051100130523     C                   WRITE     lsT6DSF
051200130524     c                   setoff                                       60
051300020801     C                   ADD       1             XB
051400020801     C                   ENDDO
051500020801     C* Salvo l'ultimo NRR
051600020801     C                   Z-ADD     NRR           SAVNRR
051700130701     C***                Z-ADD     SAVNRR        REC
051800930129     C                   SETON                                        01
051900911008     C                   ENDSR
052000020801      *
052100020801      *--- CONTROLLI FOGLIO VIAGGIO ----------------------------------*
052200020801      *
052300911008     C     CONTR1        BEGSR
052400020801     C*
052500911008     C                   SETOFF                                       303190
052600130524     C                   SETOFF                                       4767
052700130524     c                   clear                   primoV
052800020801     C* P.O. in gestione
052900020801     C* .. diverso da 0
053000020801if  1C     VIDFGS        IFEQ      0
053100130524     C                   SETON                                        90
053200000704     C                   GOTO      ENDCT1
053300020801     C* .. presente in L1
053400020801x   1C                   ELSE
053500000704     C     VIDFGS        LOOKUP    L1                                     34
053600130524     C  N34              SETON                                        90
053700000704     C   90              GOTO      ENDCT1
053800020801e   1C                   ENDIF
053900020801     C*
054000130523     C* Numero Foglio IMP
054100020801if  1C     VIDNFV        IFEQ      *BLANKS
054200911008     C     VIDNFV        OREQ      *ZEROS
054300911008     C                   SETON                                        4090
054400911008     C                   GOTO      ENDCT1
054500911008     C*
054600020801x   1C                   ELSE
054700020801     C* Ricerca
054800940218     C     '?'           SCAN      VIDNFV                                 90
054900020801if  2C     *IN90         IFEQ      *ON
055000950107     C                   Z-ADD     0             DLRNFV
055100950107     C                   MOVEL     'S'           DLRRIC
055200950107     C                   MOVEL     '3'           DLRNPG
055300950107     C                   MOVEL     31129999      DLRGAL
055400130523     C                   MOVEL     'FNlST6R'     DLRPGM
055500000615     C                   MOVEL     VIDFGS        DLRFGS
055600950107     C                   MOVEL     '2'           DLRTFV
055700950107     C                   MOVEL     DSLR01        KPJBU
055800950107     C                   CALL      'FNLR02R'
055900911008     C                   PARM                    KPJBA
056000950107     C                   MOVEL     KPJBU         DSLR01
056100950107     C                   MOVEL     DLRNFV        VIDNFV
056200130523     C                   WRITE     lsT6D03
056300000620     C                   SETON                                        67
056400911008     C                   GOTO      ENDCT1
056500020801e   2C                   ENDIF
056600020801     C* Controlli
056700911008     C                   TESTN                   VIDNFV               31
056800911010     C  N31              SETON                                        30
056900911008     C*
057000020801if  2C     *IN30         IFEQ      *OFF
057100940218     C                   MOVEL     VIDNFV        COMNFV
057200130705     C     KFVV          CHAIN     FNFVV000                           30
057300020801     C* Escludo: 1) gli Annullati
057400020801     C*          2) ciò che NON è IMP
057500020801     C*          3) i Chiusi
057600020801if  3C     FVVATB        IFNE      *BLANKS
057700930524     C     FVVSPG        ORNE      'P'
057800930524     C     FVVFCF        ORNE      *BLANKS
057900911008     C                   SETON                                        30
058000020801e   3C                   ENDIF
058100020801e   2C                   ENDIF
058200911008     C*
058300911008     C   30              SETON                                        4090
058400911008     C   30              GOTO      ENDCT1
058500020801e   1C                   ENDIF
058600020801     C*
058700130523     C* Giro Data Foglio IMP
058800950109     C                   CLEAR                   WLBDAT
058900950109     C                   MOVEL     '3'           G02ERR
059000950109     C                   Z-ADD     FVVDFV        G02INV
059100950109     C                   CALL      'XSRDA8'
059200950109     C                   PARM                    WLBDAT
059300950109     C                   Z-ADD     G02DAT        VIDDFV
059400130705     c*
059500130705     c* Calcolo la data sospensione
059600130711    2c**                 if        vidaut='SI'
059700130711     c**                 clear                   trul13ds
059800130711     c**                 Eval      I13Tla = 'L'
059900130711     c**                 Eval      I13Mod = 'P'
060000130711     c**                 Eval      I13Fil = simfel
060100130711     c**                 Eval      I13Din = fvvdfv
060200130711     c**                 Eval      I13gio = 1
060300130711     c**                 Call      'TRUL13R'
060400130711     c**                 Parm                    Trul13ds
060500130711     c**
060600130711    3c**                 if        o13err<>' '
060700130711     c**                 seton                                        9045
060800130711     c**                 leavesr
060900130711    3c**                 endif
061000130711     c**                 movel     o13dfi        dataiso
061100130711     c**                 movel     dataiso       datadmy
061200130711     c**                 movel     datadmy       viddsosp
061300130711    2c**                 endif
061400020801     C*
061500020801     C* Subfile : pulizia
061600911107     C                   SETON                                        72
061700130523     C                   WRITE     lsT6DCT
061800130705     C                   SETOFF                                       7247
061900911107     C*
062000950107     C                   Z-ADD     0             NRR
062100950107     C                   CLEAR                   VI2SCE
062200950109     C                   CLEAR                   VH2FLG
062300970603     C                   CLEAR                   VH2FL2
062400950109     C                   CLEAR                   VH2DTP
062500130524     c
062600130524     c* Carico la £6 di simfel alla data foglio IMP
062700130524     c                   clear                   trul06ds
062800130524     c                   eval      d06cod = '£6'
062900130524     c                   movel     simfel        d06key
063000130524     C                   MOVE      'S'           D06KEY
063100130524     C                   MOVEl     fvvdfv        D06drf
063200130524     c                   movel     trul06ds      kpjbu
063300130524     c                   call      'TRUL06R'
063400130524     c                   parm                    kpjba
063500130524     c                   movel     kpjbu         trul06ds
063600130524     c                   movea     lin           l6
063700130627     c*
063800130627     c* Verifico se ha clienti abilitati alla borderizzazione per codice
063900130627     c                   setoff                                       26
064000130627     c                   eval      kcod='CLI'
064100130627     c     kcod          setll     tntbe01l
064200130627     c     kcod          reade     tntbe01l
064300130627     c                   dow       not %eof(tntbe01l)
064400130627     c                   if        tbeatb=' '
064500130627     c                   movel     tbeuni        dcli
064600130627     c
064700130627     c                   if        §clibrd='S'
064800130627     c                   movel     tbeke1        filcli            3 0
064900130627     c     filcli        lookup    L6                                     26
065000130627     c  n26filcli        lookup    L1                                     26
065100130627     c
065200130627     c                   if        *in26
065300130627     c                   leave
065400130627     c                   endif
065500130627     c                   endif
065600130627     c                   endif
065700171105     c                   seton                                        26
065800130627     c
065900130627     c     kcod          reade     tntbe01l
066000130627     c                   enddo
066100130524     c*
066200020801     C*
066300130523     c* Prima carico i fogli già abbinati al foglio IMP
066400020801     C* Subfile : caricamento
066500130523     c                   eval      kimp=comnfv
066600020801     C     K02FGV07      SETLL     FNFGV007
066700020801     C     K02FGV07      READE     FNFGV007                               30
066800911008     C*
066900911108     C                   SETON                                        04
067000020801do  1C                   DOW       NOT *IN30
067100940217     C                   SETOFF                                       15
067200911031     C                   MOVEL     FGVNFV        VI2NFV
067300020801     C                   CLEAR                   VI2FCF
067400020801     C                   IF        FGVFCF <> *BLANKS
067500911107     C                   MOVEL     'C'           VI2FCF
067600940217     C                   SETON                                        15
067700020801     C                   ENDIF
067800020801     C* Decodifiche Foglio
067900950109     C                   EXSR      DECOD
068000911031     C*
068100911107     C                   ADD       1             NRR
068200020801     C                   z-add     FGVlnp        wLNP
068300020801     C                   z-add     FGVnfv        wNFV
068400020801     C                   move      wPOFV         FS(NRR)
068500130523     C                   WRITE     lsT6DSF
068600911008     C*
068700020801     C     K02FGV07      READE     FNFGV007                               30
068800020801e   1C                   ENDDO
068900940217     C                   SETOFF                                       0415
069000911031     C*
069100130523     c* Poi carico i foglio, se richeista, aperti e non ancora abbianti
069200130523     c*  a foglio IMP
069300130523     c                   if        vidsel='SI'
069400130523     c                   exsr      ricercaFGV
069500130523     c                   endif
069600130523     c
069700020801     C* Finisco di pulire il subfile
069800020801     C                   Z-ADD     NRR           SAVNFC
069900130627     C     NRR           DIV       16            RISUL
070000020801     C                   MVR                     RESTO
070100930129     C*
070200020801     C     RESTO         ADD       1             XB
070300911008     C                   EXSR      PULSFL
070400130627     C     SAVNFC        IFEQ      16
070500930129     C                   Z-ADD     1             REC
070600020801     C                   ENDIF
070700130523     c                   seton                                        25
070800911008     C*
070900911008     C     ENDCT1        ENDSR
071000130523      *
071100130523      *--- ricerca fogli viaggio ancora da chiudere ------------------*
071200130523     C     RicercaFGV    BEGSR
071300130523     c                   clear                   kimp
071400130524     C                   SETOFF                                       15
071500130523     c     k02fgv07      setll     fnfgv07l
071600130523     c     k02fgv07      reade     fnfgv07l
071700130523     c
071800130523     c                   dow       not %eof(fnfgv07l)
071900130523     c* elaboro i fogli ancora aperti, non annullati
072000130523     c*  ed  escludo i fogli D H
072100130523     c                   if        fgvfcf=' ' and fgvatb=' '  and
072200130523     c                             fgvttr<>'D' and fgvttr<>'H'
072300130701     c* escludo i fogli con data fogio > data foglio IMP
072400130701     c                   if        fgvdfv<=fvvdfv
072500130523     C* Decodifiche Foglio
072600130523     C*
072700130701     c* posizinamento cursore sul primo foglio che carico
072800130701     c                   if        primoV=' '
072900130701     c                   seton                                        60
073000130701     c     nrr           add       1             rec
073100130701     c                   eval      primoV='N'
073200130701     c                   endif
073300130701
073400130524     C                   MOVEL     FGVNFV        VI2NFV
073500130524     C                   EXSR      DECOD
073600130524     C                   CLEAR                   VI2FCF
073700130523     C                   ADD       1             NRR
073800130523     C                   z-add     FGVlnp        wLNP
073900130523     C                   z-add     FGVnfv        wNFV
074000130523     C                   move      wPOFV         FS(NRR)
074100130523     C                   WRITE     lsT6DSF
074200130701     c                   setoff                                       60
074300130523     C*
074400130523     c                   endif
074500130701     c                   endif
074600130523     c
074700130523     c     k02fgv07      reade     fnfgv07l
074800130523     c                   enddo
074900130523     c
075000130523     c                   ENDSR
075100020801      *
075200020801      *--- CONTROLLO SUB FILE ----------------------------------------*
075300020801      *
075400911008     C     CTRSFL        BEGSR
075500020801     C*
075600911031     C                   SETOFF                                       9045
075700950109     C                   Z-ADD     1             NRR
075800020801     C                   CLEAR                   FS
075900130524     c                   clear                   primoV            1
076000130524     c                   clear                   contaFGV          5 0
076100911009     C*
076200020801     C* Controllo TUTTO il subfile
076300020801do  1C     NRR           DOWLE     SAVNRR
076400130523     C     NRR           CHAIN     lsT6DSF                            30
076500940218     C*
076600020801     C* Se riempito: controllo
076700020801if  2C                   IF        not *in30 and
076800020801     C                             VI2FCF <> 'C'
076900950109     C*
077000020801if  3C     VI2NFV        IFNE      *ZEROS
077100911105     C     VI2NFV        ANDNE     *BLANKS
077200950109     C                   SETON                                        01
077300911009     C*
077400020801     C* Ricerca in tutti i Fogli Viaggio
077500940218     C     '?'           SCAN      VI2NFV                                 32
077600020801if  4C     *IN32         IFEQ      *ON
077700940218     C                   EXSR      RICNFV
077800940218     C                   SETON                                        90
077900130523     C                   WRITE     lsT6D02
078000130523     C                   WRITE     lsT6D01
078100911009     C                   GOTO      ENDCTS
078200020801e   4C                   ENDIF
078300020801     C*
078400020801     C* Controlli
078500020801     C*
078600911104     C                   TESTN                   VI2NFV               31
078700130524     c                   if        not *in31
078800130524     C                   SETON                                        469060
078900130524     C                   GOTO      ENDCTS
079000130524     c                   endif
079100911010     C*
079200020801     C                   Z-ADD     0             CO2NFV
079300911031     C                   MOVEL     VI2NFV        CO2NFV
079400020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
079500911010     C*
079600020801     C* Annullato o già chiuso
079700020801if  4C                   IF        not  *in31
079800020801     C                             and (FGVatb <> *blanks
079900020801     C                              or  FGVfcf <> *blanks)
080000911031     C                   SETON                                        31
080100020801e   4C                   ENDIF
080200020801     C*
080300020801     C* Errore
080400130524     c                   if        *in31
080500130524     C                   SETON                                        469060
080600130524     C                   GOTO      ENDCTS
080700130524     c                   endif
080800020801     C*
080900020801     C* Non è possibile chiudere un FV defluenza se LNA estero
081000020801     C*  o DPD o FEDEX
081100020801if  4C                   IF        not  *in31
081200020801     C                             and (FGVTTR = 'D'
081300020801     C                              or  FGVTTR = 'H')
081400990514     C     FGVLNA        CHAIN     AZORG                              31
081500020801if  5C                   IF        not *in31
081600020801     C                   MOVEL     ORGDE3        OG143
081700020801x   5C                   ELSE
081800020801     C                   CLEAR                   OG143
081900020801e   5C                   ENDIF
082000020801if  5C                   IF        not  *in31
082100020801     C                             and (§OGNTW = 'EEX'
082200020801     C                              or  §OGNTW = 'EUP'
082300020801     C                              or  §OGNTW = 'FED'
082400020801     C                              or  §OGNTW = 'DPD')
082500990514     C                   SETON                                        31
082600020801e   5C                   ENDIF
082700020801e   4C                   ENDIF
082800020801     C* Errore
082900130524     c                   if        *in31
083000130524     C                   SETON                                        519060
083100130524     C                   GOTO      ENDCTS
083200130524     c                   endif
083300130527     c* se inserito cod cliente o premuto F9  DEVE essere foglio locale
083400130527     c*  (almeno per ora...)
083500130527     c                   if        w4cccm>*zeros or *inki
083600130527     c     fgvlna        lookup    L6                                     31
083700151126     c**                 if        not *in31
083800151126     C**                 SETON                                        549060
083900151126     C**                 GOTO      ENDCTS
084000151126     c**                 endif
084100130527     c                   endif
084200911031     C*
084300020801     C* Memorizzo nella schiera dei FV selezionati
084400020801     C*
084500020801     C                   z-add     SIMfel        wLNP
084600020801     C                   z-add     CO2nfv        wNFV
084700020801     C     wPOFV         LOOKUP    FS                                     31
084800130524     C   31              SETON                                        479060
084900911031     C   31              GOTO      ENDCTS
085000020801     C                   move      wPOFV         FS(NRR)
085100020801     C*
085200020801     C* Richiamo la Manutenzione FV
085300020801     C*
085400020801if  4C     VI2SCE        IFEQ      '1'
085500130502     c                   clear                   fnlst4ds
085600130502     c                   eval      ilst4ges='I'
085700130502     c                   eval      ilst4fgs=%editc(vidfgs:'X')
085800130502     c                   eval      ilst4nfv=%editc(co2nfv:'X')
085900130502     c                   movel     fnlst4ds      kpjbu
086000130502     C                   CALL      'FNLST4R'
086100130502     C                   PARM                    KPJBA
086200130502     C                   MOVEL     KPJBU         fnlst4ds
086300130523
086400020801     C* Se PA3F12 <> 'S' non è stato premuto F12 -> riaggancio il foglio
086500020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
086600020801     C* Riemetto la videata
086700950109     C                   SETON                                        90
086800130523     C                   WRITE     lsT6D02
086900130523     C                   WRITE     lsT6D01
087000950109     C                   CLEAR                   VI2SCE
087100020801e   4C                   ENDIF
087200020801     C*
087300020801     C* Decodifiche
087400950109     C                   EXSR      DECOD
087500130524     C* Se Data Foglio <> Data Foglio IMP -> Errore
087600130524     C*  (esclusi i Fogli di Defluenza)
087700130524if  4C     FGVTTR        IFNE      'D'
087800130524     C     FGVTTR        ANDNE     'H'
087900130524     C     FGVDFV        ANDNE     FVVDFV
088000130524     C                   SETON                                        509060
088100130524     C                   GOTO      ENDCTS
088200130524e   4C                   ENDIF
088300020801     C*
088400130524     c* Dati obbligatori da borderò
088500130524     c                   exsr      Datiobbl
088600130524     c
088700130524     c                   if        *in90
088800130524     C                   SETON                                        529060
088900130524     C                   MOVEL     '1'           VI2SCE
089000130524     C                   GOTO      ENDCTS
089100130524     c                   endif
089200020801     C* Se mancano i Piombi o la Data Patrenza:
089300020801     C*  Errore vincolante: devo andare in manutenzione a modificarli
089400130424if  4C                   IF           (wpmb =  *blanks
089500020801     C                             or  FGVdtp =  *zeros)
089600020801     C                             and FGVttr <> 'L'
089700130524     C                   SETON                                        489060
089800950324     C                   MOVEL     '1'           VI2SCE
089900950107     C                   GOTO      ENDCTS
090000020801e   4C                   ENDIF
090100130524     c
090200130524     c
090300020801     C* Se Data Partenza < Data Foglio IMP -> Errore forzabile
090400020801if  4C     FGVDTP        IFNE      *ZERO
090500950324     C     FGVDTP        ANDNE     VH2DTP
090600950109     C                   CLEAR                   VH2FLG
090700020801e   4C                   ENDIF
090800950109     C*
090900020801if  4C     VH2FLG        IFEQ      *BLANKS
091000960129     C                   MOVEL     FGVDTP        VH2DTP
091100020801if  5C     FGVDTP        IFNE      *ZERO
091200950324     C     FGVDTP        ANDLT     FVVDFV
091300130524     C                   SETON                                        499060
091400950109     C                   MOVEL     '1'           VH2FLG
091500950109     C                   GOTO      ENDCTS
091600020801e   5C                   ENDIF
091700020801e   4C                   ENDIF
091800020801     C*
091900130524     c* conto i fogli aperti da chiudere
092000130524     c                   add       1             contafgv
092100130524     c                   z-add     nrr           unicoNRR
092200130524     c
092300020801     C* Segnalazione errore a video
092400911009     C     ENDCTS        TAG
092500920805     C   90              Z-ADD     NRR           REC
092600130523     C  N32              UPDATE    lsT6DSF
092700920805     C   90              Z-ADD     SAVNRR        NRR
092800130524     c                   setoff                                       60
092900020801     C*
093000020801x   3C                   ELSE
093100020801     C*
093200020801     C* Record vuoto del SFL
093300130524     C                   CLEAR                   lsT6DSF
093400130524     c
093500130523     C                   UPDATE    lsT6DSF
093600130524     c                   setoff                                       60
093700020801     C*
093800020801e   3C                   ENDIF
093900020801     C*
094000020801e   2C                   ENDIF
094100911031     C*
094200911031     C                   ADD       1             NRR
094300020801e   1C                   ENDDO
094400920805     C*
094500020801     C* Se non rilevati errori: posizionamento cursore sull'ultimo rec.
094600130524if  1C                   IF        not *in90
094700130524     c                   z-add     1             nrr
094800130524do  1C     NRR           DOWLE     SAVNRR
094900130524     C     NRR           CHAIN     lsT6DSF                            30
095000130524if  3C     VI2NFV        IFNE      *ZEROS
095100130524     C     VI2NFV        ANDNE     *BLANKS
095200130524     c                   add       1             nrr
095300130524     c                   else
095400130524     c                   seton                                        60
095500130524     c                   z-add     nrr           rec
095600130524     C                   UPDATE    lsT6DSF
095700130524     c                   leave
095800130524e   1C                   ENDIF
095900130524e   1C                   ENDdo
096000130524     c
096100130524e   1C                   ENDIF
096200130524     c                   setoff                                       60
096300911008     C*
096400911009     C                   ENDSR
096500020801      *
096600020801      * RICERCA E CARICAMENTO FOGLI VIAGGIO --------------------------*
096700020801      *
096800940218     C     RICNFV        BEGSR
096900020801     C*
097000130523     C                   RESET                   fnlst3ds
097100020801     C* Come data finale imposto la data del giorno
097200951108     C                   Z-ADD     UDATE8        PA7DFI
097300000615     C                   Z-ADD     VIDFGS        PA7FGS
097400130523     C                   MOVEL     fnlst3ds      KPJBU
097500130523     C                   CALL      'FNLST3R'
097600940218     C                   PARM                    KPJBA
097700940218     C*
097800130523     C                   MOVEL     KPJBU         fnlst3ds
097900940218     C*
098000020801     C* Se selezionati FV li scrivo
098100940218     C                   MOVEL     *BLANKS       VI2NFV
098200130523     C                   UPDATE    lsT6DSF
098300940218     C*
098400940218     C                   SUB       1             NRR
098500020801     C                   Z-ADD     1             XI
098600940218     C*
098700020801do  1C     XI            DOWLE     45
098800020801     C     FVA(XI)       ANDNE     *ZEROS
098900020801     C*
099000940218     C                   ADD       1             NRR
099100130523     C     NRR           CHAIN     lsT6DSF                            30
099200940218     C*
099300020801     C* Creo nuova pagina del subfile
099400020801if  eC                   IF        *IN30
099500020801     C                   Z-ADD     NRR           NUM5
099600940218     C                   SUB       1             NRR
099700020801     C                   Z-ADD     1             XB
099800940218     C                   EXSR      PULSFL
099900940218     C                   Z-ADD     NUM5          NRR
100000130523     C     NRR           CHAIN     lsT6DSF                            30
100100020801     C*
100200020801e   2C                   ENDIF
100300940218     C*
100400020801     C* Memorizzo solo se è vuoto, altrimenti su altra riga
100500020801if  2C     VI2NFV        IFLT      *ZEROS
100600020801     C                   MOVEL     FVA(XI)       VI2NFV
100700020801     C                   MOVE      FVA(XI)       VI2LNA
100800950109     C                   MOVEL     VI2NFV        CO2NFV
100900020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
101000950109     C                   EXSR      DECOD
101100940218     C*
101200130523     C                   UPDATE    lsT6DSF
101300020801     C                   ADD       1             XI
101400020801e   2C                   ENDIF
101500020801     C*
101600020801e   1C                   ENDDO
101700020801     C*
101800020801     C* Esco
101900940218     C     NRR           IFLT      1
102000940218     C                   Z-ADD     1             NRR
102100020801     C                   ENDIF
102200020801     C*
102300940218     C                   ENDSR
102400020801      *
102500020801      *--- ELABORAZIONE ABBINAMENTO ----------------------------------*
102600020801      *
102700911008     C     ELAB          BEGSR
102800130705     C                   CLEAR                   KQIEB
102900130705     C                   CLEAR                   KQEBI
103000020801     C*
103100130705     c* sottometto sospensione automatica
103200130711     c**                 if        vidaut='SI'
103300130711     c**                 clear                   fnlsu3ds
103400130711     c**                 eval      ilsu3dsp_B=%editc(fvvdfv:'X')
103500130711     c**                 eval      ilsu3dsp_S=%editc(o13dfi:'X')
103600130711     c**                 eval      ilsu3nos='S'
103700130711     c**                 movel     fnlsu3ds      kpjbu
103800130711     C**                 MOVEL     'LSU3'        KCOAZ
103900130711     C**                 CALL      'BCH10'
104000130711     C**                 PARM                    KPJBA
104100130711e   1C**                 ENDIF
104200130705     c
104300950125     C                   Z-ADD     COMNFV        PA2NFV
104400950125     C                   Z-ADD     FVVDFV        PA2DFV
104500950125     C                   Z-ADD     FVVFGS        PA2FGS
104600950125     C                   CLEAR                   PA2NUM
104700040121     C* *IN03 = *ON : utilizza peso e/o volume in spunta
104800020801     C* --> sottometto azione di calcolo Volumi e Peso
104900020801if  1C     *IN03         IFEQ      *ON
105000960209     C                   CLEAR                   KPJBU
105100961008     C                   CLEAR                   PA1NUM
105200960209     C                   MOVEL     PARAM1        KPJBU
105300950107     C                   MOVEL     'LSB7'        KCOAZ
105400931001     C                   CALL      'BCH10'
105500931001     C                   PARM                    KPJBA
105600020801e   1C                   ENDIF
105700911104     C*
105800911107     C                   Z-ADD     1             NRR
105900020801do  1C     NRR           DOWLE     SAVNRR
106000020801     C*
106100130523     C     NRR           CHAIN     lsT6DSF                            30
106200020801     C* Fogli Viaggio selezionati NO chiusi
106300020801     C* -> Sottometto chiusura FV
106400020801if  2C                   IF        not *in30
106500020801     C                             and VI2NFV > *ZEROS
106600020801     C                             and VI2FCF = *BLANKS
106700931001     C                   EXSR      LANCIO
106800020801e   2C                   ENDIF
106900911104     C*
107000911104     C                   ADD       1             NRR
107100020801e   1C                   ENDDO
107200911104     C*
107300911008     C                   ENDSR
107400020801      *
107500020801      *--- LANCIO CHIUSURA FOGLIO VIAGGIO ----------------------------*
107600020801      *
107700911104     C     LANCIO        BEGSR
107800020801     C*
107900020801     C* Aggiorno FV
108000940505     C                   MOVEL     VI2NFV        CO2NFV
108100020801     C*
108200020801     C     K02FGV01      CHAIN     FNFGV001                           33
108300950107     C                   MOVEL     VIDNFV        FGVNFA
108400960129     C                   MOVEL     VH2DTP        FGVDTP
108500960129     C                   MOVEL     VI2HMP        FGVHMP
108600950107     C                   UPDATE    FNFGV001
108700020801     C*
108800020801     C* Sottometto Chiusura FV
108900080228     C* Carico tabella Filiali Gestite £1 alla data foglio viaggio
109000080228     C                   CLEAR                   TRUL06DS
109100080228     C                   MOVE      '£1'          D06COD
109200080228     C                   MOVEL     ' '           D06TLA
109300080228     C                   MOVEL     SIMFEL        D06KEY
109400080228     C                   MOVE      'S'           D06KEY
109500080228     C                   MOVEl     fgvdfv        D06drf
109600080228     C                   MOVEL     TRUL06DS      KPJBU
109700080228     C                   CALL      'TRUL06R'
109800080228     C                   PARM                    KPJBA
109900080228     C                   MOVEL     KPJBU         TRUL06DS
110000080228     C                   MOVEA     LIN           L1ela
110100130527     c
110200130527     c                   if        w4cccm>*zeros
110300130527     c                   movel     w4cccm        pa2ccm
110400130527     c                   else
110500130527     c                   clear                   pa2ccm
110600130527     c                   endif
110700080228     c
110800130524     C                   MOVEL     fnlsu2ds      KPJBU
110900130531     C                   MOVEL     'LSU2'        KCOAZ
111000130227     c                   if        knmus=*all'1'
111100130524     C                   CALL      'FNLSU2R'
111200130227     C                   PARM                    KPJBA
111300130227     c                   else
111400130227     C                   CALL      'BCH10'
111500911104     C                   PARM                    KPJBA
111600130227     c                   endif
111700020801     C*
111800911104     C                   ENDSR
111900020801      *
112000020801      * DECODIFICHE FOGLIO -------------------------------------------*
112100020801      *
112200950109     C     DECOD         BEGSR
112300020801     C*
112400950109     C                   MOVEL     FGVLNA        VI2LNA
112500950109     C     FGVLNA        CHAIN     AZORG                              31
112600950109     C   31              MOVEL     *BLANKS       DESLNA
112700950109     C  N31              MOVEL     ORGDES        DESLNA
112800130424     c*
112900130424     C* Piombi
113000130424     c                   clear                   wpmb
113100130424     C                   MOVEL     'P'           kTRC
113200130424  155C     KFV4          SETLL     FNFV401L
113300130424  156C     KFV4          READE     FNFV401L
113400130424  156C                   IF        not %eof(fnfv401l) and fv4atb=' '
113500130424     C                   MOVEL(p)  FV4NOT        Wpmb
113600130424  156C     KFV4          READE     FNFV401L
113700130424  156C                   IF        not %eof(fnfv401l) and fv4atb=' '
113800130424  156C                   eval      %subst(Wpmb:36)=fv4not
113900130424  156C                   ENDIF
114000130424     C                   ENDIF
114100130424     c                   movel     wpmb          vi2pmb
114200130424
114300950109     C                   MOVEL     FGVHMP        VI2HMP
114400960201     C                   MOVEL     FGVTTR        VH2TTR
114500951031     C*
114600020801     C* Giro Data Partenza Reale
114700020801     C* Se NON c'è prendo la data teorica se c'è
114800020801if  1C     FGVDTP        IFEQ      0
114900960129     C     KFV2          CHAIN     FNFV2000                           31
115000020801if  2C     *IN31         IFEQ      *OFF
115100960129     C                   MOVEL     FV2DPA        FGVDTP
115200960129     C                   MOVEL     FV2HPA        VI2HMP
115300020801e   2C                   ENDIF
115400020801e   1C                   ENDIF
115500960129     C*
115600950109     C                   CLEAR                   WLBDAT
115700950109     C                   MOVEL     '3'           G02ERR
115800950109     C                   Z-ADD     FGVDTP        G02INV
115900950109     C                   CALL      'XSRDA8'
116000950109     C                   PARM                    WLBDAT
116100950109     C                   MOVEL     G02DAT        VI2DTP
116200020801     C* Giro Data Foglio
116300950109     C                   CLEAR                   WLBDAT
116400950109     C                   MOVEL     '3'           G02ERR
116500950109     C                   Z-ADD     FGVDFV        G02INV
116600950109     C                   CALL      'XSRDA8'
116700950109     C                   PARM                    WLBDAT
116800950109     C                   MOVEL     G02DAT        VI2DFV
116900020801     C* Ora Partenza Reale
117000960129     C     VI2HMP        IFGT      0
117100960129     C                   SETON                                        02
117200960129     C                   ELSE
117300960129     C                   SETOFF                                       02
117400960129     C                   ENDIF
117500130524     c
117600020801     C* Se Foglio con Tipo Traino inoltre evidenzio la Linea di Arrivo
117700960201     C     VH2TTR        IFEQ      'I'
117800960201     C                   SETON                                        06
117900960201     C                   ELSE
118000960201     C                   SETOFF                                       06
118100960201     C                   ENDIF
118200960201     C*
118300960129     C                   ENDSR
118400130524     C**************************************************************************
118500130524     C* CONTROLLO la presenza sul foglio viaggio dei dati obbligatori in border
118600130524     C**************************************************************************
118700130524     c     Datiobbl      begsr
118800130524     c                   setoff                                       90
118900130524     c* reperisco il numero di cellulare
119000130524     c                   clear                   wcell
119100130524     c                   eval      kepa='P'
119200130524     c                   eval      klai=0
119300130524     c                   eval      ktrc='L'
119400130524     c     kfv4          chain     fnfv401l
119500130524     c                   if        %found(fnfv401l)
119600130524     c                   movel     fv4not        wcell            16
119700130524     c                   endif
119800130524     c*
119900130524     c                   if        fgvtmz=*blanks or
120000130524     c                             (fgvpdr=*zeros and fgvdpd=*blanks) or
120100130524     c                             (fgvpmb=*blanks and fgvttr<>'L') or
120200130524     c                             (fgvtrm=*blanks and fgvtrr=*blank and
120300130524     c                              fgvttr<>'L') or
120400130524     c                             (wcell=*blanks and fgvttr<>'L')
120500130524     c                   seton                                        90
120600130524     c                   endif
120700130524     c                   endsr
120800130524     C**************************************************************************
120900130524     C* CONTROLLO parzializzaioni
121000130524     C**************************************************************************
121100130524     c     CtrParz       begsr
121200130527     c                   setoff                                       16
121300130524     c
121400130524     c     forw4         tag
121500130524     c
121600130524     c                   if        wriem='S'
121700130524     C                   WRITE     lsT6D02
121800130524     C                   WRITE     lsT6D01
121900130524     C                   write     lsT6DCT
122000130524     c                   endif
122100130524     c
122200130524     c                   exfmt     lst6w04
122300130524     c
122400130527     c                   setoff                                       2890
122500130524     c                   clear                   wriem             1
122600130524     c
122700130524     C* Verifico se richiesta la ricerca alfabetica
122800130524     c                   if        W4dccm<>*blanks and W4cccm<=*zeros
122900130524     c                   eval      pardut=rsut
123000130524     c                   clear                   parrag
123100130524     c                   eval      parrag=W4dccm
123200130524     c                   eval      parkcc=dutkci
123300130524     c                   eval      parsta=*zeros
123400130524     c                   clear                   parflr
123500130524     c                   clear                   pardit
123600130524     c                   eval      parnum=1
123700130524     c                   clear                   parkcm
123800130524     c                   clear                   parksm
123900130524     c                   clear                   parkdm
124000130524     c                   clear                   paresci
124100130524     c                   clear                   parerr
124200130524     c                   clear                   pariva
124300130524     c                   clear                   parcdf
124400130524     c                   call      'XALFA3BR'
124500130524     C                   PARM                    PARDUT
124600130524     C                   PARM                    codut
124700130524     C                   PARM                    parrag
124800130524     C                   PARM                    parkcc
124900130524     C                   PARM                    PARSTA
125000130524     C                   PARM                    PARFLR           90
125100130524     C                   PARM                    PARDIT            3
125200130524     C                   PARM                    PARNUM
125300130524     C                   PARM                    PARKCM
125400130524     C                   PARM                    PARKSM
125500130524     C                   PARM                    PARKDM
125600130524     C                   PARM                    PAResci
125700130524     C                   PARM                    PARerr
125800130524     C                   PARM                    PARiva
125900130524     C                   PARM                    PARcdf
126000130524     c
126100130524     c                   if        parsta<>-1 and parerr=*blanks
126200130524     c                   eval      W4cccm=parksm
126300130524     c                   eval      W4dccm=parrag
126400130524     c                   endif
126500130524
126600130524     c                   eval      wriem='S'
126700130524     c                   seton                                        90
126800130524     c                   goto      forw4
126900130524     c                   endif
127000130527     c*
127100130527      * Controllo se campo valorizzato
127200130527      * al momento campo oscurato e di fatto parzializzaz. non attiva
127300130527    1C                   IF        w4cccm<>*blanks  and w4cccm<>*zeros
127400130527     c
127500130527     c* controllo ksc
127600130527     c     c_digits      check     w4cccm                                 31
127700130527    2c                   if        *in31
127800130527     C                   SETON                                        2890
127900130527     C                   MOVEL     MSG(1)        w4cMSG
128000130527     C                   GOTO      forw4
128100130527    2C                   ENDIF
128200130527     c
128300130527     c                   movel     w4cccm        kksc
128400130527     c     kaco          chain     cnaco00f
128500130527    2c                   if        not %found(cnaco00f) or acoabl<>*blanks
128600130527     C                   SETON                                        2890
128700130527     C                   MOVEL     MSG(1)        w4cMSG
128800130527     C                   GOTO      forw4
128900130527     c                   else
129000130527     c                   movel     acorag        w4dccm
129100130527    2C                   ENDIF
129200130527     c
129300130527     c* Controllo che sia abilitato dalla tabella DCLI
129400130527     C                   CLEAR                   tibs02ds
129500130527     c                   clear                   dcli
129600130527     C                   MOVEL     'C'           T02MOD
129700130527     C                   MOVEL     KNSIF         T02SIF
129800130527     C                   MOVEL     'CLI'         T02COD
129900130527     c                   movel(P)  w4cccm        t02ke1
130000130527     C                   CALL      'TIBS02R'
130100130527     C                   PARM                    KPJBA
130200130527     C                   PARM                    tibs02ds
130300130527    2C                   IF        T02ERR = *BLANKS
130400130527     C                   MOVEL     T02uni        dcli
130500130527    2C                   ENDIF
130600130527    2c                   if        §clibrd<>'S'
130700130527     C                   SETON                                        2890
130800130527     C                   MOVEL     MSG(2)        w4cMSG
130900130527     C                   GOTO      forw4
131000130527    2C                   ENDIF
131100130527    1C                   ENDIF
131200130524     c
131300130527     c                   if        w4cccm>*zeros
131400130527     c                   seton                                        16
131500130527     c                   endif
131600130527     c
131700130524     c                   ENDSR
131800130527**
131900130527Codice Cliente errato o bloccato
132000130527Mittente non abilitato a border.x codice
