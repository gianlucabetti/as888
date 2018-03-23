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
005300970423     D  PA2CML               167    167
005400010910     D  PA2AGP               168    168
005500040121     D  PA2spv               169    169
005600040121     D  PA2spp               170    170
005700130527     D  PA2ccm               171    177
005800130531     D  PA2barcode           178    178    inz
005900020801      *
006000130502     D fnlst4ds        DS
006100130502     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
006200130502     D*          B - DA BORDERO'
006300130502     D*          A - apertura foglio
006400130502     D*          C - DA CHIUSURA FV SINGOLA
006500130502     D*          I - DA CHIUSURA DA IMP
006600130502     D*          N - DA INTERROGAZIONE  FV
006700130502     D*          M - Manutenzione FGV
006800130502     D*          P - Manutenzione FGV con videata piombi
006900130502
007000130502     D* CAMPI INPUT OBBLIGATORI:
007100130502     D*  GES / FGS
007200130502     d
007300130502     D* CAMPI INPUT PER MANUTENZIONE :
007400130502     D*  NFV
007500130502     D*
007600130502     D* CAMPI INPUT SOLO PER APERTURA :
007700130502     d* DFV /OLD / TRN / NRRP / PDR
007800130502     D
007900130502     D  ilst4DFV               3     10
008000130502     D
008100130502     D  ilst4nfv              11     15
008200130502     D  IlsT4GES              16     16
008300130502     D* Se MSG pieno --> c'è stato ERRORE
008400130502     D  olst4msg              17     94
008500130502     d* se iolst4FOR è pieno --> premuto F6 per forzare foglio di traini doppio
008600130502     D  iolst4FOR             95     97
008700130502     D
008800130502     D  IlsT4OLD              98     98
008900130502     D* OLST4F12 = 'S'    --> premuto F12
009000130502     D  olst4F12             102    102
009100130502     d
009200130502     D  ilst4fgs             103    105
009300130502     D  ilst4TRN             106    112  0
009400130502     D  ilst4NRRP            113    125
009500130502     D* se non immessa il numero relativo record della proposta
009600130502     d*  si può immettere a modo vecchio il trazionista
009700130502     D  ilst4PDR             126    132
009800020801      *
009900070423     D PARAMa3         DS
010000070423     D  PAANFV                 1      5
010100070423     D* pa3tfp deve contenere il terminal di partenza per poter elaborare e
010200070423     d* trasmettere le bolle per terminal di partenza
010300070423     d* se non passato un valore > '000' il programma elabora e trasmette
010400070423     d* TUTTE le bolle presenti nel file fnblp46f (come faceva prima)
010500070423     D  PA3tfp                 6      8
010600111123     D  PA3num                 9      9    inz('0')
010700111123     d
010800130523      * Parametri per Interrogazione FV Partenze - FNLSt3ds-
010900130523     D fnlst3ds        DS
011000951108     D  PA7FLG                 1      1    INZ('R')
011100951108     D  PA7FL2                 2      2    INZ('F')
011200951108     D  PA7NFV                 3      5P 0 INZ
011300951108     D  PA7DIN                 6     10P 0 INZ
011400951108     D  PA7DFI                11     15P 0
011500951108     D  PA7TFV                16     16    INZ('2')
011600000615     D  PA7FGS               242    244  0
011700020801      * - Schiera usata per passare al pgm chiamante tutti i
011800020801      *   Fogli Viaggio selezionati
011900951108     D  FVA                   17    241P 0 INZ
012000951108     D                                     DIM(45)
012100020801      *
012200020801     D wPOFV           DS             8    inz
012300020801     D  wLNP                   1      3  0 inz
012400020801     D  wNFV                   4      8  0 inz
012500020801      *
012600020801      * Parametri per Controllo Data & Trasform in giorni - XSRDA8 -
012700950107     D WLBDAT          DS                  INZ
012800950107     D  G02DAT                 1      8  0
012900950107     D  G02INV                 9     16  0
013000950107     D  G02ERR                17     17
013100950107     D  G02TGI                18     22  0
013200020801      *
013300020801      * D S   definite   e s t e r n a m e n t e
013400020801      *
013500900517     D KPJBA         E DS
013600020801      * DS per Interrogazione Fogli Vari
013700950109     D DSLR01        E DS                  INZ
013800020801      * DS per TRUL06R - Caricamento £X
013900020801     D TRUL06DS      E DS
014000950107     D  LIN                    1     90  0
014100940221     D                                     DIM(30)
014200000615     D OG143         E DS
014300130524     D OG148         E DS
014400130524     D OG150         E DS
014500130502     D tibs02ds      E DS
014600130527     D dcli          E DS
014800130705     d
014900130705     D TRUL13DS      E DS
015000020801      *
015100020801      * V a r i a b i l i
015200020801      *
015300020801     D KPJBU$          s                   like(KPJBU)  inz
015400020801      * Campi chiave fissi
015500130524     d codut           s              1  0 inz(1)
015600130527     d kksc            s                   like(acoksc)
015700020801     D KEPA            s                   like(FV2EPA) inz('P')
015800020801     D KLAI            s                   like(FV2LAI) inz
015900130424     D Ktrc            s                   like(FV4trc) inz
016000020801     D KTDH            s                   like(FV2TDH) inz('T')
016100020801     D PARNPG          s                   like(FVVNPG) inz(3)
016200020801     D COMNFV          s                   like(FVVNFV) inz
016300020801     D CO2NFV          s                   like(FGVNFV) inz
016400130523     D KIMP            s                   like(FGVNFV) inz
016500130627     d kcod            s                   like(tbecod)
016600130524     d pardut          s             30
016700130524     d parrag          s             48
016800130524     d parkcc          s              4  0
016900130524     d parsta          s              1  0
017000130524     d parflr          s             90
017100130524     d pardit          s              3
017200130524     d parnum          s              2  0
017300130524     d parkcm          s             80
017400130524     d parksm          s            140
017500130524     d parkdm          s             60
017600130524     d paresci         s              1
017700130524     d parerr          s              2
017800130524     d pariva          s             16
017900130524     d parcdf          s             16
018000020801      * Indici di schiera
018100020801     D XB              s              3  0 inz
018200020801     D XI              s              4  0 inz
018300020801      * Data e Ora
018400020801     D UTime           s              6  0 inz
018500020801     D Ora             s              2  0 inz
018600020801     D W0140           s             14  0 inz
018700020801     D UDate8          s              8  0 inz
018800020801      *
018900020801     D POWORK          s              3  0 inz
019000020801      *
019100020801     D NRR             s              5  0 inz
019200130524     D unicoNRR        s              5  0 inz
019300020801     D SAVNRR          s              5  0 inz
019400020801     D SAVNFC          s              3  0 inz
019500020801     D RISUL           s              5  0 inz
019600020801     D RESTO           s              3  0 inz
019700020801     D NUM5            s              5  0 inz
019800130424     D wpmb            s             48    inz
019900130705     d Dataiso         s               d   datfmt(*iso)
020000130705     d Datadmy         s               d   datfmt(*dmy)
020100130705
020200130524     d TIBS34ds      e ds
020300130524     d dDatiUte      e ds
020400130524     d AZUTEds       e ds                  extname(AZUTE00F)
020500130705
020600130705     D fnlsu3ds        DS
020700130705     D  ilsu3flg               1      1
020800130705     D  ilsu3aas               2      5
020900130705     D  ilsu3lnp               6      8
021000130705     D  ilsu3nrs               9     10
021100130705     D  ilsu3nsp              11     17
021200130705     D  ilsu3DSP_B            18     25
021300130705     D  ilsu3DSP_S            26     33
021400130705     D  ilsu3nos              34     34
021500130705     d
021600130705     d  ilsu3tcr              35     35
021700130705     d  ilsu3dcr              36     43
021800130705     d  ilsu3hcr              44     47
021900130705     D  olsu3err             100    101
022000130705     d
022100130524
022200130527     d c_Digits        c                   const('0123456789')
022300130613
022400130613      //---------------------------------------------------------------
022500130613      /copy gaitrasrc/srcprotopr,tibs02r
022600020801
022700020801      *---------------------------------------------------------------*
022800020801      * INDICATORI
022900020801      *---------------------------------------------------------------*
023000020801      *  01    - GESTISCO POSIZIONAMENTO CURSORE
023100020801      *  02    - NON EMETTO IL CAMPO DELL'ORA PER RECORD NUOVI
023200040121      *  03    - SIMFEL utilizza volume e/o peso in spunta
023300020801      *  04    - FOGLIO GIA' ABBINATO
023400020801      *  10    - ROLL UP
023500020801      *  15    - FOGLIO GIA' CHIUSO
023600130527      *  16    - F9 in alta intensità
023700130627      *  25    - Quando emetto SFL non emetto richiesta dei fogli
023800130627      *  26    - esiste cliente abilitato alla borderizzazione
023900020801      *  30/34 - DI COMODO
024000020801      *  43/51 - ERRORI
024100020801      *  67    - SCAN ?
024200020801      *  72/73 - DI SUBFILE
024300020801      *  90    - ERRORE GENERICO
024400020801      *---------------------------------------------------------------*
024500020801
024600130527     C     Kaco          KLIST
024700130527     C                   KFLD                    codut
024800130527     C                   KFLD                    dutkci
024900130527     C                   KFLD                    kksc
025000020801     C     KFVV          KLIST
025100020801     C                   KFLD                    PARNPG
025200020801     C                   KFLD                    COMNFV
025300020801     C                   KFLD                    VIDFGS
025400020801     C     KFV2          KLIST
025500020801     C                   KFLD                    FGVLNP
025600020801     C                   KFLD                    FGVNFV
025700020801     C                   KFLD                    KEPA
025800020801     C                   KFLD                    KLAI
025900020801     C                   KFLD                    KTDH
026000130424     C     KFV4          KLIST
026100130424     C                   KFLD                    fgvlnp
026200130424     C                   KFLD                    fgvNFV
026300130424     C                   KFLD                    KEPA
026400130424     C                   KFLD                    Klai
026500130424     C                   KFLD                    KTRC
026600020801     C     K02FGW01      klist
026700020801     C                   kfld                    FGWnfv
026800020801     C                   kfld                    FGWlnp
026900020801     C     K02FGV01      klist
027000020801     C                   kfld                    CO2NFV
027100020801     C                   kfld                    SIMFEL
027200020801     C     K02FGV07      klist
027300021025     C                   kfld                    SIMfel
027400130523     C                   kfld                    kimp
027500020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
027600000000     C     *ENTRY        PLIST
027700130524     C                   PARM                    KPJBA
027800130524      *
027900130524      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
028000130524     c     *dtaara       define    §azute        azuteds
028100130524     c     *dtaara       define    §datiute      ddatiute
028200130524      *
028300130524     c                   clear                   AzUteDs
028400130524     c                   clear                   DDatiUte
028500130524     c                   clear                   Tibs34Ds
028600130524     c                   in(E)     *dtaara
028700130524if  1c                   if        %Error  or  RSUT = *blanks
028800130524     c                   call      'TIBS34R'
028900130524     c                   parm                    Tibs34Ds
029000130524     c                   in        *dtaara
029100130524e   1c                   endif
029200020801     C*
029300020801     C* Carico L1 e impostazioni iniziali
029400911104     C                   EXSR      CARTAB
029500020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
029600020801     C* Prima videata
029700900517     C*
029800130524     C                   MOVEL     '?'           VIDNFV
029900130524     c                   eval      vidsel='SI'
030000130711     c                   eval      vidaut='  '
030100130627     c                   seton                                        67
030200911008     C*
030300900509     C     FOR01         TAG
030400130524     c
030500130705     c                   clear                   viddsosp
030600130705
030700130524     c                   if        *in67
030800130524     C                   WRITE     lsT6D03
030900130524     c                   endif
031000130524     c
031100130524     C                   SETOFF                                       05
031300130523     C                   EXFMT     lsT6D01
031400020801     C* F03 = Fine Lavoro
031500130902     c                   if        *inkc
031600130613     C                   GOTO      FINE
031700130613     c                   endif
031800130710     c*
031900130710     c                   if        *inkk=*on
032000130710     c                   Clear                   tibs02ds
032100130710     c                   Eval      T02Mod = 'R'
032200130710     c                   Eval      T02Sif = Knsif
032300130710     c                   Eval      T02Cod = 'BSP'
032400130710     c                   Call      'TIBS02R'
032500130710     c                   Parm                    Kpjba
032600130710     c                   Parm                    tibs02ds
032700130710     c                   seton                                        67
032800130710     C                   GOTO      FOR01
032900130710    2c                   endif
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
047800960209     C     §OGAGS        IFEQ      'E'
047900960209     C                   MOVEL     'S'           PA2AGS
048000010910     C                   MOVEL     §OGAGP        PA2AGP
048100960209     C                   ELSE
048200960209     C                   CLEAR                   PA2AGS
048300010910     C                   CLEAR                   PA2AGP
048400960209     C                   ENDIF
048500911104     C*
048600020801     C* Controllo se sono Filiale di Primo o Secondo Livello
048700130523     C                   Z-ADD     SIMfel        VIDFGS
048800130524     c     simfel        chain     azorg01l
048900130524     c                   if        %found(azorg01l)
049000130524     c                   movel     orgdes        desfgs
049100130524     c                   endif
049200130613     C                   movel     vidfgs        alfafgs           3
051700000615     C*
051800911104     C                   ENDSR
051900020801      *
052000020801      *--- PULIZIA SUBFILE -------------------------------------------*
052100020801      *
052200911008     C     PULSFL        BEGSR
052300130523     C                   SETOFF                                       0102
052400130523     C                   CLEAR                   lsT6DSF
052500130524     c
052600130627     C     XB            DOWLE     16
052700130524     c                   if        primoV=' '
052800130524     c                   seton                                        60
052900130701     C     nrr           add       1             rec
053000130524     c                   eval      primoV='N'
053100130524     c                   endif
053200130524     c
053300020801     C                   ADD       1             NRR
053400130523     C                   WRITE     lsT6DSF
053500130524     c                   setoff                                       60
053600020801     C                   ADD       1             XB
053700020801     C                   ENDDO
053800020801     C* Salvo l'ultimo NRR
053900020801     C                   Z-ADD     NRR           SAVNRR
054000130701     C***                Z-ADD     SAVNRR        REC
054100930129     C                   SETON                                        01
054200911008     C                   ENDSR
054300020801      *
054400020801      *--- CONTROLLI FOGLIO VIAGGIO ----------------------------------*
054500020801      *
054600911008     C     CONTR1        BEGSR
054700020801     C*
054800911008     C                   SETOFF                                       303190
054900130524     C                   SETOFF                                       4767
055000130524     c                   clear                   primoV
055100020801     C* P.O. in gestione
055200020801     C* .. diverso da 0
055300020801if  1C     VIDFGS        IFEQ      0
055400130524     C                   SETON                                        90
055500000704     C                   GOTO      ENDCT1
055600020801     C* .. presente in L1
055700020801x   1C                   ELSE
055800000704     C     VIDFGS        LOOKUP    L1                                     34
055900130524     C  N34              SETON                                        90
056000000704     C   90              GOTO      ENDCT1
056100020801e   1C                   ENDIF
056200020801     C*
056300130523     C* Numero Foglio IMP
056400020801if  1C     VIDNFV        IFEQ      *BLANKS
056500911008     C     VIDNFV        OREQ      *ZEROS
056600911008     C                   SETON                                        4090
056700911008     C                   GOTO      ENDCT1
056800911008     C*
056900020801x   1C                   ELSE
057000020801     C* Ricerca
057100940218     C     '?'           SCAN      VIDNFV                                 90
057200020801if  2C     *IN90         IFEQ      *ON
057300950107     C                   Z-ADD     0             DLRNFV
057400950107     C                   MOVEL     'S'           DLRRIC
057500950107     C                   MOVEL     '3'           DLRNPG
057600950107     C                   MOVEL     31129999      DLRGAL
057700130523     C                   MOVEL     'FNlST6R'     DLRPGM
057800000615     C                   MOVEL     VIDFGS        DLRFGS
057900950107     C                   MOVEL     '2'           DLRTFV
058000950107     C                   MOVEL     DSLR01        KPJBU
058100950107     C                   CALL      'FNLR02R'
058200911008     C                   PARM                    KPJBA
058300950107     C                   MOVEL     KPJBU         DSLR01
058400950107     C                   MOVEL     DLRNFV        VIDNFV
058500130523     C                   WRITE     lsT6D03
058600000620     C                   SETON                                        67
058700911008     C                   GOTO      ENDCT1
058800020801e   2C                   ENDIF
058900020801     C* Controlli
059000911008     C                   TESTN                   VIDNFV               31
059100911010     C  N31              SETON                                        30
059200911008     C*
059300020801if  2C     *IN30         IFEQ      *OFF
059400940218     C                   MOVEL     VIDNFV        COMNFV
059500130705     C     KFVV          CHAIN     FNFVV000                           30
059600020801     C* Escludo: 1) gli Annullati
059700020801     C*          2) ciò che NON è IMP
059800020801     C*          3) i Chiusi
059900020801if  3C     FVVATB        IFNE      *BLANKS
060000930524     C     FVVSPG        ORNE      'P'
060100930524     C     FVVFCF        ORNE      *BLANKS
060200911008     C                   SETON                                        30
060300020801e   3C                   ENDIF
060400020801e   2C                   ENDIF
060500911008     C*
060600911008     C   30              SETON                                        4090
060700911008     C   30              GOTO      ENDCT1
060800020801e   1C                   ENDIF
060900020801     C*
061000130523     C* Giro Data Foglio IMP
061100950109     C                   CLEAR                   WLBDAT
061200950109     C                   MOVEL     '3'           G02ERR
061300950109     C                   Z-ADD     FVVDFV        G02INV
061400950109     C                   CALL      'XSRDA8'
061500950109     C                   PARM                    WLBDAT
061600950109     C                   Z-ADD     G02DAT        VIDDFV
061700130705     c*
061800130705     c* Calcolo la data sospensione
061900130711    2c**                 if        vidaut='SI'
062000130711     c**                 clear                   trul13ds
062100130711     c**                 Eval      I13Tla = 'L'
062200130711     c**                 Eval      I13Mod = 'P'
062300130711     c**                 Eval      I13Fil = simfel
062400130711     c**                 Eval      I13Din = fvvdfv
062500130711     c**                 Eval      I13gio = 1
062600130711     c**                 Call      'TRUL13R'
062700130711     c**                 Parm                    Trul13ds
062800130711     c**
062900130711    3c**                 if        o13err<>' '
063000130711     c**                 seton                                        9045
063100130711     c**                 leavesr
063200130711    3c**                 endif
063300130711     c**                 movel     o13dfi        dataiso
063400130711     c**                 movel     dataiso       datadmy
063500130711     c**                 movel     datadmy       viddsosp
063600130711    2c**                 endif
063700020801     C*
063800020801     C* Subfile : pulizia
063900911107     C                   SETON                                        72
064000130523     C                   WRITE     lsT6DCT
064100130705     C                   SETOFF                                       7247
064200911107     C*
064300950107     C                   Z-ADD     0             NRR
064400950107     C                   CLEAR                   VI2SCE
064500950109     C                   CLEAR                   VH2FLG
064600970603     C                   CLEAR                   VH2FL2
064700950109     C                   CLEAR                   VH2DTP
064800130524     c
064900130524     c* Carico la £6 di simfel alla data foglio IMP
065000130524     c                   clear                   trul06ds
065100130524     c                   eval      d06cod = '£6'
065200130524     c                   movel     simfel        d06key
065300130524     C                   MOVE      'S'           D06KEY
065400130524     C                   MOVEl     fvvdfv        D06drf
065500130524     c                   movel     trul06ds      kpjbu
065600130524     c                   call      'TRUL06R'
065700130524     c                   parm                    kpjba
065800130524     c                   movel     kpjbu         trul06ds
065900130524     c                   movea     lin           l6
066000130627     c*
066100130627     c* Verifico se ha clienti abilitati alla borderizzazione per codice
066200130627     c                   setoff                                       26
066300130627     c                   eval      kcod='CLI'
066400130627     c     kcod          setll     tntbe01l
066500130627     c     kcod          reade     tntbe01l
066600130627     c                   dow       not %eof(tntbe01l)
066700130627     c                   if        tbeatb=' '
066800130627     c                   movel     tbeuni        dcli
066900130627     c
067000130627     c                   if        §clibrd='S'
067100130627     c                   movel     tbeke1        filcli            3 0
067200130627     c     filcli        lookup    L6                                     26
067300130627     c  n26filcli        lookup    L1                                     26
067400130627     c
067500130627     c                   if        *in26
067600130627     c                   leave
067700130627     c                   endif
067800130627     c                   endif
067900130627     c                   endif
068000130627     c
068100130627     c     kcod          reade     tntbe01l
068200130627     c                   enddo
068300130524     c*
068400020801     C*
068500130523     c* Prima carico i fogli già abbinati al foglio IMP
068600020801     C* Subfile : caricamento
068700130523     c                   eval      kimp=comnfv
068800020801     C     K02FGV07      SETLL     FNFGV007
068900020801     C     K02FGV07      READE     FNFGV007                               30
069000911008     C*
069100911108     C                   SETON                                        04
069200020801do  1C                   DOW       NOT *IN30
069300940217     C                   SETOFF                                       15
069400911031     C                   MOVEL     FGVNFV        VI2NFV
069500020801     C                   CLEAR                   VI2FCF
069600020801     C                   IF        FGVFCF <> *BLANKS
069700911107     C                   MOVEL     'C'           VI2FCF
069800940217     C                   SETON                                        15
069900020801     C                   ENDIF
070000020801     C* Decodifiche Foglio
070100950109     C                   EXSR      DECOD
070200911031     C*
070300911107     C                   ADD       1             NRR
070400020801     C                   z-add     FGVlnp        wLNP
070500020801     C                   z-add     FGVnfv        wNFV
070600020801     C                   move      wPOFV         FS(NRR)
070700130523     C                   WRITE     lsT6DSF
070800911008     C*
070900020801     C     K02FGV07      READE     FNFGV007                               30
071000020801e   1C                   ENDDO
071100940217     C                   SETOFF                                       0415
071200911031     C*
071300130523     c* Poi carico i foglio, se richeista, aperti e non ancora abbianti
071400130523     c*  a foglio IMP
071500130523     c                   if        vidsel='SI'
071600130523     c                   exsr      ricercaFGV
071700130523     c                   endif
071800130523     c
071900020801     C* Finisco di pulire il subfile
072000020801     C                   Z-ADD     NRR           SAVNFC
072100130627     C     NRR           DIV       16            RISUL
072200020801     C                   MVR                     RESTO
072300930129     C*
072400020801     C     RESTO         ADD       1             XB
072500911008     C                   EXSR      PULSFL
072600130627     C     SAVNFC        IFEQ      16
072700930129     C                   Z-ADD     1             REC
072800020801     C                   ENDIF
072900130523     c                   seton                                        25
073000911008     C*
073100911008     C     ENDCT1        ENDSR
073200130523      *
073300130523      *--- ricerca fogli viaggio ancora da chiudere ------------------*
073400130523     C     RicercaFGV    BEGSR
073500130523     c                   clear                   kimp
073600130524     C                   SETOFF                                       15
073700130523     c     k02fgv07      setll     fnfgv07l
073800130523     c     k02fgv07      reade     fnfgv07l
073900130523     c
074000130523     c                   dow       not %eof(fnfgv07l)
074100130523     c* elaboro i fogli ancora aperti, non annullati
074200130523     c*  ed  escludo i fogli D H
074300130523     c                   if        fgvfcf=' ' and fgvatb=' '  and
074400130523     c                             fgvttr<>'D' and fgvttr<>'H'
074500130701     c* escludo i fogli con data fogio > data foglio IMP
074600130701     c                   if        fgvdfv<=fvvdfv
074700130523     C* Decodifiche Foglio
074800130523     C*
074900130701     c* posizinamento cursore sul primo foglio che carico
075000130701     c                   if        primoV=' '
075100130701     c                   seton                                        60
075200130701     c     nrr           add       1             rec
075300130701     c                   eval      primoV='N'
075400130701     c                   endif
075500130701
075600130524     C                   MOVEL     FGVNFV        VI2NFV
075700130524     C                   EXSR      DECOD
075800130524     C                   CLEAR                   VI2FCF
075900130523     C                   ADD       1             NRR
076000130523     C                   z-add     FGVlnp        wLNP
076100130523     C                   z-add     FGVnfv        wNFV
076200130523     C                   move      wPOFV         FS(NRR)
076300130523     C                   WRITE     lsT6DSF
076400130701     c                   setoff                                       60
076500130523     C*
076600130523     c                   endif
076700130701     c                   endif
076800130523     c
076900130523     c     k02fgv07      reade     fnfgv07l
077000130523     c                   enddo
077100130523     c
077200130523     c                   ENDSR
077300020801      *
077400020801      *--- CONTROLLO SUB FILE ----------------------------------------*
077500020801      *
077600911008     C     CTRSFL        BEGSR
077700020801     C*
077800911031     C                   SETOFF                                       9045
077900950109     C                   Z-ADD     1             NRR
078000020801     C                   CLEAR                   FS
078100130524     c                   clear                   primoV            1
078200130524     c                   clear                   contaFGV          5 0
078300911009     C*
078400020801     C* Controllo TUTTO il subfile
078500020801do  1C     NRR           DOWLE     SAVNRR
078600130523     C     NRR           CHAIN     lsT6DSF                            30
078700940218     C*
078800020801     C* Se riempito: controllo
078900020801if  2C                   IF        not *in30 and
079000020801     C                             VI2FCF <> 'C'
079100950109     C*
079200020801if  3C     VI2NFV        IFNE      *ZEROS
079300911105     C     VI2NFV        ANDNE     *BLANKS
079400950109     C                   SETON                                        01
079500911009     C*
079600020801     C* Ricerca in tutti i Fogli Viaggio
079700940218     C     '?'           SCAN      VI2NFV                                 32
079800020801if  4C     *IN32         IFEQ      *ON
079900940218     C                   EXSR      RICNFV
080000940218     C                   SETON                                        90
080100130523     C                   WRITE     lsT6D02
080200130523     C                   WRITE     lsT6D01
080300911009     C                   GOTO      ENDCTS
080400020801e   4C                   ENDIF
080500020801     C*
080600020801     C* Controlli
080700020801     C*
080800911104     C                   TESTN                   VI2NFV               31
080900130524     c                   if        not *in31
081000130524     C                   SETON                                        469060
081100130524     C                   GOTO      ENDCTS
081200130524     c                   endif
081300911010     C*
081400020801     C                   Z-ADD     0             CO2NFV
081500911031     C                   MOVEL     VI2NFV        CO2NFV
081600020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
081700911010     C*
081800020801     C* Annullato o già chiuso
081900020801if  4C                   IF        not  *in31
082000020801     C                             and (FGVatb <> *blanks
082100020801     C                              or  FGVfcf <> *blanks)
082200911031     C                   SETON                                        31
082300020801e   4C                   ENDIF
082400020801     C*
082500020801     C* Errore
082600130524     c                   if        *in31
082700130524     C                   SETON                                        469060
082800130524     C                   GOTO      ENDCTS
082900130524     c                   endif
083000020801     C*
083100020801     C* Non è possibile chiudere un FV defluenza se LNA estero
083200020801     C*  o DPD o FEDEX
083300020801if  4C                   IF        not  *in31
083400020801     C                             and (FGVTTR = 'D'
083500020801     C                              or  FGVTTR = 'H')
083600990514     C     FGVLNA        CHAIN     AZORG                              31
083700020801if  5C                   IF        not *in31
083800020801     C                   MOVEL     ORGDE3        OG143
083900020801x   5C                   ELSE
084000020801     C                   CLEAR                   OG143
084100020801e   5C                   ENDIF
084200020801if  5C                   IF        not  *in31
084300020801     C                             and (§OGNTW = 'EEX'
084400020801     C                              or  §OGNTW = 'EUP'
084500020801     C                              or  §OGNTW = 'FED'
084600020801     C                              or  §OGNTW = 'DPD')
084700990514     C                   SETON                                        31
084800020801e   5C                   ENDIF
084900020801e   4C                   ENDIF
085000020801     C* Errore
085100130524     c                   if        *in31
085200130524     C                   SETON                                        519060
085300130524     C                   GOTO      ENDCTS
085400130524     c                   endif
085500130527     c* se inserito cod cliente o premuto F9  DEVE essere foglio locale
085600130527     c*  (almeno per ora...)
085700130527     c                   if        w4cccm>*zeros or *inki
085800130527     c     fgvlna        lookup    L6                                     31
085900130527     c                   if        not *in31
086000130527     C                   SETON                                        549060
086100130527     C                   GOTO      ENDCTS
086200130527     c                   endif
086300130527     c                   endif
086400911031     C*
086500020801     C* Memorizzo nella schiera dei FV selezionati
086600020801     C*
086700020801     C                   z-add     SIMfel        wLNP
086800020801     C                   z-add     CO2nfv        wNFV
086900020801     C     wPOFV         LOOKUP    FS                                     31
087000130524     C   31              SETON                                        479060
087100911031     C   31              GOTO      ENDCTS
087200020801     C                   move      wPOFV         FS(NRR)
087300020801     C*
087400020801     C* Richiamo la Manutenzione FV
087500020801     C*
087600020801if  4C     VI2SCE        IFEQ      '1'
087700130502     c                   clear                   fnlst4ds
087800130502     c                   eval      ilst4ges='I'
087900130502     c                   eval      ilst4fgs=%editc(vidfgs:'X')
088000130502     c                   eval      ilst4nfv=%editc(co2nfv:'X')
088100130502     c                   movel     fnlst4ds      kpjbu
088200130502     C                   CALL      'FNLST4R'
088300130502     C                   PARM                    KPJBA
088400130502     C                   MOVEL     KPJBU         fnlst4ds
088500130523
088600020801     C* Se PA3F12 <> 'S' non è stato premuto F12 -> riaggancio il foglio
088700020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
088800020801     C* Riemetto la videata
088900950109     C                   SETON                                        90
089000130523     C                   WRITE     lsT6D02
089100130523     C                   WRITE     lsT6D01
089200950109     C                   CLEAR                   VI2SCE
089300020801e   4C                   ENDIF
089400020801     C*
089500020801     C* Decodifiche
089600950109     C                   EXSR      DECOD
089700130524     C* Se Data Foglio <> Data Foglio IMP -> Errore
089800130524     C*  (esclusi i Fogli di Defluenza)
089900130524if  4C     FGVTTR        IFNE      'D'
090000130524     C     FGVTTR        ANDNE     'H'
090100130524     C     FGVDFV        ANDNE     FVVDFV
090200130524     C                   SETON                                        509060
090300130524     C                   GOTO      ENDCTS
090400130524e   4C                   ENDIF
090500020801     C*
090600130524     c* Dati obbligatori da borderò
090700130524     c                   exsr      Datiobbl
090800130524     c
090900130524     c                   if        *in90
091000130524     C                   SETON                                        529060
091100130524     C                   MOVEL     '1'           VI2SCE
091200130524     C                   GOTO      ENDCTS
091300130524     c                   endif
091400020801     C* Se mancano i Piombi o la Data Patrenza:
091500020801     C*  Errore vincolante: devo andare in manutenzione a modificarli
091600130424if  4C                   IF           (wpmb =  *blanks
091700020801     C                             or  FGVdtp =  *zeros)
091800020801     C                             and FGVttr <> 'L'
091900130524     C                   SETON                                        489060
092000950324     C                   MOVEL     '1'           VI2SCE
092100950107     C                   GOTO      ENDCTS
092200020801e   4C                   ENDIF
092300130524     c
092400130524     c
092500020801     C* Se Data Partenza < Data Foglio IMP -> Errore forzabile
092600020801if  4C     FGVDTP        IFNE      *ZERO
092700950324     C     FGVDTP        ANDNE     VH2DTP
092800950109     C                   CLEAR                   VH2FLG
092900020801e   4C                   ENDIF
093000950109     C*
093100020801if  4C     VH2FLG        IFEQ      *BLANKS
093200960129     C                   MOVEL     FGVDTP        VH2DTP
093300020801if  5C     FGVDTP        IFNE      *ZERO
093400950324     C     FGVDTP        ANDLT     FVVDFV
093500130524     C                   SETON                                        499060
093600950109     C                   MOVEL     '1'           VH2FLG
093700950109     C                   GOTO      ENDCTS
093800020801e   5C                   ENDIF
093900020801e   4C                   ENDIF
094000020801     C*
094100130524     c* conto i fogli aperti da chiudere
094200130524     c                   add       1             contafgv
094300130524     c                   z-add     nrr           unicoNRR
094400130524     c
094500020801     C* Segnalazione errore a video
094600911009     C     ENDCTS        TAG
094700920805     C   90              Z-ADD     NRR           REC
094800130523     C  N32              UPDATE    lsT6DSF
094900920805     C   90              Z-ADD     SAVNRR        NRR
095000130524     c                   setoff                                       60
095100020801     C*
095200020801x   3C                   ELSE
095300020801     C*
095400020801     C* Record vuoto del SFL
095500130524     C                   CLEAR                   lsT6DSF
095600130524     c
095700130523     C                   UPDATE    lsT6DSF
095800130524     c                   setoff                                       60
095900020801     C*
096000020801e   3C                   ENDIF
096100020801     C*
096200020801e   2C                   ENDIF
096300911031     C*
096400911031     C                   ADD       1             NRR
096500020801e   1C                   ENDDO
096600920805     C*
096700020801     C* Se non rilevati errori: posizionamento cursore sull'ultimo rec.
096800130524if  1C                   IF        not *in90
096900130524     c                   z-add     1             nrr
097000130524do  1C     NRR           DOWLE     SAVNRR
097100130524     C     NRR           CHAIN     lsT6DSF                            30
097200130524if  3C     VI2NFV        IFNE      *ZEROS
097300130524     C     VI2NFV        ANDNE     *BLANKS
097400130524     c                   add       1             nrr
097500130524     c                   else
097600130524     c                   seton                                        60
097700130524     c                   z-add     nrr           rec
097800130524     C                   UPDATE    lsT6DSF
097900130524     c                   leave
098000130524e   1C                   ENDIF
098100130524e   1C                   ENDdo
098200130524     c
098300130524e   1C                   ENDIF
098400130524     c                   setoff                                       60
098500911008     C*
098600911009     C                   ENDSR
098700020801      *
098800020801      * RICERCA E CARICAMENTO FOGLI VIAGGIO --------------------------*
098900020801      *
099000940218     C     RICNFV        BEGSR
099100020801     C*
099200130523     C                   RESET                   fnlst3ds
099300020801     C* Come data finale imposto la data del giorno
099400951108     C                   Z-ADD     UDATE8        PA7DFI
099500000615     C                   Z-ADD     VIDFGS        PA7FGS
099600130523     C                   MOVEL     fnlst3ds      KPJBU
099700130523     C                   CALL      'FNLST3R'
099800940218     C                   PARM                    KPJBA
099900940218     C*
100000130523     C                   MOVEL     KPJBU         fnlst3ds
100100940218     C*
100200020801     C* Se selezionati FV li scrivo
100300940218     C                   MOVEL     *BLANKS       VI2NFV
100400130523     C                   UPDATE    lsT6DSF
100500940218     C*
100600940218     C                   SUB       1             NRR
100700020801     C                   Z-ADD     1             XI
100800940218     C*
100900020801do  1C     XI            DOWLE     45
101000020801     C     FVA(XI)       ANDNE     *ZEROS
101100020801     C*
101200940218     C                   ADD       1             NRR
101300130523     C     NRR           CHAIN     lsT6DSF                            30
101400940218     C*
101500020801     C* Creo nuova pagina del subfile
101600020801if  eC                   IF        *IN30
101700020801     C                   Z-ADD     NRR           NUM5
101800940218     C                   SUB       1             NRR
101900020801     C                   Z-ADD     1             XB
102000940218     C                   EXSR      PULSFL
102100940218     C                   Z-ADD     NUM5          NRR
102200130523     C     NRR           CHAIN     lsT6DSF                            30
102300020801     C*
102400020801e   2C                   ENDIF
102500940218     C*
102600020801     C* Memorizzo solo se è vuoto, altrimenti su altra riga
102700020801if  2C     VI2NFV        IFLT      *ZEROS
102800020801     C                   MOVEL     FVA(XI)       VI2NFV
102900020801     C                   MOVE      FVA(XI)       VI2LNA
103000950109     C                   MOVEL     VI2NFV        CO2NFV
103100020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
103200950109     C                   EXSR      DECOD
103300940218     C*
103400130523     C                   UPDATE    lsT6DSF
103500020801     C                   ADD       1             XI
103600020801e   2C                   ENDIF
103700020801     C*
103800020801e   1C                   ENDDO
103900020801     C*
104000020801     C* Esco
104100940218     C     NRR           IFLT      1
104200940218     C                   Z-ADD     1             NRR
104300020801     C                   ENDIF
104400020801     C*
104500940218     C                   ENDSR
104600020801      *
104700020801      *--- ELABORAZIONE ABBINAMENTO ----------------------------------*
104800020801      *
104900911008     C     ELAB          BEGSR
105000130705     C                   CLEAR                   KQIEB
105100130705     C                   CLEAR                   KQEBI
105200020801     C*
105300130705     c* sottometto sospensione automatica
105400130711     c**                 if        vidaut='SI'
105500130711     c**                 clear                   fnlsu3ds
105600130711     c**                 eval      ilsu3dsp_B=%editc(fvvdfv:'X')
105700130711     c**                 eval      ilsu3dsp_S=%editc(o13dfi:'X')
105800130711     c**                 eval      ilsu3nos='S'
105900130711     c**                 movel     fnlsu3ds      kpjbu
106000130711     C**                 MOVEL     'LSU3'        KCOAZ
106100130711     C**                 CALL      'BCH10'
106200130711     C**                 PARM                    KPJBA
106300130711e   1C**                 ENDIF
106400130705     c
106500950125     C                   Z-ADD     COMNFV        PA2NFV
106600950125     C                   Z-ADD     FVVDFV        PA2DFV
106700950125     C                   Z-ADD     FVVFGS        PA2FGS
106800950125     C                   CLEAR                   PA2NUM
106900040121     C* *IN03 = *ON : utilizza peso e/o volume in spunta
107000020801     C* --> sottometto azione di calcolo Volumi e Peso
107100020801if  1C     *IN03         IFEQ      *ON
107200960209     C                   CLEAR                   KPJBU
107300961008     C                   CLEAR                   PA1NUM
107400960209     C                   MOVEL     PARAM1        KPJBU
107500950107     C                   MOVEL     'LSB7'        KCOAZ
107600931001     C                   CALL      'BCH10'
107700931001     C                   PARM                    KPJBA
107800020801e   1C                   ENDIF
107900911104     C*
108000911107     C                   Z-ADD     1             NRR
108100020801do  1C     NRR           DOWLE     SAVNRR
108200020801     C*
108300130523     C     NRR           CHAIN     lsT6DSF                            30
108400020801     C* Fogli Viaggio selezionati NO chiusi
108500020801     C* -> Sottometto chiusura FV
108600020801if  2C                   IF        not *in30
108700020801     C                             and VI2NFV > *ZEROS
108800020801     C                             and VI2FCF = *BLANKS
108900931001     C                   EXSR      LANCIO
109000020801e   2C                   ENDIF
109100911104     C*
109200911104     C                   ADD       1             NRR
109300020801e   1C                   ENDDO
109400911104     C*
109500911008     C                   ENDSR
109600020801      *
109700020801      *--- LANCIO CHIUSURA FOGLIO VIAGGIO ----------------------------*
109800020801      *
109900911104     C     LANCIO        BEGSR
110000020801     C*
110100020801     C* Aggiorno FV
110200940505     C                   MOVEL     VI2NFV        CO2NFV
110300020801     C*
110400020801     C     K02FGV01      CHAIN     FNFGV001                           33
110500950107     C                   MOVEL     VIDNFV        FGVNFA
110600960129     C                   MOVEL     VH2DTP        FGVDTP
110700960129     C                   MOVEL     VI2HMP        FGVHMP
110800950107     C                   UPDATE    FNFGV001
110900020801     C*
111000020801     C* Sottometto Chiusura FV
111100080228     C* Carico tabella Filiali Gestite £1 alla data foglio viaggio
111200080228     C                   CLEAR                   TRUL06DS
111300080228     C                   MOVE      '£1'          D06COD
111400080228     C                   MOVEL     ' '           D06TLA
111500080228     C                   MOVEL     SIMFEL        D06KEY
111600080228     C                   MOVE      'S'           D06KEY
111700080228     C                   MOVEl     fgvdfv        D06drf
111800080228     C                   MOVEL     TRUL06DS      KPJBU
111900080228     C                   CALL      'TRUL06R'
112000080228     C                   PARM                    KPJBA
112100080228     C                   MOVEL     KPJBU         TRUL06DS
112200080228     C                   MOVEA     LIN           L1ela
112300130527     c
112400130527     c                   if        w4cccm>*zeros
112500130527     c                   movel     w4cccm        pa2ccm
112600130527     c                   else
112700130527     c                   clear                   pa2ccm
112800130527     c                   endif
112900080228     c
113000130524     C                   MOVEL     fnlsu2ds      KPJBU
113100130531     C                   MOVEL     'LSU2'        KCOAZ
113200130227     c                   if        knmus=*all'1'
113300130524     C                   CALL      'FNLSU2R'
113400130227     C                   PARM                    KPJBA
113500130227     c                   else
113600130227     C                   CALL      'BCH10'
113700911104     C                   PARM                    KPJBA
113800130227     c                   endif
113900020801     C*
114000911104     C                   ENDSR
114100020801      *
114200020801      * DECODIFICHE FOGLIO -------------------------------------------*
114300020801      *
114400950109     C     DECOD         BEGSR
114500020801     C*
114600950109     C                   MOVEL     FGVLNA        VI2LNA
114700950109     C     FGVLNA        CHAIN     AZORG                              31
114800950109     C   31              MOVEL     *BLANKS       DESLNA
114900950109     C  N31              MOVEL     ORGDES        DESLNA
115000130424     c*
115100130424     C* Piombi
115200130424     c                   clear                   wpmb
115300130424     C                   MOVEL     'P'           kTRC
115400130424  155C     KFV4          SETLL     FNFV401L
115500130424  156C     KFV4          READE     FNFV401L
115600130424  156C                   IF        not %eof(fnfv401l) and fv4atb=' '
115700130424     C                   MOVEL(p)  FV4NOT        Wpmb
115800130424  156C     KFV4          READE     FNFV401L
115900130424  156C                   IF        not %eof(fnfv401l) and fv4atb=' '
116000130424  156C                   eval      %subst(Wpmb:36)=fv4not
116100130424  156C                   ENDIF
116200130424     C                   ENDIF
116300130424     c                   movel     wpmb          vi2pmb
116400130424
116500950109     C                   MOVEL     FGVHMP        VI2HMP
116600960201     C                   MOVEL     FGVTTR        VH2TTR
116700951031     C*
116800020801     C* Giro Data Partenza Reale
116900020801     C* Se NON c'è prendo la data teorica se c'è
117000020801if  1C     FGVDTP        IFEQ      0
117100960129     C     KFV2          CHAIN     FNFV2000                           31
117200020801if  2C     *IN31         IFEQ      *OFF
117300960129     C                   MOVEL     FV2DPA        FGVDTP
117400960129     C                   MOVEL     FV2HPA        VI2HMP
117500020801e   2C                   ENDIF
117600020801e   1C                   ENDIF
117700960129     C*
117800950109     C                   CLEAR                   WLBDAT
117900950109     C                   MOVEL     '3'           G02ERR
118000950109     C                   Z-ADD     FGVDTP        G02INV
118100950109     C                   CALL      'XSRDA8'
118200950109     C                   PARM                    WLBDAT
118300950109     C                   MOVEL     G02DAT        VI2DTP
118400020801     C* Giro Data Foglio
118500950109     C                   CLEAR                   WLBDAT
118600950109     C                   MOVEL     '3'           G02ERR
118700950109     C                   Z-ADD     FGVDFV        G02INV
118800950109     C                   CALL      'XSRDA8'
118900950109     C                   PARM                    WLBDAT
119000950109     C                   MOVEL     G02DAT        VI2DFV
119100020801     C* Ora Partenza Reale
119200960129     C     VI2HMP        IFGT      0
119300960129     C                   SETON                                        02
119400960129     C                   ELSE
119500960129     C                   SETOFF                                       02
119600960129     C                   ENDIF
119700130524     c
119800020801     C* Se Foglio con Tipo Traino inoltre evidenzio la Linea di Arrivo
119900960201     C     VH2TTR        IFEQ      'I'
120000960201     C                   SETON                                        06
120100960201     C                   ELSE
120200960201     C                   SETOFF                                       06
120300960201     C                   ENDIF
120400960201     C*
120500960129     C                   ENDSR
120600130524     C**************************************************************************
120700130524     C* CONTROLLO la presenza sul foglio viaggio dei dati obbligatori in border
120800130524     C**************************************************************************
120900130524     c     Datiobbl      begsr
121000130524     c                   setoff                                       90
121100130524     c* reperisco il numero di cellulare
121200130524     c                   clear                   wcell
121300130524     c                   eval      kepa='P'
121400130524     c                   eval      klai=0
121500130524     c                   eval      ktrc='L'
121600130524     c     kfv4          chain     fnfv401l
121700130524     c                   if        %found(fnfv401l)
121800130524     c                   movel     fv4not        wcell            16
121900130524     c                   endif
122000130524     c*
122100130524     c                   if        fgvtmz=*blanks or
122200130524     c                             (fgvpdr=*zeros and fgvdpd=*blanks) or
122300130524     c                             (fgvpmb=*blanks and fgvttr<>'L') or
122400130524     c                             (fgvtrm=*blanks and fgvtrr=*blank and
122500130524     c                              fgvttr<>'L') or
122600130524     c                             (wcell=*blanks and fgvttr<>'L')
122700130524     c                   seton                                        90
122800130524     c                   endif
122900130524     c                   endsr
123000130524     C**************************************************************************
123100130524     C* CONTROLLO parzializzaioni
123200130524     C**************************************************************************
123300130524     c     CtrParz       begsr
123400130527     c                   setoff                                       16
123500130524     c
123600130524     c     forw4         tag
123700130524     c
123800130524     c                   if        wriem='S'
123900130524     C                   WRITE     lsT6D02
124000130524     C                   WRITE     lsT6D01
124100130524     C                   write     lsT6DCT
124200130524     c                   endif
124300130524     c
124400130524     c                   exfmt     lst6w04
124500130524     c
124600130527     c                   setoff                                       2890
124700130524     c                   clear                   wriem             1
124800130524     c
124900130524     C* Verifico se richiesta la ricerca alfabetica
125000130524     c                   if        W4dccm<>*blanks and W4cccm<=*zeros
125100130524     c                   eval      pardut=rsut
125200130524     c                   clear                   parrag
125300130524     c                   eval      parrag=W4dccm
125400130524     c                   eval      parkcc=dutkci
125500130524     c                   eval      parsta=*zeros
125600130524     c                   clear                   parflr
125700130524     c                   clear                   pardit
125800130524     c                   eval      parnum=1
125900130524     c                   clear                   parkcm
126000130524     c                   clear                   parksm
126100130524     c                   clear                   parkdm
126200130524     c                   clear                   paresci
126300130524     c                   clear                   parerr
126400130524     c                   clear                   pariva
126500130524     c                   clear                   parcdf
126600130524     c                   call      'XALFA3BR'
126700130524     C                   PARM                    PARDUT
126800130524     C                   PARM                    codut
126900130524     C                   PARM                    parrag
127000130524     C                   PARM                    parkcc
127100130524     C                   PARM                    PARSTA
127200130524     C                   PARM                    PARFLR           90
127300130524     C                   PARM                    PARDIT            3
127400130524     C                   PARM                    PARNUM
127500130524     C                   PARM                    PARKCM
127600130524     C                   PARM                    PARKSM
127700130524     C                   PARM                    PARKDM
127800130524     C                   PARM                    PAResci
127900130524     C                   PARM                    PARerr
128000130524     C                   PARM                    PARiva
128100130524     C                   PARM                    PARcdf
128200130524     c
128300130524     c                   if        parsta<>-1 and parerr=*blanks
128400130524     c                   eval      W4cccm=parksm
128500130524     c                   eval      W4dccm=parrag
128600130524     c                   endif
128700130524
128800130524     c                   eval      wriem='S'
128900130524     c                   seton                                        90
129000130524     c                   goto      forw4
129100130524     c                   endif
129200130527     c*
129300130527      * Controllo se campo valorizzato
129400130527      * al momento campo oscurato e di fatto parzializzaz. non attiva
129500130527    1C                   IF        w4cccm<>*blanks  and w4cccm<>*zeros
129600130527     c
129700130527     c* controllo ksc
129800130527     c     c_digits      check     w4cccm                                 31
129900130527    2c                   if        *in31
130000130527     C                   SETON                                        2890
130100130527     C                   MOVEL     MSG(1)        w4cMSG
130200130527     C                   GOTO      forw4
130300130527    2C                   ENDIF
130400130527     c
130500130527     c                   movel     w4cccm        kksc
130600130527     c     kaco          chain     cnaco00f
130700130527    2c                   if        not %found(cnaco00f) or acoabl<>*blanks
130800130527     C                   SETON                                        2890
130900130527     C                   MOVEL     MSG(1)        w4cMSG
131000130527     C                   GOTO      forw4
131100130527     c                   else
131200130527     c                   movel     acorag        w4dccm
131300130527    2C                   ENDIF
131400130527     c
131500130527     c* Controllo che sia abilitato dalla tabella DCLI
131600130527     C                   CLEAR                   tibs02ds
131700130527     c                   clear                   dcli
131800130527     C                   MOVEL     'C'           T02MOD
131900130527     C                   MOVEL     KNSIF         T02SIF
132000130527     C                   MOVEL     'CLI'         T02COD
132100130527     c                   movel(P)  w4cccm        t02ke1
132200130527     C                   CALL      'TIBS02R'
132300130527     C                   PARM                    KPJBA
132400130527     C                   PARM                    tibs02ds
132500130527    2C                   IF        T02ERR = *BLANKS
132600130527     C                   MOVEL     T02uni        dcli
132700130527    2C                   ENDIF
132800130527    2c                   if        §clibrd<>'S'
132900130527     C                   SETON                                        2890
133000130527     C                   MOVEL     MSG(2)        w4cMSG
133100130527     C                   GOTO      forw4
133200130527    2C                   ENDIF
133300130527    1C                   ENDIF
133400130524     c
133500130527     c                   if        w4cccm>*zeros
133600130527     c                   seton                                        16
133700130527     c                   endif
133800130527     c
133900130524     c                   ENDSR
134000130527**
134100130527Codice Cliente errato o bloccato
134200130527Mittente non abilitato a border.x codice
