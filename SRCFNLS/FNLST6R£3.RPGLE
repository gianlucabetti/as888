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
014700130705     d
014800130705     D TRUL13DS      E DS
014900020801      *
015000020801      * V a r i a b i l i
015100020801      *
015200020801     D KPJBU$          s                   like(KPJBU)  inz
015300020801      * Campi chiave fissi
015400130524     d codut           s              1  0 inz(1)
015500130527     d kksc            s                   like(acoksc)
015600020801     D KEPA            s                   like(FV2EPA) inz('P')
015700020801     D KLAI            s                   like(FV2LAI) inz
015800130424     D Ktrc            s                   like(FV4trc) inz
015900020801     D KTDH            s                   like(FV2TDH) inz('T')
016000020801     D PARNPG          s                   like(FVVNPG) inz(3)
016100020801     D COMNFV          s                   like(FVVNFV) inz
016200020801     D CO2NFV          s                   like(FGVNFV) inz
016300130523     D KIMP            s                   like(FGVNFV) inz
016400130627     d kcod            s                   like(tbecod)
016500130524     d pardut          s             30
016600130524     d parrag          s             48
016700130524     d parkcc          s              4  0
016800130524     d parsta          s              1  0
016900130524     d parflr          s             90
017000130524     d pardit          s              3
017100130524     d parnum          s              2  0
017200130524     d parkcm          s             80
017300130524     d parksm          s            140
017400130524     d parkdm          s             60
017500130524     d paresci         s              1
017600130524     d parerr          s              2
017700130524     d pariva          s             16
017800130524     d parcdf          s             16
017900020801      * Indici di schiera
018000020801     D XB              s              3  0 inz
018100020801     D XI              s              4  0 inz
018200020801      * Data e Ora
018300020801     D UTime           s              6  0 inz
018400020801     D Ora             s              2  0 inz
018500020801     D W0140           s             14  0 inz
018600020801     D UDate8          s              8  0 inz
018700020801      *
018800020801     D POWORK          s              3  0 inz
018900020801      *
019000020801     D NRR             s              5  0 inz
019100130524     D unicoNRR        s              5  0 inz
019200020801     D SAVNRR          s              5  0 inz
019300020801     D SAVNFC          s              3  0 inz
019400020801     D RISUL           s              5  0 inz
019500020801     D RESTO           s              3  0 inz
019600020801     D NUM5            s              5  0 inz
019700130424     D wpmb            s             48    inz
019800130705     d Dataiso         s               d   datfmt(*iso)
019900130705     d Datadmy         s               d   datfmt(*dmy)
020000130705
020100130524     d TIBS34ds      e ds
020200130524     d dDatiUte      e ds
020300130524     d AZUTEds       e ds                  extname(AZUTE00F)
020400130705
020500130705     D fnlsu3ds        DS
020600130705     D  ilsu3flg               1      1
020700130705     D  ilsu3aas               2      5
020800130705     D  ilsu3lnp               6      8
020900130705     D  ilsu3nrs               9     10
021000130705     D  ilsu3nsp              11     17
021100130705     D  ilsu3DSP_B            18     25
021200130705     D  ilsu3DSP_S            26     33
021300130705     D  ilsu3nos              34     34
021400130705     d
021500130705     d  ilsu3tcr              35     35
021600130705     d  ilsu3dcr              36     43
021700130705     d  ilsu3hcr              44     47
021800130705     D  olsu3err             100    101
021900130705     d
022000130524
022100130527     d c_Digits        c                   const('0123456789')
022200130613
022300130613      //---------------------------------------------------------------
022400130613      /copy gaitrasrc/srcprotopr,tibs02r
022500020801
022600020801      *---------------------------------------------------------------*
022700020801      * INDICATORI
022800020801      *---------------------------------------------------------------*
022900020801      *  01    - GESTISCO POSIZIONAMENTO CURSORE
023000020801      *  02    - NON EMETTO IL CAMPO DELL'ORA PER RECORD NUOVI
023100040121      *  03    - SIMFEL utilizza volume e/o peso in spunta
023200020801      *  04    - FOGLIO GIA' ABBINATO
023300020801      *  10    - ROLL UP
023400020801      *  15    - FOGLIO GIA' CHIUSO
023500130527      *  16    - F9 in alta intensità
023600130627      *  25    - Quando emetto SFL non emetto richiesta dei fogli
023700130627      *  26    - esiste cliente abilitato alla borderizzazione
023800020801      *  30/34 - DI COMODO
023900020801      *  43/51 - ERRORI
024000020801      *  67    - SCAN ?
024100020801      *  72/73 - DI SUBFILE
024200020801      *  90    - ERRORE GENERICO
024300020801      *---------------------------------------------------------------*
024400020801
024500130527     C     Kaco          KLIST
024600130527     C                   KFLD                    codut
024700130527     C                   KFLD                    dutkci
024800130527     C                   KFLD                    kksc
024900020801     C     KFVV          KLIST
025000020801     C                   KFLD                    PARNPG
025100020801     C                   KFLD                    COMNFV
025200020801     C                   KFLD                    VIDFGS
025300020801     C     KFV2          KLIST
025400020801     C                   KFLD                    FGVLNP
025500020801     C                   KFLD                    FGVNFV
025600020801     C                   KFLD                    KEPA
025700020801     C                   KFLD                    KLAI
025800020801     C                   KFLD                    KTDH
025900130424     C     KFV4          KLIST
026000130424     C                   KFLD                    fgvlnp
026100130424     C                   KFLD                    fgvNFV
026200130424     C                   KFLD                    KEPA
026300130424     C                   KFLD                    Klai
026400130424     C                   KFLD                    KTRC
026500020801     C     K02FGW01      klist
026600020801     C                   kfld                    FGWnfv
026700020801     C                   kfld                    FGWlnp
026800020801     C     K02FGV01      klist
026900020801     C                   kfld                    CO2NFV
027000020801     C                   kfld                    SIMFEL
027100020801     C     K02FGV07      klist
027200021025     C                   kfld                    SIMfel
027300130523     C                   kfld                    kimp
027400020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
027500000000     C     *ENTRY        PLIST
027600130524     C                   PARM                    KPJBA
027700130524      *
027800130524      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
027900130524     c     *dtaara       define    §azute        azuteds
028000130524     c     *dtaara       define    §datiute      ddatiute
028100130524      *
028200130524     c                   clear                   AzUteDs
028300130524     c                   clear                   DDatiUte
028400130524     c                   clear                   Tibs34Ds
028500130524     c                   in(E)     *dtaara
028600130524if  1c                   if        %Error  or  RSUT = *blanks
028700130524     c                   call      'TIBS34R'
028800130524     c                   parm                    Tibs34Ds
028900130524     c                   in        *dtaara
029000130524e   1c                   endif
029100020801     C*
029200020801     C* Carico L1 e impostazioni iniziali
029300911104     C                   EXSR      CARTAB
029400020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
029500020801     C* Prima videata
029600900517     C*
029700130524     C                   MOVEL     '?'           VIDNFV
029800130524     c                   eval      vidsel='SI'
029900130711     c                   eval      vidaut='  '
030000130627     c                   seton                                        67
030100911008     C*
030200900509     C     FOR01         TAG
030300130524     c
030400130705     c                   clear                   viddsosp
030500130705
030600130524     c                   if        *in67
030700130524     C                   WRITE     lsT6D03
030800130524     c                   endif
030900130524     c
031000130524     C                   SETOFF                                       05
031100130523     C                   EXFMT     lsT6D01
031200020801     C* F03 = Fine Lavoro
031300130902     c                   if        *inkc
031400130613     C                   GOTO      FINE
031500130613     c                   endif
031600130710     c*
031700160125     c**                 if        *inkk=*on
031800160125     c**                 Clear                   tibs02ds
031900160125     c**                 Eval      T02Mod = 'R'
032000160125     c**                 Eval      T02Sif = Knsif
032100160125     c**                 Eval      T02Cod = 'BSP'
032200160125     c**                 Call      'TIBS02R'
032300160125     c**                 Parm                    Kpjba
032400160125     c**                 Parm                    tibs02ds
032500160125     c**                 seton                                        67
032600160125     C**                 GOTO      FOR01
032700160125    2c**                 endif
032800020801     C*
032900020801     C* Controllo 1.a videata
033000911008     C                   EXSR      CONTR1
033100130524     C   90              GOTO      FOR01
033200020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
033300020801     C* Seconda videata (SubFile)
033400950109     C                   SETON                                        05
033500911010     C     FORCT2        TAG
033600130523     C                   WRITE     lsT6D02
033700130523     C                   WRITE     lsT6D01
033800911008     C     FORCT         TAG
033900130523     C                   EXFMT     lsT6DCT
034000020801     C* F03 = Fine Lavoro
034100911030     C   KC              GOTO      FINE
034200020801     C* F12 = Ritorno alla videata precedente
034300130524     c                   if        *inkl
034400130524     c                   seton                                        67
034500130524     C                   GOTO      for01
034600130524     c                   endif
034700130524     c
034800020801     C* Roll Up (solo se l'ultimo record è pieno, cieè *in10 = *on)
034900020801if  1C                   IF        *IN10
035000911008     C                   Z-ADD     SAVNRR        NRR
035100130523     C     NRR           CHAIN     lsT6DSF                            30
035200020801if  2C                   IF        not *in30 and
035300020801     C                             VI2NFV < *ZEROS
035400920805     C                   SETON                                        43
035500020801x   2C                   ELSE
035600020801     C                   Z-ADD     1             XB
035700020801     C                   EXSR      PULSFL
035800020801e   2C                   ENDIF
035900920805     C                   GOTO      FORCT
036000020801e   1C                   ENDIF
036100020801     C*
036200020801     C* Controllo subfile
036300911008     C                   EXSR      CTRSFL
036400130527     c
036500130527     c                   if        *in90
036600130527     C                   GOTO      FORCT
036700130527     c                   endif
036800130524     C
036900130524     c* e ho più di un foglio da chiudere non posso attivare le parzializz
037000130524     C                   if        *inki or W4cccm>*zeros
037100130524     c                   if        contaFGV>1
037200130524     c                   seton                                        9053
037300130524     c                   goto      forct
037400130524     c                   endif
037500130527     c                   endif
037600130527     c
037700130527     c                   if        not *inkf and not *inki
037800130527     C                   GOTO      FORCT
037900130527     c                   endif
038000130524     c
038100130524     C* F09 - VISUALIZZO PARZIALIZZAZIONI
038200130524     C                   if        *inki
038300130524     c                   exsr      ctrParz
038400130527     c                   goto      forct2
038500130524     c                   endif
038600020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
038700020801     C* F06 = Elaborazione: Creo Spunte e sottometto Chiusura FV
038800931001     C                   EXSR      ELAB
038900911108     C*
039000000824     C                   CLEAR                   KQEBI
039100000824     C                   CLEAR                   KQIEB
039200950130     C*
039300020801     C* Invio File Trasmissione
039400950107     C                   MOVEL     'LSA3'        KCOAZ
039500070423     c                   clear                   parama3
039600070423     c                   move      simfel        pa3tfp
039700070423     c                   movel(P)  parama3       kpjbu
039800911108     C                   CALL      'BCH10'
039900911108     C                   PARM                    KPJBA
040000121204     c
040100020801     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
040200020801     C* Fine
040300930524     C*
040400920324     C     FINE          TAG
040500080228     C                   CLEAR                   TRUL06DS
040600080228     C                   MOVEL     'C'           D06TLA
040700080228     C                   MOVEL     TRUL06DS      KPJBU
040800080228     C                   CALL      'TRUL06R'
040900080228     C                   PARM                    KPJBA
041000911108     C*
041100020801     C                   MOVEL     *ON           *INLR
041200020801      *
041300020801      *--- CARICO TABELLE --------------------------------------------*
041400020801      *
041500911104     C     CARTAB        BEGSR
041600020801     C*
041700020801     C* Campi di passaggio
041800930127     C                   MOVEL     SIMFEL        PA2FEL
041900960209     C                   MOVEL     SIMFEL        PA1FEL
042000130524     C                   MOVEL     rsut          PA2rsut
042100921022     C*
042200020801     C                   TIME                    W0140
042300020801     C                   Z-ADD     W0140         UDATE8
042400940218     C                   MOVEL     3             PARNPG
042500940415     C                   MOVEL     3             PA2NPG
042600020801     C*
042700020801     C* Carico tabella Filiali Gestite £1
042800020801     C                   CLEAR                   TRUL06DS
042900950107     C                   MOVE      '£1'          D06COD
043000950107     C                   MOVEL     SIMFEL        D06KEY
043100020801     C                   MOVEL     TRUL06DS      KPJBU
043200950107     C                   CALL      'TRUL06R'
043300950107     C                   PARM                    KPJBA
043400020801     C                   MOVEL     KPJBU         TRUL06DS
043500950107     C                   MOVEA     LIN           L1
043600130627     c
043700130627     c
043800940415     C*
043900040121     C* Controllo se SIMFEL ha CML/VDL
044000930125     C     SIMFEL        CHAIN     AZORG                              31
044100960209     C   31              MOVEL     *BLANKS       ORGDE8
044200040121     C   31              MOVEL     *BLANKS       ORGDf0
044300040121     c                   eval      og150=orgdf0
044400960209     C*
044500040121     C                   TESTN                   §ogvol               31
044600930125     C*
044700120917     c* Sostituisco l'uso dei flag §ogspp e §ogspv con il test sulla
044800120917     c* data macchinone. Siccome il pgm di chiusura f.v. i campi pa2spv e pa2spp
044900120917     c* li utilizza, li continuo a passare ma valorizzandoli, come per il campo
045000120917      * PA2CML, in base alla data macchinone.
045100930125     C     *IN31         IFEQ      '1'
045200040121     C     §ogvol        ANDGT     *ZEROS
045300120914     C                   SETON                                        03
045400970423     C                   MOVEL     'S'           PA2CML
045500120914     C                   MOVEL     'S'           PA2spv
045600020801     C                   ENDIF
045700040121     C                   TESTN                   §ogpes               31
045800990917     C*
045900990917     C     *IN31         IFEQ      '1'
046000040121     C     §ogpes        ANDGT     *ZEROS
046100120914     C                   SETON                                        03
046200990917     C                   MOVEL     'P'           PA2CML
046300120914     C                   MOVEL     'S'           PA2spp
046400020801     C                   ENDIF
046500040121     c
046600120914     c***                eval      pa2spv=§ogspv
046700120914     c***                eval      pa2spp=§ogspp
046800120914     c***  pa2spv        ifne      *blanks
046900120914     c***  pa2spp        orne      *blanks
047000120914     c***                seton                                        03
047100120914     c***                endif
047200040121     c
047300020801     C* Dalla descrizione 148 vedo quale generazione spunte devo fare
047400020801     C*  in chiusura FV
047500960209     C                   MOVEL     ORGDE8        OG148
047600960209     C     §OGAGS        IFEQ      'E'
047700960209     C                   MOVEL     'S'           PA2AGS
047800010910     C                   MOVEL     §OGAGP        PA2AGP
047900960209     C                   ELSE
048000960209     C                   CLEAR                   PA2AGS
048100010910     C                   CLEAR                   PA2AGP
048200960209     C                   ENDIF
048300911104     C*
048400020801     C* Controllo se sono Filiale di Primo o Secondo Livello
048500130523     C                   Z-ADD     SIMfel        VIDFGS
048600130524     c     simfel        chain     azorg01l
048700130524     c                   if        %found(azorg01l)
048800130524     c                   movel     orgdes        desfgs
048900130524     c                   endif
049000130613     C                   movel     vidfgs        alfafgs           3
049100000615     C*
049200911104     C                   ENDSR
049300020801      *
049400020801      *--- PULIZIA SUBFILE -------------------------------------------*
049500020801      *
049600911008     C     PULSFL        BEGSR
049700130523     C                   SETOFF                                       0102
049800130523     C                   CLEAR                   lsT6DSF
049900130524     c
050000130627     C     XB            DOWLE     16
050100130524     c                   if        primoV=' '
050200130524     c                   seton                                        60
050300130701     C     nrr           add       1             rec
050400130524     c                   eval      primoV='N'
050500130524     c                   endif
050600130524     c
050700020801     C                   ADD       1             NRR
050800130523     C                   WRITE     lsT6DSF
050900130524     c                   setoff                                       60
051000020801     C                   ADD       1             XB
051100020801     C                   ENDDO
051200020801     C* Salvo l'ultimo NRR
051300020801     C                   Z-ADD     NRR           SAVNRR
051400130701     C***                Z-ADD     SAVNRR        REC
051500930129     C                   SETON                                        01
051600911008     C                   ENDSR
051700020801      *
051800020801      *--- CONTROLLI FOGLIO VIAGGIO ----------------------------------*
051900020801      *
052000911008     C     CONTR1        BEGSR
052100020801     C*
052200911008     C                   SETOFF                                       303190
052300130524     C                   SETOFF                                       4767
052400130524     c                   clear                   primoV
052500020801     C* P.O. in gestione
052600020801     C* .. diverso da 0
052700020801if  1C     VIDFGS        IFEQ      0
052800130524     C                   SETON                                        90
052900000704     C                   GOTO      ENDCT1
053000020801     C* .. presente in L1
053100020801x   1C                   ELSE
053200000704     C     VIDFGS        LOOKUP    L1                                     34
053300130524     C  N34              SETON                                        90
053400000704     C   90              GOTO      ENDCT1
053500020801e   1C                   ENDIF
053600020801     C*
053700130523     C* Numero Foglio IMP
053800020801if  1C     VIDNFV        IFEQ      *BLANKS
053900911008     C     VIDNFV        OREQ      *ZEROS
054000911008     C                   SETON                                        4090
054100911008     C                   GOTO      ENDCT1
054200911008     C*
054300020801x   1C                   ELSE
054400020801     C* Ricerca
054500940218     C     '?'           SCAN      VIDNFV                                 90
054600020801if  2C     *IN90         IFEQ      *ON
054700950107     C                   Z-ADD     0             DLRNFV
054800950107     C                   MOVEL     'S'           DLRRIC
054900950107     C                   MOVEL     '3'           DLRNPG
055000950107     C                   MOVEL     31129999      DLRGAL
055100130523     C                   MOVEL     'FNlST6R'     DLRPGM
055200000615     C                   MOVEL     VIDFGS        DLRFGS
055300950107     C                   MOVEL     '2'           DLRTFV
055400950107     C                   MOVEL     DSLR01        KPJBU
055500950107     C                   CALL      'FNLR02R'
055600911008     C                   PARM                    KPJBA
055700950107     C                   MOVEL     KPJBU         DSLR01
055800950107     C                   MOVEL     DLRNFV        VIDNFV
055900130523     C                   WRITE     lsT6D03
056000000620     C                   SETON                                        67
056100911008     C                   GOTO      ENDCT1
056200020801e   2C                   ENDIF
056300020801     C* Controlli
056400911008     C                   TESTN                   VIDNFV               31
056500911010     C  N31              SETON                                        30
056600911008     C*
056700020801if  2C     *IN30         IFEQ      *OFF
056800940218     C                   MOVEL     VIDNFV        COMNFV
056900130705     C     KFVV          CHAIN     FNFVV000                           30
057000020801     C* Escludo: 1) gli Annullati
057100020801     C*          2) ciò che NON è IMP
057200020801     C*          3) i Chiusi
057300020801if  3C     FVVATB        IFNE      *BLANKS
057400930524     C     FVVSPG        ORNE      'P'
057500930524     C     FVVFCF        ORNE      *BLANKS
057600911008     C                   SETON                                        30
057700020801e   3C                   ENDIF
057800020801e   2C                   ENDIF
057900911008     C*
058000911008     C   30              SETON                                        4090
058100911008     C   30              GOTO      ENDCT1
058200020801e   1C                   ENDIF
058300020801     C*
058400130523     C* Giro Data Foglio IMP
058500950109     C                   CLEAR                   WLBDAT
058600950109     C                   MOVEL     '3'           G02ERR
058700950109     C                   Z-ADD     FVVDFV        G02INV
058800950109     C                   CALL      'XSRDA8'
058900950109     C                   PARM                    WLBDAT
059000950109     C                   Z-ADD     G02DAT        VIDDFV
059100130705     c*
059200130705     c* Calcolo la data sospensione
059300130711    2c**                 if        vidaut='SI'
059400130711     c**                 clear                   trul13ds
059500130711     c**                 Eval      I13Tla = 'L'
059600130711     c**                 Eval      I13Mod = 'P'
059700130711     c**                 Eval      I13Fil = simfel
059800130711     c**                 Eval      I13Din = fvvdfv
059900130711     c**                 Eval      I13gio = 1
060000130711     c**                 Call      'TRUL13R'
060100130711     c**                 Parm                    Trul13ds
060200130711     c**
060300130711    3c**                 if        o13err<>' '
060400130711     c**                 seton                                        9045
060500130711     c**                 leavesr
060600130711    3c**                 endif
060700130711     c**                 movel     o13dfi        dataiso
060800130711     c**                 movel     dataiso       datadmy
060900130711     c**                 movel     datadmy       viddsosp
061000130711    2c**                 endif
061100020801     C*
061200020801     C* Subfile : pulizia
061300911107     C                   SETON                                        72
061400130523     C                   WRITE     lsT6DCT
061500130705     C                   SETOFF                                       7247
061600911107     C*
061700950107     C                   Z-ADD     0             NRR
061800950107     C                   CLEAR                   VI2SCE
061900950109     C                   CLEAR                   VH2FLG
062000970603     C                   CLEAR                   VH2FL2
062100950109     C                   CLEAR                   VH2DTP
062200130524     c
062300130524     c* Carico la £6 di simfel alla data foglio IMP
062400130524     c                   clear                   trul06ds
062500130524     c                   eval      d06cod = '£6'
062600130524     c                   movel     simfel        d06key
062700130524     C                   MOVE      'S'           D06KEY
062800130524     C                   MOVEl     fvvdfv        D06drf
062900130524     c                   movel     trul06ds      kpjbu
063000130524     c                   call      'TRUL06R'
063100130524     c                   parm                    kpjba
063200130524     c                   movel     kpjbu         trul06ds
063300130524     c                   movea     lin           l6
063400130627     c*
063500130627     c* Verifico se ha clienti abilitati alla borderizzazione per codice
063600130627     c                   setoff                                       26
063700130627     c                   eval      kcod='CLI'
063800130627     c     kcod          setll     tntbe01l
063900130627     c     kcod          reade     tntbe01l
064000130627     c                   dow       not %eof(tntbe01l)
064100130627     c                   if        tbeatb=' '
064200130627     c                   movel     tbeuni        dcli
064300130627     c
064400130627     c                   if        §clibrd='S'
064500130627     c                   movel     tbeke1        filcli            3 0
064600130627     c     filcli        lookup    L6                                     26
064700130627     c  n26filcli        lookup    L1                                     26
064800130627     c
064900130627     c                   if        *in26
065000130627     c                   leave
065100130627     c                   endif
065200130627     c                   endif
065300130627     c                   endif
065400130627     c
065500130627     c     kcod          reade     tntbe01l
065600130627     c                   enddo
065700130524     c*
065800020801     C*
065900130523     c* Prima carico i fogli già abbinati al foglio IMP
066000020801     C* Subfile : caricamento
066100130523     c                   eval      kimp=comnfv
066200020801     C     K02FGV07      SETLL     FNFGV007
066300020801     C     K02FGV07      READE     FNFGV007                               30
066400911008     C*
066500911108     C                   SETON                                        04
066600020801do  1C                   DOW       NOT *IN30
066700940217     C                   SETOFF                                       15
066800911031     C                   MOVEL     FGVNFV        VI2NFV
066900020801     C                   CLEAR                   VI2FCF
067000020801     C                   IF        FGVFCF <> *BLANKS
067100911107     C                   MOVEL     'C'           VI2FCF
067200940217     C                   SETON                                        15
067300020801     C                   ENDIF
067400020801     C* Decodifiche Foglio
067500950109     C                   EXSR      DECOD
067600911031     C*
067700911107     C                   ADD       1             NRR
067800020801     C                   z-add     FGVlnp        wLNP
067900020801     C                   z-add     FGVnfv        wNFV
068000020801     C                   move      wPOFV         FS(NRR)
068100130523     C                   WRITE     lsT6DSF
068200911008     C*
068300020801     C     K02FGV07      READE     FNFGV007                               30
068400020801e   1C                   ENDDO
068500940217     C                   SETOFF                                       0415
068600911031     C*
068700130523     c* Poi carico i foglio, se richeista, aperti e non ancora abbianti
068800130523     c*  a foglio IMP
068900130523     c                   if        vidsel='SI'
069000130523     c                   exsr      ricercaFGV
069100130523     c                   endif
069200130523     c
069300020801     C* Finisco di pulire il subfile
069400020801     C                   Z-ADD     NRR           SAVNFC
069500130627     C     NRR           DIV       16            RISUL
069600020801     C                   MVR                     RESTO
069700930129     C*
069800020801     C     RESTO         ADD       1             XB
069900911008     C                   EXSR      PULSFL
070000130627     C     SAVNFC        IFEQ      16
070100930129     C                   Z-ADD     1             REC
070200020801     C                   ENDIF
070300130523     c                   seton                                        25
070400911008     C*
070500911008     C     ENDCT1        ENDSR
070600130523      *
070700130523      *--- ricerca fogli viaggio ancora da chiudere ------------------*
070800130523     C     RicercaFGV    BEGSR
070900130523     c                   clear                   kimp
071000130524     C                   SETOFF                                       15
071100130523     c     k02fgv07      setll     fnfgv07l
071200130523     c     k02fgv07      reade     fnfgv07l
071300130523     c
071400130523     c                   dow       not %eof(fnfgv07l)
071500130523     c* elaboro i fogli ancora aperti, non annullati
071600130523     c*  ed  escludo i fogli D H
071700130523     c                   if        fgvfcf=' ' and fgvatb=' '  and
071800130523     c                             fgvttr<>'D' and fgvttr<>'H'
071900130701     c* escludo i fogli con data fogio > data foglio IMP
072000130701     c                   if        fgvdfv<=fvvdfv
072100130523     C* Decodifiche Foglio
072200130523     C*
072300130701     c* posizinamento cursore sul primo foglio che carico
072400130701     c                   if        primoV=' '
072500130701     c                   seton                                        60
072600130701     c     nrr           add       1             rec
072700130701     c                   eval      primoV='N'
072800130701     c                   endif
072900130701
073000130524     C                   MOVEL     FGVNFV        VI2NFV
073100130524     C                   EXSR      DECOD
073200130524     C                   CLEAR                   VI2FCF
073300130523     C                   ADD       1             NRR
073400130523     C                   z-add     FGVlnp        wLNP
073500130523     C                   z-add     FGVnfv        wNFV
073600130523     C                   move      wPOFV         FS(NRR)
073700130523     C                   WRITE     lsT6DSF
073800130701     c                   setoff                                       60
073900130523     C*
074000130523     c                   endif
074100130701     c                   endif
074200130523     c
074300130523     c     k02fgv07      reade     fnfgv07l
074400130523     c                   enddo
074500130523     c
074600130523     c                   ENDSR
074700020801      *
074800020801      *--- CONTROLLO SUB FILE ----------------------------------------*
074900020801      *
075000911008     C     CTRSFL        BEGSR
075100020801     C*
075200911031     C                   SETOFF                                       9045
075300950109     C                   Z-ADD     1             NRR
075400020801     C                   CLEAR                   FS
075500130524     c                   clear                   primoV            1
075600130524     c                   clear                   contaFGV          5 0
075700911009     C*
075800020801     C* Controllo TUTTO il subfile
075900020801do  1C     NRR           DOWLE     SAVNRR
076000130523     C     NRR           CHAIN     lsT6DSF                            30
076100940218     C*
076200020801     C* Se riempito: controllo
076300020801if  2C                   IF        not *in30 and
076400020801     C                             VI2FCF <> 'C'
076500950109     C*
076600020801if  3C     VI2NFV        IFNE      *ZEROS
076700911105     C     VI2NFV        ANDNE     *BLANKS
076800950109     C                   SETON                                        01
076900911009     C*
077000020801     C* Ricerca in tutti i Fogli Viaggio
077100940218     C     '?'           SCAN      VI2NFV                                 32
077200020801if  4C     *IN32         IFEQ      *ON
077300940218     C                   EXSR      RICNFV
077400940218     C                   SETON                                        90
077500130523     C                   WRITE     lsT6D02
077600130523     C                   WRITE     lsT6D01
077700911009     C                   GOTO      ENDCTS
077800020801e   4C                   ENDIF
077900020801     C*
078000020801     C* Controlli
078100020801     C*
078200911104     C                   TESTN                   VI2NFV               31
078300130524     c                   if        not *in31
078400130524     C                   SETON                                        469060
078500130524     C                   GOTO      ENDCTS
078600130524     c                   endif
078700911010     C*
078800020801     C                   Z-ADD     0             CO2NFV
078900911031     C                   MOVEL     VI2NFV        CO2NFV
079000020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
079100911010     C*
079200020801     C* Annullato o già chiuso
079300020801if  4C                   IF        not  *in31
079400020801     C                             and (FGVatb <> *blanks
079500020801     C                              or  FGVfcf <> *blanks)
079600911031     C                   SETON                                        31
079700020801e   4C                   ENDIF
079800020801     C*
079900020801     C* Errore
080000130524     c                   if        *in31
080100130524     C                   SETON                                        469060
080200130524     C                   GOTO      ENDCTS
080300130524     c                   endif
080400020801     C*
080500020801     C* Non è possibile chiudere un FV defluenza se LNA estero
080600020801     C*  o DPD o FEDEX
080700020801if  4C                   IF        not  *in31
080800020801     C                             and (FGVTTR = 'D'
080900020801     C                              or  FGVTTR = 'H')
081000990514     C     FGVLNA        CHAIN     AZORG                              31
081100020801if  5C                   IF        not *in31
081200020801     C                   MOVEL     ORGDE3        OG143
081300020801x   5C                   ELSE
081400020801     C                   CLEAR                   OG143
081500020801e   5C                   ENDIF
081600020801if  5C                   IF        not  *in31
081700020801     C                             and (§OGNTW = 'EEX'
081800020801     C                              or  §OGNTW = 'EUP'
081900020801     C                              or  §OGNTW = 'FED'
082000020801     C                              or  §OGNTW = 'DPD')
082100990514     C                   SETON                                        31
082200020801e   5C                   ENDIF
082300020801e   4C                   ENDIF
082400020801     C* Errore
082500130524     c                   if        *in31
082600130524     C                   SETON                                        519060
082700130524     C                   GOTO      ENDCTS
082800130524     c                   endif
082900130527     c* se inserito cod cliente o premuto F9  DEVE essere foglio locale
083000130527     c*  (almeno per ora...)
083100130527     c                   if        w4cccm>*zeros or *inki
083200130527     c     fgvlna        lookup    L6                                     31
083300151126     c**                 if        not *in31
083400151126     C**                 SETON                                        549060
083500151126     C**                 GOTO      ENDCTS
083600151126     c**                 endif
083700130527     c                   endif
083800911031     C*
083900020801     C* Memorizzo nella schiera dei FV selezionati
084000020801     C*
084100020801     C                   z-add     SIMfel        wLNP
084200020801     C                   z-add     CO2nfv        wNFV
084300020801     C     wPOFV         LOOKUP    FS                                     31
084400130524     C   31              SETON                                        479060
084500911031     C   31              GOTO      ENDCTS
084600020801     C                   move      wPOFV         FS(NRR)
084700020801     C*
084800020801     C* Richiamo la Manutenzione FV
084900020801     C*
085000020801if  4C     VI2SCE        IFEQ      '1'
085100130502     c                   clear                   fnlst4ds
085200130502     c                   eval      ilst4ges='I'
085300130502     c                   eval      ilst4fgs=%editc(vidfgs:'X')
085400130502     c                   eval      ilst4nfv=%editc(co2nfv:'X')
085500130502     c                   movel     fnlst4ds      kpjbu
085600130502     C                   CALL      'FNLST4R'
085700130502     C                   PARM                    KPJBA
085800130502     C                   MOVEL     KPJBU         fnlst4ds
085900130523
086000020801     C* Se PA3F12 <> 'S' non è stato premuto F12 -> riaggancio il foglio
086100020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
086200020801     C* Riemetto la videata
086300950109     C                   SETON                                        90
086400130523     C                   WRITE     lsT6D02
086500130523     C                   WRITE     lsT6D01
086600950109     C                   CLEAR                   VI2SCE
086700020801e   4C                   ENDIF
086800020801     C*
086900020801     C* Decodifiche
087000950109     C                   EXSR      DECOD
087100130524     C* Se Data Foglio <> Data Foglio IMP -> Errore
087200130524     C*  (esclusi i Fogli di Defluenza)
087300130524if  4C     FGVTTR        IFNE      'D'
087400130524     C     FGVTTR        ANDNE     'H'
087500130524     C     FGVDFV        ANDNE     FVVDFV
087600130524     C                   SETON                                        509060
087700130524     C                   GOTO      ENDCTS
087800130524e   4C                   ENDIF
087900020801     C*
088000130524     c* Dati obbligatori da borderò
088100130524     c                   exsr      Datiobbl
088200130524     c
088300130524     c                   if        *in90
088400130524     C                   SETON                                        529060
088500130524     C                   MOVEL     '1'           VI2SCE
088600130524     C                   GOTO      ENDCTS
088700130524     c                   endif
088800020801     C* Se mancano i Piombi o la Data Patrenza:
088900020801     C*  Errore vincolante: devo andare in manutenzione a modificarli
089000130424if  4C                   IF           (wpmb =  *blanks
089100020801     C                             or  FGVdtp =  *zeros)
089200020801     C                             and FGVttr <> 'L'
089300130524     C                   SETON                                        489060
089400950324     C                   MOVEL     '1'           VI2SCE
089500950107     C                   GOTO      ENDCTS
089600020801e   4C                   ENDIF
089700130524     c
089800130524     c
089900020801     C* Se Data Partenza < Data Foglio IMP -> Errore forzabile
090000020801if  4C     FGVDTP        IFNE      *ZERO
090100950324     C     FGVDTP        ANDNE     VH2DTP
090200950109     C                   CLEAR                   VH2FLG
090300020801e   4C                   ENDIF
090400950109     C*
090500020801if  4C     VH2FLG        IFEQ      *BLANKS
090600960129     C                   MOVEL     FGVDTP        VH2DTP
090700020801if  5C     FGVDTP        IFNE      *ZERO
090800950324     C     FGVDTP        ANDLT     FVVDFV
090900130524     C                   SETON                                        499060
091000950109     C                   MOVEL     '1'           VH2FLG
091100950109     C                   GOTO      ENDCTS
091200020801e   5C                   ENDIF
091300020801e   4C                   ENDIF
091400020801     C*
091500130524     c* conto i fogli aperti da chiudere
091600130524     c                   add       1             contafgv
091700130524     c                   z-add     nrr           unicoNRR
091800130524     c
091900020801     C* Segnalazione errore a video
092000911009     C     ENDCTS        TAG
092100920805     C   90              Z-ADD     NRR           REC
092200130523     C  N32              UPDATE    lsT6DSF
092300920805     C   90              Z-ADD     SAVNRR        NRR
092400130524     c                   setoff                                       60
092500020801     C*
092600020801x   3C                   ELSE
092700020801     C*
092800020801     C* Record vuoto del SFL
092900130524     C                   CLEAR                   lsT6DSF
093000130524     c
093100130523     C                   UPDATE    lsT6DSF
093200130524     c                   setoff                                       60
093300020801     C*
093400020801e   3C                   ENDIF
093500020801     C*
093600020801e   2C                   ENDIF
093700911031     C*
093800911031     C                   ADD       1             NRR
093900020801e   1C                   ENDDO
094000920805     C*
094100020801     C* Se non rilevati errori: posizionamento cursore sull'ultimo rec.
094200130524if  1C                   IF        not *in90
094300130524     c                   z-add     1             nrr
094400130524do  1C     NRR           DOWLE     SAVNRR
094500130524     C     NRR           CHAIN     lsT6DSF                            30
094600130524if  3C     VI2NFV        IFNE      *ZEROS
094700130524     C     VI2NFV        ANDNE     *BLANKS
094800130524     c                   add       1             nrr
094900130524     c                   else
095000130524     c                   seton                                        60
095100130524     c                   z-add     nrr           rec
095200130524     C                   UPDATE    lsT6DSF
095300130524     c                   leave
095400130524e   1C                   ENDIF
095500130524e   1C                   ENDdo
095600130524     c
095700130524e   1C                   ENDIF
095800130524     c                   setoff                                       60
095900911008     C*
096000911009     C                   ENDSR
096100020801      *
096200020801      * RICERCA E CARICAMENTO FOGLI VIAGGIO --------------------------*
096300020801      *
096400940218     C     RICNFV        BEGSR
096500020801     C*
096600130523     C                   RESET                   fnlst3ds
096700020801     C* Come data finale imposto la data del giorno
096800951108     C                   Z-ADD     UDATE8        PA7DFI
096900000615     C                   Z-ADD     VIDFGS        PA7FGS
097000130523     C                   MOVEL     fnlst3ds      KPJBU
097100130523     C                   CALL      'FNLST3R'
097200940218     C                   PARM                    KPJBA
097300940218     C*
097400130523     C                   MOVEL     KPJBU         fnlst3ds
097500940218     C*
097600020801     C* Se selezionati FV li scrivo
097700940218     C                   MOVEL     *BLANKS       VI2NFV
097800130523     C                   UPDATE    lsT6DSF
097900940218     C*
098000940218     C                   SUB       1             NRR
098100020801     C                   Z-ADD     1             XI
098200940218     C*
098300020801do  1C     XI            DOWLE     45
098400020801     C     FVA(XI)       ANDNE     *ZEROS
098500020801     C*
098600940218     C                   ADD       1             NRR
098700130523     C     NRR           CHAIN     lsT6DSF                            30
098800940218     C*
098900020801     C* Creo nuova pagina del subfile
099000020801if  eC                   IF        *IN30
099100020801     C                   Z-ADD     NRR           NUM5
099200940218     C                   SUB       1             NRR
099300020801     C                   Z-ADD     1             XB
099400940218     C                   EXSR      PULSFL
099500940218     C                   Z-ADD     NUM5          NRR
099600130523     C     NRR           CHAIN     lsT6DSF                            30
099700020801     C*
099800020801e   2C                   ENDIF
099900940218     C*
100000020801     C* Memorizzo solo se è vuoto, altrimenti su altra riga
100100020801if  2C     VI2NFV        IFLT      *ZEROS
100200020801     C                   MOVEL     FVA(XI)       VI2NFV
100300020801     C                   MOVE      FVA(XI)       VI2LNA
100400950109     C                   MOVEL     VI2NFV        CO2NFV
100500020801     C     K02FGV01      CHAIN(N)  FNFGV001                           31
100600950109     C                   EXSR      DECOD
100700940218     C*
100800130523     C                   UPDATE    lsT6DSF
100900020801     C                   ADD       1             XI
101000020801e   2C                   ENDIF
101100020801     C*
101200020801e   1C                   ENDDO
101300020801     C*
101400020801     C* Esco
101500940218     C     NRR           IFLT      1
101600940218     C                   Z-ADD     1             NRR
101700020801     C                   ENDIF
101800020801     C*
101900940218     C                   ENDSR
102000020801      *
102100020801      *--- ELABORAZIONE ABBINAMENTO ----------------------------------*
102200020801      *
102300911008     C     ELAB          BEGSR
102400130705     C                   CLEAR                   KQIEB
102500130705     C                   CLEAR                   KQEBI
102600020801     C*
102700130705     c* sottometto sospensione automatica
102800130711     c**                 if        vidaut='SI'
102900130711     c**                 clear                   fnlsu3ds
103000130711     c**                 eval      ilsu3dsp_B=%editc(fvvdfv:'X')
103100130711     c**                 eval      ilsu3dsp_S=%editc(o13dfi:'X')
103200130711     c**                 eval      ilsu3nos='S'
103300130711     c**                 movel     fnlsu3ds      kpjbu
103400130711     C**                 MOVEL     'LSU3'        KCOAZ
103500130711     C**                 CALL      'BCH10'
103600130711     C**                 PARM                    KPJBA
103700130711e   1C**                 ENDIF
103800130705     c
103900950125     C                   Z-ADD     COMNFV        PA2NFV
104000950125     C                   Z-ADD     FVVDFV        PA2DFV
104100950125     C                   Z-ADD     FVVFGS        PA2FGS
104200950125     C                   CLEAR                   PA2NUM
104300040121     C* *IN03 = *ON : utilizza peso e/o volume in spunta
104400020801     C* --> sottometto azione di calcolo Volumi e Peso
104500020801if  1C     *IN03         IFEQ      *ON
104600960209     C                   CLEAR                   KPJBU
104700961008     C                   CLEAR                   PA1NUM
104800960209     C                   MOVEL     PARAM1        KPJBU
104900950107     C                   MOVEL     'LSB7'        KCOAZ
105000931001     C                   CALL      'BCH10'
105100931001     C                   PARM                    KPJBA
105200020801e   1C                   ENDIF
105300911104     C*
105400911107     C                   Z-ADD     1             NRR
105500020801do  1C     NRR           DOWLE     SAVNRR
105600020801     C*
105700130523     C     NRR           CHAIN     lsT6DSF                            30
105800020801     C* Fogli Viaggio selezionati NO chiusi
105900020801     C* -> Sottometto chiusura FV
106000020801if  2C                   IF        not *in30
106100020801     C                             and VI2NFV > *ZEROS
106200020801     C                             and VI2FCF = *BLANKS
106300931001     C                   EXSR      LANCIO
106400020801e   2C                   ENDIF
106500911104     C*
106600911104     C                   ADD       1             NRR
106700020801e   1C                   ENDDO
106800911104     C*
106900911008     C                   ENDSR
107000020801      *
107100020801      *--- LANCIO CHIUSURA FOGLIO VIAGGIO ----------------------------*
107200020801      *
107300911104     C     LANCIO        BEGSR
107400020801     C*
107500020801     C* Aggiorno FV
107600940505     C                   MOVEL     VI2NFV        CO2NFV
107700020801     C*
107800020801     C     K02FGV01      CHAIN     FNFGV001                           33
107900950107     C                   MOVEL     VIDNFV        FGVNFA
108000960129     C                   MOVEL     VH2DTP        FGVDTP
108100960129     C                   MOVEL     VI2HMP        FGVHMP
108200950107     C                   UPDATE    FNFGV001
108300020801     C*
108400020801     C* Sottometto Chiusura FV
108500080228     C* Carico tabella Filiali Gestite £1 alla data foglio viaggio
108600080228     C                   CLEAR                   TRUL06DS
108700080228     C                   MOVE      '£1'          D06COD
108800080228     C                   MOVEL     ' '           D06TLA
108900080228     C                   MOVEL     SIMFEL        D06KEY
109000080228     C                   MOVE      'S'           D06KEY
109100080228     C                   MOVEl     fgvdfv        D06drf
109200080228     C                   MOVEL     TRUL06DS      KPJBU
109300080228     C                   CALL      'TRUL06R'
109400080228     C                   PARM                    KPJBA
109500080228     C                   MOVEL     KPJBU         TRUL06DS
109600080228     C                   MOVEA     LIN           L1ela
109700130527     c
109800130527     c                   if        w4cccm>*zeros
109900130527     c                   movel     w4cccm        pa2ccm
110000130527     c                   else
110100130527     c                   clear                   pa2ccm
110200130527     c                   endif
110300080228     c
110400130524     C                   MOVEL     fnlsu2ds      KPJBU
110500130531     C                   MOVEL     'LSU2'        KCOAZ
110600130227     c                   if        knmus=*all'1'
110700130524     C                   CALL      'FNLSU2R'
110800130227     C                   PARM                    KPJBA
110900130227     c                   else
111000130227     C                   CALL      'BCH10'
111100911104     C                   PARM                    KPJBA
111200130227     c                   endif
111300020801     C*
111400911104     C                   ENDSR
111500020801      *
111600020801      * DECODIFICHE FOGLIO -------------------------------------------*
111700020801      *
111800950109     C     DECOD         BEGSR
111900020801     C*
112000950109     C                   MOVEL     FGVLNA        VI2LNA
112100950109     C     FGVLNA        CHAIN     AZORG                              31
112200950109     C   31              MOVEL     *BLANKS       DESLNA
112300950109     C  N31              MOVEL     ORGDES        DESLNA
112400130424     c*
112500130424     C* Piombi
112600130424     c                   clear                   wpmb
112700130424     C                   MOVEL     'P'           kTRC
112800130424  155C     KFV4          SETLL     FNFV401L
112900130424  156C     KFV4          READE     FNFV401L
113000130424  156C                   IF        not %eof(fnfv401l) and fv4atb=' '
113100130424     C                   MOVEL(p)  FV4NOT        Wpmb
113200130424  156C     KFV4          READE     FNFV401L
113300130424  156C                   IF        not %eof(fnfv401l) and fv4atb=' '
113400130424  156C                   eval      %subst(Wpmb:36)=fv4not
113500130424  156C                   ENDIF
113600130424     C                   ENDIF
113700130424     c                   movel     wpmb          vi2pmb
113800130424
113900950109     C                   MOVEL     FGVHMP        VI2HMP
114000960201     C                   MOVEL     FGVTTR        VH2TTR
114100951031     C*
114200020801     C* Giro Data Partenza Reale
114300020801     C* Se NON c'è prendo la data teorica se c'è
114400020801if  1C     FGVDTP        IFEQ      0
114500960129     C     KFV2          CHAIN     FNFV2000                           31
114600020801if  2C     *IN31         IFEQ      *OFF
114700960129     C                   MOVEL     FV2DPA        FGVDTP
114800960129     C                   MOVEL     FV2HPA        VI2HMP
114900020801e   2C                   ENDIF
115000020801e   1C                   ENDIF
115100960129     C*
115200950109     C                   CLEAR                   WLBDAT
115300950109     C                   MOVEL     '3'           G02ERR
115400950109     C                   Z-ADD     FGVDTP        G02INV
115500950109     C                   CALL      'XSRDA8'
115600950109     C                   PARM                    WLBDAT
115700950109     C                   MOVEL     G02DAT        VI2DTP
115800020801     C* Giro Data Foglio
115900950109     C                   CLEAR                   WLBDAT
116000950109     C                   MOVEL     '3'           G02ERR
116100950109     C                   Z-ADD     FGVDFV        G02INV
116200950109     C                   CALL      'XSRDA8'
116300950109     C                   PARM                    WLBDAT
116400950109     C                   MOVEL     G02DAT        VI2DFV
116500020801     C* Ora Partenza Reale
116600960129     C     VI2HMP        IFGT      0
116700960129     C                   SETON                                        02
116800960129     C                   ELSE
116900960129     C                   SETOFF                                       02
117000960129     C                   ENDIF
117100130524     c
117200020801     C* Se Foglio con Tipo Traino inoltre evidenzio la Linea di Arrivo
117300960201     C     VH2TTR        IFEQ      'I'
117400960201     C                   SETON                                        06
117500960201     C                   ELSE
117600960201     C                   SETOFF                                       06
117700960201     C                   ENDIF
117800960201     C*
117900960129     C                   ENDSR
118000130524     C**************************************************************************
118100130524     C* CONTROLLO la presenza sul foglio viaggio dei dati obbligatori in border
118200130524     C**************************************************************************
118300130524     c     Datiobbl      begsr
118400130524     c                   setoff                                       90
118500130524     c* reperisco il numero di cellulare
118600130524     c                   clear                   wcell
118700130524     c                   eval      kepa='P'
118800130524     c                   eval      klai=0
118900130524     c                   eval      ktrc='L'
119000130524     c     kfv4          chain     fnfv401l
119100130524     c                   if        %found(fnfv401l)
119200130524     c                   movel     fv4not        wcell            16
119300130524     c                   endif
119400130524     c*
119500130524     c                   if        fgvtmz=*blanks or
119600130524     c                             (fgvpdr=*zeros and fgvdpd=*blanks) or
119700130524     c                             (fgvpmb=*blanks and fgvttr<>'L') or
119800130524     c                             (fgvtrm=*blanks and fgvtrr=*blank and
119900130524     c                              fgvttr<>'L') or
120000130524     c                             (wcell=*blanks and fgvttr<>'L')
120100130524     c                   seton                                        90
120200130524     c                   endif
120300130524     c                   endsr
120400130524     C**************************************************************************
120500130524     C* CONTROLLO parzializzaioni
120600130524     C**************************************************************************
120700130524     c     CtrParz       begsr
120800130527     c                   setoff                                       16
120900130524     c
121000130524     c     forw4         tag
121100130524     c
121200130524     c                   if        wriem='S'
121300130524     C                   WRITE     lsT6D02
121400130524     C                   WRITE     lsT6D01
121500130524     C                   write     lsT6DCT
121600130524     c                   endif
121700130524     c
121800130524     c                   exfmt     lst6w04
121900130524     c
122000130527     c                   setoff                                       2890
122100130524     c                   clear                   wriem             1
122200130524     c
122300130524     C* Verifico se richiesta la ricerca alfabetica
122400130524     c                   if        W4dccm<>*blanks and W4cccm<=*zeros
122500130524     c                   eval      pardut=rsut
122600130524     c                   clear                   parrag
122700130524     c                   eval      parrag=W4dccm
122800130524     c                   eval      parkcc=dutkci
122900130524     c                   eval      parsta=*zeros
123000130524     c                   clear                   parflr
123100130524     c                   clear                   pardit
123200130524     c                   eval      parnum=1
123300130524     c                   clear                   parkcm
123400130524     c                   clear                   parksm
123500130524     c                   clear                   parkdm
123600130524     c                   clear                   paresci
123700130524     c                   clear                   parerr
123800130524     c                   clear                   pariva
123900130524     c                   clear                   parcdf
124000130524     c                   call      'XALFA3BR'
124100130524     C                   PARM                    PARDUT
124200130524     C                   PARM                    codut
124300130524     C                   PARM                    parrag
124400130524     C                   PARM                    parkcc
124500130524     C                   PARM                    PARSTA
124600130524     C                   PARM                    PARFLR           90
124700130524     C                   PARM                    PARDIT            3
124800130524     C                   PARM                    PARNUM
124900130524     C                   PARM                    PARKCM
125000130524     C                   PARM                    PARKSM
125100130524     C                   PARM                    PARKDM
125200130524     C                   PARM                    PAResci
125300130524     C                   PARM                    PARerr
125400130524     C                   PARM                    PARiva
125500130524     C                   PARM                    PARcdf
125600130524     c
125700130524     c                   if        parsta<>-1 and parerr=*blanks
125800130524     c                   eval      W4cccm=parksm
125900130524     c                   eval      W4dccm=parrag
126000130524     c                   endif
126100130524
126200130524     c                   eval      wriem='S'
126300130524     c                   seton                                        90
126400130524     c                   goto      forw4
126500130524     c                   endif
126600130527     c*
126700130527      * Controllo se campo valorizzato
126800130527      * al momento campo oscurato e di fatto parzializzaz. non attiva
126900130527    1C                   IF        w4cccm<>*blanks  and w4cccm<>*zeros
127000130527     c
127100130527     c* controllo ksc
127200130527     c     c_digits      check     w4cccm                                 31
127300130527    2c                   if        *in31
127400130527     C                   SETON                                        2890
127500130527     C                   MOVEL     MSG(1)        w4cMSG
127600130527     C                   GOTO      forw4
127700130527    2C                   ENDIF
127800130527     c
127900130527     c                   movel     w4cccm        kksc
128000130527     c     kaco          chain     cnaco00f
128100130527    2c                   if        not %found(cnaco00f) or acoabl<>*blanks
128200130527     C                   SETON                                        2890
128300130527     C                   MOVEL     MSG(1)        w4cMSG
128400130527     C                   GOTO      forw4
128500130527     c                   else
128600130527     c                   movel     acorag        w4dccm
128700130527    2C                   ENDIF
128800130527     c
128900130527     c* Controllo che sia abilitato dalla tabella DCLI
129000130527     C                   CLEAR                   tibs02ds
129100130527     c                   clear                   dcli
129200130527     C                   MOVEL     'C'           T02MOD
129300130527     C                   MOVEL     KNSIF         T02SIF
129400130527     C                   MOVEL     'CLI'         T02COD
129500130527     c                   movel(P)  w4cccm        t02ke1
129600130527     C                   CALL      'TIBS02R'
129700130527     C                   PARM                    KPJBA
129800130527     C                   PARM                    tibs02ds
129900130527    2C                   IF        T02ERR = *BLANKS
130000130527     C                   MOVEL     T02uni        dcli
130100130527    2C                   ENDIF
130200130527    2c                   if        §clibrd<>'S'
130300130527     C                   SETON                                        2890
130400130527     C                   MOVEL     MSG(2)        w4cMSG
130500130527     C                   GOTO      forw4
130600130527    2C                   ENDIF
130700130527    1C                   ENDIF
130800130524     c
130900130527     c                   if        w4cccm>*zeros
131000130527     c                   seton                                        16
131100130527     c                   endif
131200130527     c
131300130524     c                   ENDSR
131400130527**
131500130527Codice Cliente errato o bloccato
131600130527Mittente non abilitato a border.x codice
