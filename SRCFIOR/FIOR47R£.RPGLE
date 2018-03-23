000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070309      * Fidg31R *----------------------------------------------------*
000300070309      *         - Interrogazione/ricerca DISTINTE DI CONSEGNA
000400070312      *         - sempre CATEGORIA foglio 4
000500070227      *--------------------------------------------------------------*
000600080219      *  15           se attivato o test del PDA
000700070314      *
000800940307      *  21           GENERICO OPERAZIONI I/O
000900940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001000070301      *  23           GENERICO Operazioni varie
001100940224      *  30           SFLDSP
001200940224      * N31           SFLCLR
001300940224      *  31           SFLDSPCTL
001400940224      *  32           SFLNXTCHG
001500940224      *  33           SFLEND
001600940224      *  39           OF PRTF
001700070302      *  40 <---> 49  DSPATR SU video
001800940608      *  Specificare l'uso dei singoli indicatori
001900070227      *  50           Emissione della window totale/parziale
002000070227      *  51 <---> 90  ERRORI SU VIDEO
002100070313      *  Specificare l'uso dei singoli indicatori
002200940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002300940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002400940224      *  99           INDIC. GENERALE DI ERRORE
002500940128     F*----------------------------------------------------*
002600070604     FFidg31D   CF   E             WORKSTN
002700070309     F                                     SFILE(DG31SF2:S2NRR)
002800070321     F                                     SFILE(DG31SF1:S1NRR)
002900070309     F                                     INFDS(DSFMT)
003000070309     FTABEL00F  IF   E           K DISK
003100070309     FFnBRV01L  IF   E           K DISK
003200070309     FAZORG01L  IF   E           K DISK
003300070403     FFiqdt01L  IF   E           K DISK
003400070312     FFidst31L  IF   E           K DISK
003500070910     FFidst51L  IF   E           K DISK
003600070406     FFidst09L  IF   E           K DISK
003700070309     FFidst03L  IF   E           K DISK
003800070309     F                                     RENAME(Fidst000:Fidst003)
003900070309     fFidst08l  if   e           k Disk    Rename(Fidst000:Fidst008)
004000070309     FFNARB78L  IF   E           K DISK
004100070309     FFNORM06L  IF   E           K DISK
004200070309     FFiapd01l  IF   E           K DISK
004300070309     FFiar601L  IF   E           K DISK
004400070309     FFiAR901L  IF   E           K DISK
004500940128     D*----------------------------------------------------*
004600070309     D PGM             S             10    DIM(1) CTDATA PERRCD(1)              PGM RICHIAMANTI
004700070830     D TES             S             34    DIM(7) CTDATA PERRCD(1)              TESTATE
004800070309     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG DI ERRORE
004900070309     D*----------------------------------------------------*
005000070404     d $FV6            s              6  0 dim(600)                             F.V. da FIMS03R
005100070309      *
005200070309     D L1              S              3  0 DIM(30)                              FIL GESTITE
005300070309     D*----------------------------------------------------*
005400070620     d sav_in03        s              1
005500070620     d sav_in04        s              1
005600070528     d sav_in05        s              1
005700070404     d sav_in08        s              1
005800070313     d sav_in11        s              1
005900080219     d sav_in15        s              1
006000070313     d sav_in19        s              1
006100070313     d sav_$Gest       s                   like($gest)
006200070321     d sav_c2aut       s                   like(c2daut)
006300070313     d sav_c2nfv       s                   like(c2cnfv)
006400070321     d sav_c1aut       s                   like(c1daut)
006500070321     d sav_c1nfv       s                   like(c1cnfv)
006600070312     d savfgs          s                   like(d1cfgs)
006700070312     d savcgi          s                   like(d1ccgi)
006800070309     d fgspkm          s                   like(§ogpkm)
006900070309     d kfnvv           s                   like(arbndc)
007000070309     d Wifp            s                   Like(Arbifp)
007100070309     d DATINW          s                   Like(dstDFV)
007200070309     d DATFIW          s                   Like(dstDFV)
007300070309     d WNPG            s                   Like(dstNPG)
007400070312     d Wdst            s                   Like(dstNFV)
007500070312     d Wfgs            s                   Like(dstFGS)
007600070309     d W0CNFV          s                   Like(BRVNFV)
007700070309     d wbar            s              1    inz('0')
007800070309     d wppt            s              1    inz('0')
007900070607     d Prima_volta     s              1    inz('S')
008000070314      *
008100070315     d c16cgids        DS
008200070315     D   cgi                         10    DIM(16)                              14 giri max
008300070314     D d2cgi1                  1     10
008400070314     D d2cgi2                 11     20
008500070314     D d2cgi3                 21     30
008600070314     D d2cgi4                 31     40
008700070314     D d2cgi5                 41     50
008800070314     D d2cgi6                 51     60
008900070314     D d2cgi7                 61     70
009000070314     D d2cgi8                 71     80
009100070314     D d2cgi9                 81     90
009200070314     D d2cgi0                 91    100
009300070314     D d2cgiA                101    110
009400070314     D d2cgiB                111    120
009500070314     D d2cgiC                121    130
009600070314     D d2cgiD                131    140
009700070315     D d2cgiE                141    150
009800070315     D d2cgiF                151    160
009900070314      *
010000070315     d d16dgids        DS
010100070315     D   dgi                         25    DIM(16)                              10 giri
010200070315     D d2dgi1                  1     25
010300070315     D d2dgi2                 26     50
010400070315     D d2dgi3                 51     75
010500070315     D d2dgi4                 76    100
010600070315     D d2dgi5                101    125
010700070315     D d2dgi6                126    150
010800070315     D d2dgi7                151    175
010900070315     D d2dgi8                176    200
011000070315     D d2dgi9                201    225
011100070315     D d2dgi0                226    250
011200070315     D d2dgiA                251    275
011300070315     D d2dgiB                276    300
011400070315     D d2dgiC                301    325
011500070315     D d2dgiD                326    350
011600070315     D d2dgiE                351    375
011700070315     D d2dgiF                376    400
011800070315      *------------
011900070309     d Status         sds           333
012000070309     d  SDSparm          *parms
012100070309     D  DSPGM            *PROC
012200070309      *------------
012300070309     D TBLKEY          DS
012400070309     D  ALF1                   1      1
012500070309      *------------
012600070309     D* DS STANDARD FOGLI VARI
012700070313     D FIDG31DS      E DS
012800070309     D*------------
012900070427     D* DS Valori assegnati in distinta x Giro
013000070427     D FIDG35DS      E DS
013100070427     D*------------
013200070313     D* DS PER FNLV24R - ricerca autotrasp.
013300070313     D fnlv24ds      E DS
013400070313      *--------------------------
013500070309     D DS7N          E DS
013600070309     D  §7NDE2                 1     10
013700070309     D*------------
013800070309     D* DS PER TRUL06R - CARICAMENTO £X
013900070309     D DSUL06        E DS                  EXTNAME(TRUL06DS)
014000070309     D  LIN                    1     90  0
014100070309     D                                     DIM(30)                              FIL. COMODO
014200070309     D OG150         E DS                  INZ
014300070309     D OG146         E DS                  INZ
014400070830     D OG147         E DS                  INZ
014500080207     D OG148         E DS                  INZ
014600080207     D DDSTFLR       E DS                  INZ
014700070320     D DgrgFLO       E DS                  INZ
014800070309     D CNCR80        E DS
014900070309     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
015000070309     d YEURDS        E DS                  EXTNAME(YEURCODS)
015100070309     D***
015200070309     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
015300070309     D***
015400070309     D WLBDAT          DS
015500070309     D  G02DAT                 1      8  0
015600070309     D  G02INV                 9     16  0
015700070309     D  G02ERR                17     17
015800070309     D  G02TGI                18     22  0
015900070309      *
016000070309     D PARA04          DS
016100070309     D  P04NFV                 1      5  0
016200070309     D  P04LNP                 6      8  0
016300070309     D  P04LNA                 9     11  0
016400070309     D  P04NRS                12     13  0
016500070309     D  P04NCD                14     20  0
016600070309     D  P04NCA                21     27  0
016700070309     D  P04NPS                28     29  0
016800070309     D  P04NPG                30     30  0
016900070309     D  P04FLG                31     31
017000070309     D  P04PGM                32     38
017100070309     D  P04FGS                39     41  0
017200070309      *---------------------------------------------------------------*
017300940211     D* Passaggio Parametri
017400940211     D KPJBA         E DS
017500070227     D KPJBUs          s                   like(KPJBU)
017600940211     D*-------------
017700940211     D DSFMT           DS           512
017800940506     D  $TASTO               369    369
017900940211     D  NRG                  370    370
018000940211     D  NCL                  371    371
018100940211     D  SFLNRR               378    379B 0
018200070320     D*-------------
018300070320     D* posizione cursore
018400070320     D CURSOR          DS
018500070320     D  RIRI                   1      2B 0 INZ
018600070320     D  R$$                    2      2
018700070320     D  COCO                   3      4B 0 INZ
018800070320     D  C$$                    4      4
018900030113     D*-------------
019000030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
019100070312$003 D S2NRR           S                   Like(C2rcd)
019200070228$003 D S2pag           S                   Like(C2rcd)
019300070312$003 D NRR_s2          S                   Like(S2nrr)
019400070321$003 D S1NRR           S                   Like(C1rcd)
019500070321$003 D S1pag           S                   Like(C1rcd)
019600070321$003 D NRR_s1          S                   Like(S1nrr)
019700070312$003 D WSfl            S                   Like(C2nrr)
019800070312$003 D Wmax            S                   Like(C2rcd)
019900070312$003 D Wpag            S                   Like(C2rcd)
020000070312$003 D Wpagine         S                   Like(C2rcd)
020100071026$003 D Su_Quale_rec    S                   Like(S2nrr)
020200071026$003 D Che_Nrec_sfl    S                   Like(S2nrr)
020300071029$003 D NRec_riposiz    S                   Like(S2nrr)
020400070322      *
020500070309      *---------------------------------------------------------------*
020600070228      * selezione (1) - con Giro
020700070228     D Con_Giro        S              1    INZ(' ')
020800070301     d wNFV            s                   like(dstNFV)
020900070312     d wCGI            s                   like(dst3CGI)
021000070228      *--------------------------
021100070305      * Per impostare in testata la Distinta
021200070305     D Distintalfa     DS
021300070305     D  num_ndc                1      7
021400070305     D  ndc_a                  1      7    DIM(7)
021500070305      *--------------------------
021600070227     d* Parametri x Controllo profilo utenti
021700070227     d TIBS34ds      e ds
021800070227     d AZUTEds       e ds                  extname(AZUTE00F)
021900070312      *--------------------------
022000070227     d* Ds per dati organigramma
022100070312     d DDatiUte      e ds                  prefix(x_)
022200070227      *--------------------------
022300070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
022400070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
022500070227      *--------------------------
022600070227     D* DS PER - ricerca giri
022700070227     D fidg09ds      E DS
022800070320      *--------------------------
022900070320     D* DS PER - Visualizzazione dettaglio distinte
023000070320     D fidg02ds      E DS
023100070309      *---------------------------------------------------------------*
023200940211     D* COSTANTI
023300070309      *---------------------------------------------------------------*
023400940506     D* Tasti di funzione
023500940506     D F01             C                   CONST(X'31')
023600940506     D F02             C                   CONST(X'32')
023700940506     D F03             C                   CONST(X'33')
023800940506     D F04             C                   CONST(X'34')
023900940506     D F05             C                   CONST(X'35')
024000940506     D F06             C                   CONST(X'36')
024100940506     D F07             C                   CONST(X'37')
024200940506     D F08             C                   CONST(X'38')
024300940506     D F09             C                   CONST(X'39')
024400940506     D F10             C                   CONST(X'3A')
024500940506     D F11             C                   CONST(X'3B')
024600940506     D F12             C                   CONST(X'3C')
024700940506     D F13             C                   CONST(X'B1')
024800940506     D F14             C                   CONST(X'B2')
024900940506     D F15             C                   CONST(X'B3')
025000940506     D F16             C                   CONST(X'B4')
025100940506     D F17             C                   CONST(X'B5')
025200940506     D F18             C                   CONST(X'B6')
025300940506     D F19             C                   CONST(X'B7')
025400940506     D F20             C                   CONST(X'B8')
025500940506     D F21             C                   CONST(X'B9')
025600940506     D F22             C                   CONST(X'BA')
025700940506     D F23             C                   CONST(X'BB')
025800940506     D F24             C                   CONST(X'BC')
025900940506     D ENTER           C                   CONST(X'F1')
026000940506     D ROLDWN          C                   CONST(X'F4')
026100940506     D ROLLUP          C                   CONST(X'F5')
026200070301     d digits          c                   '0123456789'
026300070309     D CANNUL          C                   CONST('  Annullato  ')
026400070309     D CBLOCC          C                   CONST('  Bloccato   ')
026500070315     D Imp_C_A         C                   CONST('imp.C/A')
026600070315     D Imp_ASS         C                   CONST('imp.Ass')
026700070313
026800070313      **********************************************************************
026900070313      * Constants
027000070313      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
027100070313      **********************************************************************
027200070510     D MaxKey          C                    3
027300070313$xxx D Autista         C                   '1'
027400070313$xxx D Numero          C                   '2'
027500070313     D Ascendente      C                   1
027600070313     D Discendente     C                   2
027700070313     D Carattere       C                   6
027800070313     D Put             C                   1
027900070313     D EndPut          C                   2
028000070313     D Get             C                   3
028100070403     D Numerico        C                   8
028200070313      **********************************************************************
028300070313      * Campi utili:
028400070313      *     RRN1       - Numero relativo di record del Subfile
028500070313      *     SizeList   - Dimensione della lista
028600070313      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
028700070313      **********************************************************************
028800070313     D Rrn1            S              5I 0
028900070313     D NotUsed         S             16A
029000070313     D ReturnSize      S              9B 0
029100070313     D SizeList        S              9B 0
029200070313     D RrnLast         S              5I 0
029300070313     D WrkSort         S              1
029400070313      **********************************************************************
029500070313      * Data Structures
029600070321      *     SflRcd2    - L'intero record del SFL da passare x l'ordinamento
029700070313      *     QLGSCB     - The sort request block for the QLGSORT API
029800070313      *     QLGSCB00   - The sort request block for the QLGSRTIO API
029900070313      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
030000070313      *     QUSEC      - Error structure for the QLGSORT API
030100070313      **********************************************************************
030200070321     D SflRcd2         DS
030300070313     D  S2DAUT
030400070313     D  S2CNFV
030500070313     D  S2CDFV
030600070417     D  H2CDFV
030700070510     D  H2PRGG
030800070404     D  S2SCE
030900070313     D  S2CPEM
031000070313     D  S2CPEA
031100070313     D  S2CVOM
031200070313     D  S2CVOA
031300070313     D  S2IMCA
031400070313     D  S2IMAS
031500070313     D  S2CTPM
031600070313     D  S2DTPD
031700070313     D  S2CTPD
031800070419     D  S2DPDA
031900070419     D  S2CPDA
032000080207     D  S2CTSTPDA
032100070313     D  S2CFCF
032200070321     D  S2CANN
032300070419     D  S2NBLA
032400070419     D  S2NFTL
032500070419     D  S2STPA
032600070625     D  S2TUSC
032700070820     D  S2STP
032800070904     D  S2DDES1
032900070904     D  S2DESN1
033000070904     D  S2NBLA1
033100070904     D  S2NFTL1
033200070904     D  S2STPA1
033300070904     D  S2CPEM1
033400070904     D  S2CPEA1
033500070904     D  S2CVOM1
033600070904     D  S2CVOA1
033700080222     D  H2in14
033800071029     D  H2recpos
033900070322     D  Selected2                     1A
034000070313
034100070321     D SflRcd1         DS
034200070321     D  S1DAUT
034300070321     D  S1CNFV
034400070321     D  S1CDFV
034500070417     D  H1CDFV
034600070510     D  H1PRGG
034700070404     D  S1SCE
034800070321     D  S1CPEM
034900070321     D  S1CPEA
035000070321     D  S1CVOM
035100070321     D  S1CVOA
035200070321     D  S1IMCA
035300070321     D  S1IMAS
035400070321     D  S1CTPM
035500070321     D  S1CTPD
035600070321     D  S1CPDA
035700080207     D  S1CTSTPDA
035800070321     D  S1CFCF
035900070321     D  S1CANN
036000070625     D  S1TUSC
036100070820     D  S1STP
036200080222     D  H1in14
036300070322     D  Selected1                     1A
036400080207      * ?------------------------------------------------------------------ */
036500080207      * ?set caratteri Maiuscoli/Minuscoli
036600080207     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
036700080207     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
036800080207      * ?------------------------------------------------------------------ */
036900070321
037000070313      /COPY QSYSINC/QRPGLESRC,QLGSORT
037100070313     D QLGKL                         16    DIM(MaxKey)
037200070313     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
037300070313     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
037400070313     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
037500070313     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
037600070313
037700070313      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
037800070313      /COPY QSYSINC/QRPGLESRC,QUSEC
037900940127     C*----------------------------------------------------*
038000070321      *?     MAIN LINE PROGRAM     ?
038100940127     C*----------------------------------------------------*
038200940223     C* inizializzazione variabili
038300940223     C                   EXSR      INZVAR
038400940223     C*
038500940223     C     $FINE         DOWEQ     *OFF
038600070227     C     $GEST         CASEQ     'D1'          GESD1
038700070321     C     $GEST         CASEQ     'S1'          GESS1
038800070227     C     $GEST         CASEQ     'S2'          GESS2
038900070330     C     $GEST         CASEQ     'D2'          GESD2
039000940117     C                   END
039100070309     C                   ENDdo
039200940325     C* fine programma
039300070313     C                   MOVEL(p)  FIDG31DS      KPJBU
039400071029     C                   SETON                                            LR
039500030113     C************************************************************
039600070321      *?  INIZIALIZZAZIONE VARIABILI   ?
039700030113     C************************************************************
039800030113     C     INZVAR        BEGSR
039900030113     C*
040000030113     C* Variabili per gestione videate
040100070312     C                   move      *off          $INZS1
040200070312     C                   move      *off          $INZS2
040300070312     C                   move      *off          $INZS3
040400070312     C                   move      *off          $INZD2
040500070312     C                   move      'S'           $VISS1
040600070312     C                   move      'S'           $VISS2
040700070312     C                   move      'S'           $VISS3
040800070312     C                   move      'S'           $VISD1
040900070330     C                   move      'S'           $VISD2
041000070309      *
041100070309     C                   MOVE      *OFF          $FINE
041200030113     C                   MOVE      *OFF          $EFILE
041300030113     C                   MOVE      *OFF          $ESCI
041400030113     C                   MOVE      *OFF          $RCDOK
041500070314     C                   move      *on           $PcurS2
041600070321     C                   move      *on           $PcurS1
041700070227     C* Emissione primo video
041800070227     C                   MOVE      'D1'          $GEST
041900070227     C                   MOVE      *ON           $INZD1
042000070312      *
042100070312      * ? Legge sempre i Fogli (4)
042200070312     C                   MOVEL     '4'           wNPG
042300070312      *
042400070312     C* Variabili appoggio
042500030114     C                   Z-ADD     1             WPAG
042600070312     c                   clear                   pgm_da_call
042700070313     c                   move      *blank        sav_in11
042800030113     c*
042900030113     C                   ENDSR
043000070227     C************************************************************
043100070321      *?  Emissione primo video di SCELTA   ?
043200070227     C************************************************************
043300070227     C     GESD1         BEGSR
043400070227     C*
043500070227     C* inizializzazione videata
043600070227     C     $INZD1        IFEQ      *ON
043700070227     C                   EXSR      INZD1
043800070227     C                   MOVE      *OFF          $INZD1
043900070227     C                   ENDIF
044000070227     C*
044100070228     c     Video_1       tag
044200070302     C*  Emissione Video
044300070302     c                   setoff                                       99
044400070309     c                   if        $VISD1 = 'S'
044500070228      * ?            *-----------------------*
044600070309     C                   EXFMT     DG31D01
044700070228      * ?            *-----------------------*
044800070313     C                   movel     'D1'          Sav_$GEST
044900070309     c                   end
045000070227     C* Selezioni
0451000702271    C                   SELECT
045200070227     C* F3=Fine
045300070227     C                   WHEN      $TASTO   =   F03
045400070227     C                   EXSR      F03_exit
045500070312     c                   goto      end_d01
045600070227      *
0457000702271O   C                   OTHER
045800070227     C* CONTROLLO DATI
045900070227     C                   EXSR      CTRD1
0460000702271-   C                   ENDSL
046100070227     C*
046200070228     C* Per errori o decodifiche
046300070228     c   99              goto      Video_1
046400070228      *
046500070312      * ? * Emissione Lista o Dettaglio
046600070312     c                   exsr      DSP_seguente
046700070228      *
046800070312     c     end_d01       tag
046900070227     C                   ENDSR
047000070306     C/EJECT
047100070306     C************************************************************
047200070321      *?  Quale Visualizzazione deve impostare        ?
047300070306     C************************************************************
047400070312     C     DSP_seguente  BEGSR
047500070306     C*
047600070312     C*  Se non immesso il Nr.Foglio
047700070312if  0c                   if        d1CNFV = 0
047800070312     C*
047900070309     C*  Quale SFL deve mostrare in base numero Foglio
048000070309     C*  Emissione SFL di elenco solo se non richiamato
048100070419if  1c                   if        FDGtfv  = '3'
048200070419     c                   eval      $GEST ='S1'
048300070419     C                   MOVE      *ON           $INZS1
048400070419     c                   eval      $VISS1 = 'N'
048500070419     c                   else
048600070309      *
048700070321if  1c                   if        d1tipv = 'P'
048800070309     c                   eval      $GEST ='S2'
048900070309     C                   MOVE      *ON           $INZS2
049000070326     c                   seton                                        09
049100070321     c                   else
049200070321     c                   eval      $GEST ='S1'
049300070321     C                   MOVE      *ON           $INZS1
049400070326     c                   setoff                                       09
049500070321     c                   end
049600070309     C*
049700070309e   1c                   endif
049800070309      *
049900070312x   0c                   else
050000070312     C*
050100070312     C*  Dettaglio della Distinta
050200070312     c                   eval      $GEST ='D2'
050300070312     C                   MOVE      *ON           $INZD2
050400070312     C                   exsr      GESD2
050500070312     C*
050600070312e   0c                   endif
050700070312      *
050800070306     C                   ENDSR
050900070227     C/EJECT
051000070227     C************************************************************
051100070321      *?  INIZIALIZZAZIONE Primo video      ?
051200070227     C************************************************************
051300070227     C     INZD1         BEGSR
051400070227     C*
051500070227     C* Pulizia campi e indicatori
051600070308     c                   exsr      INZ_indERR
051700070309     C*
051800070309     C* Pulizia formato record   campi video
051900070309     c                   clear                   DG31D01
052000070309     C* campi titolo
052100070312     C                   MOVEL     TES(1)        d1CTES
052200070606     c                   if        fdgPGM = 'FIDG30R'
052300070606     C                   MOVEL(p)  TES(2)        d1CTES
052400070606     c                   end
052500070312     C                   MOVEL     dsPGM         d1CPGM
052600070312     C                   MOVEL     RAGUT         D1CDSA
052700070312     C                   MOVEL     knsif         d1nsif
052800070312     C                   MOVEL     knmus         d1nmus
052900070321      *
053000070822     C                   Z-ADD     *date         d1CDIN
053100070822     C                   Z-ADD     31129999      d1CDFI
053200070321     c                   move      'N'           d1IMPO
053300070822     c                   move      ' '           d1STP
053400070309     C* Solo Fogli (4)
053500070312     C                   MOVEL     wnpg          d1cnpg
053600070330     c                   move      'T'           d1TIPV
053700070830     c                   move      '2'           d1ctfv
053800070309     C***
053900070309     C* ??????????????????????????
054000070309     C* Il p.o.deve essere impostato in base al profilo
054100070312     C                   MOVEL     x_dutpou      d1CFGS                         *FIL.GESTIONE
054200070309     C***
054300070309     C* VEDO SE SONO SIMFEL O REMOTO
054400070309     c     simtpp        ifeq      '2'
054500070309     c     simtpp        oreq      *blanks
054600070312     c                   movel     simpou        d1cfgs
054700070309     C                   ELSE
054800070312     C                   MOVEL     SIMFEL        d1CFGS
054900070309     C                   END
055000070309      *
055100070309      * ? memorizzo che è richiamato anche con 01 = *on -> pgm RICHIAMATO
055200070313     C     FDGRIC        IFNE      ' '
055300070312     c                   move      'S'           pgm_da_call       1
055400070404      *
055500070404      *  Se richiamato da Interrogazione
055600070404     c                   setoFF                                       17
055700070404     C     FDGRIC        IFeq      'I'
055800070404     c                   setoN                                        17
055900070404     c                   enD
056000070322      *
056100070322      * al momento deve ricevere a 0 questo parametro
056200070406     c********           if        FDGNPG <> 0
056300070406     C********           MOVEL     FDGNPG        d1cnpg
056400070406     c********           end
056500070322      *
056600070322     c                   if        FDGTPV <> *blank
056700070322     C                   MOVEL     FDGTPV        d1tipv
056800070322     c                   end
056900070309      *
057000070309      * ? Condiziono l'emissione del campo "1=Selezione" nei sfl
057100070404     c                   if        $FV6(1) = 999999 and $FV6(600) = 999999
057200070404     c                   setoFF                                       19
057300070404     c                   else
057400070404     c                   setoN                                        19
057500070404     c                   end
057600070313     c                   move      *in19         sav_in19
057700070419      *
057800070419      * Ripulisco eventuale flag di ritorno per FIMS03R
057900070419      * (se FNLR02R richiamato per sapere se ci sono distinte aperte
058000070419      *  non inserite nel piano a video di FIMS03R)
058100070419     c                   if        FDGtfv = '3'
058200070419     c                   clear                   FDGfs1
058300070419     c                   endif
058400070309      *
058500070321     C* IMPOSTO I CAMPI da A/M/G
058600070313     C                   MOVEL     FDGGDA        DATINW
058700070313     C                   MOVE      DATINW        G02INV
058800070313     C                   MOVEL     '3'           G02ERR
058900070313     C                   CALL      'XSRDA8'
059000070313     C                   PARM                    WLBDAT
059100070313     C                   Z-ADD     G02DAT        d1CDIN
059200070321      *
059300070313     C                   MOVEL     FDGGAL        DATFIW
059400070313     C                   MOVE      DATFIW        G02INV
059500070313     C                   MOVEL     '3'           G02ERR
059600070313     C                   CALL      'XSRDA8'
059700070313     C                   PARM                    WLBDAT
059800070313     C                   Z-ADD     G02DAT        d1CDFI
059900070321      *
060000070313     C                   MOVEL     FDGTFV        d1CTFV
060100070313     C                   MOVEL     FDGCGI        d1Ccgi
060200070313     C                   MOVEL     FDGFGS        d1CFGS
060300070313     C                   MOVEL     FDGNFV        d1CNFV
060400070625     C                   MOVEL     FDGtdis       d1Ctpm
060500070625     C                   MOVEL     FDGtusc       d1Tusc
060600070822     C                   MOVEL     FDGstp        d1stp
060700070309     C*
060800070309     C* VEDO SE VISUALIZZARE LA PRIMA VIDEATA (SCELTA PARAMETRI) O NO
060900070313     C     FDGRIC        IFNE      '§'                                          '§' SI 1° VIDEO
061000070309      * Non visualizzare il primo video
061100070309     C                   move      'N'           $VISD1
061200070309     C                   ENDIF
061300070309      *
061400070309     C                   END
061500070309     C*
061600070309     C* Aggancia Organigramma x picking su messaggerie
061700070309     c                   clear                   og150
061800070830     c                   clear                   og147
061900070927     c                   clear                   Abilita_ORM       1
062000070830     c                   setoff                                       08  02
062100070312     C     d1CFGS        chain     AZORG01l
062200070309    1C                   IF        %found(azorg01l)
062300070312     C                   MOVEL     ORGDES        d1DFGS
062400070309     c                   movel     orgdf0        og150
062500070830     c                   movel     orgde7        og147
062600080207     c                   movel     orgde8        og148
062700070927      * Abilitazione a trattare gli ORM
062800070927     c                   movel     §ogcGIO       Abilita_ORM
062900070830      *
063000070830     c* p.o. con ORM
063100070830     c                   if        §ogDDAO <> *blank
063200070830     c                   move      §ogDDAO       data_OrM          8 0
063300070830     c                   seton                                        02
063400070830     C                   MOVE      *BLANK        D1DATI
063500070904     C                   MOVE      Tipo_Prestaz  D1DATI
063600070830     c                   eLSE
063700070830     c                   setoFF                                       02
063800070830     C                   MOVE      'C'           D1DATI
063900070830     c                   end
064000070312      *
064100070309     c* p.o. con piking messaggerie
064200070309     c                   if        §ogpkm='S'
064300070309     c                   seton                                        08
064400070309     c                   endif
064500070309     c                   eval      fgspkm = §ogpkm
064600070309     c                   end
064700070309      *
064800080219     c* in test o attivo x PDA
064900080219     c                   setoff                                       15
065000080219     c                   if        §OGPDAORM <> *blank or
065100080219     c                             §OGPDACON <> *blank
065200080219     c                   seton                                        15
065300080219     c                   end
065400070309     c* salvo campi di comodo
065500070312     c                   eval      savfgs = d1cfgs
065600070313     c                   eval      sav_in08 = *in08
065700070313     c                   eval      sav_in11 = *blank
065800080219     c                   eval      sav_in15 = *in15
065900070227     C*
066000070227     C                   ENDSR
066100940127     C************************************************************
066200070321      *? Controlli primo video di SCELTA Operazione da eseguire  ?
066300940127     C************************************************************
066400070227     C     CTRD1         BEGSR
066500030113     C*
066600070227     C*  Spegne indicatori di errore
066700070308     c                   exsr      INZ_indERR
066800070309     C*
066900070309     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
067000070309     c                   setoff                                       08  99
067100070309     C                   MOVE      *BLANK        test_Parallelo    1
067200070830     C                   MOVE      *BLANK        test_Paral_ORM    1
067300080222     C*
067400080222     C* RESETTA l'emissione del SFL di scelta inzializzandolo
067500080222     C*   ordinato x Numero:
067600080222     c                   eval      sav_in11 = *blank
067700070309     C*
067800070309      * Categoria Foglio x controllo nr.distinta
067900070312     C                   move      d1cnpg        wNPG
068000070309     C*------------------------
068100070309     C* SE IMMESSA DEVE ESSERE FILIALE VALIDA
068200070309     c                   clear                   fgspkm
068300070312     c                   clear                   d1DFGS
068400070309      *
068500070406     c* Filiale apertura obbligatoria
068600070406     c*    x Categoria 4 è sempre obbligatoria
068700070312     c                   if        d1cfgs=0
068800070312     C                   MOVEL     MSG(2)        d1Cmsg
068900070309     C                   SETON                                        51  99
069000070312     C                   GOTO      end_CT1
069100070309     c                   endif
069200070309      *
069300070309      *  FGS impostata
069400070312     C     d1CFGS        IFNE      0
069500070309     C*------------------------
069600070309      * ?- Controlla la filiale in gestione
069700070309     C                   CLEAR                   DSLV50
069800070309     C                   MOVEL     KNMUS         D50PRU
069900070312     C                   z-add     d1cFGS        D50FGS
070000070309     C                   CALL      'FNLV50R'
070100070309     C                   PARM                    DSLV50
070200070309    1C     D50ERR        IFNE      ' '
070300070312     C                   MOVEL     MSG(1)        d1Cmsg
070400070309     C                   SETON                                        51  99
070500070312     C                   goto      end_CT1
070600070309    1C                   ENDIF
070700070309      *
070800070309      * Esistenza su Organigramma
070900070312     c                   clear                   wfgs
071000070312     C     d1CFGS        chain     AZORG01l
071100070309    1C                   IF        not %found(azorg01l)
071200070312      *
071300070312     C                   MOVEL     MSG(1)        d1Cmsg
071400070309     C                   SETON                                        51  99
071500070312     C                   GOTO      end_CT1
071600070312      *
071700070309     c                   else
071800070312      *
071900070312     c                   z-add     d1CFGS        wfgs
072000070309     c                   movel     orgdf0        og150
072100070309     c                   movel     §ogpkm        fgspkm
072200070312     c                   movel     orgdes        d1DFGS
072300070309     C                   MOVEL     ORGde6        og146
072400070830     C                   MOVEL     ORGde7        og147
072500080207     C                   MOVEL     ORGde8        og148
072600080219      *
072700080219      * in test o attivo x PDA
072800080219     c                   setoff                                       15
072900080219     c                   if        §OGPDAORM <> *blank or
073000080219     c                             §OGPDACON <> *blank
073100080219     c                   seton                                        15
073200080219     c                   end
073300080219     c                   eval      sav_in15 = *in15
073400070309      *
073500070309      * controllo sulla gestibilità del P.O.
073600070309     c                   if        §ogdda = *blank or §ogdda = *all'0'
073700070309     C                   MOVE      *ON           $FINE
073800070312     C                   goto      end_CT1
073900070309     c                   else
074000070309      *
074100070309      * se c'è una data in organigramma
074200070309     c                   move      §ogdda        data_org          8 0
074300070830     c                   move      §ogDDAO       data_orM
074400070927      * Abilitazione a trattare gli ORM
074500070927     c                   movel     §ogcGIO       Abilita_ORM
074600070309      *
074700070309     C*  Controlla Con il P.O. la data abilitazione
074800070309      * se siamo in fase di Parallelo prima di essere partiti
074900070426     c                   if        data_org = 20391231  or
075000070426     c                             data_org >  dateu8
075100070309     C                   eval      test_Parallelo = 'S'
075200070309     c                   end
075300070309      *
075400070830     C*  Controlla Con il P.O. la data abilitazione
075500070830      * se siamo in fase di Parallelo prima di essere partiti
075600070830     c                   if        data_OrM = 20391231  or
075700070830     c                             data_OrM >  dateu8
075800070830     C                   eval      test_Paral_ORM = 'S'
075900070830     c                   end
076000070830      *
076100070309      * se data distinte automatiche NON è
076200070309      * né = 31/12/2039 che significa p.o. in fase di test o parallelo
076300070309      * né < oggi che significa p.o. partito
076400070309      * allora fuori
076500070426     c******             if        data_org <> 20391231 and dateu8 < data_org
076600070426     C******             MOVE      *ON           $FINE
076700070426     C******             goto      end_CT1
076800070426     c******             end
076900070309     c                   end
077000070309      *
077100070309     c* P.o. con picking, messaggerie: visualizzo parzializzazione
077200070309     c                   if        fgspkm='S'
077300070309     c                   seton                                        08
077400070309    1C                   ENDif
077500070309    1C                   END
077600070309    1C                   END
077700070309      *
077800070309     C*------------------------
077900070309     c* per cambio p.o. gestione riemetto video 1 senza errori
078000070309     c*  può cambiare l'indicatore del picking messaggerie S/N
078100070312     c                   if        d1CFGS <> savfgs
078200070313     c                   if        (sav_in08='0' AND *IN08)   or
078300070313     c                             (sav_in08='1' and not *in08)
078400070309     c                   seton                                            99
078500070309     c                   endif
078600070313     c                   eval      sav_in08 = *in08
078700070312     c                   eval      savfgs  = d1CFGS
078800070309     c                   endif
078900070309     C*------------------------
079000070309      * CONTROLLO DATA INIZIALE
079100070309     C                   Z-ADD     0             DATINW
079200070312    1C     d1CDIN        IFNE      0
079300070312     C                   MOVE      d1CDIN        G02DAT
079400070309     C                   MOVEL     *BLANK        G02ERR
079500070309     C                   CALL      'XSRDA8'
079600070309     C                   PARM                    WLBDAT
079700070309    2C     G02ERR        IFEQ      '1'
079800070309     C                   SETON                                        57  99
079900070312     C                   GOTO      end_CT1
080000070309    2C                   END
080100070309     C                   Z-ADD     G02INV        DATINW
080200070312     C                   Z-ADD     G02DAT        d1CDIN
080300070309    1C                   END
080400070309      *
080500070309      * CONTROLLO DATA FINALE
080600070312    1C     d1CDFI        IFEQ      0
080700070312     C                   Z-ADD     31129999      d1CDFI
080800070309     C                   Z-ADD     99991231      DATFIW
080900070309   X1C                   ELSE
081000070312     C                   MOVE      d1CDFI        G02DAT
081100070309     C                   MOVEL     *BLANK        G02ERR
081200070309     C                   CALL      'XSRDA8'
081300070309     C                   PARM                    WLBDAT
081400070309    2C     G02ERR        IFEQ      '1'
081500070309     C                   SETON                                        58  99
081600070312     C                   GOTO      end_CT1
081700070309    2C                   END
081800070309     C*
081900070309     C                   Z-ADD     G02INV        DATFIW
082000070312     C                   Z-ADD     G02DAT        d1CDFI
082100070309      *
082200070309     C*------------------------
082300070309      * DATA INIZIALE MAGGIORE DATA FINALE
082400070309    2C     DATINW        IFGT      DATFIW
082500070309     C                   SETON                                        54  99
082600070312     C                   GOTO      end_CT1
082700070309    2C                   END
082800070309    1C                   END
082900070402      *?  Fare attenzione: si Abilita il posizionamento x Distinta...  ?
083000070402      *?   Solo se il range di date indica lo stesso giorno.
083100070402      *?   Si utilizza l'indicatore 13 per abilitare o meno il campo.
083200070402     c                   setoff                                       13
083300070403     c                   eval      cur_su_SFL = 'S'
083400070404    2C                   IF        DATFIW = DATINW  and
083500070404     c                                pgm_da_call <> 'S'
083600070403     c                   eval      cur_su_SFL = 'N'
083700070402     c                   seton                                        13
083800070402     c                   end
083900070309     C*------------------------
084000070309      * NUMERO FOGLIO
084100070312     c                   clear                   wnfv
084200070312    1C     d1CNFV        IFGT      0
084300070309     C* VEDO SE E' FOGLIO DI FILIALE GESTIBILE
084400070312     c                   z-add     d1CNFV        wnfv
084500070406     C     Kdst9         CHAIN     Fidst09L                           59
084600070309     C* FOGLIO INESISTENTE
084700070309    2C     *IN59         IFEQ      *ON
084800070309     C                   SETON                                          99
084900070312     C                   GOTO      end_CT1
085000070309    1C                   ENDIF
085100070312    1C                   ENDIF
085200070309     C*
085300070309     C*------------------------
085400070309     c                   move      'N'           Solo_aperte       1
085500070309     C* RICHIESTI SOLO FOGLI APERTI
085600070312     C     d1CTFV        IFEQ      '2'
085700070419     C     d1CTFV        orEQ      '3'
085800070309     c                   move      'S'           Solo_aperte
085900070309     C                   END
086000070309     C*------------------------
086100070309      * ?- Codice Giro
086200070309     c                   clear                   con_giro
086300070312     c                   clear                   d1dcgi
086400070312     c     '?'           scan      D1CCGI
086500070309     C*   ricerca codice giro
086600070309     c                   if        %Found
086700070309     C                   SETON                                            99
086800070309     c                   clear                   fidg09ds
086900070309     c                   move      'P01'         d09iop0
087000070312     c                   movel     d1CFGS        d09ifgs
087100071010     c*****              move      'C'           d09itug
087200071010     c                   move      d1DATI        d09itug
087300070309     c                   z-add     DATEU8        d09idat
087400070313     c                   eval      kpjbus = kpjbu
087500070309     c                   movel(p)  fidg09ds      kpjbu
087600070309     c                   call      'FIDG09R'
087700070309     c                   parm                    kpjba
087800070309     c                   movel     kpjbu         fidg09ds
087900070313     c                   eval      kpjbu = kpjbus
088000070320     c                   movel     d09oflo       dGRGflo
088100070320      * se il giro può essere messo in distinta
088200070320     c                   if        §grgDST <> 'N'
088300070312     c                   movel     d09ocgi       D1CCGI
088400070320     c                   end
088500070309     c                   end
088600070309      *
088700070309      *  controllo validità giro
088800070312     c                   if        D1CCGI <> *blank
088900070309     c                   clear                   fidg09ds
089000070309     c                   eval      D09IOP0 = '001'
089100070312     c                   eval      d09ifgs = d1CFGS
089200071010     c******             eval      d09itug = 'C'
089300071010     c                   eval      d09itug = d1DATI
089400070312     c                   eval      d09icgi = D1CCGI
089500070309     c                   move      dateu8        d09idat
089600070313     c                   eval      kpjbus = kpjbu
089700070309     c                   eval      kpjbu = fidg09ds
089800070309     c                   call      'FIDG09R'
089900070309     c                   parm                    kpjba
090000070309     c                   eval      fidg09ds = kpjbu
090100070320     c                   movel     d09oflo       dGRGflo
090200070313     c                   eval      kpjbu = kpjbus
090300070320     c                   if        d09oerr = '1' or §grgDST = 'N'
090400070309     C                   SETON                                        55  99
090500070312     C                   goto      end_CT1
090600070309     c                   else
090700070309     c                   if        d09ocgi <> ' '
090800070312     c                   eval      d1dcgi = d09odes
090900070309     c                   move      'S'           con_giro
091000070312      *
091100070312      **  Se impostato un Foglio e non ci sono giri sul foglio
091200070312      **   deve segnalare l'errore sul giro scelto
091300070312     c                   if        D1Cnfv  > *zero
091400070312     c                   move      'N'           Giro_in_NFV       1
091500070910     c                   if        d1dati = 'C'
091600070910      **
091700070312     c     kdst31        setll     fidst31l
091800070312     c     kdst31        reade     fidst31l
091900070910     c                   dow       not %Eof(fidst31l)
092000070312      **
092100070312      **  Controlla presenza del giro richiesto
092200070312     c                   if        %char(DST3NPG) = d1cNPG and
092300070312     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
092400070312     c                             and DST3CGI = d1cCGI
092500070312     c                   move      'S'           Giro_in_NFV       1
092600070312     c                   leave
092700070312     c                   end
092800070312      **
092900070312     c     kdst31        reade     fidst31l
093000070312     c                   endDo
093100070312      **
093200070910     c                   else
093300070910      **
093400070910     c     kdst51        setll     fidst51l
093500070910     c     kdst51        reade     fidst51l
093600070910     c                   dow       not %Eof(fidst51l)
093700070910      **
093800070910      **  Controlla presenza del giro richiesto
093900070910     c                   if        %char(DST5NPG) = d1cNPG and
094000070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
094100070910     c                             and DST5CGI = d1cCGI
094200070910     c                   move      'S'           Giro_in_NFV       1
094300070910     c                   leave
094400070910     c                   end
094500070910      **
094600070910     c     kdst51        reade     fidst51l
094700070910     c                   endDo
094800070910      **
094900070910     c                   end
095000070910      **
095100070312      ** Il giro selezionato non è presente nella distinta
095200070312     c                   if        Giro_in_NFV = 'N'
095300070312     C                   SETON                                        52  99
095400070312     C                   goto      end_CT1
095500070312     c                   end
095600070312      **
095700070312     c                   end
095800070312      **
095900070309     c                   end
096000070309     c                   end
096100070309     c                   end
096200070309      *
096300070309     C*------------------------
096400070309     c* Per visualizzare gli importi rchiedere tipi fogli = 2
096500070312     c                   if        d1IMPO='S' and d1CTFV<>'2'
096600070312     C                   MOVEL     MSG(3)        d1Cmsg
096700070309     C                   SETON                                        56  99
096800070312     C                   GOTO      end_CT1
096900070309     c                   endif
097000070309     C*------------------------
097100070312     C     end_CT1       TAG
097200070309     c   99              move      'S'           $VISD1
097300070309     c                   move      *in99         *in28
097400070309     C*
097500070309     C                   ENDSR
097600070301     C/EJECT
097700070312      *---------------------------------------------------------------*
097800070321      *?  GESTIONE SFL 1                       ?
097900070312      *---------------------------------------------------------------*
098000070321     C     GESS1         BEGSR
098100070312     C*
098200070312     C* inizializzazione videata
098300070321     C     $INZS1        IFEQ      *ON
098400070321     C                   EXSR      INZS1
098500070419     C*
098600070419     C* Se richiamato x il test del FIMS03R esce forzatamente
098700070419     c                   if        pgm_da_call = 'S' and sav_in19 = *on
098800070419     c                             and  FDGtfv = '3'
098900070419     c                   LeaveSr
099000070419     c                   end
099100070419     C*
099200070321     C                   MOVE      *OFF          $INZS1
099300070312     C                   ENDIF
099400070312     C*
099500070312     C* Inizializza e imposta piede videata
099600070321     C                   EXSR      INZZ1
099700070312     C* emissione piede videata
099800070312      * ?            *-----------------------*
099900070321     C                   WRITE     DG31Z02
100000070312      * ?            *-----------------------*
100100070312     C*
100200070312     C     WMAX          IFEQ      0
100300070312     C* Non ci sono records
100400070312      * ?            *-----------------------*
100500070312     C                   WRITE     DG31V02
100600070312      * ?            *-----------------------*
100700070312     c                   setoff                                       30
100800070312     C                   Else
100900070312     c                   seton                                        30
101000070312      *
101100070312     C     Wsfl          IFgt      0
101200070321     C                   Z-ADD     wsfl          C1RCD
101300070312     C                   Else
101400070312     C     Wpag          IFgt      0
101500070321     C                   Z-ADD     wpag          C1RCD
101600070312     C                   EndIF
101700070312     C                   EndIF
101800070312     C                   ENDIF
101900071026      * -----------------------
102000071026      * ?  reimposta su quale riga deve posizionarsi il cursore
102100071026      * ?    se non si è chiesto l'ordinamento
102200071026     C                   if        $TASTO   <>  F11
102300071026     C                   z-add     Su_Quale_rec  h1riga
102400071026     C                   EndIF
102500071026      * -----------------------
102600071026      *
102700070312      *  Emissione SFL dei GIRI
102800070312     c                   setoff                                       99
102900070419     c                   if        $VISS1 = 'S'
103000070312      * ?            *-----------------------*
103100070321     C                   EXFMT     DG31CT1
103200070312      * ?            *-----------------------*
103300070321     C                   movel     'S1'          Sav_$GEST
103400070419     c                   end
103500070312      *
103600070321     C     C1NRR         IFNE      0
103700070321     C                   Z-ADD     C1NRR         WSFL
103800070320     C                   Else
103900070320     C     SFLNRR        IFgt      0
104000070320     C                   Z-ADD     SFLNRR        WSFL
104100070320     c                   EndIF
104200070320     C                   ENDIF
104300070321     C                   Z-ADD     SFLNRR        C1RCD
104400070312     C* Selezioni
1045000703121    C                   SELECT
104600070313     C* F11=Cambia Ordinamento del SFL
104700070313     C                   WHEN      $TASTO   =   F11
104800070417     c*********          move      *on           $inzs1
104900070321     C                   clear                   C1DAUT
105000070321     C                   clear                   C1CNFV
105100070321     C                   clear                   sav_c1nfv
105200070321     C                   clear                   sav_c1aut
105300070403     C                   EXSR      F11_ordina1
105400070312     C* F3=Fine
105500070312     C     $TASTO        WHENEQ    F03
105600070312     C                   EXSR      F03_exit
105700070321     c                   goto      end_s01
105800070312     C* F12=Ritorno
105900070312     C     $TASTO        WHENEQ    F12
106000070312     c                   move      'D1'          $GEST
106100070330     C                   clear                   C1DAUT
106200070330     C                   clear                   C1CNFV
106300070330     C                   clear                   sav_c1nfv
106400070330     C                   clear                   sav_c1aut
106500070326     c                   clear                   d1ccgi
106600070312     C*    deve andare a fine se richiamato
106700070312     c                   if        pgm_da_call = 'S'
106800070326     c                   clear                   fdgcgi
106900070326     c                   clear                   fdgnfv
107000070312     C                   EXSR      F03_exit
107100070312     c                   end
107200070321     c                   goto      end_s01
107300070312      *
1074000703121O   C                   OTHER
107500070312     C* CONTROLLO testata
107600070321     C                   EXSR      CTRC1
107700070312     C* CONTROLLO subfile
107800070312     C     *IN99         IFEQ      *OFF
107900070321     C                   EXSR      CTRS1
108000070320
108100070312     C                   END
108200070312      *
108300070312      * ?Emette la videata di Dettaglio Distinta
108400070312     C     *IN99         IFEQ      *OFF
108500070321     C********           MOVE      *ON           $INZD2
108600070312     C********           movel     'D2'          $GEST
108700070312     C                   END
108800070312      *
1089000703121-   C                   ENDSL
109000070312     C*
109100070321     c     end_s01       tag
109200070312     C                   ENDSR
109300070312     C/EJECT
109400070312      *---------------------------------------------------------------*
109500070321      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL ?
109600070312      *---------------------------------------------------------------*
109700070321     C     INZZ1         BEGSR
109800070312     C*
109900070312     C*
110000070312     C                   ENDSR
110100070312     C/EJECT
110200070312      *---------------------------------------------------------------*
110300070321      *?  INIZIALIZZAZIONE LISTA dei GIRI da selezionare ?
110400070312      *---------------------------------------------------------------*
110500070321     C     INZS1         BEGSR
110600070314     C*
110700070312     C* reset indicatori DSPATR
110800070312     c                   exsr      INZ_indERR
110900070312     C* pulizia SFL
111000070312     C                   SETOFF                                         3031
111100070321     C                   WRITE     DG31CT1
111200070314     C                   SETON                                          31
111300070320      *
111400070314     C* Posiziona il cursore in testata x 12 = *on
111500070320     c                   if        cur_su_SFL = 'N'
111600070321     c     $PcurS1       comp      *on                                    12
111700070320     C* imposto pos. del cursore
111800070320     C                   eval           H1RIGA = 3
111900070528     C                   eval           H1COLO = 17
112000070315     c                   else
112100070320     c                   move      'N'           cur_su_SFL        1
112200070315     c                   setoff                                           12
112300070320     C* imposto pos. del cursore
112400070320     C                   eval           H1RIGA = 7
112500070320     C                   eval           H1COLO = 2
112600070315     c                   end
112700071026     C* se siamo al caricamento del SFL si deve posizionare sulla prima riga disponibile
112800071026     c                   z-add     1             Che_NRec_sfl
112900071026     C                   z-add     h1riga        Su_Quale_rec
113000071026     C                   move      'S'           prima_volta
113100080219     c                   eval      *in15 = sav_in15
113200070320     C*
113300070314     C* Se richiamato attivo 04
113400070528     C*   se però è richiamato da Assegnazione attivo 05
113500070620     c                   setoff                                       0503
113600070314     c                   if        pgm_da_call = 'S'
113700070314     C                   SETON                                          04
113800070404     c                   move      *on           sav_in04
113900070620     c                   if        fdgpgm = 'FIDG30R'
114000070620     C                   SETON                                          03
114100070620     c                   move      *on           sav_in03
114200070620     c                   end
114300070528     c                   if        fdgOPZ = '1'
114400070528     C                   SETON                                          05
114500070528     c                   move      *on           sav_in05
114600070314     c                   end
114700070528     c                   end
114800070312     C*
114900070312     C* Righe x Pagina del 1° SFL
115000071026     c                   z-add     16            s1pag
115100070312      *
115200070312     C* CARICAMENTO SFL totale
115300070321     C                   Z-ADD     0             S1NRR
115400070321     C                   Z-ADD     1             C1RCD
115500070312     C                   Z-ADD     0             WMAX
115600070312     C                   Z-ADD     0             Wsfl
115700070312     C* campi titolo
115800070321     C                   MOVEL     TES(1)        c1CTES
115900070606     c                   if        fdgPGM = 'FIDG30R'
116000070606     C                   MOVEL(p)  TES(2)        c1CTES
116100070606     c                   end
116200070321     C                   MOVEL     dsPGM         c1CPGM
116300070321     C                   MOVEL     RAGUT         c1CDSA
116400070321     C                   MOVEL     knsif         c1nsif
116500070321     C                   MOVEL     knmus         c1nmus
116600070321     C                   z-add     d1CFGS        c1cfgs
116700070321     C                   movel     d1DFGS        c1dfgs
116800070321     C                   movel     d1ccgi        c1ccgi
116900070830     c   08              movel     'Prc'         c1PRC
117000070312     C*
117100070312     C* Posizionamento su file pilota e impostazione primo record da leggere
117200070312     c                   clear                   Prima_Lettura     1
117300070312      *
117400070406     c                   if        Solo_aperte = 'N'
117500070312     c     Kdat1         setll     fidst008
117600070406     c                   else
117700070312     C     Kdst3         SETLL     Fidst003
117800070406     c                   End
117900070312     C* Carico il SFL
118000070321     C                   EXSR      ROLS1
118100070419     C*
118200070419     C* Se richiamato x il test del FIMS03R esce forzatamente
118300070419     c                   if        pgm_da_call = 'S' and sav_in19 = *on
118400070419     c                             and  FDGtfv = '3'
118500070419     c                   LeaveSr
118600070419     c                   end
118700070312     C*
118800070312     C                   Z-ADD     1             WPAG
118900070312     C*
119000070313     ** Al primo F11 ordino per Autista
119100070321     C                   EVAL      RrnLast = S1Nrr
119200070313     c                   if        sav_in11 = *blank
119300070417     C********           EVAL      WrkSort = Autista
119400070417     C                   EVAL      WrkSort = Numero
119500070417     c                   exsr      F11_ordina1
119600070313     c                   setoff                                       11
119700070313     c                   else
119800070313     c                   if        sav_in11 = *on
119900070313     C                   EVAL      WrkSort = Autista
120000070402     c                   exsr      F11_ordina1
120100070313     c                   end
120200070313     c                   end
120300070320     C*
120400070312     C                   ENDSR
120500070312     C/EJECT
120600070301      *---------------------------------------------------------------*
120700070321      *?  CARICAMENTO PAGINA                  ?
120800070301      *---------------------------------------------------------------*
120900070321     C     ROLS1         BEGSR
121000070301     C*
121100070301     C                   SETOFF                                       32
121200070301     C                   Z-ADD     0             Y
121300070321     C                   Z-ADD     WMAX          S1NRR
121400070301     C*
121500070312     C* Caricamento del Sfl x Giro
121600070321     c                   Exsr      Write_S1
121700070301     C* Fine Caricamento
121800070321     C                   Z-ADD     S1NRR         WMAX                 30
121900070419     C*
122000070419     C* Se il programma è richiamato e non ha trovato dei records
122100070419     c                   if        Wmax = 0 and pgm_da_call = 'S'
122200070419     c                   move      '9'           FDGerr
122300070419      * esce forzatamente
122400070419     c                   move      'S'           $ESCI
122500070419     C                   MOVE      *ON           $FINE
122600070419     c                   end
122700070419     C*
122800070302     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
122900070321     C     S1NRR         DIV       S1pag         PAGINE            4 0
123000070301     C                   MVR                     RESTO             3 0
123100070321     C     PAGINE        MULT      S1pag         C1RCD
1232000703011    C     RESTO         IFGT      0
123300070321     C                   ADD       1             C1RCD
1234000703011E   C                   ELSE
123500070321     C                   SUB       S1pag         C1RCD
123600070321     C                   ADD       1             C1RCD
1237000703011-   C                   ENDIF
123800070301     C*
123900070301     C                   ENDSR
124000070312      *---------------------------------------------------------------*
124100070321      *?  Scrive il SFL   ?
124200070312      *---------------------------------------------------------------*
124300070321     C     Write_S1      BEGSR
124400070312     C*
124500070312     c                   exsr      Legge_rec
124600070312     C*
1247000703121    C     $EFILE        DowEQ     *OFF
124800070312     C*
124900070321     C                   clear                   DG31SF1
125000070404     c                   move      sav_in04      *in04
125100070528     c                   move      sav_in05      *in05
125200080219     c                   move      sav_in15      *in15
125300070510     C* Priorità
125400070510     C                   z-add     dst3prgg      h1prgg
125500070510     C                   z-add     dst3sql       h1sql
125600070313     C* GIRO DATA Distinta
125700070417     C                   MOVE      dstDFV        h1CDFV
125800070312     C                   MOVE      dstDFV        G02INV
125900070312     C                   MOVEL     '3'           G02ERR
126000070312     C                   CALL      'XSRDA8'
126100070312     C                   PARM                    WLBDAT
126200070312     C*
126300070313     C                   movel     G02DAT        campo4            4
126400070321     C                   move      G02DAT        s1CDFV
126500070321     C                   movel     campo4        s1CDFV
126600070312      *
126700070321     C                   Z-ADD     dstNFV        s1CNFV
126800070312     c     kapd          chain     fiapd01l
126900070312     c                   if        %found(fiapd01l)
127000070321     c                   eval      s1DAUT = apdrsc
127100070312     c                   else
127200070321     c                   clear                   s1DAUT
127300070312     c                   end
127400070312      *
127500070321     c                   z-add     DSTPKL        s1cpem
127600070321     c                   z-add     DSTVLU        s1cvom
127700070326      *
127800070326     c                   if        s1cpem > 0 or s1cvom > 0
127900070326     c                   movel     'Max'         s1max
128000070326     c                   end
128100070313      *
128200070321     c                   z-add     DSTPKLa       s1cpea
128300070321     c                   z-add     DSTVLUa       s1cvoa
128400070905     c                   if        DSTfpp  ='M'
128500070905     c                   movel     'Mat'         s1Tusc
128600070905     c                   else
128700070905     c                   move      'Pom'         s1Tusc
128800070905     c                   end
128900070820     c                   clear                   s1STP
129000070820     c                   if        DSTfasd ='STP'
129100070820     c                   movel     'St'          s1STP
129200070820     c                   endif
129300070313      *
129400070312     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
129500070625     c                   if        *in08
129600070312     c                   if        dsttpm='M'
129700070321     c                   movel     'Mes'         s1CTPM
129800070312     c                   endif
129900070312     c                   if        dsttpm<>'M'
130000070321     c                   movel     'Prc'         s1CTPM
130100070312     c                   endif
130200070312     c                   endif
130300070312     c
130400070312      * Flag tipo distinta (solo consegna, orm o entrambi)
130500070312     C                   CLEAR                   WBOL
130600070312     C                   CLEAR                   WORM
130700070312     c                   Z-Add     d1CFGS        Wifp
130800070321     C                   Z-ADD     s1CNFV        kfnvv
130900070321     C                   Z-ADD     s1CNFV        WnFV
131000070312     c     Karb78        Setll     Fnarb78l
131100070312if  5c                   if        %equal(fnarb78l)
131200070312     c                   movel     'S'           wbol              1
131300070312x   5c                   else
131400070312     C     Kdst          SETLL     FnBRV01L
131500070312     c                   if        %equal(fnbrv01l)
131600070312     c                   movel     'S'           wbol              1
131700070312     c                   endif
131800070312e   5c                   endif
131900070312     C     KORM          SETLL     FNORM06L
132000070312do  5c                   do        *hival
132100070312     C     KORM          reade     FNORM06L
132200070312     c                   if        %eof(fnorm06l)
132300070312     c                   leave
132400070312     c                   endif
132500070312     c                   if        ormatb = *blanks and ormfao < 999
132600070312     C                   move      'S'           worm              1
132700070312     c                   endif
132800070312e   5c                   enddo
132900070312     C                   if        wbol = 'S' and worm <> 'S'
133000070321     C                   movel     'CON'         s1CTPD
133100070312     C                   endif
133200070312     C                   if        wbol <> 'S' and worm = 'S'
133300070321     C                   movel     'ORM'         s1CTPD
133400070312     C                   endif
133500070312     C                   if        wbol = 'S' and worm = 'S'
133600070321     C                   movel     'C/O'         s1CTPD
133700070312     C                   endif
133800070312      * Calcolo importi se non già fatto prima
133900070312if  5c                   if        d1CTFV = '2' and d1IMPO = 'S'
134000070312     c                   exsr      totalizzo
134100070321     c                   z-add     w_imas        s1IMAS
134200070321     c                   z-add     w_imca        s1IMCA
134300070312     c                   endif
134400070321      * Pda
134500070321     C                   movel     dstPDA        s1CPDA
134600080207     C                   movel     dstflr        DDStflr
134700080208     c                   setoff                                       14
134800080208     c                   if        dstDFV >= DateU8
134900080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
135000080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
135100080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
135200080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
135300080208     c                   seton                                        14
135400080208     c                   endIf
135500080208     c                   endIf
135600080222     c                   eval      h1in14 = *in14
135700080222      *
135800080208      *  lo Minuscolizza sempre a video
135900080207     C     Up:Lo         XLATE     §DSTtstPDA    s1CTSTPDA
136000070321      * ANNULLATO
136100070312     C     dstATB        IFEQ      'A'
136200070321     C                   MOVEL     'ANN'         s1CANN
136300070312     C                   END
136400070312if  5C     dstFCF        IFNE      ' '
136500070312      * CHIUSO
136600070312     C     dstFCF        IFEQ      'S'
136700070403     C                   MOVEL     'C   '        s1CFCF
136800070312     C                   ELSE
136900070312     C* BLOCCATO
137000070403     C                   MOVEL     'BL  '        s1CFCF
137100070312e   5C                   ENDIF
137200070312e   4C                   ENDIF
137300070315      *
137400070315      * Intestazioni seconda riga
137500070830     c                   setoFF                                       18
137600070321     c                   if        s1imca <> 0
137700070321     c                   movel     Imp_C_A       s1dica
137800070830     c                   setoN                                        18
137900070315     c                   end
138000070315      *
138100070830     c                   setoFF                                       20
138200070321     c                   if        s1imas <> 0
138300070321     c                   movel     Imp_ASS       s1dias
138400070830     c                   setoN                                        20
138500070315     c                   end
138600070510      *
138700070510     c                   setoff                                       40
138800070510if  4c                   if        h1prgg = 1
138900070510     c                   seton                                        40
139000070510     c                   end
139100070312      *---------
139200070313if  4c                   if        FDGtfv <> '3'
139300070321     c                   if        (sav_c1nfv <= s1cnfv and *in11 = *off) or
139400070321     c                             (sav_c1aut <= s1daut and *in11 = *on)  or
139500070321     c                             (sav_c1aut = *blank and sav_c1nfv =0)
139600070404     C* Imposta indicatore x abilitare campo scelta
139700070404     c                   move      sav_in19      *in19
139800070312     c********           seton                                        32
139900070321     C                   ADD       1             S1NRR
140000070312     C                   ADD       1             Y
140100070321     C                   WRITE     DG31SF1
140200070313     c                   end
140300070312e   4c                   endif
140400070312      *---------
140500070312      *  Esce da qui con un record di Totale x GIRO
140600070312     c                   exsr      Legge_rec
140700070312     c                   EndDO
140800070321      *
140900070312     C                   ENDSR
141000070321     C/EJECT
141100070321      *---------------------------------------------------------------*
141200070321      *?  CONTROLLO TESTATA LISTA   ?
141300070321      *---------------------------------------------------------------*
141400070321     C     CTRC1         BEGSR
141500070321      *
141600070321     C                   MOVE      *OFF          *IN99
141700070321      *
141800070321     C*------------------------
141900070321      *  interrogazione Giro
142000070321     c                   clear                   c1dcgi
142100070321     c     '?'           scan      c1cCGI
142200070321     C*   ricerca codice giro
142300070321     c                   if        %Found
142400070321     c                   clear                   fidg09ds
142500070321     c                   move      'P01'         d09iop0
142600070321     c                   movel     c1cFGS        d09ifgs
142700071010     c*****              movel     'C'           d09itug
142800071010     c                   movel     d1DATI        d09itug
142900070321     c                   z-add     DATEU8        d09idat
143000070321     c                   eval      kpjbus = kpjbu
143100070321     c                   movel(p)  fidg09ds      kpjbu
143200070321     c                   call      'FIDG09R'
143300070321     c                   parm                    kpjba
143400070321     c                   movel     kpjbu         fidg09ds
143500070321     c                   eval      kpjbu = kpjbus
143600070321     c                   movel     d09ocgi       c1cCGI
143700070321     c                   end
143800070321     C*------------------------
143900070321      *  controllo validità giro
144000070321     c                   if        c1cCGI <> *blank
144100070321     c                   clear                   fidg09ds
144200070321     c                   eval      D09IOP0 = '001'
144300070321     c                   eval      d09ifgs = c1cFGS
144400071010     c****               eval      d09itug = 'C'
144500071010     c                   eval      d09itug = d1DATI
144600070321     c                   eval      d09icgi = c1cCGI
144700070321     c                   move      dateu8        d09idat
144800070321     c                   eval      kpjbus = kpjbu
144900070321     c                   eval      kpjbu = fidg09ds
145000070321     c                   call      'FIDG09R'
145100070321     c                   parm                    kpjba
145200070321     c                   eval      fidg09ds = kpjbu
145300070321     c                   eval      kpjbu = kpjbus
145400070321     c                   if        d09oerr = '1'
145500070321     C                   SETON                                        54  99
145600070322     C                   goto      ENDC1
145700070321     c                   else
145800070321     c                   if        d09ocgi <> ' '
145900070321     c                   eval      c1dcgi = d09odes
146000070321     c                   move      'S'           con_giro
146100070321     c                   end
146200070321     c                   end
146300070321      *
146400070321     c                   end
146500070321     C*------------------------
146600070321      *  interrogazione AUT
146700070321      *
146800070321     c     '?'           scan      c1daut
146900070321     C*   ricerca codice giro
147000070321     c                   if        %Found
147100070321     c                   clear                   fnlv24ds
147200070321     C                   Z-ADD     c1cFGS        d24FIL
147300070321     C                   MOVEL     'R'           d24FLG
147400070321     C                   MOVEL     'A'           d24tip
147500070321     c                   eval      kpjbus = kpjbu
147600070321     C                   MOVEL(p)  fnlv24ds      KPJBU
147700070321     C                   CALL      'FNLV24R'
147800070321     C                   PARM                    KPJBA
147900070321     C                   MOVEL     KPJBU         fnlv24ds
148000070321     c                   eval      kpjbu = kpjbus
148100070321     C                   MOVEL     D24RSC        c1daut
148200070321     c                   end
148300070321     C*
148400070321     C*------------------------
148500070321     c                   move      *on           $PcurS1
148600070321     C* Riposizionamento x AUT o x Numero
148700070321     c                   if        (*in11 = *off and c1cnfv <> sav_c1nfv) or
148800070321     c                             (*in11 = *on  and c1daut <> sav_c1aut)
148900070321     c                   move      *off          $PcurS1
149000070321     c                   move      *on           $inzs1
149100070321     c                   move      'S1'          $GEST
149200070321     c  n11              z-add     c1cnfv        sav_c1nfv
149300070321     c   11              movel     c1daut        sav_c1aut
149400070321     c                   end
149500070321     C*
149600070321     C* Posiziona il cursore in testata x 12 = *on
149700070321     c     $PcurS1       comp      *on                                    12
149800070321     C  n12              eval           H1RIGA = 3
149900070528     C  n12              eval           H1COLO = 17
150000070321     C   12              eval           H1RIGA = 7
150100070321     C   12              eval           H1COLO = 2
150200070321      *
150300070321     C*------------------------
150400070321      *  se tutto OK
150500070321     c                   if        c1ccgi <> d1ccgi
150600070321     c                   move      c1ccgi        d1ccgi
150700070321     c                   move      *on           $inzs1
150800070321     c                   move      'S1'          $GEST
150900070321     c                   end
151000070321      *
151100070321     C*------------------------
151200070321      *
151300070321     c     endc1         tag
151400070321     C                   ENDSR
151500070321      *---------------------------------------------------------------*
151600070321      *?  CONTROLLO OPZIONI LISTA   ?
151700070321      *---------------------------------------------------------------*
151800070321     C     CTRS1         BEGSR
151900070321     C*
152000070321     C                   MOVEL     *OFF          $ESCI
152100070321     C                   SETOFF                                       99
152200070321     C*
152300070321     C* Leggo il sfl solo se ci sono rcd
1524000703211    C     WMAX          IFGT      0
152500070321     C                   READC     DG31SF1                                21
152600070321     C*
152700070321     C* esce se fine sfl o errore che richiede l'uscita
1528000703212    C     *IN21         DOWEQ     *OFF
152900070321     C     $ESCI         ANDEQ     *OFF
153000070321     C                   Z-ADD     S1NRR         C1RCD
153100071026     c                   eval      Che_NRec_sfl = s1nrr
153200070321     C* ctrl su riga
153300070321     C                   EXSR      RECS1
153400070321     C*
153500070321     C* gestione opzioni
1536000704043    C     s1SCE         IFNE      *blank
153700070321     C     *IN99         ANDEQ     *OFF
153800070321     C                   EXSR      SCE_S1
1539000703213-   C                   ENDIF
154000070321      *
154100070321     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1542000703213    C     *IN99         IFEQ      *ON
154300070321     C     $ESCI         OREQ      *ON
154400070321     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
154500070321     C* la ripristinerò a conclusione del ciclo di READC
154600070321     C                   MOVE      *OFF          $INZS1
1547000703213-   C                   ENDIF
154800070321     C*
154900070321      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
155000070404     c                   if        s1SCE  <> *blank
155100070321     C******             MOVE      *ON           *IN32
155200070404     c                   clear                   s1SCE
155300070321     c                   end
155400070510      *
155500070510     c                   setoff                                       40
155600070510if  4c                   if        h1prgg = 1
155700070510     c                   seton                                        40
155800070510     c                   end
155900080222     c                   eval      *in14 = h1in14
156000070510      *
156100070321     C                   UPDATE    DG31SF1
156200070321     C*
156300070321     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1564000703213    C     $ESCI         IFEQ      *OFF
156500070321     C                   READC     DG31SF1                                21
156600070321      *
156700070321     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1568000703214    C     *IN21         IFEQ      *ON
156900070321     C* calcolo pagina a cui deve posizionarsi
157000070321     c                   z-add     s1pag         Wpagine
157100070321     C                   EXSR      CLCPAG
1572000703214-   C                   ENDIF
1573000703213-   C                   ENDIF
157400070321     C*
1575000703212-   C                   ENDDO
157600071026     C*
157700071026      * -----------------------
157800071026      * ? calcola "su quale record" riposizionarsi tenendo conto che
157900071026      * ?  il SFL ha una testata di 6 righe ed inizia dalla 7°.
158000071026     c     Che_NRec_sfl  div       s1pag         Su_Quale_rec
158100071026     c                   mvr                     resto
158200071026     C     Resto         IFeq      0
158300071026     C                   z-add     16            Su_Quale_rec
158400071026     C                   Else
158500071026     C                   z-add     resto         Su_Quale_rec
158600071026     C                   ENDIF
158700071026      * ?  aggiunge le 6 righe di testata.
158800071026     C                   add       6             Su_Quale_rec
158900071026      * -----------------------
159000070321     C*
1591000703211-   C                   ENDIF
159200070321     C*
159300070321     C                   ENDSR
159400070321     C/EJECT
159500070321      *---------------------------------------------------------------*
159600070321      *?  CONTROLLO CAMPI I/O RIGA LISTA  ?
159700070321      *---------------------------------------------------------------*
159800070321     C     RECS1         BEGSR
159900070321     C*
160000070321     C* reset indicatori DSPATR
160100070321     c                   exsr      INZ_indERR
160200070528     C*
160300070528     C* se non Richiamato non sono abilitate le opzioni 2 e 3
160400070528     c                   if        pgm_da_call <> 'S'
160500070528     c                   if        s1sce = '2' or s1sce = '3'
160600070528     c                   seton                                        99  41
160700070528     c                   end
160800070528     c                   end
160900070528     C* Se richiamato in assegnazione non deve accettare opz 2,3
161000070620     c                   if        (pgm_da_call =  'S' and fdg31_opz = '1' and
161100070620     c                             fdgPGM = 'FIDG30R') or
161200070620     c                             fdgPGM <>'FIDG30R'
161300070606     c                   if        s1sce = '2' or s1sce = '3'
161400070528     c                   seton                                        99  41
161500070528     c                   end
161600070528     c                   end
161700070321     C*
161800070620     c                   if        pgm_da_call =  'S' and fdg31_opz <> '1' and
161900070620     c                             fdgPGM = 'FIDG30R'
162000070620     c                   if        s1sce = '1'
162100070620     c                   seton                                        99  41
162200070620     c                   end
162300070620     c                   end
162400070620     C*
162500070321     C                   ENDSR
162600070321      *---------------------------------------------------------------*
162700070321      *?  GESTIONE OPZIONI LISTA       ?
162800070321      *---------------------------------------------------------------*
162900070321     C     SCE_S1        BEGSR
163000070321     C*
163100070321     C* determino Riga/Colonna del cursore
163200070321     C                   MOVE      NRG           R$$
163300070321     C                   MOVE      NCL           C$$
163400070321     C                   Z-ADD     RIRI          CURR
163500070321     C                   Z-ADD     COCO          CURC
163600070321     C*
163700070404     c                   If        s1SCE  = '1'
163800070405     c                   if        pgm_da_call <> *Blank
163900070404     c                   move      *on           $ESCI
164000070404     c                   move      *on           $FINE
164100070404     c                   z-add     s1CNFV        FDGNFV
164200070528     c                   move      d1cnpg        FDGNPG
164300070528     c                   eval      fdgOPZ = s1SCE
164400070405     c                   else
164500070405     c                   z-add     s1CNFV        d1cNFV
164600070405     c                   move      'D1'          $GEST
164700070405     c                   end
164800070405     c                   end
164900070321      *
165000070528     c                   If        s1SCE  = '2' or s1SCE  = '3'
165100070528     c                   if        pgm_da_call <> *Blank
165200070528     c                   move      *on           $ESCI
165300070528     c                   move      *on           $FINE
165400070528     c                   z-add     s1CNFV        FDGNFV
165500070528     c                   move      d1cnpg        FDGNPG
165600070528     c                   eval      fdgOPZ = s1SCE
165700070528     c                   clear                   fdgCGI
165800070528     c                   end
165900070528     c                   end
166000070528      *
166100070404     c                   If        s1SCE  = '5'
166200070404      *
166300070321     c                   z-add     s1CNFV        d1CNFV
166400070321     c                   z-add     c1cfgs        d1CFGS
166500070321     c                   z-add     s1CNFV        wnfv
166600070330     c                   move      'D2'          $GEST
166700070330     C                   move      'S'           $VISD2
166800070330     c                   move      *on           $INZD2
166900070321      *
167000070321     c                   end
167100070321     C*
167200070621     c                   If        s1SCE  = '7'
167300070427     c                   clear                   fidg35ds
167400070528     c                   eval      D35FGSi = wfgs
167500070528     c                   eval      D35DDCi = H1CDFV
167600070528     c                   eval      D35NDCi = S1CNFV
167700070528     c                   eval      D35NPGi = wnpg
167800070831     c                   eval      D35PGMi = fdgPGM
167900070903      * si vuole fare vedere tutto
168000070903     c*****              eval      D35TPi  = 'C'
168100070905     c                   If        pgm_da_call <> *blank and FDGOPZ <> '1' and
168200070905     c                             fdgPGM = 'FIDG30R'
168300070528     c                   eval      D35opzi = 'S'
168400070528     c                   end
168500070427     c                   eval      kpjbus = kpjbu
168600070427     c                   movel(p)  fidg35ds      kpjbu
168700070427     c                   call      'FIDG35R'
168800070427     c                   parm                    kpjba
168900070427     c                   movel     kpjbu         fidg35ds
169000070427     c                   eval      kpjbu = kpjbus
169100070611     c                   if        pgm_da_call <> *Blank and fdgOPZ<>'1'
169200070528     c                   move      *on           $ESCI
169300070905     c                   if        d35FunO = '03'
169400070528     c                   move      *on           $FINE
169500070905     c                   end
169600070905     c                   if        d35opzO = '2' or d35opzO = '3'
169700070528     c                   z-add     s1CNFV        FDGNFV
169800070905     c                   end
169900070528     c                   move      d1cnpg        FDGNPG
170000070528     c                   eval      fdgOPZ = d35opzO
170100070528     c                   eval      fdgCGI = d35cgiO
170200070528     c                   end
170300070427     c                   end
170400070427     C*
170500070404     c                   If        s1SCE  = '9'
170600070321     c                   z-add     s1CNFV        wnfv
170700070321     c*******            move      *on           $ESCI
170800070321     c                   move      'D2'          $GEST
170900070330     C                   move      'N'           $VISD2
171000070321     c                   move      *on           $INZD2
171100070321     C                   exsr      GESD2
171200070321     c                   endIf
171300070404     C*
171400070321     C*
171500070321     C                   ENDSR
171600070321     C/EJECT
171700070321      *---------------------------------------------------------------*
171800070321      *?  GESTIONE SFL 2                       ?
171900070321      *---------------------------------------------------------------*
172000070321     C     GESS2         BEGSR
172100070321     C*
172200070321     C* inizializzazione videata
172300070321     C     $INZS2        IFEQ      *ON
172400070321     C                   EXSR      INZS2
172500070321     C                   MOVE      *OFF          $INZS2
172600070321     C                   ENDIF
172700070321     C*
172800070321     C* Inizializza e imposta piede videata
172900070321     C                   EXSR      INZZ2
173000070321     C* emissione piede videata
173100070321      * ?            *-----------------------*
173200070321     C                   WRITE     DG31Z02
173300070321      * ?            *-----------------------*
173400070321     C*
173500070321     C     WMAX          IFEQ      0
173600070321     C* Non ci sono records
173700070321      * ?            *-----------------------*
173800070321     C                   WRITE     DG31V02
173900070321      * ?            *-----------------------*
174000070321     c                   setoff                                       30
174100070321     C                   Else
174200070321     c                   seton                                        30
174300070321      *
174400070321     C     Wsfl          IFgt      0
174500070321     C                   Z-ADD     wsfl          C2RCD
174600070321     C                   Else
174700070321     C     Wpag          IFgt      0
174800070321     C                   Z-ADD     wpag          C2RCD
174900070321     C                   EndIF
175000070321     C                   EndIF
175100070321     C                   ENDIF
175200071029      * se si deve riposizionare
175300071029     c                   if        nrec_riposiz > 0
175400071029     C                   Z-ADD     nrec_riposiz  C2RCD
175500071029     C                   EndIF
175600071026      * -----------------------
175700071026      * ?  reimposta su quale riga deve posizionarsi il cursore
175800071026      * ?    se non si è chiesto l'ordinamento
175900071026     C                   if        $TASTO   <>  F11
176000071026     C                   z-add     Su_Quale_rec  h1riga
176100071026     C                   EndIF
176200071026      * -----------------------
176300070321      *
176400070321      *  Emissione SFL dei GIRI
176500070321     c                   setoff                                       99
176600070321      * ?            *-----------------------*
176700070321     C                   EXFMT     DG31CT2
176800070321      * ?            *-----------------------*
176900070321     C                   movel     'S2'          Sav_$GEST
177000071026      *
177100070321     C     C2NRR         IFNE      0
177200070321     C                   Z-ADD     C2NRR         WSFL
177300070321     C                   Else
177400070321     C     SFLNRR        IFgt      0
177500070321     C                   Z-ADD     SFLNRR        WSFL
177600070321     c                   EndIF
177700070321     C                   ENDIF
177800070321     C                   Z-ADD     SFLNRR        C2RCD
177900070321     C* Selezioni
1780000703211    C                   SELECT
178100070321     C* F11=Cambia Ordinamento del SFL
178200070321     C                   WHEN      $TASTO   =   F11
178300070417     c********           move      *on           $inzs2
178400071026     C***                clear                   C2DAUT
178500071026     C***                clear                   C2CNFV
178600071026     C***                clear                   sav_c2nfv
178700071026     C***                clear                   sav_c2aut
178800070322     C                   EXSR      F11_ordina2
178900070321     C* F3=Fine
179000070321     C     $TASTO        WHENEQ    F03
179100070321     C                   EXSR      F03_exit
179200070321     c                   goto      end_s02
179300070321     C* F12=Ritorno
179400070321     C     $TASTO        WHENEQ    F12
179500070321     c                   move      'D1'          $GEST
179600070326     c                   clear                   d1ccgi
179700070321     C*    deve andare a fine se richiamato
179800070321     c                   if        pgm_da_call = 'S'
179900070326     c                   clear                   fdgcgi
180000070326     c                   clear                   fdgnfv
180100071008     c                   clear                   fdgopz
180200071008     c                   move      '12'          fdgtfu
180300070321     C                   EXSR      F03_exit
180400070321     c                   end
180500070321     c                   goto      end_s02
180600070321      *
1807000703211O   C                   OTHER
180800070321     C* CONTROLLO testata
180900070321     C                   EXSR      CTRC2
181000070321     C* CONTROLLO subfile
181100070321     C     *IN99         IFEQ      *OFF
181200070321     C                   EXSR      CTRS2
181300070321
181400070321     C                   END
181500070321      *
181600070321      * ?Emette la videata di Dettaglio Distinta
181700070321     C     *IN99         IFEQ      *OFF
181800070321     C********           MOVE      *ON           $INZD2
181900070321     C********           movel     'D2'          $GEST
182000070321     C                   END
182100070321      *
1822000703211-   C                   ENDSL
182300070321     C*
182400070321     c     end_s02       tag
182500070321     C                   ENDSR
182600070321     C/EJECT
182700070321      *---------------------------------------------------------------*
182800070321      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL    ?
182900070321      *---------------------------------------------------------------*
183000070321     C     INZZ2         BEGSR
183100070321     C*
183200070321     C*
183300070321     C                   ENDSR
183400070321     C/EJECT
183500070321      *---------------------------------------------------------------*
183600070321      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare  ?
183700070321      *---------------------------------------------------------------*
183800070321     C     INZS2         BEGSR
183900070321     C*
184000070321     C* reset indicatori DSPATR
184100070321     c                   exsr      INZ_indERR
184200070321     C* pulizia SFL
184300070321     C                   SETOFF                                         3031
184400070321     C                   WRITE     DG31CT2
184500070321     C                   SETON                                          31
184600070515     C                   SETON                                          11
184700070515     c                   eval      sav_in11 = *in11
184800070321      *
184900070321     C* Posiziona il cursore in testata x 12 = *on
185000070321     c                   if        cur_su_SFL = 'N'
185100070321     c     $PcurS2       comp      *on                                    12
185200070321     C* imposto pos. del cursore
185300070321     C                   eval           H1RIGA = 3
185400070528     C                   eval           H1COLO = 17
185500070321     c                   else
185600070321     c                   move      'N'           cur_su_SFL        1
185700070321     c                   setoff                                           12
185800070321     C* imposto pos. del cursore
185900070321     C                   eval           H1RIGA = 7
186000070321     C                   eval           H1COLO = 2
186100070321     c                   end
186200071026     C* se siamo al caricamento del SFL si deve posizionare sulla prima riga disponibile
186300071026     c                   z-add     1             Che_NRec_sfl
186400071026     C                   z-add     h1riga        Su_Quale_rec
186500071026     C                   move      'S'           prima_volta
186600080219     c                   eval      *in15 = sav_in15
186700071026     C*
186800070321     C* Se richiamato attivo 04
186900070528     C*   se però è richiamato da Assegnazione attivo 05
187000070528     c                   setoff                                       05
187100070321     c                   if        pgm_da_call = 'S'
187200070321     C                   SETON                                          04
187300070404     c                   move      *on           sav_in04
187400070620     c                   if        fdgpgm = 'FIDG30R'
187500070620     C                   SETON                                          03
187600070620     c                   move      *on           sav_in03
187700070620     c                   end
187800070528     c                   if        fdgOPZ = '1'
187900070528     C                   SETON                                          05
188000070528     c                   move      *on           sav_in05
188100070528     c                   end
188200070321     c                   end
188300070321     C*
188400070321     C* Righe x Pagina del 1° SFL
188500071026     c                   z-add     15            s2pag
188600070321      *
188700070321     C* CARICAMENTO SFL totale
188800070321     C                   Z-ADD     0             S2NRR
188900070321     C                   Z-ADD     1             C2RCD
189000070321     C                   Z-ADD     0             WMAX
189100070321     C                   Z-ADD     0             Wsfl
189200070321     C* campi titolo
189300070321     C                   MOVEL     TES(1)        c2CTES
189400070606     c                   if        fdgPGM = 'FIDG30R'
189500070606     C                   MOVEL(p)  TES(2)        c2CTES
189600070606     c                   end
189700070903
189800070830     c                   SeLect
189900070903
190000070830     c                   whEn      d1dati= ' '
190100070830     C                   MOVEL(p)  TES(7)        c2TES2
190200070903     C                   MOVEL(p)  'Sped.'       c2DesN
190300070903
190400070830     c                   whEn      d1dati= 'C'
190500070830     C                   MOVEL(p)  TES(5)        c2TES2
190600070903     C                   MOVEL(p)  'Sped.'       c2DesN
190700070903
190800070830     c                   whEn      d1dati= 'R'
190900070830     C                   MOVEL(p)  TES(6)        c2TES2
191000070903     C                   MOVEL(p)  ' Orm.'       c2DesN
191100070903
191200070830     c                   endSL
191300070903
191400070321     C                   MOVEL     dsPGM         c2CPGM
191500070321     C                   MOVEL     RAGUT         c2CDSA
191600070321     C                   MOVEL     knsif         c2nsif
191700070321     C                   MOVEL     knmus         c2nmus
191800070321     C                   z-add     d1CFGS        c2cfgs
191900070321     C                   movel     d1DFGS        c2dfgs
192000070321     C                   movel     d1ccgi        c2ccgi
192100070321     C*
192200070321     C* Posizionamento su file pilota e impostazione primo record da leggere
192300070321     c                   clear                   Prima_Lettura     1
192400070321      *
192500070406     c                   if        Solo_aperte = 'N'
192600070321     c     Kdat1         setll     fidst008
192700070406     c                   else
192800070321     C     Kdst3         SETLL     Fidst003
192900070406     c                   End
193000070321     C* Carico il SFL
193100070321     C                   EXSR      ROLS2
193200070321     C*
193300070321     C                   Z-ADD     1             WPAG
193400070321     C*
193500070321     ** Al primo F11 ordino per Autista
193600070321     C                   EVAL      RrnLast = S2Nrr
193700070321     c                   if        sav_in11 = *blank
193800070417     C********           EVAL      WrkSort = Autista
193900070417     C                   EVAL      WrkSort = Numero
194000070417     c                   exsr      F11_ordina2
194100070321     c                   setoff                                       11
194200070321     c                   else
194300070321     c                   if        sav_in11 = *on
194400070321     C                   EVAL      WrkSort = Autista
194500070322     c                   exsr      F11_ordina2
194600070321     c                   end
194700070321     c                   end
194800070321     C*
194900070321     C                   ENDSR
195000070321     C/EJECT
195100070321      *---------------------------------------------------------------*
195200070321      *?  CARICAMENTO PAGINA da SFL dei GIRI             ?
195300070321      *---------------------------------------------------------------*
195400070321     C     ROLS2         BEGSR
195500070321     C*
195600070321     C                   SETOFF                                       32
195700070321     C                   Z-ADD     0             Y
195800070321     C                   Z-ADD     WMAX          S2NRR
195900070321     C*
196000070321     C* Caricamento del Sfl x Giro
196100070321     c                   Exsr      Write_S2
196200070321     C* Fine Caricamento
196300070321     C*
196400070321     C                   Z-ADD     S2NRR         WMAX                 30
196500070321     C*
196600070321     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
196700070321     C     S2NRR         DIV       S2pag         PAGINE            4 0
196800070321     C                   MVR                     RESTO             3 0
196900070321     C     PAGINE        MULT      S2pag         C2RCD
1970000703211    C     RESTO         IFGT      0
197100070321     C                   ADD       1             C2RCD
1972000703211E   C                   ELSE
197300070321     C                   SUB       S2pag         C2RCD
197400070321     C                   ADD       1             C2RCD
1975000703211-   C                   ENDIF
197600070321     C*
197700070321     C                   ENDSR
197800070321      *---------------------------------------------------------------*
197900070321      *?  Scrive il SFL       ?
198000070321      *---------------------------------------------------------------*
198100070321     C     Write_S2      BEGSR
198200071029     C*
198300071029     C* Pulisce il Nrecord di riposizionamento se richiesto da pgm chiamante
198400071029     c                   clear                   NRec_riposiz
198500071029     C*
198600070321     c                   exsr      Legge_rec
198700070321     C*
1988000703211    C     $EFILE        DowEQ     *OFF
198900070321     C*
199000070321     C                   clear                   DG31SF2
199100070404     c                   move      sav_in04      *in04
199200070528     c                   move      sav_in05      *in05
199300080219     c                   move      sav_in15      *in15
199400070510     C* Priorità
199500070510     C                   z-add     dst3prgg      h2prgg
199600070510     C                   z-add     dst3sql       h2sql
199700070321     C* GIRO DATA Distinta
199800070417     C                   MOVE      dstDFV        h2CDFV
199900070321     C                   MOVE      dstDFV        G02INV
200000070321     C                   MOVEL     '3'           G02ERR
200100070321     C                   CALL      'XSRDA8'
200200070321     C                   PARM                    WLBDAT
200300070321     C*
200400070321     C                   movel     G02DAT        campo4            4
200500070321     C                   move      G02DAT        s2CDFV
200600070321     C                   movel     campo4        s2CDFV
200700070321      *
200800070321     C                   Z-ADD     dstNFV        s2CNFV
200900070321     c     kapd          chain     fiapd01l
201000070321     c                   if        %found(fiapd01l)
201100070321     c                   eval      s2DAUT = apdrsc
201200070321     c                   else
201300070321     c                   clear                   s2DAUT
201400070321     c                   end
201500070321      *
201600070904     c                   if        d1dati= ' ' or d1dati= 'C'
201700070904     c                   z-add     DSTnbla       s2nbla
201800070419     c                   z-add     DSTnftl       s2nftl
201900070904     c                   z-add     DSTstpa       s2stpa
202000070904     c                   z-add     DSTPKL        s2cpem
202100070321     c                   z-add     DSTPKLa       s2cpea
202200070904     c                   z-add     DSTVLU        s2cvom
202300070321     c                   z-add     DSTVLUa       s2cvoa
202400070904
202500070904     c*****              if        d1dati= ' '
202600070927     c                   if        Abilita_ORM = 'S'
202700070904     c                   z-add     DSTRnbla      s2nbla1
202800070904     c                   z-add     DSTRnftl      s2nftl1
202900070904     c                   z-add     DSTRstpa      s2stpa1
203000070904     c                   z-add     DSTRPKL       s2cpem1
203100070904     c                   z-add     DSTRPKLa      s2cpea1
203200070904     c                   z-add     DSTRVLU       s2cvom1
203300070904     c                   z-add     DSTRVLUa      s2cvoa1
203400070927     c                   end
203500070904     c*****              end
203600070904
203700070904     c                   else
203800070904      *
203900070904     c                   z-add     DSTRnbla      s2nbla
204000070904     c                   z-add     DSTRnftl      s2nftl
204100070904     c                   z-add     DSTRstpa      s2stpa
204200070904     c                   z-add     DSTRPKL       s2cpem
204300070904     c                   z-add     DSTRPKLa      s2cpea
204400070904     c                   z-add     DSTRVLU       s2cvom
204500070904     c                   z-add     DSTRVLUa      s2cvoa
204600070904
204700070904     c******             if        d1dati= ' '
204800070904     c                   z-add     DSTnbla       s2nbla1
204900070904     c                   z-add     DSTnftl       s2nftl1
205000070904     c                   z-add     DSTstpa       s2stpa1
205100070904     c                   z-add     DSTPKL        s2cpem1
205200070904     c                   z-add     DSTPKLa       s2cpea1
205300070904     c                   z-add     DSTVLU        s2cvom1
205400070904     c                   z-add     DSTVLUa       s2cvoa1
205500070904     c******             end
205600070904
205700070904     c                   end
205800070904      *
205900070927     c*******            if        s2nbla1 >0 or s2cpem1 >0 or s2cvom1 >0 or
206000070927     c*******                      s2nftl1 >0 or s2cpea1 >0 or s2cvoa1 >0 or
206100070927     c*******                      s2stpa1 >0
206200070904
206300070904     c                   if        d1dati= ' ' or d1dati= 'C'
206400070927     c                   if        Abilita_ORM = 'S'
206500070904     C                   MOVE (p)  'Ritiri'      s2DDes1
206600070904     C                   MOVEL(p)  'Nr.Orm.'     s2DesN1
206700070927     c                   end
206800070904     c                   else
206900070904     C                   MOVE (p)  'Consegne'    s2DDes1
207000070904     C                   MOVEL(p)  'Nr.Sped'     s2DesN1
207100070904     c                   end
207200070904
207300070927     c*******            end
207400070904
207500070905     c                   if        DSTfpp  ='M'
207600070905     c                   movel     'Mat'         s2Tusc
207700070905     c                   else
207800070905     c                   movel     'Pom'         s2Tusc
207900070905     c                   end
208000070820     c                   clear                   s2STP
208100070820     c                   if        DSTfasd ='STP'
208200070820     c                   movel     'St'          s2STP
208300070820     c                   endif
208400070321      *
208500070321     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
208600070321     c                   if        *in08
208700070321     c                   if        dsttpm='M'
208800070321     c                   movel     'Mes'         s2CTPM
208900070321     c                   endif
209000070321     c                   if        dsttpm<>'M'
209100070321     c                   movel     'Prc'         s2CTPM
209200070321     c                   endif
209300070321     c                   endif
209400070321     c
209500070321      * Flag tipo distinta (solo consegna, orm o entrambi)
209600070321     C                   CLEAR                   WBOL
209700070321     C                   CLEAR                   WORM
209800070321     c                   Z-Add     d1CFGS        Wifp
209900070321     C                   Z-ADD     s2CNFV        kfnvv
210000070321     C                   Z-ADD     s2CNFV        WnFV
210100070321     c     Karb78        Setll     Fnarb78l
210200070321if  5c                   if        %equal(fnarb78l)
210300070321     c                   movel     'S'           wbol              1
210400070321x   5c                   else
210500070321     C     Kdst          SETLL     FnBRV01L
210600070321     c                   if        %equal(fnbrv01l)
210700070321     c                   movel     'S'           wbol              1
210800070321     c                   endif
210900070321e   5c                   endif
211000070321     C     KORM          SETLL     FNORM06L
211100070321do  5c                   do        *hival
211200070321     C     KORM          reade     FNORM06L
211300070321     c                   if        %eof(fnorm06l)
211400070321     c                   leave
211500070321     c                   endif
211600070321     c                   if        ormatb = *blanks and ormfao < 999
211700070321     C                   move      'S'           worm              1
211800070321     c                   endif
211900070321e   5c                   enddo
212000070321     C                   if        wbol = 'S' and worm <> 'S'
212100070321     C                   movel     'CON'         s2CTPD
212200070321     C                   movel     'TpD'         s2dTPD
212300070321     C                   endif
212400070321     C                   if        wbol <> 'S' and worm = 'S'
212500070321     C                   movel     'ORM'         s2CTPD
212600070321     C                   movel     'TpD'         s2dTPD
212700070321     C                   endif
212800070321     C                   if        wbol = 'S' and worm = 'S'
212900070321     C                   movel     'C/O'         s2CTPD
213000070321     C                   movel     'TpD'         s2dTPD
213100070321     C                   endif
213200070321      * Calcolo importi se non già fatto prima
213300070321if  5c                   if        d1CTFV = '2' and d1IMPO = 'S'
213400070321     c                   exsr      totalizzo
213500070321     c                   z-add     w_imas        s2IMAS
213600070321     c                   z-add     w_imca        s2IMCA
213700070321     c                   endif
213800070321      * Pda
213900080207     C                   movel     dstflr        DDStflr
214000080208     c                   setoff                                       14
214100080208     c                   if        dstDFV >= DateU8
214200080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
214300080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
214400080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
214500080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
214600080208     c                   seton                                        14
214700080208     c                   endIf
214800080208     c                   endIf
214900080222     c                   eval      h2in14 = *in14
215000080222      *
215100080208      *  lo Minuscolizza sempre a video
215200080207     C     Up:Lo         XLATE     §DSTtstPDA    s2CTSTPDA
215300080208      *
215400070321     C                   movel     dstPDA        s2CPDA
215500070419     C                   if        s2CPDA <> *blank
215600070419     C                   movel     'PDA'         s2DPDA
215700070419     c                   end
215800070321      * ANNULLATO
215900070321     C     dstATB        IFEQ      'A'
216000070321     C                   MOVEL     'ANN'         s2CANN
216100070321     C                   END
216200070321if  5C     dstFCF        IFNE      ' '
216300070321      * CHIUSO
216400070321     C     dstFCF        IFEQ      'S'
216500070821     C                   MOVEL     'Chiu'        s2CFCF
216600070321     C                   ELSE
216700070321     C* BLOCCATO
216800070322     C                   MOVEL     'BLoc'        s2CFCF
216900070321e   5C                   ENDIF
217000070321e   4C                   ENDIF
217100070904
217200070904      *   Se richiamato come Ritiri o Consegne o Entrambe
217300070321      *
217400070321      * Intestazioni seconda riga
217500070830     c                   setoFF                                       18
217600070321     c                   if        s2imca <> 0
217700070321     c                   movel     Imp_C_A       s2dica
217800070830     c                   setoN                                        18
217900070321     c                   end
218000070321      *
218100070830     c                   setoFF                                       20
218200070321     c                   if        s2imas <> 0
218300070321     c                   movel     Imp_ASS       s2dias
218400070830     c                   setoN                                        20
218500070321     c                   end
218600070321      *
218700070510      *  Illumina diversamente il campo x le distinte con priorità (1)
218800070510     c                   setoff                                       40
218900070510if  4c                   if        h2prgg = 1
219000070510     c                   seton                                        40
219100070510     c                   end
219200070321      *---------
219300070321if  4c                   if        FDGtfv <> '3'
219400070321     c                   if        (sav_c2nfv <= s2cnfv and *in11 = *off) or
219500070321     c                             (sav_c2aut <= s2daut and *in11 = *on)  or
219600070321     c                             (sav_c2aut = *blank and sav_c2nfv =0)
219700070404     C* Imposta indicatore x abilitare campo scelta
219800070404     c                   move      sav_in19      *in19
219900070321     c********           seton                                        32
220000071029      *
220100070321     C                   ADD       1             S2NRR
220200070321     C                   ADD       1             Y
220300071029      *
220400071029      *  se deve posizionare il cursore sul record se il pgm è
220500071029      *  chiamato per assegnare/disassegnare accende il PC sulla scelta
220600071029      *  solo se ha il nr. Foglio di riferimento per il posizionamento sfl.
220700071029     c                   if        fdgNFpos <> *blank
220800071029     c                   move      s2cNFV        sav2NFV           6
220900071029     c                   if        fdgNFpos = sav2NFV
221000071029     c                   seton                                        42
221100071029     c                   move      'X'           H2recPos
221200071029     c                   end
221300071029     c                   end
221400071029      *
221500070321     C                   WRITE     DG31SF2
221600071029     c                   setoFF                                       42
221700070321     c                   end
221800070321e   4c                   endif
221900070321      *---------
222000070321      *  Esce da qui con un record di Totale x GIRO
222100070321     c                   exsr      Legge_rec
222200070321     c                   EndDO
222300070321     C*
222400070321     C                   ENDSR
222500070321      *---------------------------------------------------------------*
222600070321      *?  CONTROLLO TESTATA LISTA    ?
222700070321      *---------------------------------------------------------------*
222800070321     C     CTRC2         BEGSR
222900070321      *
223000070321     C                   MOVE      *OFF          *IN99
223100070321      *
223200070321     C*------------------------
223300070321      *  interrogazione Giro
223400070321     c                   clear                   c2dcgi
223500070321     c     '?'           scan      c2cCGI
223600070321     C*   ricerca codice giro
223700070321     c                   if        %Found
223800070321     c                   clear                   fidg09ds
223900070321     c                   move      'P01'         d09iop0
224000070321     c                   movel     c2cFGS        d09ifgs
224100071010     c****               movel     'C'           d09itug
224200071010     c                   movel     d1DATI        d09itug
224300070321     c                   z-add     DATEU8        d09idat
224400070321     c                   eval      kpjbus = kpjbu
224500070321     c                   movel(p)  fidg09ds      kpjbu
224600070321     c                   call      'FIDG09R'
224700070321     c                   parm                    kpjba
224800070321     c                   movel     kpjbu         fidg09ds
224900070321     c                   eval      kpjbu = kpjbus
225000070321     c                   movel     d09ocgi       c2cCGI
225100070321     c                   end
225200070321     C*------------------------
225300070321      *  controllo validità giro
225400070321     c                   if        c2cCGI <> *blank
225500070321     c                   clear                   fidg09ds
225600070321     c                   eval      D09IOP0 = '001'
225700070321     c                   eval      d09ifgs = c2cFGS
225800071010     c*****              eval      d09itug = 'C'
225900071010     c                   eval      d09itug = d1DATI
226000070321     c                   eval      d09icgi = c2cCGI
226100070321     c                   move      dateu8        d09idat
226200070321     c                   eval      kpjbus = kpjbu
226300070321     c                   eval      kpjbu = fidg09ds
226400070321     c                   call      'FIDG09R'
226500070321     c                   parm                    kpjba
226600070321     c                   eval      fidg09ds = kpjbu
226700070321     c                   eval      kpjbu = kpjbus
226800070321     c                   if        d09oerr = '1'
226900070321     C                   SETON                                        54  99
227000070321     C                   goto      ENDC2
227100070321     c                   else
227200070321     c                   if        d09ocgi <> ' '
227300070321     c                   eval      c2dcgi = d09odes
227400070321     c                   move      'S'           con_giro
227500070321     c                   end
227600070321     c                   end
227700070321      *
227800070321     c                   end
227900070321     C*------------------------
228000070321      *  interrogazione AUT
228100070321      *
228200070321     c     '?'           scan      c2daut
228300070321     C*   ricerca codice giro
228400070321     c                   if        %Found
228500070321     c                   clear                   fnlv24ds
228600070321     C                   Z-ADD     c2cFGS        d24FIL
228700070321     C                   MOVEL     'R'           d24FLG
228800070321     C                   MOVEL     'A'           d24tip
228900070321     c                   eval      kpjbus = kpjbu
229000070321     C                   MOVEL(p)  fnlv24ds      KPJBU
229100070321     C                   CALL      'FNLV24R'
229200070321     C                   PARM                    KPJBA
229300070321     C                   MOVEL     KPJBU         fnlv24ds
229400070321     c                   eval      kpjbu = kpjbus
229500070321     C                   MOVEL     D24RSC        c2daut
229600070321     c                   end
229700070321     C*
229800070321     C*------------------------
229900070321     c                   move      *on           $PcurS2
230000070321     C* Riposizionamento x AUT o x Numero
230100070321     c                   if        (*in11 = *off and c2cnfv <> sav_c2nfv) or
230200070321     c                             (*in11 = *on  and c2daut <> sav_c2aut)
230300070321     c                   move      *off          $PcurS2
230400070321     c                   move      *on           $inzs2
230500070321     c                   move      'S2'          $GEST
230600070321     c  n11              z-add     c2cnfv        sav_c2nfv
230700070321     c   11              movel     c2daut        sav_c2aut
230800070321     c                   end
230900070321     C*
231000070321     C* Posiziona il cursore in testata x 12 = *on
231100070321     c     $PcurS2       comp      *on                                    12
231200070321     C  n12              eval           H1RIGA = 3
231300070528     C  n12              eval           H1COLO = 17
231400070321     C   12              eval           H1RIGA = 7
231500070321     C   12              eval           H1COLO = 2
231600070321      *
231700070321     C*------------------------
231800070321      *  se tutto OK
231900070321     c                   if        c2ccgi <> d1ccgi
232000070321     c                   move      c2ccgi        d1ccgi
232100070321     c                   move      *on           $inzs2
232200070321     c                   move      'S2'          $GEST
232300070321     c                   end
232400070321      *
232500070321     C*------------------------
232600070321      *
232700070321     c     endc2         tag
232800070321     C                   ENDSR
232900070321      *---------------------------------------------------------------*
233000070321      *? CONTROLLO OPZIONI LISTA     ?
233100070321      *---------------------------------------------------------------*
233200070321     C     CTRS2         BEGSR
233300070321     C*
233400070321     C                   MOVEL     *OFF          $ESCI
233500070321     C                   SETOFF                                       99
233600070321     C*
233700070321     C* Leggo il sfl solo se ci sono rcd
2338000703211    C     WMAX          IFGT      0
233900070321     C                   READC     DG31SF2                                21
234000071026     C*
234100070321     C* esce se fine sfl o errore che richiede l'uscita
2342000703212    C     *IN21         DOWEQ     *OFF
234300070321     C     $ESCI         ANDEQ     *OFF
234400070321     C                   Z-ADD     S2NRR         C2RCD
234500071026     c                   eval      Che_NRec_sfl = s2nrr
234600070321     C* ctrl su riga
234700070321     C                   EXSR      RECS2
234800070321     C*
234900070321     C* gestione opzioni
2350000704043    C     s2SCE         IFNE      *blank
235100070321     C     *IN99         ANDEQ     *OFF
235200070321     C                   EXSR      SCE_S2
2353000703213-   C                   ENDIF
235400070321      *
235500070321     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
2356000703213    C     *IN99         IFEQ      *ON
235700070321     C     $ESCI         OREQ      *ON
235800070321     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
235900070321     C* la ripristinerò a conclusione del ciclo di READC
236000070321     C                   MOVE      *OFF          $INZS2
2361000703213-   C                   ENDIF
236200070321     C*
236300070321      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
236400070404     c                   if        s2SCE  <> *blank
236500070321     C******             MOVE      *ON           *IN32
236600070404     c                   clear                   s2SCE
236700070321     c                   end
236800070510      *
236900070510     c                   setoff                                       40
237000070510if  4c                   if        h2prgg = 1
237100070510     c                   seton                                        40
237200070510     c                   end
237300070510      *
237400080222     c                   eval      *in14 = h2in14
237500070321     C                   UPDATE    DG31SF2
237600070321     C*
237700070321     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
2378000703213    C     $ESCI         IFEQ      *OFF
237900070321     C                   READC     DG31SF2                                21
238000070321      *
238100070321     C* a fine ciclo ripristino il flag richiesta iniz.sfl
2382000703214    C     *IN21         IFEQ      *ON
238300070321     C* calcolo pagina a cui deve posizionarsi
238400070321     c                   z-add     s2pag         Wpagine
238500070321     C                   EXSR      CLCPAG
2386000703214-   C                   ENDIF
2387000703213-   C                   ENDIF
238800070321     C*
2389000703212-   C                   ENDDO
239000071026     C*
239100071026      * -----------------------
239200071026      * ? calcola "su quale record" riposizionarsi tenendo conto che
239300071026      * ?  il SFL ha una testata di 6 righe ed inizia dalla 7°.
239400071026     c     Che_NRec_sfl  div       s2pag         Su_Quale_rec
239500071026     c                   mvr                     resto
239600071026     C     Resto         IFeq      0
239700071026     C                   z-add     15            Su_Quale_rec
239800071026     C                   Else
239900071026     C                   z-add     resto         Su_Quale_rec
240000071026     C                   ENDIF
240100071026      * ?  aggiunge le 6 righe di testata.
240200071026     C                   add       6             Su_Quale_rec
240300071026      * -----------------------
240400070321     C*
2405000703211-   C                   ENDIF
240600070321     C*
240700070321     C                   ENDSR
240800070321     C/EJECT
240900070321      *---------------------------------------------------------------*
241000070321      *? CONTROLLO CAMPI I/O RIGA LISTA  ?
241100070321      *---------------------------------------------------------------*
241200070321     C     RECS2         BEGSR
241300070321     C*
241400070321     C* reset indicatori DSPATR
241500070321     c                   exsr      INZ_indERR
241600070528     C*
241700070528     C* se non Richiamato non sono abilitate le opzioni 2 e 3
241800070528     c                   if        pgm_da_call <> 'S'
241900070528     c                   if        s2sce = '2' or s2sce = '3'
242000070528     c                   seton                                        99  41
242100070528     c                   end
242200070528     c                   end
242300070528     C* Se richiamato in assegnazione non deve accettare opz 2,3
242400070620     c                   if        (pgm_da_call =  'S' and fdg31_opz = '1' and
242500070620     c                             fdgPGM = 'FIDG30R' )  or
242600070620     c                             fdgPGM <>'FIDG30R'
242700070528     c                   if        s2sce = '2' or s2sce = '3'
242800070528     c                   seton                                        99  41
242900070528     c                   end
243000070528     c                   end
243100070620     C*
243200070620     c                   if        pgm_da_call =  'S' and fdg31_opz <> '1' and
243300070620     c                             fdgPGM = 'FIDG30R'
243400070620     c                   if        s2sce = '1'
243500070620     c                   seton                                        99  41
243600070620     c                   end
243700070620     c                   end
243800070528     C*
243900070321     C                   ENDSR
244000070321      *---------------------------------------------------------------*
244100070321      *? GESTIONE OPZIONI LISTA          ?
244200070321      *---------------------------------------------------------------*
244300070321     C     SCE_S2        BEGSR
244400070321     C*
244500070321     C* determino Riga/Colonna del cursore
244600070321     C                   MOVE      NRG           R$$
244700070321     C                   MOVE      NCL           C$$
244800070321     C                   Z-ADD     RIRI          CURR
244900070321     C                   Z-ADD     COCO          CURC
245000071029     C*
245100071029     c                   If        s2SCE <> *blank
245200071029     c                   move (p)  s2CNFV        FDGNFPOS
245300071029     c                   end
245400070404     C*
245500070404     c                   If        s2SCE  = '1'
245600070405     c                   If        pgm_da_call <> *blank
245700070404     c                   move      *on           $FINE
245800070404     c                   move      *on           $ESCI
245900070404     c                   z-add     s2CNFV        FDGNFV
246000070404     c                   move      d1cnpg        FDGNPG
246100070528     c                   eval      fdgOPZ = s1SCE
246200070405     c                   else
246300070405     c                   z-add     s2CNFV        d1cNFV
246400070405     c                   move      'D1'          $GEST
246500070404     c                   end
246600070405     c                   end
246700070528      *
246800070528     c                   If        s2SCE  = '2' or s2SCE = '3'
246900070528     c                   if        pgm_da_call <> *Blank
247000070528     c                   move      *on           $ESCI
247100070528     c                   move      *on           $FINE
247200070528     c                   z-add     s2CNFV        FDGNFV
247300070528     c                   move      d1cnpg        FDGNPG
247400070528     c                   eval      fdgOPZ = s2SCE
247500070528     c                   clear                   fdgCGI
247600070528     c                   end
247700070528     c                   end
247800070528      *
247900070404     c                   If        s2SCE  = '5'
248000070404     C*
248100070321     c                   z-add     s2CNFV        d1CNFV
248200070321     c                   z-add     c2cfgs        d1CFGS
248300070321     c                   z-add     s2CNFV        wnfv
248400070403     c                   move      'D2'          $GEST
248500070403     C                   move      'S'           $VISD2
248600070403     c                   move      *on           $INZD2
248700070404      *
248800070404     c                   end
248900070321     C*
249000070427     C*
249100070621     c                   If        s2SCE  = '7'
249200070427     c                   clear                   fidg35ds
249300070528     c                   eval      D35FGSi = wfgs
249400070528     c                   eval      D35DDCi = H2CDFV
249500070528     c                   eval      D35NDCi = S2CNFV
249600070528     c                   eval      D35NPGi = wnpg
249700070831     c                   eval      D35PGMi = fdgPGM
249800070903      * si vuole fare vedere tutto
249900070903     c******             eval      D35TPi  = 'C'
250000070905     c                   If        pgm_da_call <> *blank and FDGOPZ <> '1' and
250100070905     c                             fdgPGM = 'FIDG30R'
250200070528     c                   eval      D35opzi = 'S'
250300070528     c                   end
250400070427     c                   eval      kpjbus = kpjbu
250500070427     c                   movel(p)  fidg35ds      kpjbu
250600070427     c                   call      'FIDG35R'
250700070427     c                   parm                    kpjba
250800070427     c                   movel     kpjbu         fidg35ds
250900070427     c                   eval      kpjbu = kpjbus
251000070621     c                   if        s2nbla >0
251100070611     c                   if        pgm_da_call <> *Blank and fdgOPZ<>'1'
251200070528     c                   move      *on           $ESCI
251300070831     c                   if        d35funO <>'12'
251400070528     c                   move      *on           $FINE
251500070831     c                   end
251600070528     c                   z-add     s2CNFV        FDGNFV
251700070831     c                   if        d35funO = '12' or d35funO = '03'
251800070831     c                   z-add     0             FDGNFV
251900070831     c                   end
252000070528     c                   move      d1cnpg        FDGNPG
252100070528     c                   eval      fdgOPZ = d35opzO
252200070528     c                   eval      fdgCGI = d35cgiO
252300070528     c                   end
252400070621     c                   end
252500070427     c                   end
252600070427     C*
252700070621     c                   If        s2SCE  = '9'
252800070321     c                   z-add     s2CNFV        wnfv
252900070321     c*******            move      *on           $ESCI
253000070321     c                   move      'D2'          $GEST
253100070403     C                   move      'N'           $VISD2
253200070321     c                   move      *on           $INZD2
253300070321     C                   exsr      GESD2
253400070321     c                   endIf
253500070321     C*
253600070321     C                   ENDSR
253700070321     C/EJECT
253800070312      *---------------------------------------------------------------*
253900070321      *?  LETTURA RCD ARCHIVIO PILOTA         ?
254000070312      *---------------------------------------------------------------*
254100070312     C     LEGGE_REC     BEGSR
254200070312     C*
254300070312     C*  Per EoF deve uscire
254400070312     C                   MOVEL     *OFF          $EFILE
254500070312     C*  Per Record Ok deve uscire riportando i dati
254600070312     C                   MOVEL     *OFF          $RCDOK
254700070312     C*
2548000703121    C     $EFILE        DOUEQ     *ON
254900070312     C     $RCDOK        OREQ      *ON
255000070312     C*
255100070312if  2C                   IF        dstDFV <= DATFIW or Prima_Lettura <> 'N'
255200070312     c                   eval      Prima_Lettura = 'N'
255300070312      *
255400070406     c                   if        Solo_aperte = 'N'
255500070312     c     Kdat2         Reade     Fidst008                               33
255600070406     c                   else
255700070312     C     Kdst3         READE     Fidst003                               33
255800070406     c                   End
255900070312      *
256000070312    2C                   END
256100070312      *
256200070312      * solo se superato il limite del range di date
256300070312if  2C                   IF        dstDFV > DATFIW
256400070312     c                   If        Solo_aperte = 'N'
256500070312     C                   SETON                                        33
256600070312     c                   Else
256700070312     C     Kdst3         READE     Fidst003                               33
256800070312     c                   End
256900070312e   2C                   ENDif
257000070312      *
257100070312     C                   select
257200070312      * Fine File
257300070312     C                   when      *in33 = *on
257400070312     C                   MOVEL     *ON           $EFILE
257500070312      *
257600070312     C                   other
257700070312      *
257800070312      * ?Controlla la Spedizione se è da considerare
257900070312     C                   exsr      Check_REC
258000070312      *
258100070312      * ?     se Non è Esclusa
258200070312     c                   if        *in96 = *on
258300070312     C                   MOVE      *ON           $RCDOK
258400070312     c                   end
258500070312      *
258600070312     C                   endsl
258700070312     C*
2588000703121-   C                   ENDDO
258900070312     C*
259000070312     C                   ENDSR
259100070312      *---------------------------------------------------------------*
259200070321      *?  CONTROLLA la riga se deve essere presa in considerazione    ?
259300070312      *---------------------------------------------------------------*
259400070312     C     Check_REC     BEGSR
259500070312     C*
259600070312     C*  96 = *on x Record OK
259700070312     C                   SETOFF                                       96
259800070312      *
259900070312      * Se richieste Distinte Aperte e data foglio > data inizio (range)   oppure
260000070312      *  Se richieste Distinte Chiuse                                      oppure
260100070312      *   Tutte
260200070312     c                   If        (Solo_aperte = 'S' and dstDFV >= DATINW) or
260300070312     c                             (d1CTFV = '1' and dstFCF <> ' ') or
260400070312     c                             (d1CTFV = ' ')
260500070312     C                   SETON                                        96
260600070312     C                   END
260700070312      *
260800070312      **  Se impostato il Giro deve caricare le distinte che hanno il Giro
260900070312     c                   if        d1ccgi <> *blank
261000070312     C                   Z-ADD     dstNFV        WnFV
261100070312     c                   move      'N'           Giro_in_NFV       1
261200070910      **
261300070910     c                   if        d1dati = 'C'
261400070910      **
261500070312     c     kdst31        setll     fidst31l
261600070312     c     kdst31        reade     fidst31l
261700070312     c                   dow       not %Eof(fidst31l)
261800070312      **
261900070312      **  Controlla presenza del giro richiesto
262000070312     c                   if        %char(DST3NPG) = d1cNPG and
262100070312     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
262200070312     c                             and DST3CGI = d1cCGI
262300070312     c                   move      'S'           Giro_in_NFV       1
262400070312     c                   leave
262500070312     c                   end
262600070312      **
262700070312     c     kdst31        reade     fidst31l
262800070312     c                   endDo
262900070910      **
263000070910     c                   else
263100070910      **
263200070910     c     kdst51        setll     fidst51l
263300070910     c     kdst51        reade     fidst51l
263400070910     c                   dow       not %Eof(fidst51l)
263500070910      **
263600070910      **  Controlla presenza del giro richiesto
263700070910     c                   if        %char(DST5NPG) = d1cNPG and
263800070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
263900070910     c                             and DST5CGI = d1cCGI
264000070910     c                   move      'S'           Giro_in_NFV       1
264100070910     c                   leave
264200070910     c                   end
264300070910      **
264400070910     c     kdst51        reade     fidst51l
264500070910     c                   endDo
264600070910      **
264700070910     c                   end
264800070312      **
264900070312      ** Il giro selezionato non è presente nella distinta
265000070312     c                   if        Giro_in_NFV = 'N'
265100070312     C                   SETOff                                       96
265200070312     c                   end
265300070312      *
265400070312     c                   end
265500070312     C*
265600070312     C* CONTROLLO CHE SIA FOGLIO DI FILIALE IN GESTIONE
265700070312if  3C   96d1CFGS        IFNE      0
265800070312      *
265900070312     C     dstFGS        IFNE      d1CFGS
266000070312     C                   SETOFF                                       96
266100070312     C                   ENDIF
266200070312     C*
266300070312x   3C                   ELSE
266400070312     C*
266500070312     C* CARICO SOLO LE FILIALE NON DELL'AREA
266600070312     C     dstFGS        LOOKUP    L1                                     21
266700070312     C   21              SETOFF                                       96
266800070312e   3C                   ENDIF
266900070312
267000070312      * richiamato da FIMS03R, che passa i F.V. già inseriti
267100070504if  3c                   if        sav_in19 = *ON and *in96
267200070312     c                   eval      *in21  = *off
267300070404     c     dstnfv        lookup    $FV6                                   21
267400070504      * Per visualizzare le Distinte non presenti sul piano Packing List
267500070504if  3c                   IF        *in21 = *oN and fdgTFV = '2'
267600070504     C                   SETOFF                                       96
267700070504     c                   end
267800070312      * richiamato SOLO per sapere se esistono distinte aperte
267900070312      *   non inserito nel piano di cui sono stati passati i F.V.
268000070504if  3c                   IF        *in21 = *oFF and fdgTFV = '3'
268100070419     C                   eval      FDGfs1 = *on
268200070312     C                   MOVE      *ON           $FINE
268300070312     C                   MOVEL     *ON           $EFILE
268400070419     c                   LeaveSR
268500070312e   3c                   endif
268600070419e   3c                   endif
268700070312      *
268800070312     c                   if        *in08 and d1CTPM<>' '
268900070312     c                   if        (d1CTPM='M' and dsttpm<>'M') or
269000070312     c                             (d1CTPM='P' and dsttpm ='M')
269100070312     c                   eval      *in96  = *off
269200070312     c                   endif
269300070312     c                   endif
269400070312      *
269500070625      * Tipo Uscita Mattina/Pomeriggio
269600070625     c                   if        d1Tusc<>' '
269700070625     c                   if        (d1Tusc='M' and dstFPP<>'M') or
269800070625     c                             (d1Tusc='P' and dstFPP<>'P')
269900070625     c                   eval      *in96  = *off
270000070625     c                   endif
270100070625     c                   endif
270200070625      *
270300070417      *   Range di date entro i limiti
270400070417     C     dstDFV        IFgt      DATfiw
270500070417     C     dstDFV        ORlt      DATinw
270600070417     C                   SETOFF                                       96
270700070417     C                   ENDIF
270800070312      *
270900070607      *? Controllo se richiesto il trasferimento da pgm richiamato    ?
271000070607      *?  da assegnazione manuale e se devo impostare la distinta     ?
271100070607      *?  su cui trasferire non voglio far vedere la distinta dalla   ?
271200070607      *?  quale sto trasferendo le spedizioni.                        ?
271300070606     c                   if        fdgPGM = 'FIDG30R'  and
271400070606     c                             pgm_da_call =  'S' and fdg31_opz = '1'
271500070606     c                             and fdgNF1 = dstNFV
271600070606     C                   SETOFF                                       96
271700070606     c                   end
271800070606      *
271900070607      *?  Se sto Trasferendo o Togliendo  non voglio caricare le      ?
272000070607      *?  Distinte vuote  (senza spedizioni - non mi servono)         ?
272100070607      *?  [nel trasferimento solo quando scelgo da dove trasferire]   ?
272200070607     c                   if        fdgPGM = 'FIDG30R'  and pgm_da_call =  'S'
272300070607     c                   if        fdg31_opz = '2' and DSTnbla =0  or
272400070607     c                             fdg31_opz = '3' and DSTnbla =0 and fdgNF1 =0
272500070621      *  ?  Asteriscato in quanto si vuole continuare a vedere comunque tutto...  ?
272600070621     C***********        SETOFF                                       96
272700070607     c                   end
272800070607     c                   end
272900070820      *
273000070820      *?  Controlla se stampate o no oppure tutte.      ?
273100070820     c                   if        d1stp <> *blank
273200070820     c                   if        d1stp = 'N' and dstFASD = 'STP' or
273300070820     c                             d1stp = 'S' and dstFASD <>'STP'
273400070820     C                   SETOFF                                       96
273500070820     c                   end
273600070820     c                   end
273700070607      *
273800070312     C                   ENDSR
273900070301      *---------------------------------------------------------------*
274000070321      *?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL      ?
274100070301      *---------------------------------------------------------------*
274200070228     C     CLCPAG        BEGSR
274300940224     C* Input :
274400940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
274500940224     C*          cursore
274600070301     C* - Wpagine= numero rcd per pagina sfl
274700940224     C* Output :
274800940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
274900940224     C*
275000070301     C     WSFL          DIV       Wpagine       PAGINE            4 0
275100940224     C                   MVR                     RESTO             3 0
275200940224     C     RESTO         IFGT      0
275300940224     C                   ADD       1             PAGINE
275400940224     C                   ENDIF
275500070301     C     PAGINE        MULT      Wpagine       WPAG
275600940224     C*
275700940224     C                   ENDSR
275800070302     C/EJECT
275900070302     C************************************************************
276000070321      *?  INIZIALIZZA indicatori di errore    ?
276100070302     C************************************************************
276200070308     C     INZ_indERR    BEGSR
276300070308      *
276400070308     C                   move      *ALL'0'       IN5190           40
276500070308     C                   moveA     IN5190        *IN(51)
276600070308      *
276700070308     C                   ENDSR
276800070308     C/EJECT
276900070301      *---------------------------------------------------------------*
277000070321      *?  GESTIONE F03 x uscire LR     ?
277100070301      *---------------------------------------------------------------*
277200070227     C     F03_exit      BEGSR
277300070227     C* fine programma
277400940309     C                   MOVE      *ON           $FINE
277500070302      *
277600940309     C                   ENDSR
277700070312     C/EJECT
277800070312      *---------------------------------------------------------------*
277900070321      *? GESTIONE F11 alterna l'ordinamento x numero o Autotrasportatore  ?
278000070312      *---------------------------------------------------------------*
278100070322     C     F11_ordina1   BEGSR
278200070320     C*
278300070320     C* imposto pos. del cursore
278400070403     C                   if        WrkSort = Autista or
278500070403     C                             WrkSort = Numero and *in13 = *on
278600070403     c                   eval      cur_su_sfl = 'N'
278700070320     C                   eval           H1RIGA = 3
278800070528     C                   eval           H1COLO = 17
278900070402     c                   end
279000070402     C* imposto pos. del cursore
279100070403     C                   if        WrkSort = Numero and *in13 = *off
279200071026     c*******                      or Prima_volta = 'S' and fdgPGm = 'FIDG30R'
279300071026     c                             or Prima_volta = 'S'
279400070607     c                   eval      Prima_volta = 'N'
279500070403     c                   eval      cur_su_sfl = 'S'
279600070402     C                   eval           H1RIGA = 7
279700070402     C                   eval           H1COLO = 2
279800070402     c                   end
279900071026     C                   z-add     h1riga        Su_Quale_rec
280000071026     c                   z-add     1             Che_NRec_sfl
280100070313      *
280200070313      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
280300070313      * presente nel subfile -- Selected -- questo è aggiunto al record.
280400070313      * Il campo è usato per selezionare i records dando un ordine a quelli
280500070313      * selezionati davanti ai non selezionati.
280600070313     C                   CLEAR                   QLGSCB
280700070313     C                   CLEAR                   QLGSCB00
280800070313
280900070313     c                   eval      wsfl = 1
281000070313
281100070313     ** Gestione della scelta ordinamento.
281200070313     C                   SELECT
281300070313      *
281400070402      *?  Ordinamento per Autista.  ?
281500070313     C                   WHEN      WrkSort = Autista
281600070322     c                   clear                   c1cnfv
281700070403     C*
281800070417     C********           MOVE      *Off          $INZS1
281900070403     C*
282000070313     ** 1 campo chiave.
282100070313     C                   EVAL      QLGNBRK    = 1
282200070313     ** La descrizione è
282300070402     ** a posizione 1, xx byte, char, ascending.
282400070313     C                   EVAL      QLGSP      = 1
282500070322     C                   EVAL      QLGSS      = %SIZE(S1DAUT)
282600070313     C                   EVAL      QLGDT      = Carattere
282700070313     C                   EVAL      QLGSO      = Ascendente
282800070313     C                   EVAL      QLGKL(1)   = QLGSKL
282900070313
283000070313     ** Al prossimo F11 ordino per Numero Distinta
283100070313     C                   EVAL      WrkSort = Numero
283200070313
283300070313     C                   EVAL      *IN11 = *ON
283400070313     C                   EVAL      sav_in11 = *IN11
283500070313
283600070402      *?  Ordinamento per Numero Distinta   ?
283700070313     C                   WHEN      WrkSort = Numero
283800070322     c                   clear                   c1daut
283900070403      *
284000070510     ** 3 campi chiave.  x DATA e NUMERO
284100070510     C                   EVAL      QLGNBRK    = 3
284200070313     ** Il gruppo è
284300070403     ** a posizione xx, 6 byte, char, ascending
284400070417     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
284500070417     C                                     %SIZE(S1CDFV)
284600070417     C                   EVAL      QLGSS    = %SIZE(H1CDFV)
284700070403     C                   EVAL      QLGDT    = Numerico
284800070403     C                   EVAL      QLGSO    = Ascendente
284900070403     C                   EVAL      QLGKL(1) = QLGSKL
285000070313
285100070510     ** Priorità  è
285200070510     ** a posizione xx, 6 byte, char, ascending
285300070510     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
285400070510     C                                     %SIZE(S1CDFV) + %SIZE(H1CDFV)
285500070510     C                   EVAL      QLGSS    = %SIZE(H1PRGG)
285600070510     C                   EVAL      QLGDT    = Numerico
285700070510     C                   EVAL      QLGSO    = Ascendente
285800070510     C                   EVAL      QLGKL(2) = QLGSKL
285900070510
286000070313     ** La descrizione è
286100070403     ** a posizione 1, xx byte, char, ascending
286200070403     C                   EVAL      QLGSP      = 1 + %SIZE(S1DAUT)
286300070403     C                   EVAL      QLGSS      = %SIZE(S1CNFV)
286400070403     C                   EVAL      QLGDT      = Numerico
286500070313     C                   EVAL      QLGSO      = Ascendente
286600070510     C                   EVAL      QLGKL(3)   = QLGSKL
286700070313
286800070313     C                   EVAL      *IN11 = *OFF
286900070313     C                   EVAL      sav_in11 = *IN11
287000070313
287100070313     ** Al prossimo F11 ordino per descrizione.
287200070313     C                   EVAL      WrkSort = Autista
287300070403      *
287400070403      *   Se il range di date non è dello stesso giorno
287500070417     c******             if        *in13 = *off
287600070417     C******             MOVE      *On           $INZS1
287700070417     c******             goto      end_ordina1
287800070417     c******             end
287900070313
288000070313     C                   ENDSL
288100070313
288200070313      * Load other sort parameters.
288300070313     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
288400070322     C                   EVAL      QLGRL     = %SIZE(SflRcd1) - 1
288500070313     C                   EVAL      QLGRT     = 8
288600070313     C                   EVAL      QLGOKL    = 80
288700070313     C                   EVAL      QLGLKE    = 16
288800070313     C                   EVAL      QLGLSS    = 290
288900070313
289000070313      * Initialize Sort I/O API fields.
289100070313     C                   EVAL      QLGRL00  = QLGRL
289200070313     C                   EVAL      QLGRC00  = 1
289300070313     C                   CLEAR                   QUSEI
289400070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
289500070313
289600070313      * First step - Initialize the sort routine.
289700070313     C                   CALL      'QLGSORT'
289800070313     C                   PARM                    QLGSCB
289900070313     C                   PARM                    NotUsed
290000070313     C                   PARM                    NotUsed
290100070313     C                   PARM                    SizeList
290200070313     C                   PARM                    ReturnSize
290300070313     C                   PARM                    QUSEC
290400070313
290500070313      * Next step - Write records to I/O routine.
290600070313     C                   EVAL      QLGRT00 = Put
290700070313
290800070322     C                   DO        RrnLast       S1nrr
290900070313
291000070322     C     S1nrr         CHAIN     DG31SF1
291100070313
291200070313     ** Solo le righe con Selected = 'Y' sono riordinate,
291300070313     ** quindi per fare un ordinamento di tutte le righe
291400070313     ** metto 'Y' sempre.
291500070322     C                   EVAL      Selected1 = 'Y'
291600070313
291700070313     C                   CLEAR                   QUSEI
291800070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
291900070322      *
292000070313     C                   CALL      'QLGSRTIO'
292100070313     C                   PARM                    QLGSCB00
292200070322     C                   PARM                    SflRcd1
292300070313     C                   PARM                    NotUsed
292400070313     C                   PARM                    SizeList
292500070313     C                   PARM                    NotUsed
292600070313     C                   PARM                    QUSEC
292700070313
292800070313     C                   ENDDO
292900070313
293000070313      * Next step - Signal end of input, clear subfile for reload.
293100070313     C                   EVAL      QLGRT00 = EndPut
293200070313     C                   CLEAR                   QUSEI
293300070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
293400070313
293500070313     C                   CALL      'QLGSRTIO'
293600070313     C                   PARM                    QLGSCB00
293700070322     C                   PARM                    SflRcd1
293800070313     C                   PARM                    NotUsed
293900070313     C                   PARM                    SizeList
294000070313     C                   PARM                    NotUsed
294100070313     C                   PARM                    QUSEC
294200070313
294300070313     C* pulizia SFL
294400070313     C                   SETOFF                                         3031
294500070322     C                   WRITE     DG31CT1
294600070313     C                   SETON                                          31
294700070313
294800070313      * Final step - Write the records back to the subfile.
294900070313     C                   EVAL      QLGRT00 = Get
295000070313
295100070322     C                   DO        RrnLast       s1nrr
295200070313     C                   CLEAR                   QUSEI
295300070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
295400070313     C                   CALL      'QLGSRTIO'
295500070313     C                   PARM                    QLGSCB00
295600070313     C                   PARM                    NotUsed
295700070322     C                   PARM                    SflRcd1
295800070313     C                   PARM                    QLGRL00
295900070313     C                   PARM                    NotUsed
296000070313     C                   PARM                    QUSEC
296100070510      *
296200080208      * Ripristina gli indicatori x i DSPATR
296300070510     c                   setoff                                       40
296400070510if  4c                   if        h1prgg = 1
296500070510     c                   seton                                        40
296600070510     c                   end
296700080208      * x fase di test
296800080219     c                   eval      *in15 = sav_in15
296900080208     c                   setoff                                       14
297000080208     c                   if        h1cDFV >= DateU8
297100080208     c                   if        §ogPDAorm ='S' and (S1CTSTPDA ='o') or
297200080208     c                             §ogPDAorm ='S' and (S1CTSTPDA ='e') or
297300080208     c                             §ogPDAcon ='S' and (S1CTSTPDA ='e') or
297400080208     c                             §ogPDAcon ='S' and (S1CTSTPDA ='c')
297500080208     c                   seton                                        14
297600080208     c                   endIf
297700080208     c                   endIf
297800080222     c                   eval      h1in14 = *in14
297900070510      *
298000070322     C                   WRITE     DG31SF1
298100070313     C                   ENDDO
298200070320      *
298300070403     C     end_ordina1   ENDSR
298400070322     C/EJECT
298500070322      *---------------------------------------------------------------*
298600070322      *? GESTIONE F11 alterna l'ordinamento x numero o Autotrasportatore  ?
298700070322      *---------------------------------------------------------------*
298800070322     C     F11_ordina2   BEGSR
298900070322     C*
299000070403     C* imposto pos. del cursore
299100070403     C                   if        WrkSort = Autista or
299200070403     C                             WrkSort = Numero and *in13 = *on
299300070403     c                   eval      cur_su_sfl = 'N'
299400070403     C                   eval           H1RIGA = 3
299500070528     C                   eval           H1COLO = 17
299600070403     c                   end
299700070403     C* imposto pos. del cursore
299800070403     C                   if        WrkSort = Numero and *in13 = *off
299900071026     c*****                        or Prima_volta = 'S'  and fdgPGm = 'FIDG30R'
300000071026     c                             or Prima_volta = 'S'
300100070607     c                   eval      Prima_volta = 'N'
300200070403     c                   eval      cur_su_sfl = 'S'
300300070403     C                   eval           H1RIGA = 7
300400070403     C                   eval           H1COLO = 2
300500070403     c                   end
300600071026     C                   z-add     h1riga        Su_Quale_rec
300700071026     c                   z-add     1             Che_NRec_sfl
300800070322      *
300900070322      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
301000070322      * presente nel subfile -- Selected -- questo è aggiunto al record.
301100070322      * Il campo è usato per selezionare i records dando un ordine a quelli
301200070322      * selezionati davanti ai non selezionati.
301300070322     C                   CLEAR                   QLGSCB
301400070322     C                   CLEAR                   QLGSCB00
301500070322
301600070322     c                   eval      wsfl = 1
301700070322
301800070322     ** Gestione della scelta ordinamento.
301900070322     C                   SELECT
302000070322      *
302100070403      *?  Ordinamento per Autista.  ?
302200070322     C                   WHEN      WrkSort = Autista
302300070322     c                   clear                   c2cnfv
302400070403      *
302500070417     C*******            MOVE      *Off          $INZS2
302600070403      *
302700070322     ** 1 campo chiave.
302800070322     C                   EVAL      QLGNBRK    = 1
302900070322     ** La descrizione è
303000070322     ** a posizione 1, 28 byte, char, ascending.
303100070322     C                   EVAL      QLGSP      = 1
303200070322     C                   EVAL      QLGSS      = %SIZE(S2DAUT)
303300070322     C                   EVAL      QLGDT      = Carattere
303400070322     C                   EVAL      QLGSO      = Ascendente
303500070322     C                   EVAL      QLGKL(1)   = QLGSKL
303600070322
303700070322     ** Al prossimo F11 ordino per Numero Distinta
303800070322     C                   EVAL      WrkSort = Numero
303900070322
304000070322     C                   EVAL      *IN11 = *ON
304100070322     C                   EVAL      sav_in11 = *IN11
304200070322
304300070403      *?  Ordinamento per Numero Distinta   ?
304400070322     C                   WHEN      WrkSort = Numero
304500071026      ********
304600071026     c**********         clear                   c2daut
304700071026      ********
304800070510     ** 3 campi chiave.
304900070510     C                   EVAL      QLGNBRK    = 3
305000070322     ** Il gruppo è
305100070322     ** a posizione 29, 6 byte, char, ascending
305200070417     C                   EVAL      QLGSP   = 1 + %SIZE(S2DAUT) + %SIZE(S2CNFV) +
305300070417     C                                     %SIZE(S2CDFV)
305400070417     C                   EVAL      QLGSS    = %SIZE(h2CDFV)
305500070403     C                   EVAL      QLGDT    = Numerico
305600070403     C                   EVAL      QLGSO    = Ascendente
305700070403     C                   EVAL      QLGKL(1) = QLGSKL
305800070510     ** La priorità
305900070510     ** a posizione xx, x byte, char, ascending
306000070510     C                   EVAL      QLGSP   = 1 + %SIZE(S2DAUT) + %SIZE(S2CNFV) +
306100070510     C                                     %SIZE(S2CDFV) + %SIZE(h2CDFV)
306200070510     C                   EVAL      QLGSS    = %SIZE(h2PRGG)
306300070510     C                   EVAL      QLGDT    = Numerico
306400070510     C                   EVAL      QLGSO    = Ascendente
306500070510     C                   EVAL      QLGKL(2) = QLGSKL
306600070322     ** La descrizione è
306700070322     ** a posizione 5, 28 byte, char, ascending
306800070403     C                   EVAL      QLGSP      = 1 + %SIZE(S2DAUT)
306900070403     C                   EVAL      QLGSS      = %SIZE(S2CNFV)
307000070403     C                   EVAL      QLGDT      = Numerico
307100070322     C                   EVAL      QLGSO      = Ascendente
307200070510     C                   EVAL      QLGKL(3)   = QLGSKL
307300070322
307400070322     C                   EVAL      *IN11 = *OFF
307500070322     C                   EVAL      sav_in11 = *IN11
307600070322
307700070322     ** Al prossimo F11 ordino per descrizione.
307800070322     C                   EVAL      WrkSort = Autista
307900070403      *
308000070403      *   Se il range di date non è dello stesso giorno
308100070417     c*******            if        *in13 = *off
308200070417     C*******            MOVE      *On           $INZS2
308300070417     c*******            goto      end_ordina2
308400070417     c*******            end
308500070322
308600070322     C                   ENDSL
308700070322
308800070322      * Load other sort parameters.
308900070322     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
309000070322     C                   EVAL      QLGRL     = %SIZE(SflRcd2) - 1
309100070322     C                   EVAL      QLGRT     = 8
309200070322     C                   EVAL      QLGOKL    = 80
309300070322     C                   EVAL      QLGLKE    = 16
309400070322     C                   EVAL      QLGLSS    = 290
309500070322
309600070322      * Initialize Sort I/O API fields.
309700070322     C                   EVAL      QLGRL00  = QLGRL
309800070322     C                   EVAL      QLGRC00  = 1
309900070322     C                   CLEAR                   QUSEI
310000070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
310100070322
310200070322      * First step - Initialize the sort routine.
310300070322     C                   CALL      'QLGSORT'
310400070322     C                   PARM                    QLGSCB
310500070322     C                   PARM                    NotUsed
310600070322     C                   PARM                    NotUsed
310700070322     C                   PARM                    SizeList
310800070322     C                   PARM                    ReturnSize
310900070322     C                   PARM                    QUSEC
311000070322
311100070322      * Next step - Write records to I/O routine.
311200070322     C                   EVAL      QLGRT00 = Put
311300070322
311400070322     C                   DO        RrnLast       S2nrr
311500070322
311600070322     C     S2nrr         CHAIN     DG31SF2
311700070322
311800070322     ** Solo le righe con Selected = 'Y' sono riordinate,
311900070322     ** quindi per fare un ordinamento di tutte le righe
312000070322     ** metto 'Y' sempre.
312100070322     C                   EVAL      Selected2 = 'Y'
312200070322
312300070322     C                   CLEAR                   QUSEI
312400070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
312500070322      *
312600070322     C                   CALL      'QLGSRTIO'
312700070322     C                   PARM                    QLGSCB00
312800070322     C                   PARM                    SflRcd2
312900070322     C                   PARM                    NotUsed
313000070322     C                   PARM                    SizeList
313100070322     C                   PARM                    NotUsed
313200070322     C                   PARM                    QUSEC
313300070322
313400070322     C                   ENDDO
313500070322
313600070322      * Next step - Signal end of input, clear subfile for reload.
313700070322     C                   EVAL      QLGRT00 = EndPut
313800070322     C                   CLEAR                   QUSEI
313900070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
314000070322
314100070322     C                   CALL      'QLGSRTIO'
314200070322     C                   PARM                    QLGSCB00
314300070322     C                   PARM                    SflRcd2
314400070322     C                   PARM                    NotUsed
314500070322     C                   PARM                    SizeList
314600070322     C                   PARM                    NotUsed
314700070322     C                   PARM                    QUSEC
314800070322
314900070322     C* pulizia SFL
315000070322     C                   SETOFF                                         3031
315100070322     C                   WRITE     DG31CT2
315200070322     C                   SETON                                          31
315300070322
315400070322      * Final step - Write the records back to the subfile.
315500070322     C                   EVAL      QLGRT00 = Get
315600070322
315700070322     C                   DO        RrnLast       s2nrr
315800070322     C                   CLEAR                   QUSEI
315900070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
316000070322     C                   CALL      'QLGSRTIO'
316100070322     C                   PARM                    QLGSCB00
316200070322     C                   PARM                    NotUsed
316300070322     C                   PARM                    SflRcd2
316400070322     C                   PARM                    QLGRL00
316500070322     C                   PARM                    NotUsed
316600070322     C                   PARM                    QUSEC
316700070510      *
316800080208      * Ripristina gli indicatori x i DSPATR
316900070510     c                   setoff                                       40
317000070510if  4c                   if        h2prgg = 1
317100070510     c                   seton                                        40
317200070510     c                   end
317300071029      *
317400071029     c                   setoff                                       42
317500071029if  4c                   if        h2recpos = 'X'
317600071029     c                   z-add     s2nrr         nrec_riposiz
317700071029     C                   z-add     riri          Su_Quale_rec
317800071029     c                   end
317900080208      * x fase di test
318000080219     c                   eval      *in15 = sav_in15
318100080208     c                   setoff                                       14
318200080208     c                   if        h2cDFV >= DateU8
318300080208     c                   if        §ogPDAorm ='S' and (S2CTSTPDA ='o') or
318400080208     c                             §ogPDAorm ='S' and (S2CTSTPDA ='e') or
318500080208     c                             §ogPDAcon ='S' and (S2CTSTPDA ='e') or
318600080208     c                             §ogPDAcon ='S' and (S2CTSTPDA ='c')
318700080208     c                   seton                                        14
318800080208     c                   endIf
318900080208     c                   endIf
319000080222     c                   eval      h2in14 = *in14
319100070510      *
319200070322     C                   WRITE     DG31SF2
319300070322     C                   ENDDO
319400070322      *
319500070403     C     End_ordina2   ENDSR
319600070313     C/EJECT
319700070313      *---------------------------------------------------------------*
319800070321      *? GESTIONE F21 interrogazione spunte    ?
319900070313      *---------------------------------------------------------------*
320000070313     C     F21_interroga BEGSR
320100070313     C*
320200070312     C                   CLEAR                   PARA04
320300070314     C                   Z-ADD     wNFV          P04NFV
320400070312     C                   Z-ADD     99            P04NRS
320500070312     C                   Z-ADD     99            P04NPS
320600070314     C                   MOVEL     wnpg          P04NPG
320700070312     C                   MOVEL     '1'           P04FLG
320800070312     C                   MOVEL     'FIDG04R'     P04PGM
320900070314     C                   Z-ADD     wfgs          P04FGS
321000070313     c                   eval      kpjbus = kpjbu
321100070312     C                   MOVEL     PARA04        KPJBU
321200070312     C                   CALL      'FNLR04R'
321300070312     C                   PARM                    KPJBA
321400070313     c                   eval      kpjbu = kpjbus
321500070312      *
321600070312     C                   ENDSR
321700070301     C/EJECT
321800070321     C************************************************************
321900070321      *?  Emissione video di Dettaglio           ?
322000070312     C************************************************************
322100070312     C     GESD2         BEGSR
322200070315     C*
322300070312     C* inizializzazione videata
322400070312     C     $INZD2        IFEQ      *ON
322500070312     C                   EXSR      INZD2
322600070312     C                   MOVE      *OFF          $INZD2
322700070312     C                   ENDIF
322800070312     C*
322900070312     c     Video_2       tag
323000070312     C*  Emissione Video
323100070312     c                   setoff                                       99
323200070312     c                   if        $VISD2 = 'S'
323300070320      *-----
323400070312      * ?            *-----------------------*
323500070312     C                   EXFMT     DG31D02
323600070312      * ?            *-----------------------*
323700070320      *-----
323800070320     c                   else
323900070320      *-----
324000070320      *  Richiama il programma di visualizzazione del dettaglio distinta
324100070402     c                   exsr      Detta_GIRI
324200070406
324300070406     c                   end
324400070320     C*
324500070320     C* imposto pos. del cursore
324600070320     C                   eval           H1RIGA = CURR
324700070320     C                   eval           H1COLO = CURC
324800070312     C* Selezioni
3249000703121    C                   SELECT
325000070402      *-----
325100070402     C                   WHEN      $TASTO   =   F02
325200070402     c                   exsr      Detta_GIRI
325300070402     C                   if        comando = 'KC'
325400070402     C                   clear                   comando
325500070402     c                   move      'S'           $ESCI
325600070402     C                   EXSR      F03_exit
325700070402     c                   goto      end_d02
325800070402     c                   end
325900070402     C*--------
326000070312     C* F3=Fine
326100070320     C                   WHEN      $TASTO   =   F03  or comando = 'KC'
326200070330     C                   clear                   comando
326300070314     c                   move      'S'           $ESCI
326400070312     C                   EXSR      F03_exit
326500070312     c                   goto      end_d02
326600070312     C* F12=Ritorno
326700070320     C                   WHEN      $TASTO   =   F12  or comando = 'KL'
326800070330     C                   clear                   comando
326900070329     c                   if        sav_$GEST <> *blank
327000070313     C                   movel     Sav_$GEST     $gest
327100070329     c                   else
327200070329     c                   move      'S'           $ESCI
327300070329     C                   EXSR      F03_exit
327400070329     c                   end
327500070312     c                   goto      end_d02
327600070312      *
327700070312     C* F21=Spunte del foglio richiesto
327800070312     C                   WHEN      $TASTO   =   F21
327900070312     C                   EXSR      F21_interroga
328000070312     c                   seton                                        99
328100070404     C* F6=Conferma Scelta
328200070404     C                   WHEN      $TASTO   =   F06
328300070404     c                   z-add     d2CNFV        FDGNFV
328400070404     c                   move      d1cnpg        FDGNPG
328500070404     c                   move      'S'           $ESCI
328600070404     C                   EXSR      F03_exit
328700070404     c                   goto      end_d02
328800070312      *
3289000703121O   C                   OTHER
329000070312     C* CONTROLLO DATI
329100070312     C                   EXSR      CTRD2
3292000703121-   C                   ENDSL
329300070320      *-----
329400070312     C*
329500070312     C* Per errori o decodifiche
329600070320     c                   if        $VISD2 = 'S'
329700070312     c   99              goto      Video_2
329800070320     c                   end
329900070312      *
330000070312     c     end_d02       tag
330100070312      *
330200070312     C                   ENDSR
330300070312     C/EJECT
330400070312     C************************************************************
330500070402      *?  Visualizza anche il dettaglio GIRI     ?
330600070312     C************************************************************
330700070402     C     Detta_GIRI    BEGSR
330800070320     C*
330900070402      *  Richiama il programma di visualizzazione del dettaglio distinta
331000070402     c                   clear                   fidg02ds
331100070402     C                   MOVEL     wfgs          D02FIL
331200070402     C                   MOVEL     'A'           D02TIPA
331300070402     C                   MOVEL     dstpdr        D02PDR
331400070402     C                   MOVEL     '05'          D02CMD
331500070402     C                   MOVEL     'N'           D02STRCMT
331600070402     C                   MOVEL     'N'           D02OPNCMT
331700070402     C                   MOVEL     'N'           D02COM
331800070402     C                   MOVEL     'FIDST00F'    D02FILE
331900070402     C                   MOVEL     dstDFV        D02DFV
332000070402     C                   MOVEL     dstNFV        D02NFV
332100070402     C                   MOVEL     '4'           D02NPG
332200070402     C                   MOVEL     dstFPP        D02FPP
332300070402     C                   MOVEL     dstTPM        D02TPM
332400070904     C                   MOVEL     d1dati        D02TP
332500070402     c                   eval      kpjbus = kpjbu
332600070402     C                   eval      KPJBU  = fidg02ds
332700070402     c                   call      'FIDG02C'
332800070402     c                   parm                    kpjba
332900070402     C                   MOVEL     KPJBU         fidg02ds
333000070905      *
333100070905      *  secondo giro per visualizzare le informazioni successive
333200070905      *   solo se NON F3
333300070905     C                   if        D02CMDE <> 'KC'
333400070905     C                   if        d1dati = 'C'
333500070905     C                   MOVEL     'R'           D02TP
333600070905     c                   else
333700070905     C                   MOVEL     'C'           D02TP
333800070905     c                   end
333900070905      *
334000070926     C                   if        test_Paral_ORM = 'S' and D02TP = 'R' or
334100070926     C                             test_Parallelo = 'S' and D02TP = 'C'
334200070905     c                   eval      kpjbus = kpjbu
334300070905     C                   eval      KPJBU  = fidg02ds
334400070905     c                   call      'FIDG02C'
334500070905     c                   parm                    kpjba
334600070905     C                   MOVEL     KPJBU         fidg02ds
334700070926     c                   endIf
334800070905     c                   end
334900070905      *
335000070402     c                   eval      kpjbu = kpjbus
335100070402     C                   MOVEL     D02CMDE       comando           2
335200070402     C                   MOVEL     D02ERR        errore            1
335300070402      *
335400070402     C                   ENDSR
335500070402     C/EJECT
335600070402     C************************************************************
335700070402      *?  INIZIALIZZAZIONE Vidoe di Dettaglio    ?
335800070402     C************************************************************
335900070402     C     INZD2         BEGSR
336000070402     C*
336100070312     C* Pulizia campi e indicatori
336200070312     c                   exsr      INZ_indERR
336300070312     C*
336400070312     C* Pulizia formato record   campi video
336500070312     c                   clear                   DG31D02
336600070403     c                   if        se_EDP = 'S'
336700070830     C***                SETON                                          15
336800070403     c                   end
336900070314     C*
337000070314     C* Se richiamato attivo 04
337100070314     c                   if        pgm_da_call = 'S'
337200070314     C                   SETON                                          04
337300070314     c                   end
337400070312     C*
337500070312     C* Imposta indicatore di selezione
337600070313     c                   move      sav_in19      *in19
337700080219     c                   move      sav_in15      *in15
337800070312     C* campi titolo
337900070312     C                   MOVEL     TES(1)        d2cTES
338000070606     c                   if        fdgPGM = 'FIDG30R'
338100070606     C                   MOVEL(p)  TES(2)        d2CTES
338200070606     c                   end
338300070312     C                   MOVEL     dsPGM         d2cPGM
338400070312     C                   MOVEL     RAGUT         d2cDSA
338500070312     C                   MOVEL     knsif         d2nsif
338600070312     C                   MOVEL     knmus         d2nmus
338700070312      *
338800070312     C*  Aggancia la distinta
338900070406     c     kDST9         chain     Fidst09L
339000070406     c                   if        %Found(Fidst09l)
339100070312     C*
339200070312     C                   SETOFF                                       52  53
339300070312     C                   SETOFF                                       06  07
339400070312     C                   z-add     wnfv          d2cnfv
339500070312     C                   z-add     dstfgs        d2cfgs
339600070314     C                   z-add     dstpdr        d2caut
339700070314     c     kapd          chain     fiapd01l
339800070314     c                   if        %found(fiapd01l)
339900070314     c                   eval      d2DAUT = apdrsc
340000070314     c                   end
340100070314      *
340200070312     C* FILIALE GESTIONE
340300070312     C     dstFGS        CHAIN     AZORG                              21
340400070312     C  N21              MOVEL     ORGDES        d2DFGS
340500070312     C* DATA FOGLIO
340600070312     C                   MOVE      dstDFV        G02INV
340700070312     C                   MOVE      *ZEROS        G02DAT
340800070312     C                   MOVEL     '3'           G02ERR
340900070312     C                   CALL      'XSRDA8'
341000070312     C                   PARM                    WLBDAT
341100070312     C                   Z-ADD     G02DAT        d2CDFV
341200070312     c                   eval      d2cpkla= DSTPKLA
341300070312     c                   eval      d2cvlua= DSTVLUA
341400070312     c                   eval      d2cstpa= DSTSTPA
341500070312     c                   eval      d2cnbla= DSTnblA
341600070313     c                   eval      d2cpkl = DSTPKL
341700070313     c                   eval      d2cvlu = DSTVLU
341800070313     c                   eval      d2cstp = DSTNFTL
341900070313     c                   eval      d2cnbl = DSTnbl
342000070903     c                   eval      d2cPDA = DSTpda
342100080207      *
342200080207      * per Entrambe prima era segnalato con (S) ora invece si vuole vedere (E)
342300080207      * inoltre se non è attivo il PDA si deve visualizzare Blank invece che "N".
342400080207     c                   if        d2cPDA = 'N'
342500080207     c                   eval      d2cPDA = ' '
342600080207     c                   end
342700080207      *
342800080207      *  nel campo filler c'è il tipo di attivazione
342900080207     c                   eval      DDSTFLR =  DstFLR
343000080208     c                   setoff                                       14
343100080208     c                   if        dstDFV >= DateU8
343200080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
343300080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
343400080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
343500080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
343600080208     c                   seton                                        14
343700080208     c                   endIf
343800080208     c                   endIf
343900080213      *
344000080222     c                   if        dstDFV >= dateu8
344100080213     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
344200080213     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
344300080213     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
344400080213     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
344500080213     C                   eval      D2msgv= 'ATT. gestione PDA attivata'
344600080213     c                   endIf
344700080222     c                   endIf
344800080212      *
344900080212     C                   movel     §DSTtstPDA    d2CTSTPDA
345000080207      ***
345100070312     C                   MOVEL     dstFPP        d2CFPP                         *MATT/POM
345200070312     C                   MOVEL     dstPEP        d2CPEP                         *PIKING(S/N)
345300070312     C                   MOVEL     dsttpm        d2ctpm                         *MATT/POM
345400070312     C                   MOVEL     dstfca        d2cfca                         *MATT/POM
345500070403     c                   eval      d2cnot  = dstnot
345600070312     c                   if        d2ctpm = ' '
345700070312     c                   eval      d2ctpm = 'P'
345800070312     c                   end
345900070403      * BANCALI
346000070403     C     Kqdt          CHAIN     Fiqdt01l
346100070403     c                   if        %found(Fiqdt01l)
346200070403     C                   MOVEL     qdtnbnaf      D2CNBN                         *NR.BANCALI
346300070403     C                   MOVEL     qdtfnbaf      D2CFNBN                        *NR.BANCALI
346400070403     c                   end
346500070312      * VEDO SE ANNULLATO
346600070312     C     dstATB        IFEQ      'A'
346700070312     C                   SETON                                        5206
346800070312     C                   MOVEL     CANNUL        d2DTFV
346900070312     C                   END
347000070312      * VEDO SE CHIUSO
347100070312     C     dstFCF        IFNE      *BLANKS
347200070312     C     dstFCF        IFEQ      'S'
347300070312     C                   SETON                                        53
347400070312     C                   ELSE
347500070312     C                   SETON                                        06
347600070312     C                   MOVEL     CBLOCC        d2DTFV
347700070312     C                   ENDIF
347800070312     C                   ENDIF
347900070312     C*
348000070312     C* 07 ON ESISTONO SPUNTE SUL FOGLIO
348100070312     C     Kdst          SETLL     FnBRV000                               07     ON=TROVATO
348200070312     C*
348300070910     c                   if        d1Dati = 'C'
348400070910     C*
348500070314     C* Carica i GIRI della Distinta
348600070314     c     kdst31        setll     fidst31l
348700070314     c     kdst31        reade     fidst31l
348800070314     c                   z-add     0             x                 3 0
348900070314      **
349000070314     c                   dow       not %Eof(fidst31l)
349100070314      **  Controlla presenza del giro richiesto
349200070314     c                   if        %char(DST3NPG) = d1cNPG and
349300070314     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
349400070314     c                   add       1             x
349500070315     c                   if        x > 16
349600070314     c                   leave
349700070314     c                   end
349800070314      *
349900070314     c                   movel     dst3cgi       cgi(x)
350000070314      *  controllo validità giro
350100070314     c                   clear                   fidg09ds
350200070314     c                   eval      D09IOP0 = '001'
350300070314     c                   eval      d09ifgs = wfgs
350400071010     c*****              eval      d09itug = 'C'
350500071010     c                   eval      d09itug = d1DATI
350600070314     c                   eval      d09icgi = dst3cgi
350700070314     c                   move      dateu8        d09idat
350800070314     c                   eval      kpjbu = fidg09ds
350900070315     c                   call      'FIDG09R'
351000070314     c                   parm                    kpjba
351100070314     c                   eval      fidg09ds = kpjbu
351200070314     c                   if        d09oerr <> '1' and d09ocgi <> ' '
351300070314     c                   eval      dgi(x) = d09odes
351400070314     c                   end
351500070314      *
351600070314     c                   end
351700070314     c     kdst31        reade     fidst31l
351800070314     c                   endDo
351900070910      **
352000070910     c                   else
352100070910      **
352200070910     C* Carica i GIRI della Distinta
352300070910     c     kdst51        setll     fidst51l
352400070910     c     kdst51        reade     fidst51l
352500070910     c                   z-add     0             x                 3 0
352600070910      **
352700070910     c                   dow       not %Eof(fidst51l)
352800070910      **  Controlla presenza del giro richiesto
352900070910     c                   if        %char(DST5NPG) = d1cNPG and
353000070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
353100070910     c                   add       1             x
353200070910     c                   if        x > 16
353300070910     c                   leave
353400070910     c                   end
353500070910      *
353600070910     c                   movel     dst5cgi       cgi(x)
353700070910      *  controllo validità giro
353800070910     c                   clear                   fidg09ds
353900070910     c                   eval      D09IOP0 = '001'
354000070910     c                   eval      d09ifgs = wfgs
354100071010     c****               eval      d09itug = 'C'
354200071010     c                   eval      d09itug = d1DATI
354300070910     c                   eval      d09icgi = dst5cgi
354400070910     c                   move      dateu8        d09idat
354500070910     c                   eval      kpjbu = fidg09ds
354600070910     c                   call      'FIDG09R'
354700070910     c                   parm                    kpjba
354800070910     c                   eval      fidg09ds = kpjbu
354900070910     c                   if        d09oerr <> '1' and d09ocgi <> ' '
355000070910     c                   eval      dgi(x) = d09odes
355100070910     c                   end
355200070910      *
355300070910     c                   end
355400070910     c     kdst51        reade     fidst51l
355500070910     c                   endDo
355600070910      **
355700070910      *
355800070910     c                   end
355900070910     C*
356000070406     c     d2cgi1        comp      *blank                                 65
356100070314      **
356200070312     c                   end
356300070312     C*
356400070312     C                   ENDSR
356500070312     C************************************************************
356600070321      *?  Controlli Video di Dettaglio                           ?
356700070312     C************************************************************
356800070312     C     CTRD2         BEGSR
356900070312     C*
357000070312     C*  Spegne indicatori di errore
357100070312     c                   exsr      INZ_indERR
357200070312     C*
357300070312     c                   seton                                        99
357400070406     c     d2cgi1        comp      *blank                                 65
357500070312      *
357600070312      * ? * Ritorno al primo video
357700070312     c                   move      'D1'          $gest
357800070312     C*------------------------
357900070312     C*
358000070312     C                   ENDSR
358100070301      *---------------------------------------------------------------*
358200070321      *
    OPERAZIONI INIZIALI        ?
358300070301      *---------------------------------------------------------------*
358400940131     C     *INZSR        BEGSR
358500030113     C*
358600030113     C* Reperimento parametri
358700030113     C     *ENTRY        PLIST
358800030113     C                   PARM                    KPJBA
358900070404     c                   parm                    $FV6
359000070309      *
359100070313     C                   MOVEL     KPJBU         FIDG31DS
359200070904      * default -> Consegne
359300070904     C                   IF        fdgTpoP      = *blank
359400070904     c                   movel     'C'           fdgTpoP
359500070904     c                   end
359600070903     c                   movel     fdgTpoP       Tipo_Prestaz      1
359700070309      *
359800070606      * memorizza se richiamato da Assegnazione Manuale da quale tipo
359900070606      *  di operazione è stato chiamato Assegnazione oppure altro.
360000070606     c                   clear                   fdg31_opz         1
360100070606     c                   if        fdgPGM = 'FIDG30R'
360200070606     c                   move      fdgOPZ        fdg31_opz         1
360300070607     c                   eval      cur_su_SFL = 'S'
360400070606     c                   end
360500070606      *
360600070309     C                   Z-ADD     1             CODUT
360700070309     C                   CALL      'X§PARUT'
360800070309     C                   PARM                    UT§DSE
360900070309     C                   MOVEL     REC80         CNCR80
361000070309     C***
361100070309     C     Kdst          KLIST
361200070309     C                   KFLD                    WNPG
361300070312     C                   KFLD                    wNFV
361400070312     C                   KFLD                    wFGS
361500070312     C***
361600070406     C     Kdst9         KLIST
361700070406     C                   KFLD                    wFGS
361800070406     C                   KFLD                    WNPG
361900070406     C                   KFLD                    wNFV
362000070406     C***
362100070312     C     Kdst31        KLIST
362200070312     C                   KFLD                    wFGS
362300070312     C                   KFLD                    wNFV
362400070314      *
362500070910     C     Kdst51        KLIST
362600070910     C                   KFLD                    wFGS
362700070910     C                   KFLD                    wNFV
362800070910      *
362900070403     C     Kqdt          KLIST
363000070403     C                   KFLD                    wFGS
363100070403     C                   KFLD                    WNFV
363200070403      *
363300070314     C     KORM          KLIST
363400070314     C                   KFLD                    wFGS
363500070314     C                   KFLD                    WNFV
363600070309     C***
363700070309     C     Kapd          KLIST
363800070309     C                   KFLD                    dsttipa
363900070309     C                   KFLD                    dstpdr
364000070309      * Fnarb
364100070309     c     Karb78        Klist
364200070309     c                   Kfld                    Wifp
364300070309     c                   Kfld                    Kfnvv
364400070309      *
364500070309     C     KDAT          KLIST
364600070309     C                   KFLD                    WNPG
364700070309     C                   KFLD                    DATINW
364800070309
364900070309     c     Kdat1         Klist
365000070314     c                   Kfld                    wFGS
365100070309     c                   Kfld                    Wnpg
365200070309     c                   Kfld                    Datinw
365300070309
365400070309     c     Kdat2         Klist
365500070314     c                   Kfld                    wFGS
365600070309     c                   Kfld                    Wnpg
365700070309      *
365800070309     C     Kdst3         KLIST
365900070314     C                   KFLD                    wFGS
366000070309     C                   KFLD                    WNPG
366100070309      *
366200070309     C     Kimpo         Klist
366300070309     C                   Kfld                    arbaas
366400070309     C                   Kfld                    arblnp
366500070309     C                   Kfld                    arbnrs
366600070309     C                   Kfld                    arbnsp
366700070309     C***
366800070309     C* CARICO TABELLA FILIALI GESTITE £1
366900070309     C                   CLEAR                   DSUL06
367000070309     C                   MOVE      '£1'          D06COD
367100070309     C                   MOVEL     SIMFEL        D06KEY
367200070309     C                   MOVEL     'L'           D06TLA
367300070313     c                   eval      kpjbus = kpjbu
367400070309     C                   MOVEL     DSUL06        KPJBU
367500070309     C                   CALL      'TRUL06R'
367600070309     C                   PARM                    KPJBA
367700070309     C                   MOVEL     KPJBU         DSUL06
367800070313     c                   eval      kpjbu = kpjbus
367900070309     C                   MOVEA     LIN           L1
368000070309     C*
368100070309      *  Se richiamata da EDP
368200070308     c                   CLEAR                   se_EDP            1
368300070308     c                   if        %subst(Knmus:1:3) = 'EDP'
368400070308     c                   MOVE      'S'           se_EDP
368500070307     c                   end
368600070307      *
368700070227      * Reperisco dati job
368800070227     c                   exsr      DatiJob
368900070227
369000030113     C* Variabili per gestione videate
369100070313     C                   MOVE      *BLANK        Sav_$GEST
369200070313     C                   MOVE      *BLANK        $GEST             2
369300030113     C                   MOVE      *BLANK        $FINE             1
369400070312     C                   MOVE      *off          $INZD1            1
369500070312     C                   MOVE      *off          $INZD2            1
369600070312     C                   MOVE      *off          $INZS1            1
369700070312     C                   MOVE      *off          $INZS2            1
369800070312     C                   MOVE      *off          $INZS3            1
369900070312     C                   clear                   $visD1            1
370000070312     C                   clear                   $visD2            1
370100070312     C                   clear                   $visS1            1
370200070312     C                   clear                   $visS2            1
370300070312     C                   clear                   $visS3            1
370400030113     C                   MOVE      *BLANK        $EFILE            1
370500030113     C                   MOVE      *BLANK        $ESCI             1
370600030113     C                   MOVE      *BLANK        $RCDOK            1
370700070314     C                   MOVE      *on           $PcurS2           1
370800070321     C                   MOVE      *on           $PcurS1           1
370900070320     C*
371000070320     C                   Z-ADD     0             CURR              2 0
371100070320     C                   Z-ADD     0             CURC              2 0
371200030113     C* Indici
371300030113     C                   Z-ADD     0             X                 3 0
371400030113     C                   Z-ADD     0             Y                 3 0
371500070227     C***
371600070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
371700070227     C***
371800070227     C                   TIME                    W0140            14 0
371900070227     C                   MOVE      W0140         UDATE8            8 0
372000070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
372100070227     C*
372200070227     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
372300070227     C                   MOVEL     *BLANK        G02ERR
372400070227     C                   CALL      'XSRDA8'
372500070227     C                   PARM                    WLBDAT
372600070301     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
372700940127     C*
372800940117     C                   ENDSR
372900070227      *---------------------------------------------------------------*
373000070321      *
  Reperimento Dati del job (Utente/Operativi)     ?
373100070227      *---------------------------------------------------------------*
373200070228     c     DatiJOB       BEGSR
373300070227      *
373400070227     c     *dtaara       define    §azute        azuteds
373500070227     c     *dtaara       define    §datiute      ddatiute
373600070227      *
373700070227     c                   in(E)     *dtaara
373800070312     c                   IF        %ERROR or X_RSUT = *blanks
373900070227     c                   clear                   Tibs34Ds
374000070227     c                   call      'TIBS34R'
374100070227     c                   parm                    Tibs34Ds
374200070227     c                   in        *dtaara
374300070227     c                   ENDIF
374400070227      *
374500070227     c                   ENDSR
374600070312     C*-----------------------------------------------------***********
374700070321      *
  Imposta i campi di totale   ?
374800070312     C*-----------------------------------------------------***********
374900070312     c     totalizzo     begsr
375000070312      *--
375100070321     c                   clear                   w_imca           13 3
375200070321     c                   clear                   w_imas           13 3
375300070321      *--
375400070312     c     Karb78        setll     fnarb78l
375500070312     c                   do        *hival
375600070321     c     karb78        reade     fnarb78l
375700070321     c                   if        %eof(fnarb78l)
375800070321     c                   leave
375900070321     c                   endif
376000070321
376100070312      * totalizzo gli assegnati
376200070312     c                   if        arbica = *blank or arbica = 'R'
376300070312     c     kimpo         chain     fiar601l                           21
376400070321     c  n21              add       ar6ift        w_imas
376500070312     c                   endif
376600070312      * totalizzo i contrassegni
376700070312     c                   if        arbicc = *blank or arbicc = 'R'
376800070312     c     kimpo         chain     fiar901l                           21
376900070312     c                   if        not *in21
377000070312     c                   if        ar9vca = 'EUR'
377100070321     c                   add       ar9cas        w_imca
377200070312     c                   else
377300070312      *se la divisa non è EUR traduco con yeurco l'importo
377400070312     c                   CLEAR                   YEURDS
377500070312     c                   MOVEL     ar9vca        YECDVI
377600070312     c                   Z-ADD     ar9cas        YECIMI
377700070312     c                   MOVEL     'EUR'         YECDVO
377800070312     c                   MOVEL     'H'           YECTAR
377900070312     c                   CALL      'YEURCO'
378000070312     c                   PARM                    YEURDS
378100070321     c                   add       YECIMO        w_imca
378200070312     c                   endif
378300070312     c                   endif
378400070312     c                   endif
378500070312     c                   enddo
378600070312      *
378700070312     c                   endsr
378800070309     C/EJECT
378900070309     C*-----------------------------------------------------***********
379000070309**  PGM RICHIAMANTI
379100070309FIDG31R
379200070309**  TES
379300070830     INTERROGAZIONE DISTINTE
379400070606 **    ASSEGNAZIONE MANUALE    **
379500070606 **  Interrogazione Distinte   **
379600070830 **                            **
379700070830 **    Dettaglio   Consegne    **
379800070830 **    Dettaglio    Ritiri     **
379900070830 **     Consegne e  Ritiri     **
380000070309**
380100070309Filiale in gestione errata o mancante
380200070312Punto Operativo obbligatorio
380300070309Importi visualizzabili soltanto richiedendo Tipo fogli=2 non chiusi
