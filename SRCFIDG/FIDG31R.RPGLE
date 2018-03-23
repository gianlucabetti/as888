000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070309      * Fidg31R *----------------------------------------------------*
000300070309      *         - Interrogazione/ricerca DISTINTE DI CONSEGNA
000400070312      *         - sempre CATEGORIA foglio 4
000500070227      *--------------------------------------------------------------*
000600080219      *  15           se attivato o test del PDA
000700080703      *  16           Esclude il DROP sui 2 SFL
000800070314      *
000900940307      *  21           GENERICO OPERAZIONI I/O
001000940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001100070301      *  23           GENERICO Operazioni varie
001200940224      *  30           SFLDSP
001300940224      * N31           SFLCLR
001400940224      *  31           SFLDSPCTL
001500940224      *  32           SFLNXTCHG
001600940224      *  33           SFLEND
001700940224      *  39           OF PRTF
001800070302      *  40 <---> 49  DSPATR SU video
001900940608      *  Specificare l'uso dei singoli indicatori
002000070227      *  50           Emissione della window totale/parziale
002100070227      *  51 <---> 90  ERRORI SU VIDEO
002200070313      *  Specificare l'uso dei singoli indicatori
002300940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002400940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002500940224      *  99           INDIC. GENERALE DI ERRORE
002600940128     F*----------------------------------------------------*
002700070604     FFidg31D   CF   E             WORKSTN
002800070309     F                                     SFILE(DG31SF2:S2NRR)
002900070321     F                                     SFILE(DG31SF1:S1NRR)
003000070309     F                                     INFDS(DSFMT)
003100070309     FTABEL00F  IF   E           K DISK
003200080702     FTnTBE01L  IF   E           K DISK
003300070309     FFnBRV01L  IF   E           K DISK
003400070309     FAZORG01L  IF   E           K DISK
003500070403     FFiqdt01L  IF   E           K DISK
003600080613     FFidsf01L  IF   E           K DISK
003700070312     FFidst31L  IF   E           K DISK
003800070910     FFidst51L  IF   E           K DISK
003900070406     FFidst09L  IF   E           K DISK
004000070309     FFidst03L  IF   E           K DISK
004100070309     F                                     RENAME(Fidst000:Fidst003)
004200070309     fFidst08l  if   e           k Disk    Rename(Fidst000:Fidst008)
004300070309     FFNARB78L  IF   E           K DISK
004400070309     FFNORM06L  IF   E           K DISK
004500070309     FFiapd01l  IF   E           K DISK
004600070309     FFiar601L  IF   E           K DISK
004700070309     FFiAR901L  IF   E           K DISK
004800940128     D*----------------------------------------------------*
004900070309     D PGM             S             10    DIM(1) CTDATA PERRCD(1)              PGM RICHIAMANTI
005000070830     D TES             S             34    DIM(7) CTDATA PERRCD(1)              TESTATE
005100160503     D MSG             S             78    DIM(5) CTDATA PERRCD(1)              MSG DI ERRORE
005200070309     D*----------------------------------------------------*
005300070404     d $FV6            s              6  0 dim(600)                             F.V. da FIMS03R
005400070309      *
005500070309     D L1              S              3  0 DIM(30)                              FIL GESTITE
005600070309     D*----------------------------------------------------*
005700070620     d sav_in03        s              1
005800070620     d sav_in04        s              1
005900070528     d sav_in05        s              1
006000070404     d sav_in08        s              1
006100160506     d sav_in38        s              1
006200160504     d sav_in10        s              1
006300070313     d sav_in11        s              1
006400080219     d sav_in15        s              1
006500080701     d sav_in18        s              1
006600070313     d sav_in19        s              1
006700070313     d sav_$Gest       s                   like($gest)
006800070321     d sav_c2aut       s                   like(c2daut)
006900070313     d sav_c2nfv       s                   like(c2cnfv)
007000070321     d sav_c1aut       s                   like(c1daut)
007100070321     d sav_c1nfv       s                   like(c1cnfv)
007200070312     d savfgs          s                   like(d1cfgs)
007300070312     d savcgi          s                   like(d1ccgi)
007400070309     d fgspkm          s                   like(§ogpkm)
007500160503     d fgsturno        s                   like(§ogturno)
007600070309     d kfnvv           s                   like(arbndc)
007700070309     d Wifp            s                   Like(Arbifp)
007800070309     d DATINW          s                   Like(dstDFV)
007900070309     d DATFIW          s                   Like(dstDFV)
008000070309     d WNPG            s                   Like(dstNPG)
008100070312     d Wdst            s                   Like(dstNFV)
008200070312     d Wfgs            s                   Like(dstFGS)
008300070309     d W0CNFV          s                   Like(BRVNFV)
008400070309     d wbar            s              1    inz('0')
008500070309     d wppt            s              1    inz('0')
008600070607     d Prima_volta     s              1    inz('S')
008700080606     d SAVfdgNFV       s                   like(fdgNFV)
008800080611      *
008900080613      * caricate dal FIDSF
009000080613     D  Fasi_DSF       s              3    DIM(30)                              30 Fasi
009100080613      *
009200080611     d d1Fasids        DS
009300080623     D  d1Fasi                        3    DIM(10)                              10 Fasi
009400080616     D d1fa01                  1      3
009500080616     D d1fa02                  4      6
009600080616     D d1fa03                  7      9
009700080616     D d1fa04                 10     12
009800080616     D d1fa05                 13     15
009900080616     D d1fa06                 16     18
010000080616     D d1fa07                 19     21
010100080616     D d1fa08                 22     24
010200080616     D d1fa09                 25     27
010300080616     D d1fa10                 28     30
010400070314      *
010500070315     d c16cgids        DS
010600070315     D   cgi                         10    DIM(16)                              14 giri max
010700070314     D d2cgi1                  1     10
010800070314     D d2cgi2                 11     20
010900070314     D d2cgi3                 21     30
011000070314     D d2cgi4                 31     40
011100070314     D d2cgi5                 41     50
011200070314     D d2cgi6                 51     60
011300070314     D d2cgi7                 61     70
011400070314     D d2cgi8                 71     80
011500070314     D d2cgi9                 81     90
011600070314     D d2cgi0                 91    100
011700070314     D d2cgiA                101    110
011800070314     D d2cgiB                111    120
011900070314     D d2cgiC                121    130
012000070314     D d2cgiD                131    140
012100070315     D d2cgiE                141    150
012200070315     D d2cgiF                151    160
012300070314      *
012400070315     d d16dgids        DS
012500070315     D   dgi                         25    DIM(16)                              10 giri
012600070315     D d2dgi1                  1     25
012700070315     D d2dgi2                 26     50
012800070315     D d2dgi3                 51     75
012900070315     D d2dgi4                 76    100
013000070315     D d2dgi5                101    125
013100070315     D d2dgi6                126    150
013200070315     D d2dgi7                151    175
013300070315     D d2dgi8                176    200
013400070315     D d2dgi9                201    225
013500070315     D d2dgi0                226    250
013600070315     D d2dgiA                251    275
013700070315     D d2dgiB                276    300
013800070315     D d2dgiC                301    325
013900070315     D d2dgiD                326    350
014000070315     D d2dgiE                351    375
014100070315     D d2dgiF                376    400
014200070315      *------------
014300070309     d Status         sds           333
014400070309     d  SDSparm          *parms
014500070309     D  DSPGM            *PROC
014600070309      *------------
014700070309     D TBLKEY          DS
014800070309     D  ALF1                   1      1
014900070309      *------------
015000070309     D* DS STANDARD FOGLI VARI
015100070313     D FIDG31DS      E DS
015200080616     D   fdgFASI              61     90    DIM(10)                              10 FAsi
015300080616     D   fdgFASIds            61     90
015400080820     D*------------
015500080820     D* DS Dettaglio Distinta
015600070309     D*------------
015700080820     D FIDG311DS     E DS
015800080820     D*------------
015900080616     D* DS FAsi passate da PGM chiamanti
016000070427     D FIDG35DS      E DS
016100080624     D*------------
016200080624     D* DS Visualizzazione FASI
016300080624     D FIDG39DS      E DS
016400070427     D*------------
016500070313     D* DS PER FNLV24R - ricerca autotrasp.
016600070313     D fnlv24ds      E DS
016700070313      *--------------------------
016800070309     D DS7N          E DS
016900070309     D  §7NDE2                 1     10
017000070309     D*------------
017100070309     D* DS PER TRUL06R - CARICAMENTO £X
017200070309     D DSUL06        E DS                  EXTNAME(TRUL06DS)
017300070309     D  LIN                    1     90  0
017400070309     D                                     DIM(30)                              FIL. COMODO
017500070309     D OG150         E DS                  INZ
017600070309     D OG146         E DS                  INZ
017700070830     D OG147         E DS                  INZ
017800080207     D OG148         E DS                  INZ
017900080207     D DDSTFLR       E DS                  INZ
018000070320     D DgrgFLO       E DS                  INZ
018100070309     D CNCR80        E DS
018200070309     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018300070309     d YEURDS        E DS                  EXTNAME(YEURCODS)
018400070309     D***
018500070309     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
018600070309     D***
018700070309     D WLBDAT          DS
018800070309     D  G02DAT                 1      8  0
018900070309     D  G02INV                 9     16  0
019000070309     D  G02ERR                17     17
019100070309     D  G02TGI                18     22  0
019200070309      *
019300070309     D PARA04          DS
019400070309     D  P04NFV                 1      5  0
019500070309     D  P04LNP                 6      8  0
019600070309     D  P04LNA                 9     11  0
019700070309     D  P04NRS                12     13  0
019800070309     D  P04NCD                14     20  0
019900070309     D  P04NCA                21     27  0
020000070309     D  P04NPS                28     29  0
020100070309     D  P04NPG                30     30  0
020200070309     D  P04FLG                31     31
020300070309     D  P04PGM                32     38
020400070309     D  P04FGS                39     41  0
020500101203     d* bolla
020600101203     d  p04bolp               42     44  0
020700101203     d  p04bors               45     46  0
020800101203     d  p04bosp               47     53  0
020900101203     d  p04boan               54     57  0
021000101203     D  PA4ERR                58     58
021100070309      *---------------------------------------------------------------*
021200940211     D* Passaggio Parametri
021300940211     D KPJBA         E DS
021400070227     D KPJBUs          s                   like(KPJBU)
021500940211     D*-------------
021600940211     D DSFMT           DS           512
021700940506     D  $TASTO               369    369
021800940211     D  NRG                  370    370
021900940211     D  NCL                  371    371
022000940211     D  SFLNRR               378    379B 0
022100070320     D*-------------
022200070320     D* posizione cursore
022300070320     D CURSOR          DS
022400070320     D  RIRI                   1      2B 0 INZ
022500070320     D  R$$                    2      2
022600070320     D  COCO                   3      4B 0 INZ
022700070320     D  C$$                    4      4
022800030113     D*-------------
022900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
023000070312$003 D S2NRR           S                   Like(C2rcd)
023100070228$003 D S2pag           S                   Like(C2rcd)
023200070312$003 D NRR_s2          S                   Like(S2nrr)
023300070321$003 D S1NRR           S                   Like(C1rcd)
023400070321$003 D S1pag           S                   Like(C1rcd)
023500070321$003 D NRR_s1          S                   Like(S1nrr)
023600070312$003 D WSfl            S                   Like(C2nrr)
023700070312$003 D Wmax            S                   Like(C2rcd)
023800070312$003 D Wpag            S                   Like(C2rcd)
023900070312$003 D Wpagine         S                   Like(C2rcd)
024000071026$003 D Su_Quale_rec    S                   Like(S2nrr)
024100071026$003 D Che_Nrec_sfl    S                   Like(S2nrr)
024200071029$003 D NRec_riposiz    S                   Like(S2nrr)
024300080611     d PDAconsegne     s              1    inz(' ')
024400080623     d PDAritiri       s              1    inz(' ')
024500080623     d Fase_STP        s              1
024600080613     d Trovato_DSF     s              1
024700080703     d Apri_Drop       s              1
024800080725     d Filiale_con_PDA...
024900080725     d                 s              1
025000080613     d Dist_consegne_PDA...
025100080613     d                 s              1
025200080623     d Dist_ritiri_PDA...
025300080623     d                 s              1
025400080701     d d2Fasi          s               N
025500080701     d Indicatore      s              2s 0
025600070309      *---------------------------------------------------------------*
025700070228      * selezione (1) - con Giro
025800070228     D Con_Giro        S              1    INZ(' ')
025900070301     d wNFV            s                   like(dstNFV)
026000070312     d wCGI            s                   like(dst3CGI)
026100070228      *--------------------------
026200070305      * Per impostare in testata la Distinta
026300070305     D Distintalfa     DS
026400070305     D  num_ndc                1      7
026500070305     D  ndc_a                  1      7    DIM(7)
026600070305      *--------------------------
026700070227     d* Parametri x Controllo profilo utenti
026800070227     d TIBS34ds      e ds
026900070227     d AZUTEds       e ds                  extname(AZUTE00F)
027000070312      *--------------------------
027100070227     d* Ds per dati organigramma
027200070312     d DDatiUte      e ds                  prefix(x_)
027300070227      *--------------------------
027400070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
027500070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
027600070227      *--------------------------
027700070227     D* DS PER - ricerca giri
027800070227     D fidg09ds      E DS
027900070320      *--------------------------
028000070320     D* DS PER - Visualizzazione dettaglio distinte
028100070320     D fidg02ds      E DS
028200070309      *---------------------------------------------------------------*
028300940211     D* COSTANTI
028400070309      *---------------------------------------------------------------*
028500940506     D* Tasti di funzione
028600940506     D F01             C                   CONST(X'31')
028700940506     D F02             C                   CONST(X'32')
028800940506     D F03             C                   CONST(X'33')
028900940506     D F04             C                   CONST(X'34')
029000940506     D F05             C                   CONST(X'35')
029100940506     D F06             C                   CONST(X'36')
029200940506     D F07             C                   CONST(X'37')
029300940506     D F08             C                   CONST(X'38')
029400940506     D F09             C                   CONST(X'39')
029500940506     D F10             C                   CONST(X'3A')
029600940506     D F11             C                   CONST(X'3B')
029700940506     D F12             C                   CONST(X'3C')
029800940506     D F13             C                   CONST(X'B1')
029900940506     D F14             C                   CONST(X'B2')
030000940506     D F15             C                   CONST(X'B3')
030100940506     D F16             C                   CONST(X'B4')
030200940506     D F17             C                   CONST(X'B5')
030300940506     D F18             C                   CONST(X'B6')
030400940506     D F19             C                   CONST(X'B7')
030500940506     D F20             C                   CONST(X'B8')
030600940506     D F21             C                   CONST(X'B9')
030700940506     D F22             C                   CONST(X'BA')
030800940506     D F23             C                   CONST(X'BB')
030900940506     D F24             C                   CONST(X'BC')
031000940506     D ENTER           C                   CONST(X'F1')
031100940506     D ROLDWN          C                   CONST(X'F4')
031200940506     D ROLLUP          C                   CONST(X'F5')
031300070301     d digits          c                   '0123456789'
031400070309     D CANNUL          C                   CONST('  Annullato  ')
031500070309     D CBLOCC          C                   CONST('  Bloccato   ')
031600070315     D Imp_C_A         C                   CONST('imp.C/A')
031700070315     D Imp_ASS         C                   CONST('imp.Ass')
031800070313      **********************************************************************
031900070313      * Constants
032000070313      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
032100070313      **********************************************************************
032200160505     D MaxKey          C                    4
032300070313$xxx D Autista         C                   '1'
032400070313$xxx D Numero          C                   '2'
032500160504$xxx D Turno           C                   '3'
032600070313     D Ascendente      C                   1
032700070313     D Discendente     C                   2
032800070313     D Carattere       C                   6
032900070313     D Put             C                   1
033000070313     D EndPut          C                   2
033100070313     D Get             C                   3
033200070403     D Numerico        C                   8
033300070313      **********************************************************************
033400070313      * Campi utili:
033500070313      *     RRN1       - Numero relativo di record del Subfile
033600070313      *     SizeList   - Dimensione della lista
033700070313      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
033800070313      **********************************************************************
033900070313     D Rrn1            S              5I 0
034000070313     D NotUsed         S             16A
034100070313     D ReturnSize      S              9B 0
034200070313     D SizeList        S              9B 0
034300070313     D RrnLast         S              5I 0
034400070313     D WrkSort         S              1
034500070313      **********************************************************************
034600070313      * Data Structures
034700070321      *     SflRcd2    - L'intero record del SFL da passare x l'ordinamento
034800070313      *     QLGSCB     - The sort request block for the QLGSORT API
034900070313      *     QLGSCB00   - The sort request block for the QLGSRTIO API
035000070313      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
035100070313      *     QUSEC      - Error structure for the QLGSORT API
035200070313      **********************************************************************
035300070321     D SflRcd2         DS
035400070313     D  S2DAUT
035500070313     D  S2CNFV
035600070313     D  S2CDFV
035700070417     D  H2CDFV
035800070510     D  H2PRGG
035900160505     D  S2TURNO
036000160505     D  S2CTUR
036100070404     D  S2SCE
036200070313     D  S2CPEM
036300070313     D  S2CPEA
036400070313     D  S2CVOM
036500070313     D  S2CVOA
036600070313     D  S2IMCA
036700070313     D  S2IMAS
036800070313     D  S2CTPM
036900070313     D  S2DTPD
037000070313     D  S2CTPD
037100070419     D  S2DPDA
037200070419     D  S2CPDA
037300080207     D  S2CTSTPDA
037400070313     D  S2CFCF
037500070321     D  S2CANN
037600070419     D  S2NBLA
037700070419     D  S2NFTL
037800070419     D  S2STPA
037900070625     D  S2TUSC
038000070820     D  S2STP
038100070904     D  S2DDES1
038200070904     D  S2DESN1
038300070904     D  S2NBLA1
038400070904     D  S2NFTL1
038500070904     D  S2STPA1
038600070904     D  S2CPEM1
038700070904     D  S2CPEA1
038800070904     D  S2CVOM1
038900070904     D  S2CVOA1
039000080222     D  H2in14
039100170508     D  H2in15
039200170508     D  H2in18
039300170508     D  H2in19
039400170508     D  H2in20
039500071029     D  H2recpos
039600070322     D  Selected2                     1A
039700070313
039800070321     D SflRcd1         DS
039900070321     D  S1DAUT
040000070321     D  S1CNFV
040100070321     D  S1CDFV
040200070417     D  H1CDFV
040300070510     D  H1PRGG
040400160505     D  S1CTUR
040500070404     D  S1SCE
040600070321     D  S1CPEM
040700070321     D  S1CPEA
040800070321     D  S1CVOM
040900070321     D  S1CVOA
041000070321     D  S1IMCA
041100070321     D  S1IMAS
041200070321     D  S1CTPM
041300070321     D  S1CTPD
041400070321     D  S1CPDA
041500080207     D  S1CTSTPDA
041600070321     D  S1CFCF
041700070321     D  S1CANN
041800070625     D  S1TUSC
041900070820     D  S1STP
042000080222     D  H1in14
042100170508     D  H1in15
042200170508     D  H1in18
042300170508     D  H1in19
042400170508     D  H1in20
042500070322     D  Selected1                     1A
042600080207      * ?------------------------------------------------------------------ */
042700080207      * ?set caratteri Maiuscoli/Minuscoli
042800080207     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
042900080207     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
043000080207      * ?------------------------------------------------------------------ */
043100070313      /COPY QSYSINC/QRPGLESRC,QLGSORT
043200070313     D QLGKL                         16    DIM(MaxKey)
043300070313     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
043400070313     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
043500070313     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
043600070313     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
043700070313
043800070313      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
043900070313      /COPY QSYSINC/QRPGLESRC,QUSEC
044000940127     C*----------------------------------------------------*
044100070321      *?     MAIN LINE PROGRAM     ?
044200940127     C*----------------------------------------------------*
044300940223     C* inizializzazione variabili
044400940223     C                   EXSR      INZVAR
044500940223     C*
044600940223     C     $FINE         DOWEQ     *OFF
044700070227     C     $GEST         CASEQ     'D1'          GESD1
044800070321     C     $GEST         CASEQ     'S1'          GESS1
044900070227     C     $GEST         CASEQ     'S2'          GESS2
045000070330     C     $GEST         CASEQ     'D2'          GESD2
045100940117     C                   END
045200070309     C                   ENDdo
045300940325     C* fine programma
045400070313     C                   MOVEL(p)  FIDG31DS      KPJBU
045500071029     C                   SETON                                            LR
045600030113     C************************************************************
045700070321      *?  INIZIALIZZAZIONE VARIABILI   ?
045800030113     C************************************************************
045900030113     C     INZVAR        BEGSR
046000030113     C*
046100030113     C* Variabili per gestione videate
046200070312     C                   move      *off          $INZS1
046300070312     C                   move      *off          $INZS2
046400070312     C                   move      *off          $INZS3
046500070312     C                   move      *off          $INZD2
046600070312     C                   move      'S'           $VISS1
046700070312     C                   move      'S'           $VISS2
046800070312     C                   move      'S'           $VISS3
046900070312     C                   move      'S'           $VISD1
047000070330     C                   move      'S'           $VISD2
047100070309      *
047200070309     C                   MOVE      *OFF          $FINE
047300030113     C                   MOVE      *OFF          $EFILE
047400030113     C                   MOVE      *OFF          $ESCI
047500030113     C                   MOVE      *OFF          $RCDOK
047600070314     C                   move      *on           $PcurS2
047700070321     C                   move      *on           $PcurS1
047800070227     C* Emissione primo video
047900070227     C                   MOVE      'D1'          $GEST
048000070227     C                   MOVE      *ON           $INZD1
048100070312      *
048200070312      * ? Legge sempre i Fogli (4)
048300070312     C                   MOVEL     '4'           wNPG
048400070312     C* Variabili appoggio
048500030114     C                   Z-ADD     1             WPAG
048600070312     c                   clear                   pgm_da_call
048700160601     c                   move      *off          sav_in11
048800160505     c                   move      *off          sav_in10
048900030113     c*
049000030113     C                   ENDSR
049100070227     C************************************************************
049200070321      *?  Emissione primo video di SCELTA   ?
049300070227     C************************************************************
049400070227     C     GESD1         BEGSR
049500070227     C*
049600070227     C* inizializzazione videata
049700070227     C     $INZD1        IFEQ      *ON
049800070227     C                   EXSR      INZD1
049900070227     C                   MOVE      *OFF          $INZD1
050000070227     C                   ENDIF
050100070227     C*
050200070228     c     Video_1       tag
050300070302     C*  Emissione Video
050400070302     c                   setoff                                       99
050500070309     c                   if        $VISD1 = 'S'
050600070228      * ?            *-----------------------*
050700070309     C                   EXFMT     DG31D01
050800070228      * ?            *-----------------------*
050900070313     C                   movel     'D1'          Sav_$GEST
051000070309     c                   end
051100070227     C* Selezioni
0512000702271    C                   SELECT
051300070227     C* F3=Fine
051400070227     C                   WHEN      $TASTO   =   F03
051500080606     c                   move      '03'          fdgtfu
051600070227     C                   EXSR      F03_exit
051700070312     c                   goto      end_d01
051800070227      *
0519000702271O   C                   OTHER
052000070227     C* CONTROLLO DATI
052100070227     C                   EXSR      CTRD1
0522000702271-   C                   ENDSL
052300070227     C*
052400070228     C* Per errori o decodifiche
052500070228     c   99              goto      Video_1
052600070228      *
052700070312      * ? * Emissione Lista o Dettaglio
052800070312     c                   exsr      DSP_seguente
052900070228      *
053000070312     c     end_d01       tag
053100070227     C                   ENDSR
053200070306     C/EJECT
053300070306     C************************************************************
053400070321      *?  Quale Visualizzazione deve impostare        ?
053500070306     C************************************************************
053600070312     C     DSP_seguente  BEGSR
053700070306     C*
053800070312     C*  Se non immesso il Nr.Foglio
053900070312if  0c                   if        d1CNFV = 0
054000070312     C*
054100070309     C*  Quale SFL deve mostrare in base numero Foglio
054200070309     C*  Emissione SFL di elenco solo se non richiamato
054300070419if  1c                   if        FDGtfv  = '3'
054400070419     c                   eval      $GEST ='S1'
054500070419     C                   MOVE      *ON           $INZS1
054600070419     c                   eval      $VISS1 = 'N'
054700070419     c                   else
054800070309      *
054900070321if  1c                   if        d1tipv = 'P'
055000070309     c                   eval      $GEST ='S2'
055100070309     C                   MOVE      *ON           $INZS2
055200070326     c                   seton                                        09
055300070321     c                   else
055400070321     c                   eval      $GEST ='S1'
055500070321     C                   MOVE      *ON           $INZS1
055600070326     c                   setoff                                       09
055700070321     c                   end
055800070309     C*
055900070309e   1c                   endif
056000070309      *
056100070312x   0c                   else
056200070312     C*
056300070312     C*  Dettaglio della Distinta
056400070312     c                   eval      $GEST ='D2'
056500070312     C                   MOVE      *ON           $INZD2
056600070312     C                   exsr      GESD2
056700070312     C*
056800070312e   0c                   endif
056900070312      *
057000070306     C                   ENDSR
057100070227     C/EJECT
057200070227     C************************************************************
057300070321      *?  INIZIALIZZAZIONE Primo video      ?
057400070227     C************************************************************
057500070227     C     INZD1         BEGSR
057600070227     C*
057700070227     C* Pulizia campi e indicatori
057800070308     c                   exsr      INZ_indERR
057900070309     C*
058000070309     C* Pulizia formato record   campi video
058100070309     c                   clear                   DG31D01
058200070309     C* campi titolo
058300070312     C                   MOVEL     TES(1)        d1CTES
058400080715     c                   if        fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR'
058500070606     C                   MOVEL(p)  TES(2)        d1CTES
058600070606     c                   end
058700070312     C                   MOVEL     dsPGM         d1CPGM
058800070312     C                   MOVEL     RAGUT         D1CDSA
058900070312     C                   MOVEL     knsif         d1nsif
059000070312     C                   MOVEL     knmus         d1nmus
059100070321      *
059200070822     C                   Z-ADD     *date         d1CDIN
059300070822     C                   Z-ADD     31129999      d1CDFI
059400070321     c                   move      'N'           d1IMPO
059500070822     c                   move      ' '           d1STP
059600070309     C* Solo Fogli (4)
059700070312     C                   MOVEL     wnpg          d1cnpg
059800070330     c                   move      'T'           d1TIPV
059900070830     c                   move      '2'           d1ctfv
060000080610     c                   move      'I'           d1ieFA
060100160503     C                   MOVEL     '1'           d1Ctur1
060200160503     C                   MOVEL     '2'           d1Ctur2
060300160505     C                   MOVEL     ' '           d1Ctpm
060400070309     C***
060500070309     C* ??????????????????????????
060600070309     C* Il p.o.deve essere impostato in base al profilo
060700070312     C                   MOVEL     x_dutpou      d1CFGS                         *FIL.GESTIONE
060800070309     C***
060900070309     C* VEDO SE SONO SIMFEL O REMOTO
061000070309     c     simtpp        ifeq      '2'
061100070309     c     simtpp        oreq      *blanks
061200070312     c                   movel     simpou        d1cfgs
061300070309     C                   ELSE
061400070312     C                   MOVEL     SIMFEL        d1CFGS
061500070309     C                   END
061600070309      *
061700070309      * ? memorizzo che è richiamato anche con 01 = *on -> pgm RICHIAMATO
061800070313     C     FDGRIC        IFNE      ' '
061900070312     c                   move      'S'           pgm_da_call       1
062000070404      *
062100070404      *  Se richiamato da Interrogazione
062200070404     c                   setoFF                                       17
062300070404     C     FDGRIC        IFeq      'I'
062400070404     c                   setoN                                        17
062500070404     c                   enD
062600070322      *
062700070322      * al momento deve ricevere a 0 questo parametro
062800070406     c********           if        FDGNPG <> 0
062900070406     C********           MOVEL     FDGNPG        d1cnpg
063000070406     c********           end
063100070322      *
063200070322     c                   if        FDGTPV <> *blank
063300070322     C                   MOVEL     FDGTPV        d1tipv
063400070322     c                   end
063500070309      *
063600070309      * ? Condiziono l'emissione del campo "1=Selezione" nei sfl
063700070404     c                   if        $FV6(1) = 999999 and $FV6(600) = 999999
063800070404     c                   setoFF                                       19
063900070404     c                   else
064000070404     c                   setoN                                        19
064100070404     c                   end
064200170508     c                   move      *in19         sav_in19
064300070419      *
064400070419      * Ripulisco eventuale flag di ritorno per FIMS03R
064500070419      * (se FNLR02R richiamato per sapere se ci sono distinte aperte
064600070419      *  non inserite nel piano a video di FIMS03R)
064700070419     c                   if        FDGtfv = '3'
064800070419     c                   clear                   FDGfs1
064900070419     c                   endif
065000070309      *
065100070321     C* IMPOSTO I CAMPI da A/M/G
065200070313     C                   MOVEL     FDGGDA        DATINW
065300070313     C                   MOVE      DATINW        G02INV
065400070313     C                   MOVEL     '3'           G02ERR
065500070313     C                   CALL      'XSRDA8'
065600070313     C                   PARM                    WLBDAT
065700070313     C                   Z-ADD     G02DAT        d1CDIN
065800070321      *
065900070313     C                   MOVEL     FDGGAL        DATFIW
066000070313     C                   MOVE      DATFIW        G02INV
066100070313     C                   MOVEL     '3'           G02ERR
066200070313     C                   CALL      'XSRDA8'
066300070313     C                   PARM                    WLBDAT
066400070313     C                   Z-ADD     G02DAT        d1CDFI
066500070321      *
066600070313     C                   MOVEL     FDGTFV        d1CTFV
066700070313     C                   MOVEL     FDGCGI        d1Ccgi
066800070313     C                   MOVEL     FDGFGS        d1CFGS
066900070313     C                   MOVEL     FDGNFV        d1CNFV
067000070625     C                   MOVEL     FDGtdis       d1Ctpm
067100070625     C                   MOVEL     FDGtusc       d1Tusc
067200080611     C*
067300070822     C                   MOVEL     FDGstp        d1stp
067400160512     C*
067500160512     C* se chiamato come UNICO deve far vedere tutto dei turni
067600160512     c                   if        FDGtur1 ='U' or FDGtur2 ='U'
067700160512     C                   MOVEL     '1'           FDGtur1
067800160512     C                   MOVEL     '2'           FDGtur2
067900160512     c                   end
068000160503     C*
068100160503     c                   if        FDGtdis <>'M'
068200160504     C                   MOVEL     fdgtur1       d1Ctur1
068300160504     C                   MOVEL     fdgtur2       d1Ctur2
068400160504     c                   end
068500070309     C*
068600160504     c                   if        FDGtdis <>'M'  and
068700160504     c                             fdgtur1 = ' '  and fdgtur2 = ' '
068800160504     C                   MOVEL     '1'           d1Ctur1
068900160504     C                   MOVEL     '2'           d1Ctur2
069000160504     c                   end
069100160504     C*
069200070309     C* VEDO SE VISUALIZZARE LA PRIMA VIDEATA (SCELTA PARAMETRI) O NO
069300070313     C     FDGRIC        IFNE      '§'                                          '§' SI 1° VIDEO
069400070309      * Non visualizzare il primo video
069500070309     C                   move      'N'           $VISD1
069600070309     C                   ENDIF
069700070309      *
069800070309     C                   END
069900070309     C*
070000070309     C* Aggancia Organigramma x picking su messaggerie
070100070309     c                   clear                   og150
070200070830     c                   clear                   og147
070300070927     c                   clear                   Abilita_ORM       1
070400160503     c                   setoff                                       083802
070500160506     c                   eval      sav_in38 = *in38
070600070312     C     d1CFGS        chain     AZORG01l
070700070309    1C                   IF        %found(azorg01l)
070800070312     C                   MOVEL     ORGDES        d1DFGS
070900070309     c                   movel     orgdf0        og150
071000070830     c                   movel     orgde7        og147
071100080207     c                   movel     orgde8        og148
071200070927      * Abilitazione a trattare gli ORM
071300070927     c                   movel     §ogcGIO       Abilita_ORM
071400070830      *
071500070830     c* p.o. con ORM
071600070830     c                   if        §ogDDAO <> *blank
071700070830     c                   move      §ogDDAO       data_OrM          8 0
071800070830     c                   seton                                        02
071900070830     C                   MOVE      *BLANK        D1DATI
072000070904     C                   MOVE      Tipo_Prestaz  D1DATI
072100070830     c                   eLSE
072200070830     c                   setoFF                                       02
072300070830     C                   MOVE      'C'           D1DATI
072400070830     c                   end
072500070312      *
072600070309     c* p.o. con piking messaggerie
072700070309     c                   if        §ogpkm='S'
072800070309     c                   seton                                        08
072900070309     c                   endif
073000070309     c                   eval      fgspkm = §ogpkm
073100160503      *
073200160503     c* p.o. con TURNO
073300160503     c                   if        §OGTURNO='S'
073400160503     c                   seton                                        38
073500160506     c                   eval      sav_in38 = *in38
073600160503     c                   endif
073700160503     c                   eval      fgsturno = §OGTURNO
073800070309     c                   end
073900070309      *
074000080219     c* in test o attivo x PDA
074100080219     c                   setoff                                       15
074200080725     c                   eval      Filiale_con_PDA = 'N'
074300080611     c                   clear                   PDAconsegne
074400080623     c                   clear                   PDAritiri
074500080623      *
074600080623      * se attivo PDA x Consegne o Ritiri
074700080219     c                   if        §OGPDAORM <> *blank or
074800080219     c                             §OGPDACON <> *blank
074900080725     c                   eval      Filiale_con_PDA = 'S'
075000080623     c                   seton                                        15
075100080611     c                   eval        PDAconsegne = §OGPDACON
075200080623     c                   eval        PDAritiri   = §OGPDAORM
075300080724      *
075400080724     c                   end
075500080616      *
075600080611      *  se richiesta/o non richiesta Fase "Stampata":
075700080724      *   e se congruente con eventuali altre selezioni di fasi
075800080724      *    in Inclusione o Eslusione altrimenti comunque durante la
075900080724      *    selezione dei records esegue il controllo x le Stampate
076000080724     c                   if          fdgIEfas = *blank or
076100080724     c                               (fdgIEfas ='I' and d1STP = 'S') or
076200080724     c                               (fdgIEfas ='E' and d1STP = 'N')
076300080623     c                   If        d1STP = 'S'
076400080611     c                   move      'I'           d1ieFA
076500080611     c                   eval      d1FA01 ='STP'
076600080623     c                   elseIf    d1STP = 'N'
076700080611     c                   move      'E'           d1ieFA
076800080611     c                   eval      d1FA01 ='STP'
076900080611     c                   endIf
077000080724      *
077100080724     c                   end
077200080623      *
077300080623      *  Attenzione se però sono passate le FASI a livello di
077400080623      *    Inclusione/Esclusione
077500080623      *  queste prevalgono sul flag di Stampato:
077600080724      * SARA' LA ROUTINE DI CONTROLLO, che filtra i records letti,
077700080724      *  a selezionare i records in base ALLE FASI passate e
077800080724      *   alle STAMPATE si/no.
077900080724      *
078000080623     c                   If        fdgFASIds <>*blank
078100080623      * includi/escludi
078200080623     c                   move      fdgIEfas      d1ieFA
078300080623      * Se invece ha passato delle Fasi le imposta
078400080623     c                   clear                   fs
078500080623     c                   Do        10            fas               3 0
078600080623     c                   if        fdgFASI(fas) <> *blank
078700080623     c                   add       1             fs
078800080623     c                   eval      d1fasi(fs) = fdgFASI(fas)
078900080623     c                   end
079000080623     c                   endDo
079100080616     c                   endIf
079200080616      *
079300070309     c* salvo campi di comodo
079400070312     c                   eval      savfgs = d1cfgs
079500070313     c                   eval      sav_in08 = *in08
079600160601     c                   eval      sav_in11 = *off
079700160505     c                   eval      sav_in10 = *off
079800080219     c                   eval      sav_in15 = *in15
079900170508     c                   eval      sav_in19 = *in19
080000070227     C*
080100070227     C                   ENDSR
080200940127     C************************************************************
080300070321      *? Controlli primo video di SCELTA Operazione da eseguire  ?
080400940127     C************************************************************
080500070227     C     CTRD1         BEGSR
080600030113     C*
080700070227     C*  Spegne indicatori di errore
080800070308     c                   exsr      INZ_indERR
080900080715     c                   clear                   d1cmsg
081000070309     C*
081100070309     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
081200160503     c                   setoff                                       083899
081300160506     c                   eval      sav_in38 = *in38
081400070309     C                   MOVE      *BLANK        test_Parallelo    1
081500070830     C                   MOVE      *BLANK        test_Paral_ORM    1
081600080222     C*
081700080222     C* RESETTA l'emissione del SFL di scelta inzializzandolo
081800080222     C*   ordinato x Numero:
081900160601     c                   eval      sav_in11 = *off
082000160505     c                   eval      sav_in10 = *off
082100070309     C*
082200070309      * Categoria Foglio x controllo nr.distinta
082300070312     C                   move      d1cnpg        wNPG
082400070309     C*------------------------
082500070309     C* SE IMMESSA DEVE ESSERE FILIALE VALIDA
082600070309     c                   clear                   fgspkm
082700160503     c                   clear                   fgsturno
082800070312     c                   clear                   d1DFGS
082900070309      *
083000070406     c* Filiale apertura obbligatoria
083100070406     c*    x Categoria 4 è sempre obbligatoria
083200070312     c                   if        d1cfgs=0
083300070312     C                   MOVEL     MSG(2)        d1Cmsg
083400070309     C                   SETON                                        51  99
083500070312     C                   GOTO      end_CT1
083600070309     c                   endif
083700070309      *
083800070309      *  FGS impostata
083900070312     C     d1CFGS        IFNE      0
084000070309     C*------------------------
084100070309      * ?- Controlla la filiale in gestione
084200070309     C                   CLEAR                   DSLV50
084300070309     C                   MOVEL     KNMUS         D50PRU
084400070312     C                   z-add     d1cFGS        D50FGS
084500070309     C                   CALL      'FNLV50R'
084600070309     C                   PARM                    DSLV50
084700070309    1C     D50ERR        IFNE      ' '
084800070312     C                   MOVEL     MSG(1)        d1Cmsg
084900070309     C                   SETON                                        51  99
085000070312     C                   goto      end_CT1
085100070309    1C                   ENDIF
085200070309      *
085300070309      * Esistenza su Organigramma
085400070312     c                   clear                   wfgs
085500070312     C     d1CFGS        chain     AZORG01l
085600070309    1C                   IF        not %found(azorg01l)
085700070312      *
085800070312     C                   MOVEL     MSG(1)        d1Cmsg
085900070309     C                   SETON                                        51  99
086000070312     C                   GOTO      end_CT1
086100070312      *
086200070309     c                   else
086300070312      *
086400070312     c                   z-add     d1CFGS        wfgs
086500070309     c                   movel     orgdf0        og150
086600070309     c                   movel     §ogpkm        fgspkm
086700160503     c                   movel     §ogturno      fgsturno
086800070312     c                   movel     orgdes        d1DFGS
086900070309     C                   MOVEL     ORGde6        og146
087000070830     C                   MOVEL     ORGde7        og147
087100080207     C                   MOVEL     ORGde8        og148
087200080219      *
087300080219      * in test o attivo x PDA
087400080219     c                   setoff                                       15
087500080725     c                   eval      Filiale_con_PDA = 'N'
087600080611     c                   clear                   PDAconsegne
087700080623     c                   clear                   PDAritiri
087800080623      *
087900080219     c                   if        §OGPDAORM <> *blank or
088000080219     c                             §OGPDACON <> *blank
088100080725     c                   eval      Filiale_con_PDA = 'S'
088200080623     c                   seton                                        15
088300080611     c                   eval        PDAconsegne = §OGPDACON
088400080623     c                   eval        PDAritiri   = §OGPDAORM
088500080219     c                   end
088600080623      *
088700080219     c                   eval      sav_in15 = *in15
088800080702      *
088900080702      *
089000080724      *  Controlli sulle fasi  del Video da fare solo se la filiale
089100080724      *   è partita con il PDA
089200080724     c                   if        (PDAconsegne <> ' ' or PDAritiri <> ' ')
089300080724     c                               and d1Fasids <> *blank
089400080702      *
089500080702      * Controlla se richiesta una fase non presente in tabella
089600080702     c                   Do        10            fas               3 0
089700080702     c                   if        d1fasi(fas) <> *blank
089800080702     c     '?'           scan      d1fasi(fas)
089900080702     c                   if        %Found
090000080702     c                   eval      kpjbus = kpjbu
090100080702     c                   movel(p)  '?'           kpjbu
090200080702     c                   call      'TNTB68R'
090300080702     c                   parm                    kpjba
090400080702     c                   movel     kpjbu         d1fasi(fas)
090500080702     c                   eval      kpjbu  = kpjbus
090600080702     c                   end
090700080702      *
090800080702      *  se la fase non è vuota
090900080702     c                   if        d1fasi(fas) <> *blank
091000080702      *
091100080702      *  se la filiale è partita con PDA la scelta "STAMPATE - S/N Tutte"
091200080702      *   è sostituita dalle fasi da scegliere:
091300080702      *    ma se si vogliono vedere solo distinte senza PDA
091400080702      *   x qualsiasi altra fase NON "STP" deve essere dato l'errore.
091500080702     c                   if        d1cPDA ='N' and d1fasi(fas)<>'STP'
091600080702     c                   eval      indicatore = 60 + fas
091700080702     c                   eval      *IN(indicatore) = *on
091800080702     c                   seton                                        99
091900080702     c                   leave
092000080702     c                   end
092100080702      *
092200080702      * controlla se la fase esiste sulla tabella DSF
092300080702     C                   movel(p)  'DSF'         TBEcod
092400080702     C                   movel(p)  d1fasi(fas)   TBEke1
092500080702     c     ktbe1         setll     tntbe01l
092600080702     c                   if        not %equal
092700080702     c                   eval      indicatore = 70 + fas
092800080702     c                   eval      *IN(indicatore) = *on
092900080702     c                   seton                                        99
093000080702     c                   leave
093100080702     c                   end
093200080702     c                   end
093300080702     c                   end
093400080702     c                   endDo
093500080702      *
093600080702     c                   end
093700080616      *
093800070309      * controllo sulla gestibilità del P.O.
093900070309     c                   if        §ogdda = *blank or §ogdda = *all'0'
094000070309     C                   MOVE      *ON           $FINE
094100070312     C                   goto      end_CT1
094200070309     c                   else
094300070309      *
094400070309      * se c'è una data in organigramma
094500070309     c                   move      §ogdda        data_org          8 0
094600070830     c                   move      §ogDDAO       data_orM
094700070927      * Abilitazione a trattare gli ORM
094800070927     c                   movel     §ogcGIO       Abilita_ORM
094900070309      *
095000070309     C*  Controlla Con il P.O. la data abilitazione
095100070309      * se siamo in fase di Parallelo prima di essere partiti
095200070426     c                   if        data_org = 20391231  or
095300070426     c                             data_org >  dateu8
095400070309     C                   eval      test_Parallelo = 'S'
095500070309     c                   end
095600070309      *
095700070830     C*  Controlla Con il P.O. la data abilitazione
095800070830      * se siamo in fase di Parallelo prima di essere partiti
095900070830     c                   if        data_OrM = 20391231  or
096000070830     c                             data_OrM >  dateu8
096100070830     C                   eval      test_Paral_ORM = 'S'
096200070830     c                   end
096300070830      *
096400070309      * se data distinte automatiche NON è
096500070309      * né = 31/12/2039 che significa p.o. in fase di test o parallelo
096600070309      * né < oggi che significa p.o. partito
096700070309      * allora fuori
096800070426     c******             if        data_org <> 20391231 and dateu8 < data_org
096900070426     C******             MOVE      *ON           $FINE
097000070426     C******             goto      end_CT1
097100070426     c******             end
097200070309     c                   end
097300070309      *
097400070309     c* P.o. con picking, messaggerie: visualizzo parzializzazione
097500070309     c                   if        fgspkm='S'
097600070309     c                   seton                                        08
097700070309    1C                   ENDif
097800160503      *
097900160503     c* P.o. con TURNO
098000160503     c                   if        fgsturno ='S'
098100160503     c                   seton                                        38
098200160506     c                   eval      sav_in38 = *in38
098300160503    1C                   ENDif
098400160503      *
098500070309    1C                   END
098600070309    1C                   END
098700070309      *
098800070309     C*------------------------
098900070309     c* per cambio p.o. gestione riemetto video 1 senza errori
099000070309     c*  può cambiare l'indicatore del picking messaggerie S/N
099100070312     c                   if        d1CFGS <> savfgs
099200160503     c**********         if        (sav_in08='0' AND *IN08)   or
099300160503     c**********                   (sav_in08='1' and not *in08)
099400160503     c**********         seton                                            99
099500160503     c**********         endif
099600080715      * si vuole forzare la riemissione del video al cambio della filiale
099700080715      * di gestione --> quindi 99 sempre ON
099800080715     c                   seton                                            99
099900080715      *
100000070313     c                   eval      sav_in08 = *in08
100100070312     c                   eval      savfgs  = d1CFGS
100200070309     c                   endif
100300070309     C*------------------------
100400070309      * CONTROLLO DATA INIZIALE
100500070309     C                   Z-ADD     0             DATINW
100600070312    1C     d1CDIN        IFNE      0
100700070312     C                   MOVE      d1CDIN        G02DAT
100800070309     C                   MOVEL     *BLANK        G02ERR
100900070309     C                   CALL      'XSRDA8'
101000070309     C                   PARM                    WLBDAT
101100070309    2C     G02ERR        IFEQ      '1'
101200070309     C                   SETON                                        57  99
101300070312     C                   GOTO      end_CT1
101400070309    2C                   END
101500070309     C                   Z-ADD     G02INV        DATINW
101600070312     C                   Z-ADD     G02DAT        d1CDIN
101700070309    1C                   END
101800070309      *
101900070309      * CONTROLLO DATA FINALE
102000070312    1C     d1CDFI        IFEQ      0
102100070312     C                   Z-ADD     31129999      d1CDFI
102200070309     C                   Z-ADD     99991231      DATFIW
102300070309   X1C                   ELSE
102400070312     C                   MOVE      d1CDFI        G02DAT
102500070309     C                   MOVEL     *BLANK        G02ERR
102600070309     C                   CALL      'XSRDA8'
102700070309     C                   PARM                    WLBDAT
102800070309    2C     G02ERR        IFEQ      '1'
102900070309     C                   SETON                                        58  99
103000070312     C                   GOTO      end_CT1
103100070309    2C                   END
103200070309     C*
103300070309     C                   Z-ADD     G02INV        DATFIW
103400070312     C                   Z-ADD     G02DAT        d1CDFI
103500070309      *
103600070309     C*------------------------
103700070309      * DATA INIZIALE MAGGIORE DATA FINALE
103800070309    2C     DATINW        IFGT      DATFIW
103900070309     C                   SETON                                        54  99
104000070312     C                   GOTO      end_CT1
104100070309    2C                   END
104200070309    1C                   END
104300070402      *?  Fare attenzione: si Abilita il posizionamento x Distinta...  ?
104400070402      *?   Solo se il range di date indica lo stesso giorno.
104500070402      *?   Si utilizza l'indicatore 13 per abilitare o meno il campo.
104600070402     c                   setoff                                       13
104700070403     c                   eval      cur_su_SFL = 'S'
104800070404    2C                   IF        DATFIW = DATINW  and
104900070404     c                                pgm_da_call <> 'S'
105000070403     c                   eval      cur_su_SFL = 'N'
105100070402     c                   seton                                        13
105200070402     c                   end
105300070309     C*------------------------
105400070309      * NUMERO FOGLIO
105500070312     c                   clear                   wnfv
105600070312    1C     d1CNFV        IFGT      0
105700070309     C* VEDO SE E' FOGLIO DI FILIALE GESTIBILE
105800070312     c                   z-add     d1CNFV        wnfv
105900070406     C     Kdst9         CHAIN     Fidst09L                           59
106000070309     C* FOGLIO INESISTENTE
106100070309    2C     *IN59         IFEQ      *ON
106200070309     C                   SETON                                          99
106300070312     C                   GOTO      end_CT1
106400070309    1C                   ENDIF
106500070312    1C                   ENDIF
106600070309     C*
106700070309     C*------------------------
106800070309     c                   move      'N'           Solo_aperte       1
106900070309     C* RICHIESTI SOLO FOGLI APERTI
107000070312     C     d1CTFV        IFEQ      '2'
107100070419     C     d1CTFV        orEQ      '3'
107200070309     c                   move      'S'           Solo_aperte
107300070309     C                   END
107400070309     C*------------------------
107500070309      * ?- Codice Giro
107600070309     c                   clear                   con_giro
107700070312     c                   clear                   d1dcgi
107800070312     c     '?'           scan      D1CCGI
107900070309     C*   ricerca codice giro
108000070309     c                   if        %Found
108100070309     C                   SETON                                            99
108200070309     c                   clear                   fidg09ds
108300070309     c                   move      'P01'         d09iop0
108400070312     c                   movel     d1CFGS        d09ifgs
108500071010     c*****              move      'C'           d09itug
108600071010     c                   move      d1DATI        d09itug
108700070309     c                   z-add     DATEU8        d09idat
108800070313     c                   eval      kpjbus = kpjbu
108900070309     c                   movel(p)  fidg09ds      kpjbu
109000070309     c                   call      'FIDG09R'
109100070309     c                   parm                    kpjba
109200070309     c                   movel     kpjbu         fidg09ds
109300070313     c                   eval      kpjbu = kpjbus
109400070320     c                   movel     d09oflo       dGRGflo
109500070320      * se il giro può essere messo in distinta
109600070320     c                   if        §grgDST <> 'N'
109700070312     c                   movel     d09ocgi       D1CCGI
109800070320     c                   end
109900070309     c                   end
110000070309      *
110100070309      *  controllo validità giro
110200070312     c                   if        D1CCGI <> *blank
110300070309     c                   clear                   fidg09ds
110400070309     c                   eval      D09IOP0 = '001'
110500070312     c                   eval      d09ifgs = d1CFGS
110600071010     c******             eval      d09itug = 'C'
110700071010     c                   eval      d09itug = d1DATI
110800070312     c                   eval      d09icgi = D1CCGI
110900070309     c                   move      dateu8        d09idat
111000070313     c                   eval      kpjbus = kpjbu
111100070309     c                   eval      kpjbu = fidg09ds
111200070309     c                   call      'FIDG09R'
111300070309     c                   parm                    kpjba
111400070309     c                   eval      fidg09ds = kpjbu
111500070320     c                   movel     d09oflo       dGRGflo
111600070313     c                   eval      kpjbu = kpjbus
111700070320     c                   if        d09oerr = '1' or §grgDST = 'N'
111800070309     C                   SETON                                        55  99
111900070312     C                   goto      end_CT1
112000070309     c                   else
112100070309     c                   if        d09ocgi <> ' '
112200070312     c                   eval      d1dcgi = d09odes
112300070309     c                   move      'S'           con_giro
112400070312      *
112500070312      **  Se impostato un Foglio e non ci sono giri sul foglio
112600070312      **   deve segnalare l'errore sul giro scelto
112700070312     c                   if        D1Cnfv  > *zero
112800070312     c                   move      'N'           Giro_in_NFV       1
112900080715     c                   clear                   Prior_GiroCon     1 0
113000080715     c                   clear                   Prior_GiroRit     1 0
113100070910     c                   if        d1dati = 'C'
113200070910      **
113300070312     c     kdst31        setll     fidst31l
113400070312     c     kdst31        reade     fidst31l
113500070910     c                   dow       not %Eof(fidst31l)
113600070312      **
113700070312      **  Controlla presenza del giro richiesto
113800070312     c                   if        %char(DST3NPG) = d1cNPG and
113900070312     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
114000070312     c                             and DST3CGI = d1cCGI
114100070312     c                   move      'S'           Giro_in_NFV       1
114200080715     c                   z-add     dst3prgG      Prior_GiroCon
114300070312     c                   leave
114400070312     c                   end
114500070312      **
114600070312     c     kdst31        reade     fidst31l
114700070312     c                   endDo
114800070312      **
114900070910     c                   else
115000070910      **
115100070910     c     kdst51        setll     fidst51l
115200070910     c     kdst51        reade     fidst51l
115300070910     c                   dow       not %Eof(fidst51l)
115400070910      **
115500070910      **  Controlla presenza del giro richiesto
115600070910     c                   if        %char(DST5NPG) = d1cNPG and
115700070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
115800070910     c                             and DST5CGI = d1cCGI
115900070910     c                   move      'S'           Giro_in_NFV       1
116000080715     c                   z-add     dst5prgG      Prior_GiroRit
116100070910     c                   leave
116200070910     c                   end
116300070910      **
116400070910     c     kdst51        reade     fidst51l
116500070910     c                   endDo
116600070910      **
116700070910     c                   end
116800070910      **
116900070312      ** Il giro selezionato non è presente nella distinta
117000070312     c                   if        Giro_in_NFV = 'N'
117100070312     C                   SETON                                        52  99
117200070312     C                   goto      end_CT1
117300070312     c                   end
117400070312      **
117500070312     c                   end
117600070312      **
117700070309     c                   end
117800070309     c                   end
117900070309     c                   end
118000070309      *
118100070309     C*------------------------
118200070309     c* Per visualizzare gli importi rchiedere tipi fogli = 2
118300070312     c                   if        d1IMPO='S' and d1CTFV<>'2'
118400070312     C                   MOVEL     MSG(3)        d1Cmsg
118500070309     C                   SETON                                        56  99
118600070312     C                   GOTO      end_CT1
118700070309     c                   endif
118800160503     C*------------------------
118900160506    1C                   IF        *in08 and sav_in38 = *on
119000160503      *
119100160503     c                   if        d1Ctpm ='M'  and
119200160503     c                              (d1ctur1 <>' ' or d1ctur2 <>' ')
119300160504     C*********          MOVEL     MSG(4)        d1Cmsg
119400160504     C*********          SETON                                        53  99
119500160509     C*********          GOTO      end_CT1
119600160503      *
119700160503     c                   elseIF    d1ctur1=' ' and d1ctur2 = ' ' and d1Ctpm<>'M'
119800160503     C                   MOVEL     MSG(5)        d1Cmsg
119900160503     C                   SETON                                        60  99
120000160503     C                   GOTO      end_CT1
120100160503      *
120200160503     c                   end
120300160503      *
120400160503     c                   end
120500160503      *
120600070309     C*------------------------
120700070312     C     end_CT1       TAG
120800070309     c   99              move      'S'           $VISD1
120900080715      *
121000080715      * Solo se ci sono messaggi di errore attiva il 28 per
121100080715      *  visualizzarli.
121200080715     c******             move      *in99         *in28
121300080715     c                   if        d1Cmsg <> *blank
121400080715     c                   move      *in99         *in28
121500080715     c                   end
121600160505     c**
121700160505     c                   clear                   sav_c1aut
121800160505     c                   clear                   sav_c1nfv
121900160505     c                   clear                   sav_c2aut
122000160505     c                   clear                   sav_c2nfv
122100070309     C*
122200160505     c                   clear                   c1daut
122300160505     c                   clear                   c1cnfv
122400160505     c                   clear                   c2daut
122500160505     c                   clear                   c2cnfv
122600160505     C*
122700070309     C                   ENDSR
122800070301     C/EJECT
122900070312      *---------------------------------------------------------------*
123000070321      *?  GESTIONE SFL 1                       ?
123100070312      *---------------------------------------------------------------*
123200070321     C     GESS1         BEGSR
123300070312     C*
123400070312     C* inizializzazione videata
123500070321     C     $INZS1        IFEQ      *ON
123600070321     C                   EXSR      INZS1
123700070419     C*
123800070419     C* Se richiamato x il test del FIMS03R esce forzatamente
123900070419     c                   if        pgm_da_call = 'S' and sav_in19 = *on
124000070419     c                             and  FDGtfv = '3'
124100070419     c                   LeaveSr
124200070419     c                   end
124300070419     C*
124400070321     C                   MOVE      *OFF          $INZS1
124500070312     C                   ENDIF
124600070312     C*
124700070312     C* Inizializza e imposta piede videata
124800070321     C                   EXSR      INZZ1
124900070312     C* emissione piede videata
125000070312      * ?            *-----------------------*
125100070321     C                   WRITE     DG31Z02
125200070312      * ?            *-----------------------*
125300070312     C*
125400070312     C     WMAX          IFEQ      0
125500070312     C* Non ci sono records
125600070312      * ?            *-----------------------*
125700070312     C                   WRITE     DG31V02
125800070312      * ?            *-----------------------*
125900070312     c                   setoff                                       30
126000070312     C                   Else
126100070312     c                   seton                                        30
126200070312      *
126300070312     C     Wsfl          IFgt      0
126400070321     C                   Z-ADD     wsfl          C1RCD
126500070312     C                   Else
126600070312     C     Wpag          IFgt      0
126700070321     C                   Z-ADD     wpag          C1RCD
126800070312     C                   EndIF
126900070312     C                   EndIF
127000070312     C                   ENDIF
127100071026      * -----------------------
127200071026      * ?  reimposta su quale riga deve posizionarsi il cursore
127300071026      * ?    se non si è chiesto l'ordinamento
127400071026     C                   if        $TASTO   <>  F11
127500071026     C                   z-add     Su_Quale_rec  h1riga
127600071026     C                   EndIF
127700071026      * -----------------------
127800080703      * solo se richiamato dal MONITOR e solo la prima volta
127900080703     c                   if        fdgpgm = 'MONITOR' and Apri_DROP ='S'
128000080703     c                   clear                   Apri_DROP
128100080703     c                   eval      *in16 = *ON
128200080703     c                   end
128300071026      *
128400070312      *  Emissione SFL dei GIRI
128500070312     c                   setoff                                       99
128600070419     c                   if        $VISS1 = 'S'
128700070312      * ?            *-----------------------*
128800070321     C                   EXFMT     DG31CT1
128900070312      * ?            *-----------------------*
129000070321     C                   movel     'S1'          Sav_$GEST
129100070419     c                   end
129200070312      *
129300070321     C     C1NRR         IFNE      0
129400070321     C                   Z-ADD     C1NRR         WSFL
129500070320     C                   Else
129600070320     C     SFLNRR        IFgt      0
129700070320     C                   Z-ADD     SFLNRR        WSFL
129800070320     c                   EndIF
129900070320     C                   ENDIF
130000070321     C                   Z-ADD     SFLNRR        C1RCD
130100070312     C* Selezioni
1302000703121    C                   SELECT
130300070313     C* F11=Cambia Ordinamento del SFL
130400070313     C                   WHEN      $TASTO   =   F11
130500160505     c                   if        sav_c1aut <> *blank or
130600160505     c                             sav_c1nfv  > 0
130700160505     c                   move      *on           $inzs1
130800160505     C                   clear                   C1DAUT
130900160505     C                   clear                   C1CNFV
131000160505     C                   clear                   sav_c1nfv
131100160505     C                   clear                   sav_c1aut
131200160505     c                   else
131300070321     C                   clear                   C1DAUT
131400070321     C                   clear                   C1CNFV
131500070321     C                   clear                   sav_c1nfv
131600070321     C                   clear                   sav_c1aut
131700070403     C                   EXSR      F11_ordina1
131800160505     c                   end
131900070312     C* F3=Fine
132000070312     C     $TASTO        WHENEQ    F03
132100080606     c                   move      '03'          fdgtfu
132200070312     C                   EXSR      F03_exit
132300070321     c                   goto      end_s01
132400070312     C* F12=Ritorno
132500070312     C     $TASTO        WHENEQ    F12
132600070312     c                   move      'D1'          $GEST
132700070330     C                   clear                   C1DAUT
132800070330     C                   clear                   C1CNFV
132900070330     C                   clear                   sav_c1nfv
133000070330     C                   clear                   sav_c1aut
133100070326     c                   clear                   d1ccgi
133200070312     C*    deve andare a fine se richiamato
133300070312     c                   if        pgm_da_call = 'S'
133400070326     c                   clear                   fdgcgi
133500070326     c                   clear                   fdgnfv
133600080606     c                   move      '12'          fdgtfu
133700070312     C                   EXSR      F03_exit
133800070312     c                   end
133900070321     c                   goto      end_s01
134000070312      *
1341000703121O   C                   OTHER
134200070312     C* CONTROLLO testata
134300070321     C                   EXSR      CTRC1
134400070312     C* CONTROLLO subfile
134500070312     C     *IN99         IFEQ      *OFF
134600070321     C                   EXSR      CTRS1
134700070320
134800070312     C                   END
134900070312      *
135000070312      * ?Emette la videata di Dettaglio Distinta
135100070312     C     *IN99         IFEQ      *OFF
135200070321     C********           MOVE      *ON           $INZD2
135300070312     C********           movel     'D2'          $GEST
135400070312     C                   END
135500070312      *
1356000703121-   C                   ENDSL
135700070312     C*
135800070321     c     end_s01       tag
135900070312     C                   ENDSR
136000070312     C/EJECT
136100070312      *---------------------------------------------------------------*
136200070321      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL ?
136300070312      *---------------------------------------------------------------*
136400070321     C     INZZ1         BEGSR
136500070312     C*
136600070312     C*
136700070312     C                   ENDSR
136800070312     C/EJECT
136900070312      *---------------------------------------------------------------*
137000070321      *?  INIZIALIZZAZIONE LISTA dei GIRI da selezionare ?
137100070312      *---------------------------------------------------------------*
137200070321     C     INZS1         BEGSR
137300070314     C*
137400070312     C* reset indicatori DSPATR
137500070312     c                   exsr      INZ_indERR
137600070312     C* pulizia SFL
137700070312     C                   SETOFF                                         3031
137800070321     C                   WRITE     DG31CT1
137900070314     C                   SETON                                          31
138000070320      *
138100070314     C* Posiziona il cursore in testata x 12 = *on
138200070320     c                   if        cur_su_SFL = 'N'
138300070321     c     $PcurS1       comp      *on                                    12
138400070320     C* imposto pos. del cursore
138500070320     C                   eval           H1RIGA = 3
138600070528     C                   eval           H1COLO = 17
138700070315     c                   else
138800070320     c                   move      'N'           cur_su_SFL        1
138900070315     c                   setoff                                           12
139000070320     C* imposto pos. del cursore
139100070320     C                   eval           H1RIGA = 7
139200070320     C                   eval           H1COLO = 2
139300070315     c                   end
139400071026     C* se siamo al caricamento del SFL si deve posizionare sulla prima riga disponibile
139500071026     c                   z-add     1             Che_NRec_sfl
139600071026     C                   z-add     h1riga        Su_Quale_rec
139700071026     C                   move      'S'           prima_volta
139800080219     c                   eval      *in15 = sav_in15
139900160506     c                   eval      *in38 = sav_in38
140000070320     C*
140100070314     C* Se richiamato attivo 04
140200070528     C*   se però è richiamato da Assegnazione attivo 05
140300070620     c                   setoff                                       0503
140400080625     c                   move      *off          sav_in05
140500170508     c                   move      *off          sav_in04
140600080625     C*
140700070314     c                   if        pgm_da_call = 'S'
140800070314     C                   SETON                                          04
140900070404     c                   move      *on           sav_in04
141000080715     c                   if        fdgpgm = 'ASSMANUALE' or fdgpgm = 'MONITOR'
141100070620     C                   SETON                                          03
141200070620     c                   move      *on           sav_in03
141300070620     c                   end
141400070528     c                   if        fdgOPZ = '1'
141500070528     C                   SETON                                          05
141600070528     c                   move      *on           sav_in05
141700070314     c                   end
141800070528     c                   end
141900070312     C*
142000070312     C* Righe x Pagina del 1° SFL
142100071026     c                   z-add     16            s1pag
142200070312      *
142300070312     C* CARICAMENTO SFL totale
142400070321     C                   Z-ADD     0             S1NRR
142500070321     C                   Z-ADD     1             C1RCD
142600070312     C                   Z-ADD     0             WMAX
142700070312     C                   Z-ADD     0             Wsfl
142800070312     C* campi titolo
142900070321     C                   MOVEL     TES(1)        c1CTES
143000080715     c                   if        fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR'
143100070606     C                   MOVEL(p)  TES(2)        c1CTES
143200070606     c                   end
143300070321     C                   MOVEL     dsPGM         c1CPGM
143400070321     C                   MOVEL     RAGUT         c1CDSA
143500070321     C                   MOVEL     knsif         c1nsif
143600070321     C                   MOVEL     knmus         c1nmus
143700070321     C                   z-add     d1CFGS        c1cfgs
143800070321     C                   movel     d1DFGS        c1dfgs
143900070321     C                   movel     d1ccgi        c1ccgi
144000070830     c   08              movel     'Prc'         c1PRC
144100160503     c   38              movel     'T'           c1TUR1
144200160503     c   38              movel     'r'           c1TUR2
144300070312     C*
144400070312     C* Posizionamento su file pilota e impostazione primo record da leggere
144500070312     c                   clear                   Prima_Lettura     1
144600070312      *
144700070406     c                   if        Solo_aperte = 'N'
144800070312     c     Kdat1         setll     fidst008
144900070406     c                   else
145000070312     C     Kdst3         SETLL     Fidst003
145100070406     c                   End
145200070312     C* Carico il SFL
145300070321     C                   EXSR      ROLS1
145400070419     C*
145500070419     C* Se richiamato x il test del FIMS03R esce forzatamente
145600070419     c                   if        pgm_da_call = 'S' and sav_in19 = *on
145700070419     c                             and  FDGtfv = '3'
145800070419     c                   LeaveSr
145900070419     c                   end
146000070312     C*
146100070312     C                   Z-ADD     1             WPAG
146200070312     C*
146300070313     ** Al primo F11 ordino per Autista
146400070321     C                   EVAL      RrnLast = S1Nrr
146500160504      **
146600160505     c                   if          sav_in10 = *off
146700160504     C*
146800160601     c                   if        sav_in11 = *off
146900070417     C                   EVAL      WrkSort = Numero
147000070417     c                   exsr      F11_ordina1
147100160601     c                   setoff                                       11
147200160601     c                   elseif    sav_in11 = *on
147300070313     C                   EVAL      WrkSort = Autista
147400070402     c                   exsr      F11_ordina1
147500070313     c                   end
147600160504     C*
147700160505     c                   elseif      sav_in10 <> *off
147800160504     C                   EVAL      WrkSort = Turno
147900160504     c                   exsr      F11_ordina1
148000160504     c                   end
148100070320     C*
148200070312     C                   ENDSR
148300070312     C/EJECT
148400070301      *---------------------------------------------------------------*
148500070321      *?  CARICAMENTO PAGINA                  ?
148600070301      *---------------------------------------------------------------*
148700070321     C     ROLS1         BEGSR
148800070301     C*
148900070301     C                   SETOFF                                       32
149000070301     C                   Z-ADD     0             Y
149100070321     C                   Z-ADD     WMAX          S1NRR
149200070301     C*
149300070312     C* Caricamento del Sfl x Giro
149400070321     c                   Exsr      Write_S1
149500070301     C* Fine Caricamento
149600070321     C                   Z-ADD     S1NRR         WMAX                 30
149700070419     C*
149800070419     C* Se il programma è richiamato e non ha trovato dei records
149900070419     c                   if        Wmax = 0 and pgm_da_call = 'S'
150000070419     c                   move      '9'           FDGerr
150100070419      * esce forzatamente
150200080821     c                   move      *on           $ESCI
150300070419     C                   MOVE      *ON           $FINE
150400070419     c                   end
150500070419     C*
150600070302     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
150700070321     C     S1NRR         DIV       S1pag         PAGINE            4 0
150800070301     C                   MVR                     RESTO             3 0
150900070321     C     PAGINE        MULT      S1pag         C1RCD
1510000703011    C     RESTO         IFGT      0
151100070321     C                   ADD       1             C1RCD
1512000703011E   C                   ELSE
151300070321     C                   SUB       S1pag         C1RCD
151400070321     C                   ADD       1             C1RCD
1515000703011-   C                   ENDIF
151600070301     C*
151700070301     C                   ENDSR
151800070312      *---------------------------------------------------------------*
151900070321      *?  Scrive il SFL   ?
152000070312      *---------------------------------------------------------------*
152100070321     C     Write_S1      BEGSR
152200070312     C*
152300070312     c                   exsr      Legge_rec
152400070312     C*
1525000703121    C     $EFILE        DowEQ     *OFF
152600070312     C*
152700070321     C                   clear                   DG31SF1
152800070404     c                   move      sav_in04      *in04
152900070528     c                   move      sav_in05      *in05
153000080219     c                   move      sav_in15      *in15
153100160506     c                   move      sav_in38      *in38
153200070510     C* Priorità
153300080715     C*    Consegne/Ritiri/Entrambe
153400080715     c                   if        d1dati = 'C'
153500080715     C*****              z-add     dst3prgg      h1prgg
153600080715     c                   z-add     prior_giroCon h1prgg
153700080715      *
153800080715     c                   elseIf    d1dati = 'R'
153900080715     c                   z-add     prior_giroRit h1prgg
154000080715      *
154100080715     c                   end
154200080715      *
154300070510     C                   z-add     dst3sql       h1sql
154400070313     C* GIRO DATA Distinta
154500070417     C                   MOVE      dstDFV        h1CDFV
154600070312     C                   MOVE      dstDFV        G02INV
154700070312     C                   MOVEL     '3'           G02ERR
154800070312     C                   CALL      'XSRDA8'
154900070312     C                   PARM                    WLBDAT
155000070312     C*
155100070313     C                   movel     G02DAT        campo4            4
155200070321     C                   move      G02DAT        s1CDFV
155300070321     C                   movel     campo4        s1CDFV
155400070312      *
155500070321     C                   Z-ADD     dstNFV        s1CNFV
155600070312     c     kapd          chain     fiapd01l
155700070312     c                   if        %found(fiapd01l)
155800070321     c                   eval      s1DAUT = apdrsc
155900070312     c                   else
156000070321     c                   clear                   s1DAUT
156100070312     c                   end
156200070312      *
156300070321     c                   z-add     DSTPKL        s1cpem
156400070321     c                   z-add     DSTVLU        s1cvom
156500070326      *
156600070326     c                   if        s1cpem > 0 or s1cvom > 0
156700070326     c                   movel     'Max'         s1max
156800070326     c                   end
156900070313      *
157000070321     c                   z-add     DSTPKLa       s1cpea
157100070321     c                   z-add     DSTVLUa       s1cvoa
157200070905     c                   if        DSTfpp  ='M'
157300070905     c                   movel     'Mat'         s1Tusc
157400070905     c                   else
157500070905     c                   move      'Pom'         s1Tusc
157600070905     c                   end
157700070820     c                   clear                   s1STP
157800080623     c                   if        DSTfasd ='STP' or
157900080623     c                             Fase_STP = 'S'
158000080625     c                   movel     'St'          s1STP
158100070820     c                   endif
158200070313      *
158300080625      * Controlla se ci sono delle Fasi presenti da segnalare
158400080625     c                   clear                   fidg39dS
158500080625     c                   move      d1CNPG        fdg39NPG
158600080625     c                   z-add     s1CNFV        fdg39NFV
158700080625     c                   z-add     c1cfgs        fdg39FGS
158800080625     c                   move      'N'           fdg39VIDno
158900080625     c                   eval      kpjbus = kpjbu
159000080625     c                   movel(p)  fidg39ds      KPJBU
159100080625     c                   call      'FIDG39R'
159200080625     c                   parm                    kpjba
159300080625     c                   eval      fidg39ds = KPJBU
159400080625     c                   eval      kpjbu = kpjbus
159500080625      * se ci sono delle Fasi da vedere imposta l'asterisco nel primo Byte
159600080625      *  del flag di stampato.
159700080625     c                   if        fdg39FASI = 'S'
159800080625     c                   move      '*'           s1STP
159900080625     c                   endif
160000080613      *
160100070312     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
160200070625     c                   if        *in08
160300070312     c                   if        dsttpm='M'
160400070321     c                   movel     'Mes'         s1CTPM
160500070312     c                   endif
160600070312     c                   if        dsttpm<>'M'
160700070321     c                   movel     'Prc'         s1CTPM
160800070312     c                   endif
160900070312     c                   endif
161000160503      **  nel filler
161100160503     C                   movel     dstflr        DDStflr
161200160503     c* x il TURNO
161300160503     c                   if        *in38
161400160503     c                   movel     §DSTTURNO     s1CTUR
161500160504     c                   if        dsttpm <> 'M' and
161600160504     c                               s1CTUR = *blank
161700160504     c                   eval        s1CTUR = '1'
161800160503     c                   endif
161900160504     c                   endif
162000160503     c
162100070312      * Flag tipo distinta (solo consegna, orm o entrambi)
162200070312     C                   CLEAR                   WBOL
162300070312     C                   CLEAR                   WORM
162400070312     c                   Z-Add     d1CFGS        Wifp
162500070321     C                   Z-ADD     s1CNFV        kfnvv
162600070321     C                   Z-ADD     s1CNFV        WnFV
162700070312     c     Karb78        Setll     Fnarb78l
162800070312if  5c                   if        %equal(fnarb78l)
162900070312     c                   movel     'S'           wbol              1
163000070312x   5c                   else
163100070312     C     Kdst          SETLL     FnBRV01L
163200070312     c                   if        %equal(fnbrv01l)
163300070312     c                   movel     'S'           wbol              1
163400070312     c                   endif
163500070312e   5c                   endif
163600070312     C     KORM          SETLL     FNORM06L
163700070312do  5c                   do        *hival
163800070312     C     KORM          reade     FNORM06L
163900070312     c                   if        %eof(fnorm06l)
164000070312     c                   leave
164100070312     c                   endif
164200070312     c                   if        ormatb = *blanks and ormfao < 999
164300070312     C                   move      'S'           worm              1
164400070312     c                   endif
164500070312e   5c                   enddo
164600070312     C                   if        wbol = 'S' and worm <> 'S'
164700070321     C                   movel     'CON'         s1CTPD
164800070312     C                   endif
164900070312     C                   if        wbol <> 'S' and worm = 'S'
165000070321     C                   movel     'ORM'         s1CTPD
165100070312     C                   endif
165200070312     C                   if        wbol = 'S' and worm = 'S'
165300070321     C                   movel     'C/O'         s1CTPD
165400070312     C                   endif
165500070312      * Calcolo importi se non già fatto prima
165600070312if  5c                   if        d1CTFV = '2' and d1IMPO = 'S'
165700070312     c                   exsr      totalizzo
165800070321     c                   z-add     w_imas        s1IMAS
165900070321     c                   z-add     w_imca        s1IMCA
166000070312     c                   endif
166100070321      * Pda
166200070321     C                   movel     dstPDA        s1CPDA
166300080208     c                   setoff                                       14
166400080208     c                   if        dstDFV >= DateU8
166500080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
166600080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
166700080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
166800080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
166900080208     c                   seton                                        14
167000080208     c                   endIf
167100080208     c                   endIf
167200080222     c                   eval      h1in14 = *in14
167300080222      *
167400080208      *  lo Minuscolizza sempre a video
167500080207     C     Up:Lo         XLATE     §DSTtstPDA    s1CTSTPDA
167600070321      * ANNULLATO
167700070312     C     dstATB        IFEQ      'A'
167800070321     C                   MOVEL     'ANN'         s1CANN
167900070312     C                   END
168000070312if  5C     dstFCF        IFNE      ' '
168100070312      * CHIUSO
168200070312     C     dstFCF        IFEQ      'S'
168300070403     C                   MOVEL     'C   '        s1CFCF
168400070312     C                   ELSE
168500070312     C* BLOCCATO
168600070403     C                   MOVEL     'BL  '        s1CFCF
168700070312e   5C                   ENDIF
168800070312e   4C                   ENDIF
168900070315      *
169000070315      * Intestazioni seconda riga
169100070830     c                   setoFF                                       18
169200070321     c                   if        s1imca <> 0
169300070321     c                   movel     Imp_C_A       s1dica
169400070830     c                   setoN                                        18
169500070315     c                   end
169600070315      *
169700070830     c                   setoFF                                       20
169800070321     c                   if        s1imas <> 0
169900070321     c                   movel     Imp_ASS       s1dias
170000070830     c                   setoN                                        20
170100070315     c                   end
170200070510      *
170300070510     c                   setoff                                       40
170400070510if  4c                   if        h1prgg = 1
170500070510     c                   seton                                        40
170600070510     c                   end
170700070312      *---------
170800070313if  4c                   if        FDGtfv <> '3'
170900160601     c                   if        (sav_c1nfv <= s1cnfv and *in11 = *off) or
171000160601     c                             (sav_c1aut <= s1daut and *in11 = *on ) or
171100070321     c                             (sav_c1aut = *blank and sav_c1nfv =0)
171200070404     C* Imposta indicatore x abilitare campo scelta
171300070404     c                   move      sav_in19      *in19
171400170508     c                   eval      h1in15 = *in15
171500170508     c                   eval      h1in18 = *in18
171600170508     c                   eval      h1in19 = *in19
171700170508     c                   eval      h1in20 = *in20
171800070312     c********           seton                                        32
171900070321     C                   ADD       1             S1NRR
172000070312     C                   ADD       1             Y
172100070321     C                   WRITE     DG31SF1
172200070313     c                   end
172300070312e   4c                   endif
172400070312      *---------
172500070312      *  Esce da qui con un record di Totale x GIRO
172600070312     c                   exsr      Legge_rec
172700070312     c                   EndDO
172800070321      *
172900070312     C                   ENDSR
173000070321     C/EJECT
173100070321      *---------------------------------------------------------------*
173200070321      *?  CONTROLLO TESTATA LISTA   ?
173300070321      *---------------------------------------------------------------*
173400070321     C     CTRC1         BEGSR
173500070321      *
173600070321     C                   MOVE      *OFF          *IN99
173700070321      *
173800070321     C*------------------------
173900070321      *  interrogazione Giro
174000070321     c                   clear                   c1dcgi
174100070321     c     '?'           scan      c1cCGI
174200070321     C*   ricerca codice giro
174300070321     c                   if        %Found
174400070321     c                   clear                   fidg09ds
174500070321     c                   move      'P01'         d09iop0
174600070321     c                   movel     c1cFGS        d09ifgs
174700071010     c*****              movel     'C'           d09itug
174800071010     c                   movel     d1DATI        d09itug
174900070321     c                   z-add     DATEU8        d09idat
175000070321     c                   eval      kpjbus = kpjbu
175100070321     c                   movel(p)  fidg09ds      kpjbu
175200070321     c                   call      'FIDG09R'
175300070321     c                   parm                    kpjba
175400070321     c                   movel     kpjbu         fidg09ds
175500070321     c                   eval      kpjbu = kpjbus
175600070321     c                   movel     d09ocgi       c1cCGI
175700070321     c                   end
175800070321     C*------------------------
175900070321      *  controllo validità giro
176000070321     c                   if        c1cCGI <> *blank
176100070321     c                   clear                   fidg09ds
176200070321     c                   eval      D09IOP0 = '001'
176300070321     c                   eval      d09ifgs = c1cFGS
176400071010     c****               eval      d09itug = 'C'
176500071010     c                   eval      d09itug = d1DATI
176600070321     c                   eval      d09icgi = c1cCGI
176700070321     c                   move      dateu8        d09idat
176800070321     c                   eval      kpjbus = kpjbu
176900070321     c                   eval      kpjbu = fidg09ds
177000070321     c                   call      'FIDG09R'
177100070321     c                   parm                    kpjba
177200070321     c                   eval      fidg09ds = kpjbu
177300070321     c                   eval      kpjbu = kpjbus
177400070321     c                   if        d09oerr = '1'
177500070321     C                   SETON                                        54  99
177600070322     C                   goto      ENDC1
177700070321     c                   else
177800070321     c                   if        d09ocgi <> ' '
177900070321     c                   eval      c1dcgi = d09odes
178000070321     c                   move      'S'           con_giro
178100070321     c                   end
178200070321     c                   end
178300070321      *
178400070321     c                   end
178500070321     C*------------------------
178600070321      *  interrogazione AUT
178700070321      *
178800070321     c     '?'           scan      c1daut
178900070321     C*   ricerca codice giro
179000070321     c                   if        %Found
179100070321     c                   clear                   fnlv24ds
179200070321     C                   Z-ADD     c1cFGS        d24FIL
179300070321     C                   MOVEL     'R'           d24FLG
179400070321     C                   MOVEL     'A'           d24tip
179500070321     c                   eval      kpjbus = kpjbu
179600070321     C                   MOVEL(p)  fnlv24ds      KPJBU
179700070321     C                   CALL      'FNLV24R'
179800070321     C                   PARM                    KPJBA
179900070321     C                   MOVEL     KPJBU         fnlv24ds
180000070321     c                   eval      kpjbu = kpjbus
180100070321     C                   MOVEL     D24RSC        c1daut
180200070321     c                   end
180300070321     C*
180400070321     C*------------------------
180500070321     c                   move      *on           $PcurS1
180600070321     C* Riposizionamento x AUT o x Numero
180700160601     c                   if        (*in11 = *off and c1cnfv <> sav_c1nfv) or
180800160601     c                             (*in11 = *on  and c1daut <> sav_c1aut)
180900070321     c                   move      *off          $PcurS1
181000070321     c                   move      *on           $inzs1
181100070321     c                   move      'S1'          $GEST
181200160601     c   11              movel     c1daut        sav_c1aut
181300160601     c  n11              z-add     c1cnfv        sav_c1nfv
181400070321     c                   end
181500070321     C*
181600070321     C* Posiziona il cursore in testata x 12 = *on
181700070321     c     $PcurS1       comp      *on                                    12
181800070321     C  n12              eval           H1RIGA = 3
181900070528     C  n12              eval           H1COLO = 17
182000070321     C   12              eval           H1RIGA = 7
182100070321     C   12              eval           H1COLO = 2
182200070321      *
182300070321     C*------------------------
182400070321      *  se tutto OK
182500070321     c                   if        c1ccgi <> d1ccgi
182600070321     c                   move      c1ccgi        d1ccgi
182700070321     c                   move      *on           $inzs1
182800070321     c                   move      'S1'          $GEST
182900070321     c                   end
183000070321      *
183100070321     C*------------------------
183200070321      *
183300070321     c     endc1         tag
183400070321     C                   ENDSR
183500070321      *---------------------------------------------------------------*
183600070321      *?  CONTROLLO OPZIONI LISTA   ?
183700070321      *---------------------------------------------------------------*
183800070321     C     CTRS1         BEGSR
183900070321     C*
184000070321     C                   MOVEL     *OFF          $ESCI
184100070321     C                   SETOFF                                       99
184200070321     C*
184300070321     C* Leggo il sfl solo se ci sono rcd
1844000703211    C     WMAX          IFGT      0
184500070321     C                   READC     DG31SF1                                21
184600070321     C*
184700070321     C* esce se fine sfl o errore che richiede l'uscita
1848000703212    C     *IN21         DOWEQ     *OFF
184900070321     C     $ESCI         ANDEQ     *OFF
185000070321     C                   Z-ADD     S1NRR         C1RCD
185100071026     c                   eval      Che_NRec_sfl = s1nrr
185200070321     C* ctrl su riga
185300070321     C                   EXSR      RECS1
185400070321     C*
185500070321     C* gestione opzioni
1856000704043    C     s1SCE         IFNE      *blank
185700070321     C     *IN99         ANDEQ     *OFF
185800070321     C                   EXSR      SCE_S1
1859000703213-   C                   ENDIF
186000070321      *
186100070321     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1862000703213    C     *IN99         IFEQ      *ON
186300070321     C     $ESCI         OREQ      *ON
186400070321     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
186500070321     C* la ripristinerò a conclusione del ciclo di READC
186600070321     C                   MOVE      *OFF          $INZS1
1867000703213-   C                   ENDIF
186800070321     C*
186900070321      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
187000070404     c                   if        s1SCE  <> *blank
187100070321     C******             MOVE      *ON           *IN32
187200070404     c                   clear                   s1SCE
187300070321     c                   end
187400070510      *
187500070510     c                   setoff                                       40
187600070510if  4c                   if        h1prgg = 1
187700070510     c                   seton                                        40
187800070510     c                   end
187900080222     c                   eval      *in14 = h1in14
188000070510      *
188100070321     C                   UPDATE    DG31SF1
188200070321     C*
188300070321     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1884000703213    C     $ESCI         IFEQ      *OFF
188500070321     C                   READC     DG31SF1                                21
188600070321      *
188700070321     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1888000703214    C     *IN21         IFEQ      *ON
188900070321     C* calcolo pagina a cui deve posizionarsi
189000070321     c                   z-add     s1pag         Wpagine
189100070321     C                   EXSR      CLCPAG
1892000703214-   C                   ENDIF
1893000703213-   C                   ENDIF
189400070321     C*
1895000703212-   C                   ENDDO
189600071026     C*
189700071026      * -----------------------
189800071026      * ? calcola "su quale record" riposizionarsi tenendo conto che
189900071026      * ?  il SFL ha una testata di 6 righe ed inizia dalla 7°.
190000071026     c     Che_NRec_sfl  div       s1pag         Su_Quale_rec
190100071026     c                   mvr                     resto
190200071026     C     Resto         IFeq      0
190300071026     C                   z-add     16            Su_Quale_rec
190400071026     C                   Else
190500071026     C                   z-add     resto         Su_Quale_rec
190600071026     C                   ENDIF
190700071026      * ?  aggiunge le 6 righe di testata.
190800071026     C                   add       6             Su_Quale_rec
190900071026      * -----------------------
191000070321     C*
1911000703211-   C                   ENDIF
191200070321     C*
191300070321     C                   ENDSR
191400070321     C/EJECT
191500070321      *---------------------------------------------------------------*
191600070321      *?  CONTROLLO CAMPI I/O RIGA LISTA  ?
191700070321      *---------------------------------------------------------------*
191800070321     C     RECS1         BEGSR
191900070321     C*
192000070321     C* reset indicatori DSPATR
192100070321     c                   exsr      INZ_indERR
192200070528     C*
192300070528     C* se non Richiamato non sono abilitate le opzioni 2 e 3
192400070528     c                   if        pgm_da_call <> 'S'
192500070528     c                   if        s1sce = '2' or s1sce = '3'
192600070528     c                   seton                                        99  41
192700070528     c                   end
192800070528     c                   end
192900070528     C* Se richiamato in assegnazione non deve accettare opz 2,3
193000070620     c                   if        (pgm_da_call =  'S' and fdg31_opz = '1' and
193100080715     c                             fdgPGM = 'ASSMANUALE')
193200080715     c                             or
193300080715     c                             (fdgPGM<>'ASSMANUALE' and fdgPGM <>'MONITOR')
193400070606     c                   if        s1sce = '2' or s1sce = '3'
193500070528     c                   seton                                        99  41
193600070528     c                   end
193700070528     c                   end
193800070321     C*
193900070620     c                   if        pgm_da_call =  'S' and fdg31_opz <> '1' and
194000080715     c                             (fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR')
194100070620     c                   if        s1sce = '1'
194200070620     c                   seton                                        99  41
194300070620     c                   end
194400070620     c                   end
194500080624      * se la filiale non è partita con il PDA non si può chiedere di
194600080624      * vedere le Fasi della distinta
194700080725     c*******            if        s1sce = 'F' and not *in15 or
194800080725     c*******                      s1sce = 'F' and S1CPDA = 'N' and
194900080725     c*******                      %subst(s1stp:3:1)= ' '
195000080725     c*******            seton                                        99  41
195100080725     c*******            end
195200070620     C*
195300070321     C                   ENDSR
195400070321      *---------------------------------------------------------------*
195500070321      *?  GESTIONE OPZIONI LISTA       ?
195600070321      *---------------------------------------------------------------*
195700070321     C     SCE_S1        BEGSR
195800070321     C*
195900070321     C* determino Riga/Colonna del cursore
196000070321     C                   MOVE      NRG           R$$
196100070321     C                   MOVE      NCL           C$$
196200070321     C                   Z-ADD     RIRI          CURR
196300070321     C                   Z-ADD     COCO          CURC
196400070321     C*
196500070404     c                   If        s1SCE  = '1'
196600070405     c                   if        pgm_da_call <> *Blank
196700070404     c                   move      *on           $ESCI
196800070404     c                   move      *on           $FINE
196900070404     c                   z-add     s1CNFV        FDGNFV
197000070528     c                   move      d1cnpg        FDGNPG
197100070528     c                   eval      fdgOPZ = s1SCE
197200070405     c                   else
197300070405     c                   z-add     s1CNFV        d1cNFV
197400070405     c                   move      'D1'          $GEST
197500070405     c                   end
197600070405     c                   end
197700070321      *
197800070528     c                   If        s1SCE  = '2' or s1SCE  = '3'
197900070528     c                   if        pgm_da_call <> *Blank
198000070528     c                   move      *on           $ESCI
198100070528     c                   move      *on           $FINE
198200070528     c                   z-add     s1CNFV        FDGNFV
198300070528     c                   move      d1cnpg        FDGNPG
198400070528     c                   eval      fdgOPZ = s1SCE
198500070528     c                   clear                   fdgCGI
198600070528     c                   end
198700070528     c                   end
198800070528      *
198900070404     c                   If        s1SCE  = '5'
199000070404      *
199100080701      * Se la distinta ha delle Fasi da mostrare -> abilita il tasto
199200080701      *  funzionale per poter richiamare la window di visualizzazione
199300080701     c                   move      *oFf          d2fasi
199400080704     c                   if        %subst(s1stp:3:1)='*'
199500080701     c                   move      *oN           d2fasi
199600080701     c                   end
199700080701      *
199800070321     c                   z-add     s1CNFV        d1CNFV
199900070321     c                   z-add     c1cfgs        d1CFGS
200000070321     c                   z-add     s1CNFV        wnfv
200100070330     c                   move      'D2'          $GEST
200200070330     C                   move      'S'           $VISD2
200300070330     c                   move      *on           $INZD2
200400070321      *
200500070321     c                   end
200600070321     C*
200700070621     c                   If        s1SCE  = '7'
200800070427     c                   clear                   fidg35ds
200900070528     c                   eval      D35FGSi = wfgs
201000070528     c                   eval      D35DDCi = H1CDFV
201100070528     c                   eval      D35NDCi = S1CNFV
201200070528     c                   eval      D35NPGi = wnpg
201300070831     c                   eval      D35PGMi = fdgPGM
201400070903      * si vuole fare vedere tutto
201500070903     c*****              eval      D35TPi  = 'C'
201600070905     c                   If        pgm_da_call <> *blank and FDGOPZ <> '1' and
201700080715     c                             (fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR')
201800070528     c                   eval      D35opzi = 'S'
201900070528     c                   end
202000070427     c                   eval      kpjbus = kpjbu
202100070427     c                   movel(p)  fidg35ds      kpjbu
202200070427     c                   call      'FIDG35R'
202300070427     c                   parm                    kpjba
202400070427     c                   movel     kpjbu         fidg35ds
202500070427     c                   eval      kpjbu = kpjbus
202600070611     c                   if        pgm_da_call <> *Blank and fdgOPZ<>'1'
202700070528     c                   move      *on           $ESCI
202800070905     c                   if        d35FunO = '03'
202900070528     c                   move      *on           $FINE
203000070905     c                   end
203100070905     c                   if        d35opzO = '2' or d35opzO = '3'
203200070528     c                   z-add     s1CNFV        FDGNFV
203300070905     c                   end
203400070528     c                   move      d1cnpg        FDGNPG
203500070528     c                   eval      fdgOPZ = d35opzO
203600070528     c                   eval      fdgCGI = d35cgiO
203700070528     c                   end
203800070427     c                   end
203900070427     C*
204000070404     c                   If        s1SCE  = '9'
204100070321     c                   z-add     s1CNFV        wnfv
204200070321     c*******            move      *on           $ESCI
204300070321     c                   move      'D2'          $GEST
204400070330     C                   move      'N'           $VISD2
204500070321     c                   move      *on           $INZD2
204600070321     C                   exsr      GESD2
204700070321     c                   endIf
204800080624      *
204900080624      *  Visionare le Fasi della Distinta
205000080624     c                   If        s1SCE  = 'F'
205100080624     c                   write     FRCDTA
205200080624      *
205300080625     c                   clear                   fidg39dS
205400080624     c                   move      d1CNPG        fdg39NPG
205500080624     c                   z-add     s1CNFV        fdg39NFV
205600080624     c                   z-add     c1cfgs        fdg39FGS
205700080625     c                   move      'N'           fdg39noREC
205800080625     c                   clear                   fdg39VIDno
205900080624     c                   eval      kpjbus = kpjbu
206000080624     c                   movel(p)  fidg39ds      KPJBU
206100080624     c                   call      'FIDG39R'
206200080624     c                   parm                    kpjba
206300080624     c                   eval      kpjbu = kpjbus
206400080624      *
206500080624     c                   end
206600070321     C*
206700080820      *  Dettaglio Distinta
206800080820     c                   If        s1SCE  = 'D'
206900080820     c                   write     FRCDTA
207000080820      *
207100080820     c                   clear                   fidg311dS
207200080820     c                   z-add     c1cfgs        f31FGS
207300080821     c                   z-add     Wnpg          f31Npg
207400080820     c                   z-add     s1CNFV        f31NumDis
207500080820     c                   z-add     h1CDFV        f31DatDis
207600080820     c                   move      d1Dati        f31PriDati
207700081111     c**********         if        D1CPDA <> 'C' and D1CPDA <> 'O'
207800081111     c**********         move      'E'           f31DatiVis
207900081111     c**********         else
208000080820     c                   move      D1CPDA        f31DatiVis
208100081111     c**********         end
208200080821      * x gestione test/parallelo
208300080821     c                   if        Test_parallelo = 'S' and test_Paral_ORM = 'S'
208400080826     c******             move      'E'           f31Test
208500080821     c                   elseIf    Test_parallelo = 'S' and test_Paral_ORM <>'S'
208600080826     c******             move      'C'           f31Test
208700080821     c                   elseIf    Test_parallelo <>'S' and test_Paral_ORM = 'S'
208800080826     c******             move      'R'           f31Test
208900080821     c                   end
209000080821
209100080820     c                   eval      kpjbus = kpjbu
209200080820     c                   movel(p)  fidg311ds     KPJBU
209300080820     c                   call      'FIDG311R'
209400080820     c                   parm                    kpjba
209500080821     c                   eval      fidg311ds =   KPJBU
209600080821     c                   if        F31Tfunz = '03'
209700080821     c                   move      *on           $ESCI
209800080821     C                   EXSR      F03_exit
209900080821     c                   end
210000080820     c                   eval      kpjbu = kpjbus
210100080820      *
210200080820     c                   end
210300080820     C*
210400070321     C                   ENDSR
210500070321     C/EJECT
210600070321      *---------------------------------------------------------------*
210700070321      *?  GESTIONE SFL 2                       ?
210800070321      *---------------------------------------------------------------*
210900070321     C     GESS2         BEGSR
211000070321     C*
211100070321     C* inizializzazione videata
211200070321     C     $INZS2        IFEQ      *ON
211300070321     C                   EXSR      INZS2
211400070321     C                   MOVE      *OFF          $INZS2
211500070321     C                   ENDIF
211600070321     C*
211700070321     C* Inizializza e imposta piede videata
211800070321     C                   EXSR      INZZ2
211900070321     C* emissione piede videata
212000070321      * ?            *-----------------------*
212100070321     C                   WRITE     DG31Z02
212200070321      * ?            *-----------------------*
212300070321     C*
212400070321     C     WMAX          IFEQ      0
212500070321     C* Non ci sono records
212600070321      * ?            *-----------------------*
212700070321     C                   WRITE     DG31V02
212800070321      * ?            *-----------------------*
212900070321     c                   setoff                                       30
213000070321     C                   Else
213100070321     c                   seton                                        30
213200070321      *
213300070321     C     Wsfl          IFgt      0
213400070321     C                   Z-ADD     wsfl          C2RCD
213500070321     C                   Else
213600070321     C     Wpag          IFgt      0
213700070321     C                   Z-ADD     wpag          C2RCD
213800070321     C                   EndIF
213900070321     C                   EndIF
214000070321     C                   ENDIF
214100071029      * se si deve riposizionare
214200071029     c                   if        nrec_riposiz > 0
214300071029     C                   Z-ADD     nrec_riposiz  C2RCD
214400071029     C                   EndIF
214500071026      * -----------------------
214600071026      * ?  reimposta su quale riga deve posizionarsi il cursore
214700071026      * ?    se non si è chiesto l'ordinamento
214800071026     C                   if        $TASTO   <>  F11
214900071026     C                   z-add     Su_Quale_rec  h1riga
215000071026     C                   EndIF
215100071026      * -----------------------
215200080703      * solo se richiamato dal MONITOR e solo la prima volta
215300080703     c                   if        fdgpgm = 'MONITOR' and Apri_DROP ='S'
215400080703     c                   clear                   Apri_DROP
215500080703     c                   eval      *in16 = *ON
215600080703     c                   end
215700070321      *
215800070321      *  Emissione SFL dei GIRI
215900070321     c                   setoff                                       99
216000070321      * ?            *-----------------------*
216100070321     C                   EXFMT     DG31CT2
216200070321      * ?            *-----------------------*
216300070321     C                   movel     'S2'          Sav_$GEST
216400071026      *
216500070321     C     C2NRR         IFNE      0
216600070321     C                   Z-ADD     C2NRR         WSFL
216700070321     C                   Else
216800070321     C     SFLNRR        IFgt      0
216900070321     C                   Z-ADD     SFLNRR        WSFL
217000070321     c                   EndIF
217100070321     C                   ENDIF
217200070321     C                   Z-ADD     SFLNRR        C2RCD
217300070321     C* Selezioni
2174000703211    C                   SELECT
217500070321     C* F11=Cambia Ordinamento del SFL
217600070321     C                   WHEN      $TASTO   =   F11
217700160505     c                   if        sav_c2aut <> *blank or
217800160505     c                             sav_c2nfv  > 0
217900160505     c                   move      *on           $inzs2
218000160505     C                   clear                   C2DAUT
218100160505     C                   clear                   C2CNFV
218200160505     C                   clear                   sav_c2nfv
218300160505     C                   clear                   sav_c2aut
218400160505     c                   else
218500070322     C                   EXSR      F11_ordina2
218600160505     c                   end
218700070321     C* F3=Fine
218800070321     C     $TASTO        WHENEQ    F03
218900080606     c                   move      '03'          fdgtfu
219000070321     C                   EXSR      F03_exit
219100070321     c                   goto      end_s02
219200070321     C* F12=Ritorno
219300070321     C     $TASTO        WHENEQ    F12
219400070321     c                   move      'D1'          $GEST
219500070326     c                   clear                   d1ccgi
219600070321     C*    deve andare a fine se richiamato
219700070321     c                   if        pgm_da_call = 'S'
219800070326     c                   clear                   fdgcgi
219900070326     c                   clear                   fdgnfv
220000071008     c                   clear                   fdgopz
220100071008     c                   move      '12'          fdgtfu
220200070321     C                   EXSR      F03_exit
220300070321     c                   end
220400070321     c                   goto      end_s02
220500070321      *
2206000703211O   C                   OTHER
220700070321     C* CONTROLLO testata
220800070321     C                   EXSR      CTRC2
220900070321     C* CONTROLLO subfile
221000070321     C     *IN99         IFEQ      *OFF
221100070321     C                   EXSR      CTRS2
221200070321
221300070321     C                   END
221400070321      *
221500070321      * ?Emette la videata di Dettaglio Distinta
221600070321     C     *IN99         IFEQ      *OFF
221700070321     C********           MOVE      *ON           $INZD2
221800070321     C********           movel     'D2'          $GEST
221900070321     C                   END
222000070321      *
2221000703211-   C                   ENDSL
222200070321     C*
222300070321     c     end_s02       tag
222400070321     C                   ENDSR
222500070321     C/EJECT
222600070321      *---------------------------------------------------------------*
222700070321      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL    ?
222800070321      *---------------------------------------------------------------*
222900070321     C     INZZ2         BEGSR
223000070321     C*
223100070321     C*
223200070321     C                   ENDSR
223300070321     C/EJECT
223400070321      *---------------------------------------------------------------*
223500070321      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare  ?
223600070321      *---------------------------------------------------------------*
223700070321     C     INZS2         BEGSR
223800070321     C*
223900160505     C* reset indicatori DSPATR   10 -> ord.x Turno inesistente
224000160505     c                   setoff                                         10
224100160505     c                   eval      sav_in10 = *in10
224200070321     c                   exsr      INZ_indERR
224300070321     C* pulizia SFL
224400070321     C                   SETOFF                                         3031
224500070321     C                   WRITE     DG31CT2
224600070321     C                   SETON                                          31
224700160601     C                   SETON                                          11
224800160601     c                   eval      sav_in11 = *in11
224900070321      *
225000070321     C* Posiziona il cursore in testata x 12 = *on
225100070321     c                   if        cur_su_SFL = 'N'
225200070321     c     $PcurS2       comp      *on                                    12
225300070321     C* imposto pos. del cursore
225400070321     C                   eval           H1RIGA = 3
225500070528     C                   eval           H1COLO = 17
225600070321     c                   else
225700070321     c                   move      'N'           cur_su_SFL        1
225800070321     c                   setoff                                           12
225900070321     C* imposto pos. del cursore
226000070321     C                   eval           H1RIGA = 7
226100070321     C                   eval           H1COLO = 2
226200070321     c                   end
226300071026     C* se siamo al caricamento del SFL si deve posizionare sulla prima riga disponibile
226400071026     c                   z-add     1             Che_NRec_sfl
226500071026     C                   z-add     h1riga        Su_Quale_rec
226600071026     C                   move      'S'           prima_volta
226700080219     c                   eval      *in15 = sav_in15
226800071026     C*
226900070321     C* Se richiamato attivo 04
227000070528     C*   se però è richiamato da Assegnazione attivo 05
227100070528     c                   setoff                                       05
227200080625     c                   move      *off          sav_in05
227300170508     c                   move      *off          sav_in04
227400080625     C*
227500070321     c                   if        pgm_da_call = 'S'
227600070321     C                   SETON                                          04
227700070404     c                   move      *on           sav_in04
227800080715     c                   if        fdgpgm ='ASSMANUALE' or fdgPGM = 'MONITOR'
227900070620     C                   SETON                                          03
228000070620     c                   move      *on           sav_in03
228100070620     c                   end
228200070528     c                   if        fdgOPZ = '1'
228300070528     C                   SETON                                          05
228400070528     c                   move      *on           sav_in05
228500070528     c                   end
228600070321     c                   end
228700070321     C*
228800070321     C* Righe x Pagina del 1° SFL
228900071026     c                   z-add     15            s2pag
229000070321      *
229100070321     C* CARICAMENTO SFL totale
229200070321     C                   Z-ADD     0             S2NRR
229300070321     C                   Z-ADD     1             C2RCD
229400070321     C                   Z-ADD     0             WMAX
229500070321     C                   Z-ADD     0             Wsfl
229600070321     C* campi titolo
229700070321     C                   MOVEL     TES(1)        c2CTES
229800080715     c                   if        fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR'
229900070606     C                   MOVEL(p)  TES(2)        c2CTES
230000070606     c                   end
230100070903
230200070830     c                   SeLect
230300070903
230400070830     c                   whEn      d1dati= ' '
230500070830     C                   MOVEL(p)  TES(7)        c2TES2
230600070903     C                   MOVEL(p)  'Sped.'       c2DesN
230700070903
230800070830     c                   whEn      d1dati= 'C'
230900070830     C                   MOVEL(p)  TES(5)        c2TES2
231000070903     C                   MOVEL(p)  'Sped.'       c2DesN
231100070903
231200070830     c                   whEn      d1dati= 'R'
231300070830     C                   MOVEL(p)  TES(6)        c2TES2
231400070903     C                   MOVEL(p)  ' Orm.'       c2DesN
231500070903
231600070830     c                   endSL
231700070903
231800070321     C                   MOVEL     dsPGM         c2CPGM
231900070321     C                   MOVEL     RAGUT         c2CDSA
232000070321     C                   MOVEL     knsif         c2nsif
232100070321     C                   MOVEL     knmus         c2nmus
232200070321     C                   z-add     d1CFGS        c2cfgs
232300070321     C                   movel     d1DFGS        c2dfgs
232400070321     C                   movel     d1ccgi        c2ccgi
232500070321     C*
232600070321     C* Posizionamento su file pilota e impostazione primo record da leggere
232700070321     c                   clear                   Prima_Lettura     1
232800070321      *
232900070406     c                   if        Solo_aperte = 'N'
233000070321     c     Kdat1         setll     fidst008
233100070406     c                   else
233200070321     C     Kdst3         SETLL     Fidst003
233300070406     c                   End
233400070321     C* Carico il SFL
233500070321     C                   EXSR      ROLS2
233600070321     C*
233700070321     C                   Z-ADD     1             WPAG
233800070321     C*
233900070321     ** Al primo F11 ordino per Autista
234000070321     C                   EVAL      RrnLast = S2Nrr
234100160504     C*
234200160505     c                   if        sav_in10 = *off
234300160504     C*
234400160601     c                   if        sav_in11 = *off
234500070417     C                   EVAL      WrkSort = Numero
234600070417     c                   exsr      F11_ordina2
234700160601     c                   setoff                                       11
234800160601     c                   elseIF    sav_in11 = *on
234900070321     C                   EVAL      WrkSort = Autista
235000070322     c                   exsr      F11_ordina2
235100070321     c                   end
235200160504     C*
235300160505     c                   elseif    sav_in10 <>*off
235400160504     C                   EVAL      WrkSort = Turno
235500160504     c                   exsr      F11_ordina2
235600160504     c                   end
235700160504     C*
235800070321     C                   ENDSR
235900070321     C/EJECT
236000070321      *---------------------------------------------------------------*
236100070321      *?  CARICAMENTO PAGINA da SFL dei GIRI             ?
236200070321      *---------------------------------------------------------------*
236300070321     C     ROLS2         BEGSR
236400070321     C*
236500070321     C                   SETOFF                                       32
236600070321     C                   Z-ADD     0             Y
236700070321     C                   Z-ADD     WMAX          S2NRR
236800070321     C*
236900070321     C* Caricamento del Sfl x Giro
237000070321     c                   Exsr      Write_S2
237100070321     C* Fine Caricamento
237200070321     C*
237300070321     C                   Z-ADD     S2NRR         WMAX                 30
237400070321     C*
237500070321     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
237600070321     C     S2NRR         DIV       S2pag         PAGINE            4 0
237700070321     C                   MVR                     RESTO             3 0
237800070321     C     PAGINE        MULT      S2pag         C2RCD
2379000703211    C     RESTO         IFGT      0
238000070321     C                   ADD       1             C2RCD
2381000703211E   C                   ELSE
238200070321     C                   SUB       S2pag         C2RCD
238300070321     C                   ADD       1             C2RCD
2384000703211-   C                   ENDIF
238500070321     C*
238600070321     C                   ENDSR
238700070321      *---------------------------------------------------------------*
238800070321      *?  Scrive il SFL       ?
238900070321      *---------------------------------------------------------------*
239000070321     C     Write_S2      BEGSR
239100071029     C*
239200071029     C* Pulisce il Nrecord di riposizionamento se richiesto da pgm chiamante
239300071029     c                   clear                   NRec_riposiz
239400071029     C*
239500070321     c                   exsr      Legge_rec
239600070321     C*
2397000703211    C     $EFILE        DowEQ     *OFF
239800070321     C*
239900070321     C                   clear                   DG31SF2
240000070404     c                   move      sav_in04      *in04
240100070528     c                   move      sav_in05      *in05
240200080219     c                   move      sav_in15      *in15
240300160506     c                   move      sav_in38      *in38
240400070510     C* Priorità
240500080715     C*    Consegne/Ritiri/Entrambe
240600080715     c                   if        d1dati = 'C'
240700080715     C*****              z-add     dst3prgg      h2prgg
240800080715     c                   z-add     prior_giroCon h2prgg
240900080715      *
241000080715     c                   elseIf    d1dati = 'R'
241100080715     c                   z-add     prior_giroRit h2prgg
241200080715      *
241300080715     c                   end
241400080715      *
241500070510     C                   z-add     dst3sql       h2sql
241600070321     C* GIRO DATA Distinta
241700070417     C                   MOVE      dstDFV        h2CDFV
241800070321     C                   MOVE      dstDFV        G02INV
241900070321     C                   MOVEL     '3'           G02ERR
242000070321     C                   CALL      'XSRDA8'
242100070321     C                   PARM                    WLBDAT
242200070321     C*
242300070321     C                   movel     G02DAT        campo4            4
242400070321     C                   move      G02DAT        s2CDFV
242500070321     C                   movel     campo4        s2CDFV
242600070321      *
242700070321     C                   Z-ADD     dstNFV        s2CNFV
242800070321     c     kapd          chain     fiapd01l
242900070321     c                   if        %found(fiapd01l)
243000070321     c                   eval      s2DAUT = apdrsc
243100070321     c                   else
243200070321     c                   clear                   s2DAUT
243300070321     c                   end
243400070321      *
243500070904     c                   if        d1dati= ' ' or d1dati= 'C'
243600070904     c                   z-add     DSTnbla       s2nbla
243700070419     c                   z-add     DSTnftl       s2nftl
243800070904     c                   z-add     DSTstpa       s2stpa
243900070904     c                   z-add     DSTPKL        s2cpem
244000070321     c                   z-add     DSTPKLa       s2cpea
244100070904     c                   z-add     DSTVLU        s2cvom
244200070321     c                   z-add     DSTVLUa       s2cvoa
244300070904
244400070904     c*****              if        d1dati= ' '
244500070927     c                   if        Abilita_ORM = 'S'
244600070904     c                   z-add     DSTRnbla      s2nbla1
244700070904     c                   z-add     DSTRnftl      s2nftl1
244800070904     c                   z-add     DSTRstpa      s2stpa1
244900070904     c                   z-add     DSTRPKL       s2cpem1
245000070904     c                   z-add     DSTRPKLa      s2cpea1
245100070904     c                   z-add     DSTRVLU       s2cvom1
245200070904     c                   z-add     DSTRVLUa      s2cvoa1
245300070927     c                   end
245400070904     c*****              end
245500070904
245600070904     c                   else
245700070904      *
245800070904     c                   z-add     DSTRnbla      s2nbla
245900070904     c                   z-add     DSTRnftl      s2nftl
246000070904     c                   z-add     DSTRstpa      s2stpa
246100070904     c                   z-add     DSTRPKL       s2cpem
246200070904     c                   z-add     DSTRPKLa      s2cpea
246300070904     c                   z-add     DSTRVLU       s2cvom
246400070904     c                   z-add     DSTRVLUa      s2cvoa
246500070904
246600070904     c******             if        d1dati= ' '
246700070904     c                   z-add     DSTnbla       s2nbla1
246800070904     c                   z-add     DSTnftl       s2nftl1
246900070904     c                   z-add     DSTstpa       s2stpa1
247000070904     c                   z-add     DSTPKL        s2cpem1
247100070904     c                   z-add     DSTPKLa       s2cpea1
247200070904     c                   z-add     DSTVLU        s2cvom1
247300070904     c                   z-add     DSTVLUa       s2cvoa1
247400070904     c******             end
247500070904
247600070904     c                   end
247700070904      *
247800070927     c*******            if        s2nbla1 >0 or s2cpem1 >0 or s2cvom1 >0 or
247900070927     c*******                      s2nftl1 >0 or s2cpea1 >0 or s2cvoa1 >0 or
248000070927     c*******                      s2stpa1 >0
248100070904
248200070904     c                   if        d1dati= ' ' or d1dati= 'C'
248300070927     c                   if        Abilita_ORM = 'S'
248400070904     C                   MOVE (p)  'Ritiri'      s2DDes1
248500070904     C                   MOVEL(p)  'Nr.Orm.'     s2DesN1
248600070927     c                   end
248700070904     c                   else
248800070904     C                   MOVE (p)  'Consegne'    s2DDes1
248900070904     C                   MOVEL(p)  'Nr.Sped'     s2DesN1
249000070904     c                   end
249100070904
249200070927     c*******            end
249300070904
249400070905     c                   if        DSTfpp  ='M'
249500070905     c                   movel     'Mat'         s2Tusc
249600070905     c                   else
249700070905     c                   movel     'Pom'         s2Tusc
249800070905     c                   end
249900070820     c                   clear                   s2STP
250000070820     c                   if        DSTfasd ='STP'
250100080625     c                   movel     'St'          s2STP
250200070820     c                   endif
250300070321      *
250400080625      * Controlla se ci sono delle Fasi presenti da segnalare
250500080625     c                   clear                   fidg39dS
250600080625     c                   move      d1CNPG        fdg39NPG
250700080625     c                   z-add     s2CNFV        fdg39NFV
250800080625     c                   z-add     c2cfgs        fdg39FGS
250900080625     c                   move      'N'           fdg39VIDno
251000080625     c                   eval      kpjbus = kpjbu
251100080625     c                   movel(p)  fidg39ds      KPJBU
251200080625     c                   call      'FIDG39R'
251300080625     c                   parm                    kpjba
251400080625     c                   eval      fidg39ds = KPJBU
251500080625     c                   eval      kpjbu = kpjbus
251600080625      * se ci sono delle Fasi da vedere imposta l'asterisco nel primo Byte
251700080625      *  del flag di stampato.
251800080625     c                   if        fdg39FASI = 'S'
251900080625     c                   move      '*'           s2STP
252000080625     c                   endif
252100080613      *
252200070321     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
252300070321     c                   if        *in08
252400070321     c                   if        dsttpm='M'
252500070321     c                   movel     'Mes'         s2CTPM
252600070321     c                   endif
252700070321     c                   if        dsttpm<>'M'
252800070321     c                   movel     'Prc'         s2CTPM
252900070321     c                   endif
253000070321     c                   endif
253100160503     c** dal filler
253200160503     C                   movel     dstflr        DDStflr
253300160503     c* x il TURNO
253400160503     c                   if        *in38
253500160505     c                   if        dsttpm <> 'M'
253600160505     c                   movel     'Turno'       s2turno
253700160505     c                   endif
253800160505     c                   movel     §DSTTURNO     s2CTUR
253900160504     c                   if        dsttpm <> 'M' and
254000160505     c                             s2CTUR = *blank
254100160504     c                   eval        s2CTUR = '1'
254200160504     c                   endif
254300160503     c                   endif
254400070321      * Flag tipo distinta (solo consegna, orm o entrambi)
254500070321     C                   CLEAR                   WBOL
254600070321     C                   CLEAR                   WORM
254700070321     c                   Z-Add     d1CFGS        Wifp
254800070321     C                   Z-ADD     s2CNFV        kfnvv
254900070321     C                   Z-ADD     s2CNFV        WnFV
255000070321     c     Karb78        Setll     Fnarb78l
255100070321if  5c                   if        %equal(fnarb78l)
255200070321     c                   movel     'S'           wbol              1
255300070321x   5c                   else
255400070321     C     Kdst          SETLL     FnBRV01L
255500070321     c                   if        %equal(fnbrv01l)
255600070321     c                   movel     'S'           wbol              1
255700070321     c                   endif
255800070321e   5c                   endif
255900070321     C     KORM          SETLL     FNORM06L
256000070321do  5c                   do        *hival
256100070321     C     KORM          reade     FNORM06L
256200070321     c                   if        %eof(fnorm06l)
256300070321     c                   leave
256400070321     c                   endif
256500070321     c                   if        ormatb = *blanks and ormfao < 999
256600070321     C                   move      'S'           worm              1
256700070321     c                   endif
256800070321e   5c                   enddo
256900070321     C                   if        wbol = 'S' and worm <> 'S'
257000070321     C                   movel     'CON'         s2CTPD
257100070321     C                   movel     'TpD'         s2dTPD
257200070321     C                   endif
257300070321     C                   if        wbol <> 'S' and worm = 'S'
257400070321     C                   movel     'ORM'         s2CTPD
257500070321     C                   movel     'TpD'         s2dTPD
257600070321     C                   endif
257700070321     C                   if        wbol = 'S' and worm = 'S'
257800070321     C                   movel     'C/O'         s2CTPD
257900070321     C                   movel     'TpD'         s2dTPD
258000070321     C                   endif
258100070321      * Calcolo importi se non già fatto prima
258200070321if  5c                   if        d1CTFV = '2' and d1IMPO = 'S'
258300070321     c                   exsr      totalizzo
258400070321     c                   z-add     w_imas        s2IMAS
258500070321     c                   z-add     w_imca        s2IMCA
258600070321     c                   endif
258700070321      * Pda
258800080208     c                   setoff                                       14
258900080208     c                   if        dstDFV >= DateU8
259000080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
259100080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
259200080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
259300080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
259400080208     c                   seton                                        14
259500080208     c                   endIf
259600080208     c                   endIf
259700080222     c                   eval      h2in14 = *in14
259800080222      *
259900080208      *  lo Minuscolizza sempre a video
260000080207     C     Up:Lo         XLATE     §DSTtstPDA    s2CTSTPDA
260100080208      *
260200070321     C                   movel     dstPDA        s2CPDA
260300070419     C                   if        s2CPDA <> *blank
260400070419     C                   movel     'PDA'         s2DPDA
260500070419     c                   end
260600070321      * ANNULLATO
260700070321     C     dstATB        IFEQ      'A'
260800070321     C                   MOVEL     'ANN'         s2CANN
260900070321     C                   END
261000070321if  5C     dstFCF        IFNE      ' '
261100070321      * CHIUSO
261200070321     C     dstFCF        IFEQ      'S'
261300070821     C                   MOVEL     'Chiu'        s2CFCF
261400070321     C                   ELSE
261500070321     C* BLOCCATO
261600070322     C                   MOVEL     'BLoc'        s2CFCF
261700070321e   5C                   ENDIF
261800070321e   4C                   ENDIF
261900070904
262000070904      *   Se richiamato come Ritiri o Consegne o Entrambe
262100070321      *
262200070321      * Intestazioni seconda riga
262300070830     c                   setoFF                                       18
262400070321     c                   if        s2imca <> 0
262500070321     c                   movel     Imp_C_A       s2dica
262600070830     c                   setoN                                        18
262700070321     c                   end
262800070321      *
262900070830     c                   setoFF                                       20
263000070321     c                   if        s2imas <> 0
263100070321     c                   movel     Imp_ASS       s2dias
263200070830     c                   setoN                                        20
263300070321     c                   end
263400070321      *
263500070510      *  Illumina diversamente il campo x le distinte con priorità (1)
263600070510     c                   setoff                                       40
263700070510if  4c                   if        h2prgg = 1
263800070510     c                   seton                                        40
263900070510     c                   end
264000070321      *---------
264100070321if  4c                   if        FDGtfv <> '3'
264200160601     c                   if        (sav_c2nfv <= s2cnfv and *in11 = *off) or
264300160601     c                             (sav_c2aut <= s2daut and *in11 = *on ) or
264400070321     c                             (sav_c2aut = *blank and sav_c2nfv =0)
264500070404     C* Imposta indicatore x abilitare campo scelta
264600070404     c                   move      sav_in19      *in19
264700170508     c                   eval      h2in15 = *in15
264800170508     c                   eval      h2in18 = *in18
264900170508     c                   eval      h2in19 = *in19
265000170508     c                   eval      h2in20 = *in20
265100070321     c********           seton                                        32
265200071029      *
265300070321     C                   ADD       1             S2NRR
265400070321     C                   ADD       1             Y
265500071029      *
265600071029      *  se deve posizionare il cursore sul record se il pgm è
265700071029      *  chiamato per assegnare/disassegnare accende il PC sulla scelta
265800071029      *  solo se ha il nr. Foglio di riferimento per il posizionamento sfl.
265900071029     c                   if        fdgNFpos <> *blank
266000071029     c                   move      s2cNFV        sav2NFV           6
266100071029     c                   if        fdgNFpos = sav2NFV
266200071029     c                   seton                                        42
266300071029     c                   move      'X'           H2recPos
266400071029     c                   end
266500071029     c                   end
266600071029      *
266700070321     C                   WRITE     DG31SF2
266800071029     c                   setoFF                                       42
266900070321     c                   end
267000070321e   4c                   endif
267100070321      *---------
267200070321      *  Esce da qui con un record di Totale x GIRO
267300070321     c                   exsr      Legge_rec
267400070321     c                   EndDO
267500070321     C*
267600070321     C                   ENDSR
267700070321      *---------------------------------------------------------------*
267800070321      *?  CONTROLLO TESTATA LISTA    ?
267900070321      *---------------------------------------------------------------*
268000070321     C     CTRC2         BEGSR
268100070321      *
268200070321     C                   MOVE      *OFF          *IN99
268300070321      *
268400070321     C*------------------------
268500070321      *  interrogazione Giro
268600070321     c                   clear                   c2dcgi
268700070321     c     '?'           scan      c2cCGI
268800070321     C*   ricerca codice giro
268900070321     c                   if        %Found
269000070321     c                   clear                   fidg09ds
269100070321     c                   move      'P01'         d09iop0
269200070321     c                   movel     c2cFGS        d09ifgs
269300071010     c****               movel     'C'           d09itug
269400071010     c                   movel     d1DATI        d09itug
269500070321     c                   z-add     DATEU8        d09idat
269600070321     c                   eval      kpjbus = kpjbu
269700070321     c                   movel(p)  fidg09ds      kpjbu
269800070321     c                   call      'FIDG09R'
269900070321     c                   parm                    kpjba
270000070321     c                   movel     kpjbu         fidg09ds
270100070321     c                   eval      kpjbu = kpjbus
270200070321     c                   movel     d09ocgi       c2cCGI
270300070321     c                   end
270400070321     C*------------------------
270500070321      *  controllo validità giro
270600070321     c                   if        c2cCGI <> *blank
270700070321     c                   clear                   fidg09ds
270800070321     c                   eval      D09IOP0 = '001'
270900070321     c                   eval      d09ifgs = c2cFGS
271000071010     c*****              eval      d09itug = 'C'
271100071010     c                   eval      d09itug = d1DATI
271200070321     c                   eval      d09icgi = c2cCGI
271300070321     c                   move      dateu8        d09idat
271400070321     c                   eval      kpjbus = kpjbu
271500070321     c                   eval      kpjbu = fidg09ds
271600070321     c                   call      'FIDG09R'
271700070321     c                   parm                    kpjba
271800070321     c                   eval      fidg09ds = kpjbu
271900070321     c                   eval      kpjbu = kpjbus
272000070321     c                   if        d09oerr = '1'
272100070321     C                   SETON                                        54  99
272200070321     C                   goto      ENDC2
272300070321     c                   else
272400070321     c                   if        d09ocgi <> ' '
272500070321     c                   eval      c2dcgi = d09odes
272600070321     c                   move      'S'           con_giro
272700070321     c                   end
272800070321     c                   end
272900070321      *
273000070321     c                   end
273100070321     C*------------------------
273200070321      *  interrogazione AUT
273300070321      *
273400070321     c     '?'           scan      c2daut
273500070321     C*   ricerca codice giro
273600070321     c                   if        %Found
273700070321     c                   clear                   fnlv24ds
273800070321     C                   Z-ADD     c2cFGS        d24FIL
273900070321     C                   MOVEL     'R'           d24FLG
274000070321     C                   MOVEL     'A'           d24tip
274100070321     c                   eval      kpjbus = kpjbu
274200070321     C                   MOVEL(p)  fnlv24ds      KPJBU
274300070321     C                   CALL      'FNLV24R'
274400070321     C                   PARM                    KPJBA
274500070321     C                   MOVEL     KPJBU         fnlv24ds
274600070321     c                   eval      kpjbu = kpjbus
274700070321     C                   MOVEL     D24RSC        c2daut
274800070321     c                   end
274900070321     C*
275000070321     C*------------------------
275100070321     c                   move      *on           $PcurS2
275200070321     C* Riposizionamento x AUT o x Numero
275300160601     c                   if        (*in11 = *off and c2cnfv <> sav_c2nfv) or
275400160601     c                             (*in11 = *on  and c2daut <> sav_c2aut)
275500070321     c                   move      *off          $PcurS2
275600070321     c                   move      *on           $inzs2
275700070321     c                   move      'S2'          $GEST
275800160601     c   11              movel     c2daut        sav_c2aut
275900160601     c  n11              z-add     c1cnfv        sav_c1nfv
276000070321     c                   end
276100070321     C*
276200070321     C* Posiziona il cursore in testata x 12 = *on
276300070321     c     $PcurS2       comp      *on                                    12
276400070321     C  n12              eval           H1RIGA = 3
276500070528     C  n12              eval           H1COLO = 17
276600070321     C   12              eval           H1RIGA = 7
276700070321     C   12              eval           H1COLO = 2
276800070321      *
276900070321     C*------------------------
277000070321      *  se tutto OK
277100070321     c                   if        c2ccgi <> d1ccgi
277200070321     c                   move      c2ccgi        d1ccgi
277300070321     c                   move      *on           $inzs2
277400070321     c                   move      'S2'          $GEST
277500070321     c                   end
277600070321      *
277700070321     C*------------------------
277800070321      *
277900070321     c     endc2         tag
278000070321     C                   ENDSR
278100070321      *---------------------------------------------------------------*
278200070321      *? CONTROLLO OPZIONI LISTA     ?
278300070321      *---------------------------------------------------------------*
278400070321     C     CTRS2         BEGSR
278500070321     C*
278600070321     C                   MOVEL     *OFF          $ESCI
278700070321     C                   SETOFF                                       99
278800070321     C*
278900070321     C* Leggo il sfl solo se ci sono rcd
2790000703211    C     WMAX          IFGT      0
279100070321     C                   READC     DG31SF2                                21
279200071026     C*
279300070321     C* esce se fine sfl o errore che richiede l'uscita
2794000703212    C     *IN21         DOWEQ     *OFF
279500070321     C     $ESCI         ANDEQ     *OFF
279600070321     C                   Z-ADD     S2NRR         C2RCD
279700071026     c                   eval      Che_NRec_sfl = s2nrr
279800070321     C* ctrl su riga
279900070321     C                   EXSR      RECS2
280000070321     C*
280100070321     C* gestione opzioni
2802000704043    C     s2SCE         IFNE      *blank
280300070321     C     *IN99         ANDEQ     *OFF
280400070321     C                   EXSR      SCE_S2
2805000703213-   C                   ENDIF
280600070321      *
280700070321     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
2808000703213    C     *IN99         IFEQ      *ON
280900070321     C     $ESCI         OREQ      *ON
281000070321     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
281100070321     C* la ripristinerò a conclusione del ciclo di READC
281200070321     C                   MOVE      *OFF          $INZS2
2813000703213-   C                   ENDIF
281400070321     C*
281500070321      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
281600070404     c                   if        s2SCE  <> *blank
281700070321     C******             MOVE      *ON           *IN32
281800070404     c                   clear                   s2SCE
281900070321     c                   end
282000070510      *
282100070510     c                   setoff                                       40
282200070510if  4c                   if        h2prgg = 1
282300070510     c                   seton                                        40
282400070510     c                   end
282500070510      *
282600080222     c                   eval      *in14 = h2in14
282700070321     C                   UPDATE    DG31SF2
282800070321     C*
282900070321     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
2830000703213    C     $ESCI         IFEQ      *OFF
283100070321     C                   READC     DG31SF2                                21
283200070321      *
283300070321     C* a fine ciclo ripristino il flag richiesta iniz.sfl
2834000703214    C     *IN21         IFEQ      *ON
283500070321     C* calcolo pagina a cui deve posizionarsi
283600070321     c                   z-add     s2pag         Wpagine
283700070321     C                   EXSR      CLCPAG
2838000703214-   C                   ENDIF
2839000703213-   C                   ENDIF
284000070321     C*
2841000703212-   C                   ENDDO
284200071026     C*
284300071026      * -----------------------
284400071026      * ? calcola "su quale record" riposizionarsi tenendo conto che
284500071026      * ?  il SFL ha una testata di 6 righe ed inizia dalla 7°.
284600071026     c     Che_NRec_sfl  div       s2pag         Su_Quale_rec
284700071026     c                   mvr                     resto
284800071026     C     Resto         IFeq      0
284900071026     C                   z-add     15            Su_Quale_rec
285000071026     C                   Else
285100071026     C                   z-add     resto         Su_Quale_rec
285200071026     C                   ENDIF
285300071026      * ?  aggiunge le 6 righe di testata.
285400071026     C                   add       6             Su_Quale_rec
285500071026      * -----------------------
285600070321     C*
2857000703211-   C                   ENDIF
285800070321     C*
285900070321     C                   ENDSR
286000070321     C/EJECT
286100070321      *---------------------------------------------------------------*
286200070321      *? CONTROLLO CAMPI I/O RIGA LISTA  ?
286300070321      *---------------------------------------------------------------*
286400070321     C     RECS2         BEGSR
286500070321     C*
286600070321     C* reset indicatori DSPATR
286700070321     c                   exsr      INZ_indERR
286800070528     C*
286900070528     C* se non Richiamato non sono abilitate le opzioni 2 e 3
287000070528     c                   if        pgm_da_call <> 'S'
287100070528     c                   if        s2sce = '2' or s2sce = '3'
287200070528     c                   seton                                        99  41
287300070528     c                   end
287400070528     c                   end
287500070528     C* Se richiamato in assegnazione non deve accettare opz 2,3
287600070620     c                   if        (pgm_da_call =  'S' and fdg31_opz = '1' and
287700080715     c                             fdgPGM = 'ASSMANUALE')
287800080715     c                             or
287900080715     c                             (fdgPGM<>'ASSMANUALE' and fdgPGM<>'MONITOR')
288000070528     c                   if        s2sce = '2' or s2sce = '3'
288100070528     c                   seton                                        99  41
288200070528     c                   end
288300070528     c                   end
288400070620     C*
288500070620     c                   if        pgm_da_call =  'S' and fdg31_opz <> '1' and
288600080715     c                             (fdgPGM ='ASSMANUALE' or fdgPGM ='MONITOR')
288700070620     c                   if        s2sce = '1'
288800070620     c                   seton                                        99  41
288900070620     c                   end
289000070620     c                   end
289100080625      * se la filiale non è partita con il PDA non si può chiedere di
289200080625      * vedere le Fasi della distinta
289300080725     c*******            if        s2sce = 'F' and not *in15 or
289400080725     c*******                      s2sce = 'F' and S2CPDA = 'N'
289500080725     c*******            seton                                        99  41
289600080725     c*******            end
289700070528     C*
289800070321     C                   ENDSR
289900070321      *---------------------------------------------------------------*
290000070321      *? GESTIONE OPZIONI LISTA          ?
290100070321      *---------------------------------------------------------------*
290200070321     C     SCE_S2        BEGSR
290300070321     C*
290400070321     C* determino Riga/Colonna del cursore
290500070321     C                   MOVE      NRG           R$$
290600070321     C                   MOVE      NCL           C$$
290700070321     C                   Z-ADD     RIRI          CURR
290800070321     C                   Z-ADD     COCO          CURC
290900071029     C*
291000071029     c                   If        s2SCE <> *blank
291100071029     c                   move (p)  s2CNFV        FDGNFPOS
291200071029     c                   end
291300070404     C*
291400070404     c                   If        s2SCE  = '1'
291500070405     c                   If        pgm_da_call <> *blank
291600070404     c                   move      *on           $FINE
291700070404     c                   move      *on           $ESCI
291800070404     c                   z-add     s2CNFV        FDGNFV
291900070404     c                   move      d1cnpg        FDGNPG
292000070528     c                   eval      fdgOPZ = s1SCE
292100070405     c                   else
292200070405     c                   z-add     s2CNFV        d1cNFV
292300070405     c                   move      'D1'          $GEST
292400070404     c                   end
292500070405     c                   end
292600070528      *
292700070528     c                   If        s2SCE  = '2' or s2SCE = '3'
292800070528     c                   if        pgm_da_call <> *Blank
292900080704      *
293000080704      * Forza la scrittura per non far cancellare la pagina emessa
293100070528     c                   move      *on           $ESCI
293200070528     c                   move      *on           $FINE
293300070528     c                   z-add     s2CNFV        FDGNFV
293400070528     c                   move      d1cnpg        FDGNPG
293500070528     c                   eval      fdgOPZ = s2SCE
293600070528     c                   clear                   fdgCGI
293700070528     c                   end
293800070528     c                   end
293900070528      *
294000070404     c                   If        s2SCE  = '5'
294100080701      *
294200080701      * Se la distinta ha delle Fasi da mostrare -> abilita il tasto
294300080701      *  funzionale per poter richiamare la window di visualizzazione
294400080701     c                   move      *oFf          d2fasi
294500080704     c                   if        %subst(s2stp:3:1)='*'
294600080701     c                   move      *oN           d2fasi
294700080701     c                   end
294800080701      *
294900070321     c                   z-add     s2CNFV        d1CNFV
295000070321     c                   z-add     c2cfgs        d1CFGS
295100070321     c                   z-add     s2CNFV        wnfv
295200070403     c                   move      'D2'          $GEST
295300070403     C                   move      'S'           $VISD2
295400070403     c                   move      *on           $INZD2
295500070404      *
295600070404     c                   end
295700070321     C*
295800070427     C*
295900070621     c                   If        s2SCE  = '7'
296000070427     c                   clear                   fidg35ds
296100070528     c                   eval      D35FGSi = wfgs
296200070528     c                   eval      D35DDCi = H2CDFV
296300070528     c                   eval      D35NDCi = S2CNFV
296400070528     c                   eval      D35NPGi = wnpg
296500070831     c                   eval      D35PGMi = fdgPGM
296600070903      * si vuole fare vedere tutto
296700070903     c******             eval      D35TPi  = 'C'
296800070905     c                   If        pgm_da_call <> *blank and FDGOPZ <> '1' and
296900080715     c                             (fdgPGM ='ASSMANUALE' or fdgPGM ='MONITOR')
297000070528     c                   eval      D35opzi = 'S'
297100070528     c                   end
297200070427     c                   eval      kpjbus = kpjbu
297300070427     c                   movel(p)  fidg35ds      kpjbu
297400070427     c                   call      'FIDG35R'
297500070427     c                   parm                    kpjba
297600070427     c                   movel     kpjbu         fidg35ds
297700070427     c                   eval      kpjbu = kpjbus
297800070621     c                   if        s2nbla >0
297900070611     c                   if        pgm_da_call <> *Blank and fdgOPZ<>'1'
298000070528     c                   move      *on           $ESCI
298100070831     c                   if        d35funO <>'12'
298200070528     c                   move      *on           $FINE
298300070831     c                   end
298400070528     c                   z-add     s2CNFV        FDGNFV
298500070831     c                   if        d35funO = '12' or d35funO = '03'
298600070831     c                   z-add     0             FDGNFV
298700070831     c                   end
298800070528     c                   move      d1cnpg        FDGNPG
298900070528     c                   eval      fdgOPZ = d35opzO
299000070528     c                   eval      fdgCGI = d35cgiO
299100070528     c                   end
299200070621     c                   end
299300070427     c                   end
299400070427     C*
299500070621     c                   If        s2SCE  = '9'
299600070321     c                   z-add     s2CNFV        wnfv
299700070321     c*******            move      *on           $ESCI
299800070321     c                   move      'D2'          $GEST
299900070403     C                   move      'N'           $VISD2
300000070321     c                   move      *on           $INZD2
300100070321     C                   exsr      GESD2
300200070321     c                   endIf
300300080625      *
300400080625      *  Visionare le Fasi della Distinta
300500080625     c                   If        s2SCE  = 'F'
300600080625     c                   write     FRCDTA
300700080625      *
300800080625     c                   clear                   fidg39dS
300900080625     c                   move      d1CNPG        fdg39NPG
301000080625     c                   z-add     s2CNFV        fdg39NFV
301100080625     c                   z-add     c2cfgs        fdg39FGS
301200080625     c                   move      'N'           fdg39noREC
301300080625     c                   clear                   fdg39VIDno
301400080625     c                   eval      kpjbus = kpjbu
301500080625     c                   movel(p)  fidg39ds      KPJBU
301600080625     c                   call      'FIDG39R'
301700080625     c                   parm                    kpjba
301800080625     c                   eval      kpjbu = kpjbus
301900080625      *
302000080625     c                   end
302100080625     C*
302200080820      *  Dettaglio Distinta
302300080820     c                   If        s2SCE  = 'D'
302400080820     c                   write     FRCDTA
302500080820      *
302600080820     c                   clear                   fidg311dS
302700080820     c                   z-add     c2cfgs        f31FGS
302800080820     c                   z-add     s2CNFV        f31NumDis
302900080820     c                   z-add     h2CDFV        f31DatDis
303000080820     c                   move      d1Dati        f31PriDati
303100081111     c**********         if        D1CPDA <> 'C' and D1CPDA <> 'O'
303200081111     c**********         move      'E'           f31DatiVis
303300081111     c**********         else
303400080820     c                   move      D1CPDA        f31DatiVis
303500081111     c**********         end
303600080820     c                   eval      kpjbus = kpjbu
303700080820     c                   movel(p)  fidg311ds     KPJBU
303800080820     c                   call      'FIDG311R'
303900080820     c                   parm                    kpjba
304000080821     c                   eval      fidg311ds =   KPJBU
304100080821     c                   if        F31Tfunz = '03'
304200080821     c                   move      *on           $ESCI
304300080821     C                   EXSR      F03_exit
304400080821     c                   end
304500080820     c                   eval      kpjbu = kpjbus
304600080820      *
304700080820     c                   end
304800080820     C*
304900070321     C*
305000070321     C                   ENDSR
305100070321     C/EJECT
305200070312      *---------------------------------------------------------------*
305300070321      *?  LETTURA RCD ARCHIVIO PILOTA         ?
305400070312      *---------------------------------------------------------------*
305500070312     C     LEGGE_REC     BEGSR
305600070312     C*
305700070312     C*  Per EoF deve uscire
305800070312     C                   MOVEL     *OFF          $EFILE
305900070312     C*  Per Record Ok deve uscire riportando i dati
306000070312     C                   MOVEL     *OFF          $RCDOK
306100070312     C*
3062000703121    C     $EFILE        DOUEQ     *ON
306300070312     C     $RCDOK        OREQ      *ON
306400070312     C*
306500070312if  2C                   IF        dstDFV <= DATFIW or Prima_Lettura <> 'N'
306600070312     c                   eval      Prima_Lettura = 'N'
306700070312      *
306800070406     c                   if        Solo_aperte = 'N'
306900070312     c     Kdat2         Reade     Fidst008                               33
307000070406     c                   else
307100070312     C     Kdst3         READE     Fidst003                               33
307200070406     c                   End
307300070312      *
307400070312    2C                   END
307500070312      *
307600070312      * solo se superato il limite del range di date
307700070312if  2C                   IF        dstDFV > DATFIW
307800070312     c                   If        Solo_aperte = 'N'
307900070312     C                   SETON                                        33
308000070312     c                   Else
308100070312     C     Kdst3         READE     Fidst003                               33
308200070312     c                   End
308300070312e   2C                   ENDif
308400070312      *
308500070312     C                   select
308600070312      * Fine File
308700070312     C                   when      *in33 = *on
308800070312     C                   MOVEL     *ON           $EFILE
308900070312      *
309000070312     C                   other
309100070312      *
309200070312      * ?Controlla la Spedizione se è da considerare
309300070312     C                   exsr      Check_REC
309400070312      *
309500070312      * ?     se Non è Esclusa
309600070312     c                   if        *in96 = *on
309700070312     C                   MOVE      *ON           $RCDOK
309800070312     c                   end
309900070312      *
310000070312     C                   endsl
310100070312     C*
3102000703121-   C                   ENDDO
310300070312     C*
310400070312     C                   ENDSR
310500070312      *---------------------------------------------------------------*
310600070321      *?  CONTROLLA la riga se deve essere presa in considerazione    ?
310700070312      *---------------------------------------------------------------*
310800070312     C     Check_REC     BEGSR
310900070312     C*
311000070312     C*  96 = *on x Record OK
311100070312     C                   SETOFF                                       96
311200070312      *
311300080724      *  pro-memoria
311400080724     c                   clear                   Fase_STP
311500080613     c                   clear                   Fasi_DSF
311600080613     c                   clear                   Trovato_DSF
311700080613      *
311800080724      *  Controlla se la distinta è stata fatta con PDA
311900080613     c                   eval      Dist_consegne_PDA = ' '
312000080623     c                   eval      Dist_ritiri_PDA = ' '
312100080623      *
312200160503      *  se in test nel filler (anche per il TURNO)
312300080613     c                   eval      ddstFLR = dstFLR
312400160503      *
312500080613     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
312600080613     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
312700080613     c                   eval      Dist_consegne_PDA = 'S'
312800080613     c                   end
312900080613      *
313000080623     c                   If          dstPDA     = 'O' or dstPDA     = 'E' or
313100080623     c                               §DSTTSTPDA = 'O' or §DSTTSTPDA = 'E'
313200080623     c                   eval      Dist_ritiri_PDA = 'S'
313300080623     c                   end
313400080624      *
313500080624      * ?Al momento si tiene in considerazione anche la fase presente in testata
313600080624     c                   if        dstFASD = 'STP'
313700080624     c                   eval      Fase_STP = 'S'
313800080624     c                   eval      Fasi_DSF(1) = dstFASD
313900080624     c                   end
314000080624      *
314100080616      *  poi controllo sul dettaglio delle Fasi
314200080613     C     Kdsf_01       setll     fiDSF01l
314300080613     C     Kdsf_01       reade     fiDSF01l
314400080613      *
314500080613     c                   dow       not %Eof(fiDSF01l)
314600080624      *
314700080613     c                   if        dsfTRD <> *BLANK
314800080613     c                   eval      Trovato_DSF = 'S'
314900080624      * Se c'è la fase STP.
315000080624     c                   if        dsfTRD = 'STP'
315100080624     c                   eval       Fase_STP = 'S'
315200080624     c                   end
315300080623      *
315400080613      * carica le fasi su una schiera
315500080613     c                   z-add     1             dsf               3 0
315600080613     c     dsfTRD        lookup    Fasi_DSF(dsf)                          21
315700080613     c                   if         not *in21
315800080613     c                   z-add     1             dsf
315900080613     c     *blank        lookup    Fasi_DSF(dsf)                          21
316000080613      * imposta nel primo libero
316100080613     c                   if         *in21
316200080613     c                   eval      Fasi_DSF(dsf) = dsfTRD
316300080613     c                   end
316400080613     c                   end
316500080613      *
316600080613     c                   end
316700080613     C     Kdsf_01       reade     fiDSF01l
316800080613     c                   end
316900080613      *
317000070312      * Se richieste Distinte Aperte e data foglio > data inizio (range)   oppure
317100070312      *  Se richieste Distinte Chiuse                                      oppure
317200070312      *   Tutte
317300080624      *?  Abilita il record  per  essere caricato sul SFL     --> 96=ON     ?
317400070312     c                   If        (Solo_aperte = 'S' and dstDFV >= DATINW) or
317500070312     c                             (d1CTFV = '1' and dstFCF <> ' ') or
317600070312     c                             (d1CTFV = ' ')
317700070312     C                   SETON                                        96
317800070312     C                   END
317900080624      *?  Poi Controlla con i Parametri se SCARTARE il record  --> 96 OFF   ?
318000070312      *
318100070312      **  Se impostato il Giro deve caricare le distinte che hanno il Giro
318200070312     c                   if        d1ccgi <> *blank
318300070312     C                   Z-ADD     dstNFV        WnFV
318400070312     c                   move      'N'           Giro_in_NFV       1
318500080715     c                   clear                   Prior_GiroCon     1 0
318600080715     c                   clear                   Prior_GiroRit     1 0
318700070910      **
318800070910     c                   if        d1dati = 'C'
318900070910      **
319000070312     c     kdst31        setll     fidst31l
319100070312     c     kdst31        reade     fidst31l
319200070312     c                   dow       not %Eof(fidst31l)
319300070312      **
319400070312      **  Controlla presenza del giro richiesto
319500070312     c                   if        %char(DST3NPG) = d1cNPG and
319600070312     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
319700070312     c                             and DST3CGI = d1cCGI
319800070312     c                   move      'S'           Giro_in_NFV       1
319900080715     c                   z-add     dst3prgG      Prior_GiroCon
320000070312     c                   leave
320100070312     c                   end
320200070312      **
320300070312     c     kdst31        reade     fidst31l
320400070312     c                   endDo
320500070910      **
320600070910     c                   else
320700070910      **
320800070910     c     kdst51        setll     fidst51l
320900070910     c     kdst51        reade     fidst51l
321000070910     c                   dow       not %Eof(fidst51l)
321100070910      **
321200070910      **  Controlla presenza del giro richiesto
321300070910     c                   if        %char(DST5NPG) = d1cNPG and
321400070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
321500070910     c                             and DST5CGI = d1cCGI
321600070910     c                   move      'S'           Giro_in_NFV       1
321700080715     c                   z-add     dst5prgG      Prior_GiroRit
321800070910     c                   leave
321900070910     c                   end
322000070910      **
322100070910     c     kdst51        reade     fidst51l
322200070910     c                   endDo
322300070910      **
322400070910     c                   end
322500070312      **
322600070312      ** Il giro selezionato non è presente nella distinta
322700070312     c                   if        Giro_in_NFV = 'N'
322800070312     C                   SETOff                                       96
322900070312     c                   end
323000070312      *
323100070312     c                   end
323200070312     C*
323300070312     C* CONTROLLO CHE SIA FOGLIO DI FILIALE IN GESTIONE
323400070312if  3C   96d1CFGS        IFNE      0
323500070312      *
323600070312     C     dstFGS        IFNE      d1CFGS
323700070312     C                   SETOFF                                       96
323800070312     C                   ENDIF
323900070312     C*
324000070312x   3C                   ELSE
324100070312     C*
324200070312     C* CARICO SOLO LE FILIALE NON DELL'AREA
324300070312     C     dstFGS        LOOKUP    L1                                     21
324400070312     C   21              SETOFF                                       96
324500070312e   3C                   ENDIF
324600070312
324700070312      * richiamato da FIMS03R, che passa i F.V. già inseriti
324800070504if  3c                   if        sav_in19 = *ON and *in96
324900070312     c                   eval      *in21  = *off
325000070404     c     dstnfv        lookup    $FV6                                   21
325100070504      * Per visualizzare le Distinte non presenti sul piano Packing List
325200070504if  3c                   IF        *in21 = *oN and fdgTFV = '2'
325300070504     C                   SETOFF                                       96
325400070504     c                   end
325500070312      * richiamato SOLO per sapere se esistono distinte aperte
325600070312      *   non inserito nel piano di cui sono stati passati i F.V.
325700070504if  3c                   IF        *in21 = *oFF and fdgTFV = '3'
325800070419     C                   eval      FDGfs1 = *on
325900070312     C                   MOVE      *ON           $FINE
326000070312     C                   MOVEL     *ON           $EFILE
326100070419     c                   LeaveSR
326200070312e   3c                   endif
326300070419e   3c                   endif
326400070312      *
326500070312     c                   if        *in08 and d1CTPM<>' '
326600070312     c                   if        (d1CTPM='M' and dsttpm<>'M') or
326700070312     c                             (d1CTPM='P' and dsttpm ='M')
326800070312     c                   eval      *in96  = *off
326900070312     c                   endif
327000070312     c                   endif
327100070312      *
327200160503      *   TURNO  solo per PARCEL e NON messaggerie
327300160504      *    aggiunto controllo se c'è "M" Messaggerie non controlla nulla
327400160503     c                   if        *in38 and d1ctpm <> 'M'
327500160503     c                   if        ((d1Ctur1='1'  or d1Ctur2='1')   and
327600160503     c                             ((§dstturno=' ' and dsttpm<>'M') or
327700160503     c                              §dstturno='1'))
327800160503     c                                or
327900160503     c                             ((d1Ctur1='2'  or d1Ctur2='2')   and
328000160503     c                                               §dstturno ='2')
328100160503     c                                or
328200160503     c                              (d1Ctur1=' ' and d1Ctur2=' ')
328300160504     c                                or
328400160504     c                              dsttpm ='M'
328500160503      * qui sarebbe OK
328600160503     c                   else
328700160503      * da scartare
328800160503     c                   eval      *in96  = *off
328900160503     c                   endif
329000160503     c                   endif
329100160503      *
329200070625      * Tipo Uscita Mattina/Pomeriggio
329300070625     c                   if        d1Tusc<>' '
329400070625     c                   if        (d1Tusc='M' and dstFPP<>'M') or
329500070625     c                             (d1Tusc='P' and dstFPP<>'P')
329600070625     c                   eval      *in96  = *off
329700070625     c                   endif
329800070625     c                   endif
329900070625      *
330000070417      *   Range di date entro i limiti
330100070417     C     dstDFV        IFgt      DATfiw
330200070417     C     dstDFV        ORlt      DATinw
330300070417     C                   SETOFF                                       96
330400070417     C                   ENDIF
330500070312      *
330600080623      *   se si vuole vedere solo un Tipo di distinte con PDA o senza o tutte
330700080701      * Senza PDA
330800080701     C                   IF        D1cPDA = 'N' and (
330900080701     c                             Dist_Consegne_PDA <>' ' or
331000080701     c                             Dist_Ritiri_PDA <>' ' )
331100080701     C                   SETOFF                                       96
331200080701     C                   ENDIF
331300080701      * PDA con Ritiri e Consegne
331400081111     C                   IF        D1cPDA = 'S' and (
331500080915     c                             Dist_Consegne_PDA = ' ' and
331600080702     c                             Dist_Ritiri_PDA = ' ' )
331700080623     C                   SETOFF                                       96
331800080623     C                   ENDIF
331900080701      * PDA con Consegne
332000081111     C**********         IF        D1cPDA = 'C' and
332100081111     c**********                   Dist_Consegne_PDA = ' '
332200081111     C**********         SETOFF                                       96
332300081111     C**********         ENDIF
332400080701      * PDA con Ritiri
332500081111     C**********         IF        D1cPDA = 'O' and
332600081111     c**********                   Dist_Ritiri_PDA = ' '
332700081111     C**********         SETOFF                                       96
332800081111     C**********         ENDIF
332900080701      *
333000080701      *  se si vuole vedere solo quelle senza PDA pulisco la lista
333100080724      *   delle fasi scelte tranne per la fase stampata
333200080701     C                   IF        D1cPDA = 'N'
333300080724     c                   do        10            Fas
333400080724     c                   if        d1Fasi(fas) <> 'STP'
333500080724     c                   clear                   d1Fasi(fas)
333600080724     c                   end
333700080724     c                   endDo
333800080623     C                   ENDIF
333900080623      *
334000070607      *? Controllo se richiesto il trasferimento da pgm richiamato    ?
334100070607      *?  da assegnazione manuale e se devo impostare la distinta     ?
334200070607      *?  su cui trasferire non voglio far vedere la distinta dalla   ?
334300070607      *?  quale sto trasferendo le spedizioni.                        ?
334400080715     c                   if        (fdgPGM ='ASSMANUALE' or fdgPGM = 'MONITOR')
334500080701     c                             and pgm_da_call =  'S' and fdg31_opz = '1'
334600070606     c                             and fdgNF1 = dstNFV
334700070606     C                   SETOFF                                       96
334800070606     c                   end
334900070606      *
335000070607      *?  Se sto Trasferendo o Togliendo  non voglio caricare le      ?
335100070607      *?  Distinte vuote  (senza spedizioni - non mi servono)         ?
335200070607      *?  [nel trasferimento solo quando scelgo da dove trasferire]   ?
335300080715     c                   if        (fdgPGM ='ASSMANUALE' or fdgPGM ='MONITOR')
335400080606     c                               and pgm_da_call =  'S'
335500070607     c                   if        fdg31_opz = '2' and DSTnbla =0  or
335600070607     c                             fdg31_opz = '3' and DSTnbla =0 and fdgNF1 =0
335700070621      *  ?  Asteriscato in quanto si vuole continuare a vedere comunque tutto...  ?
335800070621     C***********        SETOFF                                       96
335900070607     c                   end
336000070607     c                   end
336100080613      *
336200080606      *  ?  se Richiamato da MONITOR deve caricare la sola Distinta...  ?
336300080606      *? Passata e nascosta nel SAVfdgNFV.    ?
336400080606     c                   if        fdgPGM = 'MONITOR'  and
336500080606     c                             pgm_da_call =  'S' and
336600080606     c                             SAVfdgNFV <> dstNFV
336700110309      *********
336800110309      *********  avendo eliminato sul chiamante FIDG30R occorre filtrare qui
336900110309      *********   con il parametro inerente all'azione da compiere su questo pgm.
337000110309     c                             and fdg31_opz  ='2'
337100110309      ***
337200080606     C                   SETOFF                                       96
337300080606     c                   end
337400080606      *
337500080613      * >>>>>>>>>
337600080613      *?  Controlla se stampate o no oppure tutte.      ?
337700080623      *  si deve controllare se la distinta ha la fase STAMPATA.
337800110308      *    solo se non richiamato dal MONITOR.
337900110308     c                   if        fdgPGM <>'MONITOR'
338000080624     c                   if        d1stp = 'N' and fase_STP ='S' or
338100080703     c                             d1stp = 'S' and fase_STP =' '
338200080613     C                   SETOFF                                       96
338300080613     c                   end
338400110308     c                   end
338500080624      * >>>>>>>>>
338600080716      * >>>>>>>>>   Filtro mediante le FASI
338700080716      * >>>>>>>>>
338800080701      *
338900080724      * Altrimenti x le altre Fasi in schiera:
339000080624     c                   move      'N'           trovata_fase      1
339100080624      *   e se almeno 1 fase è stata selezionata:
339200080624     c                   if        d1fasiDS <> *blank
339300080613      *
339400080611     c                   do        10            Fs                3 0
339500080623     c                   if        d1fasi(fs) <> *blank
339600080613     c                   z-add     1             dsf               3 0
339700080623     c     d1fasi(fs)    lookup    Fasi_dsf(dsf)                          21
339800080613     c                   if        *in21
339900080611     c                   move      'S'           trovata_fase
340000080611     c                   leave
340100080611     c                   end
340200080613     c                   end
340300080611     c                   enddo
340400080611      * include o esclude
340500080613     c                   if        (D1IEFA = 'I' and trovata_fase = 'N') or
340600080613     c                             (D1IEFA = 'E' and trovata_fase = 'S')
340700080611     C                   SETOFF                                       96
340800080611     c                   end
340900080611      *
341000080716     c                   end
341100080701      *
341200070312     C                   ENDSR
341300070301      *---------------------------------------------------------------*
341400070321      *?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL      ?
341500070301      *---------------------------------------------------------------*
341600070228     C     CLCPAG        BEGSR
341700940224     C* Input :
341800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
341900940224     C*          cursore
342000070301     C* - Wpagine= numero rcd per pagina sfl
342100940224     C* Output :
342200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
342300940224     C*
342400070301     C     WSFL          DIV       Wpagine       PAGINE            4 0
342500940224     C                   MVR                     RESTO             3 0
342600940224     C     RESTO         IFGT      0
342700940224     C                   ADD       1             PAGINE
342800940224     C                   ENDIF
342900070301     C     PAGINE        MULT      Wpagine       WPAG
343000940224     C*
343100940224     C                   ENDSR
343200070302     C/EJECT
343300070302     C************************************************************
343400070321      *?  INIZIALIZZA indicatori di errore    ?
343500070302     C************************************************************
343600070308     C     INZ_indERR    BEGSR
343700070308      *
343800070308     C                   move      *ALL'0'       IN5190           40
343900070308     C                   moveA     IN5190        *IN(51)
344000070308      *
344100070308     C                   ENDSR
344200070308     C/EJECT
344300070301      *---------------------------------------------------------------*
344400070321      *?  GESTIONE F03 x uscire LR     ?
344500070301      *---------------------------------------------------------------*
344600070227     C     F03_exit      BEGSR
344700070227     C* fine programma
344800940309     C                   MOVE      *ON           $FINE
344900070302      *
345000940309     C                   ENDSR
345100070312     C/EJECT
345200070312      *---------------------------------------------------------------*
345300070321      *? GESTIONE F11 alterna l'ordinamento x numero o Autotrasportatore  ?
345400070312      *---------------------------------------------------------------*
345500070322     C     F11_ordina1   BEGSR
345600070320     C*
345700160505     c                   setoff                                       22
345800160506     c                   eval      *in38 = sav_in38
345900070320     C* imposto pos. del cursore
346000070403     C                   if        WrkSort = Autista or
346100160512     C                             WrkSort = Numero and *in13 = *oN
346200160512     c                             or Prima_volta ='S'
346300160512     c                   eval      Prima_volta = 'N'
346400070403     c                   eval      cur_su_sfl = 'N'
346500070320     C                   eval           H1RIGA = 3
346600070528     C                   eval           H1COLO = 17
346700070402     c                   end
346800070402     C* imposto pos. del cursore
346900160512     C                   if        WrkSort = Numero and *in13 = *oFF
347000160505     c                             or WrkSort = Turno
347100070403     c                   eval      cur_su_sfl = 'S'
347200070402     C                   eval           H1RIGA = 7
347300070402     C                   eval           H1COLO = 2
347400070402     c                   end
347500160505      *
347600071026     C                   z-add     h1riga        Su_Quale_rec
347700071026     c                   z-add     1             Che_NRec_sfl
347800070313      *
347900070313      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
348000070313      * presente nel subfile -- Selected -- questo è aggiunto al record.
348100070313      * Il campo è usato per selezionare i records dando un ordine a quelli
348200070313      * selezionati davanti ai non selezionati.
348300070313     C                   CLEAR                   QLGSCB
348400070313     C                   CLEAR                   QLGSCB00
348500070313
348600070313     c                   eval      wsfl = 1
348700070313
348800070313     ** Gestione della scelta ordinamento.
348900070313     C                   SELECT
349000070313      *
349100070402      *?  Ordinamento per Autista.  ?
349200070313     C                   WHEN      WrkSort = Autista
349300070322     c                   clear                   c1cnfv
349400160505     c                   seton                                        22
349500070403     C*
349600070313     ** 1 campo chiave.
349700070313     C                   EVAL      QLGNBRK    = 1
349800070313     ** La descrizione è
349900070402     ** a posizione 1, xx byte, char, ascending.
350000070313     C                   EVAL      QLGSP      = 1
350100070322     C                   EVAL      QLGSS      = %SIZE(S1DAUT)
350200070313     C                   EVAL      QLGDT      = Carattere
350300070313     C                   EVAL      QLGSO      = Ascendente
350400070313     C                   EVAL      QLGKL(1)   = QLGSKL
350500070313
350600070313     ** Al prossimo F11 ordino per Numero Distinta
350700070313     C                   EVAL      WrkSort = Numero
350800070313
350900160601     C                   EVAL      *IN11 = *ON
351000160505     C                   EVAL      *IN10 = *OFF
351100070313     C                   EVAL      sav_in11 = *IN11
351200160504     C                   EVAL      sav_in10 = *IN10
351300070313
351400070402      *?  Ordinamento per Numero Distinta   ?
351500070313     C                   WHEN      WrkSort = Numero
351600070322     c                   clear                   c1daut
351700070403      *
351800070510     ** 3 campi chiave.  x DATA e NUMERO
351900070510     C                   EVAL      QLGNBRK    = 3
352000070313     ** Il gruppo è
352100070403     ** a posizione xx, 6 byte, char, ascending
352200070417     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
352300070417     C                                     %SIZE(S1CDFV)
352400070417     C                   EVAL      QLGSS    = %SIZE(H1CDFV)
352500070403     C                   EVAL      QLGDT    = Numerico
352600070403     C                   EVAL      QLGSO    = Ascendente
352700070403     C                   EVAL      QLGKL(1) = QLGSKL
352800070313
352900070510     ** Priorità  è
353000070510     ** a posizione xx, 6 byte, char, ascending
353100070510     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
353200070510     C                                     %SIZE(S1CDFV) + %SIZE(H1CDFV)
353300070510     C                   EVAL      QLGSS    = %SIZE(H1PRGG)
353400070510     C                   EVAL      QLGDT    = Numerico
353500070510     C                   EVAL      QLGSO    = Ascendente
353600070510     C                   EVAL      QLGKL(2) = QLGSKL
353700070510
353800070313     ** La descrizione è
353900070403     ** a posizione 1, xx byte, char, ascending
354000070403     C                   EVAL      QLGSP      = 1 + %SIZE(S1DAUT)
354100070403     C                   EVAL      QLGSS      = %SIZE(S1CNFV)
354200070403     C                   EVAL      QLGDT      = Numerico
354300070313     C                   EVAL      QLGSO      = Ascendente
354400070510     C                   EVAL      QLGKL(3)   = QLGSKL
354500070313
354600160601     C                   EVAL      *IN11 = *OFF
354700160505     C   38              EVAL      *IN10 = *ON
354800070313     C                   EVAL      sav_in11 = *IN11
354900160504     C                   EVAL      sav_in10 = *IN10
355000070313
355100070313     ** Al prossimo F11 ordino per descrizione.
355200070313     C                   EVAL      WrkSort = Autista
355300160505     C   38              EVAL      WrkSort = Turno
355400070403      *
355500070403      *   Se il range di date non è dello stesso giorno
355600070417     c******             if        *in13 = *off
355700070417     C******             MOVE      *On           $INZS1
355800070417     c******             goto      end_ordina1
355900070417     c******             end
356000160505
356100160505      *?  Ordinamento per Turno   ?
356200160505     C                   WHEN      WrkSort = Turno
356300160505     c                   clear                   c1daut
356400160505      *
356500160505     ** 4 campi chiave.  Turno  e NUMERO
356600160505     C                   EVAL      QLGNBRK    = 4
356700160505     ** Il turno  è
356800160505     ** a posizione xx, 6 byte, char, ascending
356900160505     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
357000160505     C                                     %SIZE(S1CDFV) + %SIZE(H1CDFV) +
357100160505     c                                     %SIZE(H1PRGG)
357200160505     C                   EVAL      QLGSS    = %SIZE(s1cTUR)
357300160505     C                   EVAL      QLGDT    = Numerico
357400160505     C                   EVAL      QLGSO    = Ascendente
357500160505     C                   EVAL      QLGKL(1) = QLGSKL
357600160505
357700160505     ** Il gruppo è
357800160505     ** a posizione xx, 6 byte, char, ascending
357900160505     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
358000160505     C                                     %SIZE(S1CDFV)
358100160505     C                   EVAL      QLGSS    = %SIZE(H1CDFV)
358200160505     C                   EVAL      QLGDT    = Numerico
358300160505     C                   EVAL      QLGSO    = Ascendente
358400160505     C                   EVAL      QLGKL(2) = QLGSKL
358500160505
358600160505     ** Priorità  è
358700160505     ** a posizione xx, 6 byte, char, ascending
358800160505     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
358900160505     C                                     %SIZE(S1CDFV) + %SIZE(H1CDFV)
359000160505     C                   EVAL      QLGSS    = %SIZE(H1PRGG)
359100160505     C                   EVAL      QLGDT    = Numerico
359200160505     C                   EVAL      QLGSO    = Ascendente
359300160505     C                   EVAL      QLGKL(3) = QLGSKL
359400160505
359500160505     ** La descrizione è
359600160505     ** a posizione 1, xx byte, char, ascending
359700160505     C                   EVAL      QLGSP      = 1 + %SIZE(S1DAUT)
359800160505     C                   EVAL      QLGSS      = %SIZE(S1CNFV)
359900160505     C                   EVAL      QLGDT      = Numerico
360000160505     C                   EVAL      QLGSO      = Ascendente
360100160505     C                   EVAL      QLGKL(4)   = QLGSKL
360200160505
360300160601     C                   EVAL      *IN11 = *OFF
360400160505     C                   EVAL      *IN10 = *OFF
360500160505     C                   EVAL      sav_in11 = *IN11
360600160505     C                   EVAL      sav_in10 = *IN10
360700160505
360800160505     ** Al prossimo F11 ordino per descrizione.
360900160505     C                   EVAL      WrkSort = Autista
361000160505      *
361100070313
361200070313     C                   ENDSL
361300070313
361400070313      * Load other sort parameters.
361500070313     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
361600070322     C                   EVAL      QLGRL     = %SIZE(SflRcd1) - 1
361700070313     C                   EVAL      QLGRT     = 8
361800070313     C                   EVAL      QLGOKL    = 80
361900070313     C                   EVAL      QLGLKE    = 16
362000070313     C                   EVAL      QLGLSS    = 290
362100070313
362200070313      * Initialize Sort I/O API fields.
362300070313     C                   EVAL      QLGRL00  = QLGRL
362400070313     C                   EVAL      QLGRC00  = 1
362500070313     C                   CLEAR                   QUSEI
362600070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
362700070313
362800070313      * First step - Initialize the sort routine.
362900070313     C                   CALL      'QLGSORT'
363000070313     C                   PARM                    QLGSCB
363100070313     C                   PARM                    NotUsed
363200070313     C                   PARM                    NotUsed
363300070313     C                   PARM                    SizeList
363400070313     C                   PARM                    ReturnSize
363500070313     C                   PARM                    QUSEC
363600070313
363700070313      * Next step - Write records to I/O routine.
363800070313     C                   EVAL      QLGRT00 = Put
363900070313
364000070322     C                   DO        RrnLast       S1nrr
364100070313
364200070322     C     S1nrr         CHAIN     DG31SF1
364300070313
364400070313     ** Solo le righe con Selected = 'Y' sono riordinate,
364500070313     ** quindi per fare un ordinamento di tutte le righe
364600070313     ** metto 'Y' sempre.
364700070322     C                   EVAL      Selected1 = 'Y'
364800070313
364900070313     C                   CLEAR                   QUSEI
365000070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
365100070322      *
365200070313     C                   CALL      'QLGSRTIO'
365300070313     C                   PARM                    QLGSCB00
365400070322     C                   PARM                    SflRcd1
365500070313     C                   PARM                    NotUsed
365600070313     C                   PARM                    SizeList
365700070313     C                   PARM                    NotUsed
365800070313     C                   PARM                    QUSEC
365900070313
366000070313     C                   ENDDO
366100070313
366200070313      * Next step - Signal end of input, clear subfile for reload.
366300070313     C                   EVAL      QLGRT00 = EndPut
366400070313     C                   CLEAR                   QUSEI
366500070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
366600070313
366700070313     C                   CALL      'QLGSRTIO'
366800070313     C                   PARM                    QLGSCB00
366900070322     C                   PARM                    SflRcd1
367000070313     C                   PARM                    NotUsed
367100070313     C                   PARM                    SizeList
367200070313     C                   PARM                    NotUsed
367300070313     C                   PARM                    QUSEC
367400070313
367500070313     C* pulizia SFL
367600070313     C                   SETOFF                                         3031
367700070322     C                   WRITE     DG31CT1
367800070313     C                   SETON                                          31
367900070313
368000070313      * Final step - Write the records back to the subfile.
368100070313     C                   EVAL      QLGRT00 = Get
368200070313
368300070322     C                   DO        RrnLast       s1nrr
368400070313     C                   CLEAR                   QUSEI
368500070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
368600070313     C                   CALL      'QLGSRTIO'
368700070313     C                   PARM                    QLGSCB00
368800070313     C                   PARM                    NotUsed
368900070322     C                   PARM                    SflRcd1
369000070313     C                   PARM                    QLGRL00
369100070313     C                   PARM                    NotUsed
369200070313     C                   PARM                    QUSEC
369300070510      *
369400080208      * Ripristina gli indicatori x i DSPATR
369500070510     c                   setoff                                       40
369600070510if  4c                   if        h1prgg = 1
369700070510     c                   seton                                        40
369800070510     c                   end
369900080208      * x fase di test
370000080219     c                   eval      *in15 = sav_in15
370100080208     c                   setoff                                       14
370200080208     c                   if        h1cDFV >= DateU8
370300080208     c                   if        §ogPDAorm ='S' and (S1CTSTPDA ='o') or
370400080208     c                             §ogPDAorm ='S' and (S1CTSTPDA ='e') or
370500080208     c                             §ogPDAcon ='S' and (S1CTSTPDA ='e') or
370600080208     c                             §ogPDAcon ='S' and (S1CTSTPDA ='c')
370700080208     c                   seton                                        14
370800080208     c                   endIf
370900080208     c                   endIf
371000080222     c                   eval      h1in14 = *in14
371100170508     c                   eval               *in15  =  h1in15
371200170508     c                   eval               *in18  =  h1in18
371300170508     c                   eval               *in19  =  h1in19
371400170508     c                   eval               *in20  =  h1in20
371500070510      *
371600070322     C                   WRITE     DG31SF1
371700070313     C                   ENDDO
371800070320      *
371900070403     C     end_ordina1   ENDSR
372000070322     C/EJECT
372100070322      *---------------------------------------------------------------*
372200070322      *? GESTIONE F11 alterna l'ordinamento x numero o Autotrasportatore  ?
372300070322      *---------------------------------------------------------------*
372400070322     C     F11_ordina2   BEGSR
372500070322     C*
372600070403     C* imposto pos. del cursore
372700070403     C                   if        WrkSort = Autista or
372800070403     C                             WrkSort = Numero and *in13 = *on
372900070403     c                   eval      cur_su_sfl = 'N'
373000070403     C                   eval           H1RIGA = 3
373100070528     C                   eval           H1COLO = 17
373200070403     c                   end
373300070403     C* imposto pos. del cursore
373400070403     C                   if        WrkSort = Numero and *in13 = *off
373500080715     c                             or Prima_volta ='S'
373600160505     c                             or WrkSort = Turno
373700070607     c                   eval      Prima_volta = 'N'
373800070403     c                   eval      cur_su_sfl = 'S'
373900070403     C                   eval           H1RIGA = 7
374000070403     C                   eval           H1COLO = 2
374100070403     c                   end
374200071026     C                   z-add     h1riga        Su_Quale_rec
374300071026     c                   z-add     1             Che_NRec_sfl
374400070322      *
374500070322      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
374600070322      * presente nel subfile -- Selected -- questo è aggiunto al record.
374700070322      * Il campo è usato per selezionare i records dando un ordine a quelli
374800070322      * selezionati davanti ai non selezionati.
374900070322     C                   CLEAR                   QLGSCB
375000070322     C                   CLEAR                   QLGSCB00
375100070322
375200070322     c                   eval      wsfl = 1
375300070322
375400070322     ** Gestione della scelta ordinamento.
375500070322     C                   SELECT
375600070322      *
375700070403      *?  Ordinamento per Autista.  ?
375800070322     C                   WHEN      WrkSort = Autista
375900070322     c                   clear                   c2cnfv
376000070403      *
376100070322     ** 1 campo chiave.
376200070322     C                   EVAL      QLGNBRK    = 1
376300070322     ** La descrizione è
376400070322     ** a posizione 1, 28 byte, char, ascending.
376500070322     C                   EVAL      QLGSP      = 1
376600070322     C                   EVAL      QLGSS      = %SIZE(S2DAUT)
376700070322     C                   EVAL      QLGDT      = Carattere
376800070322     C                   EVAL      QLGSO      = Ascendente
376900070322     C                   EVAL      QLGKL(1)   = QLGSKL
377000070322
377100070322     ** Al prossimo F11 ordino per Numero Distinta
377200070322     C                   EVAL      WrkSort = Numero
377300070322
377400160601     C                   EVAL      *IN11 = *ON
377500070322     C                   EVAL      sav_in11 = *IN11
377600160504     C                   EVAL      sav_in10 = *IN10
377700070322
377800070403      *?  Ordinamento per Numero Distinta   ?
377900070322     C                   WHEN      WrkSort = Numero
378000071026      ********
378100071026     c**********         clear                   c2daut
378200071026      ********
378300070510     ** 3 campi chiave.
378400070510     C                   EVAL      QLGNBRK    = 3
378500070322     ** Il gruppo è
378600070322     ** a posizione 29, 6 byte, char, ascending
378700070417     C                   EVAL      QLGSP   = 1 + %SIZE(S2DAUT) + %SIZE(S2CNFV) +
378800070417     C                                     %SIZE(S2CDFV)
378900070417     C                   EVAL      QLGSS    = %SIZE(h2CDFV)
379000070403     C                   EVAL      QLGDT    = Numerico
379100070403     C                   EVAL      QLGSO    = Ascendente
379200070403     C                   EVAL      QLGKL(1) = QLGSKL
379300070510     ** La priorità
379400070510     ** a posizione xx, x byte, char, ascending
379500070510     C                   EVAL      QLGSP   = 1 + %SIZE(S2DAUT) + %SIZE(S2CNFV) +
379600070510     C                                     %SIZE(S2CDFV) + %SIZE(h2CDFV)
379700070510     C                   EVAL      QLGSS    = %SIZE(h2PRGG)
379800070510     C                   EVAL      QLGDT    = Numerico
379900070510     C                   EVAL      QLGSO    = Ascendente
380000070510     C                   EVAL      QLGKL(2) = QLGSKL
380100070322     ** La descrizione è
380200070322     ** a posizione 5, 28 byte, char, ascending
380300070403     C                   EVAL      QLGSP      = 1 + %SIZE(S2DAUT)
380400070403     C                   EVAL      QLGSS      = %SIZE(S2CNFV)
380500070403     C                   EVAL      QLGDT      = Numerico
380600070322     C                   EVAL      QLGSO      = Ascendente
380700070510     C                   EVAL      QLGKL(3)   = QLGSKL
380800070322
380900160601     C                   EVAL      *IN11 = *OFF
381000070322     C                   EVAL      sav_in11 = *IN11
381100160504     C                   EVAL      sav_in10 = *IN10
381200070322
381300070322     ** Al prossimo F11 ordino per descrizione.
381400070322     C                   EVAL      WrkSort = Autista
381500070403      *
381600070403      *   Se il range di date non è dello stesso giorno
381700070417     c*******            if        *in13 = *off
381800070417     C*******            MOVE      *On           $INZS2
381900070417     c*******            goto      end_ordina2
382000070417     c*******            end
382100070322
382200070322     C                   ENDSL
382300070322
382400070322      * Load other sort parameters.
382500070322     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
382600070322     C                   EVAL      QLGRL     = %SIZE(SflRcd2) - 1
382700070322     C                   EVAL      QLGRT     = 8
382800070322     C                   EVAL      QLGOKL    = 80
382900070322     C                   EVAL      QLGLKE    = 16
383000070322     C                   EVAL      QLGLSS    = 290
383100070322
383200070322      * Initialize Sort I/O API fields.
383300070322     C                   EVAL      QLGRL00  = QLGRL
383400070322     C                   EVAL      QLGRC00  = 1
383500070322     C                   CLEAR                   QUSEI
383600070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
383700070322
383800070322      * First step - Initialize the sort routine.
383900070322     C                   CALL      'QLGSORT'
384000070322     C                   PARM                    QLGSCB
384100070322     C                   PARM                    NotUsed
384200070322     C                   PARM                    NotUsed
384300070322     C                   PARM                    SizeList
384400070322     C                   PARM                    ReturnSize
384500070322     C                   PARM                    QUSEC
384600070322
384700070322      * Next step - Write records to I/O routine.
384800070322     C                   EVAL      QLGRT00 = Put
384900070322
385000070322     C                   DO        RrnLast       S2nrr
385100070322
385200070322     C     S2nrr         CHAIN     DG31SF2
385300070322
385400070322     ** Solo le righe con Selected = 'Y' sono riordinate,
385500070322     ** quindi per fare un ordinamento di tutte le righe
385600070322     ** metto 'Y' sempre.
385700070322     C                   EVAL      Selected2 = 'Y'
385800070322
385900070322     C                   CLEAR                   QUSEI
386000070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
386100070322      *
386200070322     C                   CALL      'QLGSRTIO'
386300070322     C                   PARM                    QLGSCB00
386400070322     C                   PARM                    SflRcd2
386500070322     C                   PARM                    NotUsed
386600070322     C                   PARM                    SizeList
386700070322     C                   PARM                    NotUsed
386800070322     C                   PARM                    QUSEC
386900070322
387000070322     C                   ENDDO
387100070322
387200070322      * Next step - Signal end of input, clear subfile for reload.
387300070322     C                   EVAL      QLGRT00 = EndPut
387400070322     C                   CLEAR                   QUSEI
387500070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
387600070322
387700070322     C                   CALL      'QLGSRTIO'
387800070322     C                   PARM                    QLGSCB00
387900070322     C                   PARM                    SflRcd2
388000070322     C                   PARM                    NotUsed
388100070322     C                   PARM                    SizeList
388200070322     C                   PARM                    NotUsed
388300070322     C                   PARM                    QUSEC
388400070322
388500070322     C* pulizia SFL
388600070322     C                   SETOFF                                         3031
388700070322     C                   WRITE     DG31CT2
388800070322     C                   SETON                                          31
388900070322
389000070322      * Final step - Write the records back to the subfile.
389100070322     C                   EVAL      QLGRT00 = Get
389200070322
389300070322     C                   DO        RrnLast       s2nrr
389400070322     C                   CLEAR                   QUSEI
389500070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
389600070322     C                   CALL      'QLGSRTIO'
389700070322     C                   PARM                    QLGSCB00
389800070322     C                   PARM                    NotUsed
389900070322     C                   PARM                    SflRcd2
390000070322     C                   PARM                    QLGRL00
390100070322     C                   PARM                    NotUsed
390200070322     C                   PARM                    QUSEC
390300070510      *
390400080208      * Ripristina gli indicatori x i DSPATR
390500070510     c                   setoff                                       40
390600070510if  4c                   if        h2prgg = 1
390700070510     c                   seton                                        40
390800070510     c                   end
390900071029      *
391000071029     c                   setoff                                       42
391100071029if  4c                   if        h2recpos = 'X'
391200071029     c                   z-add     s2nrr         nrec_riposiz
391300071029     C                   z-add     riri          Su_Quale_rec
391400071029     c                   end
391500080208      * x fase di test
391600080219     c                   eval      *in15 = sav_in15
391700080208     c                   setoff                                       14
391800080208     c                   if        h2cDFV >= DateU8
391900080208     c                   if        §ogPDAorm ='S' and (S2CTSTPDA ='o') or
392000080208     c                             §ogPDAorm ='S' and (S2CTSTPDA ='e') or
392100080208     c                             §ogPDAcon ='S' and (S2CTSTPDA ='e') or
392200080208     c                             §ogPDAcon ='S' and (S2CTSTPDA ='c')
392300080208     c                   seton                                        14
392400080208     c                   endIf
392500080208     c                   endIf
392600080222     c                   eval      h2in14 = *in14
392700170508     c                   eval               *in15  =  h2in15
392800170508     c                   eval               *in18  =  h2in18
392900170508     c                   eval               *in19  =  h2in19
393000170508     c                   eval               *in20  =  h2in20
393100070510      *
393200070322     C                   WRITE     DG31SF2
393300070322     C                   ENDDO
393400070322      *
393500070403     C     End_ordina2   ENDSR
393600070313     C/EJECT
393700070313      *---------------------------------------------------------------*
393800070321      *? GESTIONE F21 interrogazione spunte    ?
393900070313      *---------------------------------------------------------------*
394000070313     C     F21_interroga BEGSR
394100070313     C*
394200070312     C                   CLEAR                   PARA04
394300070314     C                   Z-ADD     wNFV          P04NFV
394400070312     C                   Z-ADD     99            P04NRS
394500070312     C                   Z-ADD     99            P04NPS
394600070314     C                   MOVEL     wnpg          P04NPG
394700070312     C                   MOVEL     '1'           P04FLG
394800070312     C                   MOVEL     'FIDG04R'     P04PGM
394900070314     C                   Z-ADD     wfgs          P04FGS
395000070313     c                   eval      kpjbus = kpjbu
395100070312     C                   MOVEL     PARA04        KPJBU
395200070312     C                   CALL      'FNLR04R'
395300070312     C                   PARM                    KPJBA
395400070313     c                   eval      kpjbu = kpjbus
395500070312      *
395600070312     C                   ENDSR
395700070301     C/EJECT
395800070321     C************************************************************
395900070321      *?  Emissione video di Dettaglio           ?
396000070312     C************************************************************
396100070312     C     GESD2         BEGSR
396200070315     C*
396300070312     C* inizializzazione videata
396400070312     C     $INZD2        IFEQ      *ON
396500070312     C                   EXSR      INZD2
396600070312     C                   MOVE      *OFF          $INZD2
396700070312     C                   ENDIF
396800070312     C*
396900070312     c     Video_2       tag
397000070312     C*  Emissione Video
397100070312     c                   setoff                                       99
397200160506     c                   eval      *in38 = sav_in38
397300070312     c                   if        $VISD2 = 'S'
397400070320      *-----
397500070312      * ?            *-----------------------*
397600070312     C                   EXFMT     DG31D02
397700070312      * ?            *-----------------------*
397800070320      *-----
397900070320     c                   else
398000070320      *-----
398100070320      *  Richiama il programma di visualizzazione del dettaglio distinta
398200070402     c                   exsr      Detta_GIRI
398300070406
398400070406     c                   end
398500070320     C*
398600070320     C* imposto pos. del cursore
398700070320     C                   eval           H1RIGA = CURR
398800070320     C                   eval           H1COLO = CURC
398900070312     C* Selezioni
3990000703121    C                   SELECT
399100070402      *-----
399200070402     C                   WHEN      $TASTO   =   F02
399300070402     c                   exsr      Detta_GIRI
399400070402     C                   if        comando = 'KC'
399500070402     C                   clear                   comando
399600080821     c                   move      *on           $ESCI
399700080606     c                   move      '03'          fdgtfu
399800070402     C                   EXSR      F03_exit
399900070402     c                   goto      end_d02
400000070402     c                   end
400100070402     C*--------
400200070312     C* F3=Fine
400300070320     C                   WHEN      $TASTO   =   F03  or comando = 'KC'
400400070330     C                   clear                   comando
400500080821     c                   move      *on           $ESCI
400600080606     c                   move      '03'          fdgtfu
400700070312     C                   EXSR      F03_exit
400800070312     c                   goto      end_d02
400900070312     C* F12=Ritorno
401000070320     C                   WHEN      $TASTO   =   F12  or comando = 'KL'
401100070330     C                   clear                   comando
401200070329     c                   if        sav_$GEST <> *blank
401300070313     C                   movel     Sav_$GEST     $gest
401400070329     c                   else
401500080606     c                   move      '12'          fdgtfu
401600080821     c                   move      *on           $ESCI
401700070329     C                   EXSR      F03_exit
401800070329     c                   end
401900070312     c                   goto      end_d02
402000070312      *
402100070312     C* F21=Spunte del foglio richiesto
402200070312     C                   WHEN      $TASTO   =   F21
402300070312     C                   EXSR      F21_interroga
402400070312     c                   seton                                        99
402500070404     C* F6=Conferma Scelta
402600070404     C                   WHEN      $TASTO   =   F06
402700070404     c                   z-add     d2CNFV        FDGNFV
402800070404     c                   move      d1cnpg        FDGNPG
402900080821     c                   move      *on           $ESCI
403000080606     c                   move      '06'          fdgtfu
403100070404     C                   EXSR      F03_exit
403200070404     c                   goto      end_d02
403300080820      *
403400080701     C* F8=Visualizza Fasi
403500080701     C                   WHEN      $TASTO   =   F08
403600080701     c                   write     FRCDTA
403700080701      *
403800080701     c                   clear                   fidg39dS
403900080701     c                   move      d1CNPG        fdg39NPG
404000080701     c                   z-add     d2CNFV        fdg39NFV
404100080701     c                   z-add     d2cfgs        fdg39FGS
404200080701     c                   move      'N'           fdg39noREC
404300080701     c                   clear                   fdg39VIDno
404400080701     c                   eval      kpjbus = kpjbu
404500080701     c                   movel(p)  fidg39ds      KPJBU
404600080701     c                   call      'FIDG39R'
404700080701     c                   parm                    kpjba
404800080701     c                   eval      kpjbu = kpjbus
404900080820      *
405000080820     C* F9=Dettaglio Distinta
405100080820     C                   WHEN      $TASTO   =   F09
405200080820     c                   write     FRCDTA
405300080820      *
405400080820     c                   clear                   fidg311dS
405500080820     c                   z-add     d2cfgs        f31FGS
405600080827     c                   z-add     wnpg          f31npg
405700080820     c                   z-add     d2CNFV        f31NumDis
405800080820     c                   z-add     h2CDFV        f31DatDis
405900080820     c                   move      d1Dati        f31PriDati
406000081111     c**********         if        D1CPDA <> 'C' and D1CPDA <> 'O'
406100081111     c**********         move      'E'           f31DatiVis
406200081111     c**********         else
406300080820     c                   move      D1CPDA        f31DatiVis
406400081111     c**********         end
406500080820     c                   eval      kpjbus = kpjbu
406600080820     c                   movel(p)  fidg311ds     KPJBU
406700080820     c                   call      'FIDG311R'
406800080820     c                   parm                    kpjba
406900080821     c                   eval      fidg311ds =   KPJBU
407000080821     c                   if        F31Tfunz = '03'
407100080821     c                   move      *on           $ESCI
407200080821     C                   EXSR      F03_exit
407300080821     c                   end
407400080820     c                   eval      kpjbu = kpjbus
407500080820      *
407600070312      *
4077000703121O   C                   OTHER
407800070312     C* CONTROLLO DATI
407900070312     C                   EXSR      CTRD2
4080000703121-   C                   ENDSL
408100070320      *-----
408200070312     C*
408300070312     C* Per errori o decodifiche
408400070320     c                   if        $VISD2 = 'S'
408500070312     c   99              goto      Video_2
408600070320     c                   end
408700070312      *
408800070312     c     end_d02       tag
408900070312      *
409000070312     C                   ENDSR
409100070312     C/EJECT
409200070312     C************************************************************
409300070402      *?  Visualizza anche il dettaglio GIRI     ?
409400070312     C************************************************************
409500070402     C     Detta_GIRI    BEGSR
409600070320     C*
409700070402      *  Richiama il programma di visualizzazione del dettaglio distinta
409800070402     c                   clear                   fidg02ds
409900070402     C                   MOVEL     wfgs          D02FIL
410000070402     C                   MOVEL     'A'           D02TIPA
410100070402     C                   MOVEL     dstpdr        D02PDR
410200070402     C                   MOVEL     '05'          D02CMD
410300070402     C                   MOVEL     'N'           D02STRCMT
410400070402     C                   MOVEL     'N'           D02OPNCMT
410500070402     C                   MOVEL     'N'           D02COM
410600070402     C                   MOVEL     'FIDST00F'    D02FILE
410700070402     C                   MOVEL     dstDFV        D02DFV
410800070402     C                   MOVEL     dstNFV        D02NFV
410900070402     C                   MOVEL     '4'           D02NPG
411000070402     C                   MOVEL     dstFPP        D02FPP
411100070402     C                   MOVEL     dstTPM        D02TPM
411200070904     C                   MOVEL     d1dati        D02TP
411300070402     c                   eval      kpjbus = kpjbu
411400070402     C                   eval      KPJBU  = fidg02ds
411500070402     c                   call      'FIDG02C'
411600070402     c                   parm                    kpjba
411700070402     C                   MOVEL     KPJBU         fidg02ds
411800070905      *
411900070905      *  secondo giro per visualizzare le informazioni successive
412000070905      *   solo se NON F3
412100070905     C                   if        D02CMDE <> 'KC'
412200070905     C                   if        d1dati = 'C'
412300070905     C                   MOVEL     'R'           D02TP
412400070905     c                   else
412500070905     C                   MOVEL     'C'           D02TP
412600070905     c                   end
412700070905      *
412800070926     C                   if        test_Paral_ORM = 'S' and D02TP = 'R' or
412900070926     C                             test_Parallelo = 'S' and D02TP = 'C'
413000070905     c                   eval      kpjbus = kpjbu
413100070905     C                   eval      KPJBU  = fidg02ds
413200070905     c                   call      'FIDG02C'
413300070905     c                   parm                    kpjba
413400070905     C                   MOVEL     KPJBU         fidg02ds
413500070926     c                   endIf
413600070905     c                   end
413700070905      *
413800070402     c                   eval      kpjbu = kpjbus
413900070402     C                   MOVEL     D02CMDE       comando           2
414000070402     C                   MOVEL     D02ERR        errore            1
414100070402      *
414200070402     C                   ENDSR
414300070402     C/EJECT
414400070402     C************************************************************
414500070402      *?  INIZIALIZZAZIONE Vidoe di Dettaglio    ?
414600070402     C************************************************************
414700070402     C     INZD2         BEGSR
414800070402     C*
414900070312     C* Pulizia campi e indicatori
415000070312     c                   exsr      INZ_indERR
415100070312     C*
415200080701     C* Abilita il tasto F8 x visualizzare le FASI presenti su FIDSF
415300080701     c                   eval      *in18 = d2fasi
415400080701     c                   eval      sav_in18 = *in18
415500080701     C*
415600070312     C* Pulizia formato record   campi video
415700070312     c                   clear                   DG31D02
415800070403     c                   if        se_EDP = 'S'
415900070830     C***                SETON                                          15
416000070403     c                   end
416100070314     C*
416200070314     C* Se richiamato attivo 04
416300070314     c                   if        pgm_da_call = 'S'
416400070314     C                   SETON                                          04
416500070314     c                   end
416600070312     C*
416700070312     C* Imposta indicatore di selezione
416800080701     c                   move      sav_in18      *in18
416900070313     c                   move      sav_in19      *in19
417000080219     c                   move      sav_in15      *in15
417100080818     c                   move      sav_in08      *in08
417200070312     C* campi titolo
417300070312     C                   MOVEL     TES(1)        d2cTES
417400080715     c                   if        fdgPGM ='ASSMANUALE' or fdgPGM ='MONITOR'
417500070606     C                   MOVEL(p)  TES(2)        d2CTES
417600070606     c                   end
417700070312     C                   MOVEL     dsPGM         d2cPGM
417800070312     C                   MOVEL     RAGUT         d2cDSA
417900070312     C                   MOVEL     knsif         d2nsif
418000070312     C                   MOVEL     knmus         d2nmus
418100070312      *
418200070312     C*  Aggancia la distinta
418300070406     c     kDST9         chain     Fidst09L
418400070406     c                   if        %Found(Fidst09l)
418500080704     C*
418600080704     C* Controlla se ci sono delle Fasi
418700080704     C     Kdsf_01       setll     fiDSF01l
418800080704     c                   if        %Equal
418900080704     c                   eval      *in18 = *On
419000080704     c                   eval      sav_in18 = *in18
419100080704     c                   end
419200070312     C*
419300070312     C                   SETOFF                                       52  53
419400070312     C                   SETOFF                                       06  07
419500070312     C                   z-add     wnfv          d2cnfv
419600070312     C                   z-add     dstfgs        d2cfgs
419700070314     C                   z-add     dstpdr        d2caut
419800070314     c     kapd          chain     fiapd01l
419900070314     c                   if        %found(fiapd01l)
420000070314     c                   eval      d2DAUT = apdrsc
420100070314     c                   end
420200070314      *
420300070312     C* FILIALE GESTIONE
420400070312     C     dstFGS        CHAIN     AZORG                              21
420500070312     C  N21              MOVEL     ORGDES        d2DFGS
420600070312     C* DATA FOGLIO
420700070312     C                   MOVE      dstDFV        G02INV
420800070312     C                   MOVE      *ZEROS        G02DAT
420900070312     C                   MOVEL     '3'           G02ERR
421000070312     C                   CALL      'XSRDA8'
421100070312     C                   PARM                    WLBDAT
421200070312     C                   Z-ADD     G02DAT        d2CDFV
421300080820     C                   Z-ADD     dstDFV        h2CDFV
421400070312     c                   eval      d2cpkla= DSTPKLA
421500070312     c                   eval      d2cvlua= DSTVLUA
421600070312     c                   eval      d2cstpa= DSTSTPA
421700070312     c                   eval      d2cnbla= DSTnblA
421800070313     c                   eval      d2cpkl = DSTPKL
421900070313     c                   eval      d2cvlu = DSTVLU
422000070313     c                   eval      d2cstp = DSTNFTL
422100070313     c                   eval      d2cnbl = DSTnbl
422200070903     c                   eval      d2cPDA = DSTpda
422300080917     c                   clear                   d2ann
422400080917     c                   if        DSTATB ='A'
422500080917     c                   movel     'Annullata'   d2ann
422600080917     c                   end
422700080207      *
422800080207      * per Entrambe prima era segnalato con (S) ora invece si vuole vedere (E)
422900080207      * inoltre se non è attivo il PDA si deve visualizzare Blank invece che "N".
423000080710     c*******            if        d2cPDA = 'N'
423100080710     c*******            eval      d2cPDA = ' '
423200080710     c*******            end
423300080207      *
423400080207      *  nel campo filler c'è il tipo di attivazione
423500080207     c                   eval      DDSTFLR =  DstFLR
423600080208     c                   setoff                                       14
423700080208     c                   if        dstDFV >= DateU8
423800080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
423900080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
424000080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
424100080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
424200080208     c                   seton                                        14
424300080208     c                   endIf
424400080208     c                   endIf
424500080213      *
424600080222     c                   if        dstDFV >= dateu8
424700080213     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
424800080213     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
424900080213     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
425000080213     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
425100080213     C                   eval      D2msgv= 'ATT. gestione PDA attivata'
425200080213     c                   endIf
425300080222     c                   endIf
425400080212      *
425500080212     C                   movel     §DSTtstPDA    d2CTSTPDA
425600080207      ***
425700070312     C                   MOVEL     dstFPP        d2CFPP                         *MATT/POM
425800070312     C                   MOVEL     dstPEP        d2CPEP                         *PIKING(S/N)
425900070312     C                   MOVEL     dsttpm        d2ctpm                         *MATT/POM
426000070312     C                   MOVEL     dstfca        d2cfca                         *MATT/POM
426100160503     C                   MOVEL     §dstturno     d2ctur                         Turno
426200160505     c                   if        d2ctpm <> 'M'
426300160505     C                   MOVEL     'Turno'       d2turno
426400160505     c                   endif
426500160504     c                   if        d2ctpm <> 'M' and
426600160504     c                               d2CTUR = *blank
426700160504     c                   eval        d2CTUR = '1'
426800160504     c                   endif
426900070403     c                   eval      d2cnot  = dstnot
427000070312     c                   if        d2ctpm = ' '
427100070312     c                   eval      d2ctpm = 'P'
427200070312     c                   end
427300070403      * BANCALI
427400070403     C     Kqdt          CHAIN     Fiqdt01l
427500070403     c                   if        %found(Fiqdt01l)
427600070403     C                   MOVEL     qdtnbnaf      D2CNBN                         *NR.BANCALI
427700070403     C                   MOVEL     qdtfnbaf      D2CFNBN                        *NR.BANCALI
427800070403     c                   end
427900070312      * VEDO SE ANNULLATO
428000070312     C     dstATB        IFEQ      'A'
428100070312     C                   SETON                                        5206
428200070312     C                   MOVEL     CANNUL        d2DTFV
428300070312     C                   END
428400070312      * VEDO SE CHIUSO
428500070312     C     dstFCF        IFNE      *BLANKS
428600070312     C     dstFCF        IFEQ      'S'
428700070312     C                   SETON                                        53
428800070312     C                   ELSE
428900070312     C                   SETON                                        06
429000070312     C                   MOVEL     CBLOCC        d2DTFV
429100070312     C                   ENDIF
429200070312     C                   ENDIF
429300070312     C*
429400070312     C* 07 ON ESISTONO SPUNTE SUL FOGLIO
429500070312     C     Kdst          SETLL     FnBRV000                               07     ON=TROVATO
429600070312     C*
429700070910     c                   if        d1Dati = 'C'
429800070910     C*
429900070314     C* Carica i GIRI della Distinta
430000070314     c     kdst31        setll     fidst31l
430100070314     c     kdst31        reade     fidst31l
430200070314     c                   z-add     0             x                 3 0
430300070314      **
430400070314     c                   dow       not %Eof(fidst31l)
430500070314      **  Controlla presenza del giro richiesto
430600070314     c                   if        %char(DST3NPG) = d1cNPG and
430700070314     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
430800070314     c                   add       1             x
430900070315     c                   if        x > 16
431000070314     c                   leave
431100070314     c                   end
431200070314      *
431300070314     c                   movel     dst3cgi       cgi(x)
431400070314      *  controllo validità giro
431500070314     c                   clear                   fidg09ds
431600070314     c                   eval      D09IOP0 = '001'
431700070314     c                   eval      d09ifgs = wfgs
431800071010     c*****              eval      d09itug = 'C'
431900071010     c                   eval      d09itug = d1DATI
432000070314     c                   eval      d09icgi = dst3cgi
432100070314     c                   move      dateu8        d09idat
432200070314     c                   eval      kpjbu = fidg09ds
432300070315     c                   call      'FIDG09R'
432400070314     c                   parm                    kpjba
432500070314     c                   eval      fidg09ds = kpjbu
432600070314     c                   if        d09oerr <> '1' and d09ocgi <> ' '
432700070314     c                   eval      dgi(x) = d09odes
432800070314     c                   end
432900070314      *
433000070314     c                   end
433100070314     c     kdst31        reade     fidst31l
433200070314     c                   endDo
433300070910      **
433400070910     c                   else
433500070910      **
433600070910     C* Carica i GIRI della Distinta
433700070910     c     kdst51        setll     fidst51l
433800070910     c     kdst51        reade     fidst51l
433900070910     c                   z-add     0             x                 3 0
434000070910      **
434100070910     c                   dow       not %Eof(fidst51l)
434200070910      **  Controlla presenza del giro richiesto
434300070910     c                   if        %char(DST5NPG) = d1cNPG and
434400070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
434500070910     c                   add       1             x
434600070910     c                   if        x > 16
434700070910     c                   leave
434800070910     c                   end
434900070910      *
435000070910     c                   movel     dst5cgi       cgi(x)
435100070910      *  controllo validità giro
435200070910     c                   clear                   fidg09ds
435300070910     c                   eval      D09IOP0 = '001'
435400070910     c                   eval      d09ifgs = wfgs
435500071010     c****               eval      d09itug = 'C'
435600071010     c                   eval      d09itug = d1DATI
435700070910     c                   eval      d09icgi = dst5cgi
435800070910     c                   move      dateu8        d09idat
435900070910     c                   eval      kpjbu = fidg09ds
436000070910     c                   call      'FIDG09R'
436100070910     c                   parm                    kpjba
436200070910     c                   eval      fidg09ds = kpjbu
436300070910     c                   if        d09oerr <> '1' and d09ocgi <> ' '
436400070910     c                   eval      dgi(x) = d09odes
436500070910     c                   end
436600070910      *
436700070910     c                   end
436800070910     c     kdst51        reade     fidst51l
436900070910     c                   endDo
437000070910      **
437100070910      *
437200070910     c                   end
437300070910     C*
437400070406     c     d2cgi1        comp      *blank                                 65
437500070314      **
437600070312     c                   end
437700070312     C*
437800070312     C                   ENDSR
437900070312     C************************************************************
438000070321      *?  Controlli Video di Dettaglio                           ?
438100070312     C************************************************************
438200070312     C     CTRD2         BEGSR
438300070312     C*
438400070312     C*  Spegne indicatori di errore
438500070312     c                   exsr      INZ_indERR
438600070312     C*
438700070312     c                   seton                                        99
438800070406     c     d2cgi1        comp      *blank                                 65
438900070312      *
439000070312      * ? * Ritorno al primo video
439100070312     c                   move      'D1'          $gest
439200070312     C*------------------------
439300070312     C*
439400070312     C                   ENDSR
439500070301      *---------------------------------------------------------------*
439600070321      *
439601170714      *
    OPERAZIONI INIZIALI        ?
439800070301      *---------------------------------------------------------------*
439900940131     C     *INZSR        BEGSR
440000030113     C*
440100030113     C* Reperimento parametri
440200030113     C     *ENTRY        PLIST
440300030113     C                   PARM                    KPJBA
440400070404     c                   parm                    $FV6
440500070309      *
440600070313     C                   MOVEL     KPJBU         FIDG31DS
440700080625     c                   if        fdgIEfas = *blank
440800080625     c                   eval      fdgIEfas = 'I'
440900080625     c                   end
441000080616      *
441100070904      * default -> Consegne
441200070904     C                   IF        fdgTpoP      = *blank
441300070904     c                   movel     'C'           fdgTpoP
441400070904     c                   end
441500070903     c                   movel     fdgTpoP       Tipo_Prestaz      1
441600070309      *
441700070606      * memorizza se richiamato da Assegnazione Manuale da quale tipo
441800070606      *  di operazione è stato chiamato Assegnazione oppure altro.
441900070606     c                   clear                   fdg31_opz         1
442000080715     c                   if        fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR'
442100080606     c                   z-add     fdgNFV        SAVfdgNFV
442200080606     c                   z-add     0             fdgNFV
442300070606     c                   move      fdgOPZ        fdg31_opz         1
442400110309     c*********
442500110309     c********* Solo nel caso si trattasse di Togliere da una distinta apro il DROP
442600110309     c*********   altrimenti NON mi serve aprire direttamente i dati sulle singole DISTINTE.
442700110309     c                   if        fdg31_opz = '2'
442800080703     c                   eval      Apri_Drop = 'S'
442900110309     c                   end
443000110309     c*********
443100070607     c                   eval      cur_su_SFL = 'S'
443200070606     c                   end
443300070606      *
443400070309     C                   Z-ADD     1             CODUT
443500070309     C                   CALL      'X§PARUT'
443600070309     C                   PARM                    UT§DSE
443700070309     C                   MOVEL     REC80         CNCR80
443800070309     C***
443900070309     C     Kdst          KLIST
444000070309     C                   KFLD                    WNPG
444100070312     C                   KFLD                    wNFV
444200070312     C                   KFLD                    wFGS
444300070312     C***
444400080613     C     Kdsf_01       KLIST
444500080613     C                   KFLD                    dstNPG
444600080613     C                   KFLD                    dstNFV
444700080613     C                   KFLD                    dstFGS
444800080613     C***
444900070406     C     Kdst9         KLIST
445000070406     C                   KFLD                    wFGS
445100070406     C                   KFLD                    WNPG
445200070406     C                   KFLD                    wNFV
445300070406     C***
445400070312     C     Kdst31        KLIST
445500070312     C                   KFLD                    wFGS
445600070312     C                   KFLD                    wNFV
445700070314      *
445800070910     C     Kdst51        KLIST
445900070910     C                   KFLD                    wFGS
446000070910     C                   KFLD                    wNFV
446100070910      *
446200070403     C     Kqdt          KLIST
446300070403     C                   KFLD                    wFGS
446400070403     C                   KFLD                    WNFV
446401170714     C                   kfld                    dstdfv
446500070403      *
446600070314     C     KORM          KLIST
446700070314     C                   KFLD                    wFGS
446800070314     C                   KFLD                    WNFV
446900070309     C***
447000070309     C     Kapd          KLIST
447100070309     C                   KFLD                    dsttipa
447200070309     C                   KFLD                    dstpdr
447300070309      * Fnarb
447400070309     c     Karb78        Klist
447500070309     c                   Kfld                    Wifp
447600070309     c                   Kfld                    Kfnvv
447700070309      *
447800070309     C     KDAT          KLIST
447900070309     C                   KFLD                    WNPG
448000070309     C                   KFLD                    DATINW
448100070309
448200070309     c     Kdat1         Klist
448300070314     c                   Kfld                    wFGS
448400070309     c                   Kfld                    Wnpg
448500070309     c                   Kfld                    Datinw
448600070309
448700070309     c     Kdat2         Klist
448800070314     c                   Kfld                    wFGS
448900070309     c                   Kfld                    Wnpg
449000070309      *
449100070309     C     Kdst3         KLIST
449200070314     C                   KFLD                    wFGS
449300070309     C                   KFLD                    WNPG
449400070309      *
449500070309     C     Kimpo         Klist
449600070309     C                   Kfld                    arbaas
449700070309     C                   Kfld                    arblnp
449800070309     C                   Kfld                    arbnrs
449900070309     C                   Kfld                    arbnsp
450000080702      *
450100080702     C     KTbe1         KLIST
450200080702     C                   KFLD                    TBEcod
450300080702     C                   KFLD                    TBEke1
450400070309     C***
450500070309     C* CARICO TABELLA FILIALI GESTITE £1
450600070309     C                   CLEAR                   DSUL06
450700070309     C                   MOVE      '£1'          D06COD
450800070309     C                   MOVEL     SIMFEL        D06KEY
450900070309     C                   MOVEL     'L'           D06TLA
451000070313     c                   eval      kpjbus = kpjbu
451100070309     C                   MOVEL     DSUL06        KPJBU
451200070309     C                   CALL      'TRUL06R'
451300070309     C                   PARM                    KPJBA
451400070309     C                   MOVEL     KPJBU         DSUL06
451500070313     c                   eval      kpjbu = kpjbus
451600070309     C                   MOVEA     LIN           L1
451700070309     C*
451800070309      *  Se richiamata da EDP
451900070308     c                   CLEAR                   se_EDP            1
452000070308     c                   if        %subst(Knmus:1:3) = 'EDP'
452100070308     c                   MOVE      'S'           se_EDP
452200070307     c                   end
452300070307      *
452400070227      * Reperisco dati job
452500070227     c                   exsr      DatiJob
452600070227
452700030113     C* Variabili per gestione videate
452800070313     C                   MOVE      *BLANK        Sav_$GEST
452900070313     C                   MOVE      *BLANK        $GEST             2
453000030113     C                   MOVE      *BLANK        $FINE             1
453100070312     C                   MOVE      *off          $INZD1            1
453200070312     C                   MOVE      *off          $INZD2            1
453300070312     C                   MOVE      *off          $INZS1            1
453400070312     C                   MOVE      *off          $INZS2            1
453500070312     C                   MOVE      *off          $INZS3            1
453600070312     C                   clear                   $visD1            1
453700070312     C                   clear                   $visD2            1
453800070312     C                   clear                   $visS1            1
453900070312     C                   clear                   $visS2            1
454000070312     C                   clear                   $visS3            1
454100030113     C                   MOVE      *BLANK        $EFILE            1
454200030113     C                   MOVE      *BLANK        $ESCI             1
454300030113     C                   MOVE      *BLANK        $RCDOK            1
454400070314     C                   MOVE      *on           $PcurS2           1
454500070321     C                   MOVE      *on           $PcurS1           1
454600070320     C*
454700070320     C                   Z-ADD     0             CURR              2 0
454800070320     C                   Z-ADD     0             CURC              2 0
454900030113     C* Indici
455000030113     C                   Z-ADD     0             X                 3 0
455100030113     C                   Z-ADD     0             Y                 3 0
455200070227     C***
455300070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
455400070227     C***
455500070227     C                   TIME                    W0140            14 0
455600070227     C                   MOVE      W0140         UDATE8            8 0
455700070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
455800070227     C*
455900070227     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
456000070227     C                   MOVEL     *BLANK        G02ERR
456100070227     C                   CALL      'XSRDA8'
456200070227     C                   PARM                    WLBDAT
456300070301     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
456400940127     C*
456500940117     C                   ENDSR
456600070227      *---------------------------------------------------------------*
456700070321      *
456701170714      *
  Reperimento Dati del job (Utente/Operativi)     ?
456900070227      *---------------------------------------------------------------*
457000070228     c     DatiJOB       BEGSR
457100070227      *
457200070227     c     *dtaara       define    §azute        azuteds
457300070227     c     *dtaara       define    §datiute      ddatiute
457400070227      *
457500070227     c                   in(E)     *dtaara
457600070312     c                   IF        %ERROR or X_RSUT = *blanks
457700070227     c                   clear                   Tibs34Ds
457800070227     c                   call      'TIBS34R'
457900070227     c                   parm                    Tibs34Ds
458000070227     c                   in        *dtaara
458100070227     c                   ENDIF
458200070227      *
458300070227     c                   ENDSR
458400070312     C*-----------------------------------------------------***********
458500070321      *
458501170714      *
  Imposta i campi di totale   ?
458700070312     C*-----------------------------------------------------***********
458800070312     c     totalizzo     begsr
458900070312      *--
459000070321     c                   clear                   w_imca           13 3
459100070321     c                   clear                   w_imas           13 3
459200070321      *--
459300070312     c     Karb78        setll     fnarb78l
459400070312     c                   do        *hival
459500070321     c     karb78        reade     fnarb78l
459600070321     c                   if        %eof(fnarb78l)
459700070321     c                   leave
459800070321     c                   endif
459900070321
460000070312      * totalizzo gli assegnati
460100070312     c                   if        arbica = *blank or arbica = 'R'
460200070312     c     kimpo         chain     fiar601l                           21
460300070321     c  n21              add       ar6ift        w_imas
460400070312     c                   endif
460500070312      * totalizzo i contrassegni
460600070312     c                   if        arbicc = *blank or arbicc = 'R'
460700070312     c     kimpo         chain     fiar901l                           21
460800070312     c                   if        not *in21
460900070312     c                   if        ar9vca = 'EUR'
461000070321     c                   add       ar9cas        w_imca
461100070312     c                   else
461200070312      *se la divisa non è EUR traduco con yeurco l'importo
461300070312     c                   CLEAR                   YEURDS
461400070312     c                   MOVEL     ar9vca        YECDVI
461500070312     c                   Z-ADD     ar9cas        YECIMI
461600070312     c                   MOVEL     'EUR'         YECDVO
461700070312     c                   MOVEL     'H'           YECTAR
461800070312     c                   CALL      'YEURCO'
461900070312     c                   PARM                    YEURDS
462000070321     c                   add       YECIMO        w_imca
462100070312     c                   endif
462200070312     c                   endif
462300070312     c                   endif
462400070312     c                   enddo
462500070312      *
462600070312     c                   endsr
462700070309     C/EJECT
462800070309     C*-----------------------------------------------------***********
462900070309**  PGM RICHIAMANTI
463000070309FIDG31R
463100070309**  TES
463200070830     INTERROGAZIONE DISTINTE
463300070606 **    ASSEGNAZIONE MANUALE    **
463400070606 **  Interrogazione Distinte   **
463500070830 **                            **
463600070830 **    Dettaglio   Consegne    **
463700070830 **    Dettaglio    Ritiri     **
463800070830 **     Consegne e  Ritiri     **
463900070309**
464000070309Filiale in gestione errata o mancante
464100160503Filiale obbligatoria
464200070309Importi visualizzabili soltanto richiedendo Tipo fogli=2 non chiusi
464300160503I Turni possono essere soltanto su distinte PARCEL e NON messaggerie
464400160503I Turni sono una scelta obbligata su distinte PARCEL
