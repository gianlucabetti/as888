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
039100071029     D  H2recpos
039200070322     D  Selected2                     1A
039300070313
039400070321     D SflRcd1         DS
039500070321     D  S1DAUT
039600070321     D  S1CNFV
039700070321     D  S1CDFV
039800070417     D  H1CDFV
039900070510     D  H1PRGG
040000160505     D  S1CTUR
040100070404     D  S1SCE
040200070321     D  S1CPEM
040300070321     D  S1CPEA
040400070321     D  S1CVOM
040500070321     D  S1CVOA
040600070321     D  S1IMCA
040700070321     D  S1IMAS
040800070321     D  S1CTPM
040900070321     D  S1CTPD
041000070321     D  S1CPDA
041100080207     D  S1CTSTPDA
041200070321     D  S1CFCF
041300070321     D  S1CANN
041400070625     D  S1TUSC
041500070820     D  S1STP
041600080222     D  H1in14
041700070322     D  Selected1                     1A
041800080207      * ?------------------------------------------------------------------ */
041900080207      * ?set caratteri Maiuscoli/Minuscoli
042000080207     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
042100080207     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
042200080207      * ?------------------------------------------------------------------ */
042300070313      /COPY QSYSINC/QRPGLESRC,QLGSORT
042400070313     D QLGKL                         16    DIM(MaxKey)
042500070313     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
042600070313     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
042700070313     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
042800070313     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
042900070313
043000070313      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
043100070313      /COPY QSYSINC/QRPGLESRC,QUSEC
043200940127     C*----------------------------------------------------*
043300070321      *?     MAIN LINE PROGRAM     ?
043400940127     C*----------------------------------------------------*
043500940223     C* inizializzazione variabili
043600940223     C                   EXSR      INZVAR
043700940223     C*
043800940223     C     $FINE         DOWEQ     *OFF
043900070227     C     $GEST         CASEQ     'D1'          GESD1
044000070321     C     $GEST         CASEQ     'S1'          GESS1
044100070227     C     $GEST         CASEQ     'S2'          GESS2
044200070330     C     $GEST         CASEQ     'D2'          GESD2
044300940117     C                   END
044400070309     C                   ENDdo
044500940325     C* fine programma
044600070313     C                   MOVEL(p)  FIDG31DS      KPJBU
044700071029     C                   SETON                                            LR
044800030113     C************************************************************
044900070321      *?  INIZIALIZZAZIONE VARIABILI   ?
045000030113     C************************************************************
045100030113     C     INZVAR        BEGSR
045200030113     C*
045300030113     C* Variabili per gestione videate
045400070312     C                   move      *off          $INZS1
045500070312     C                   move      *off          $INZS2
045600070312     C                   move      *off          $INZS3
045700070312     C                   move      *off          $INZD2
045800070312     C                   move      'S'           $VISS1
045900070312     C                   move      'S'           $VISS2
046000070312     C                   move      'S'           $VISS3
046100070312     C                   move      'S'           $VISD1
046200070330     C                   move      'S'           $VISD2
046300070309      *
046400070309     C                   MOVE      *OFF          $FINE
046500030113     C                   MOVE      *OFF          $EFILE
046600030113     C                   MOVE      *OFF          $ESCI
046700030113     C                   MOVE      *OFF          $RCDOK
046800070314     C                   move      *on           $PcurS2
046900070321     C                   move      *on           $PcurS1
047000070227     C* Emissione primo video
047100070227     C                   MOVE      'D1'          $GEST
047200070227     C                   MOVE      *ON           $INZD1
047300070312      *
047400070312      * ? Legge sempre i Fogli (4)
047500070312     C                   MOVEL     '4'           wNPG
047600070312     C* Variabili appoggio
047700030114     C                   Z-ADD     1             WPAG
047800070312     c                   clear                   pgm_da_call
047900160601     c                   move      *off          sav_in11
048000160505     c                   move      *off          sav_in10
048100030113     c*
048200030113     C                   ENDSR
048300070227     C************************************************************
048400070321      *?  Emissione primo video di SCELTA   ?
048500070227     C************************************************************
048600070227     C     GESD1         BEGSR
048700070227     C*
048800070227     C* inizializzazione videata
048900070227     C     $INZD1        IFEQ      *ON
049000070227     C                   EXSR      INZD1
049100070227     C                   MOVE      *OFF          $INZD1
049200070227     C                   ENDIF
049300070227     C*
049400070228     c     Video_1       tag
049500070302     C*  Emissione Video
049600070302     c                   setoff                                       99
049700070309     c                   if        $VISD1 = 'S'
049800070228      * ?            *-----------------------*
049900070309     C                   EXFMT     DG31D01
050000070228      * ?            *-----------------------*
050100070313     C                   movel     'D1'          Sav_$GEST
050200070309     c                   end
050300070227     C* Selezioni
0504000702271    C                   SELECT
050500070227     C* F3=Fine
050600070227     C                   WHEN      $TASTO   =   F03
050700080606     c                   move      '03'          fdgtfu
050800070227     C                   EXSR      F03_exit
050900070312     c                   goto      end_d01
051000070227      *
0511000702271O   C                   OTHER
051200070227     C* CONTROLLO DATI
051300070227     C                   EXSR      CTRD1
0514000702271-   C                   ENDSL
051500070227     C*
051600070228     C* Per errori o decodifiche
051700070228     c   99              goto      Video_1
051800070228      *
051900070312      * ? * Emissione Lista o Dettaglio
052000070312     c                   exsr      DSP_seguente
052100070228      *
052200070312     c     end_d01       tag
052300070227     C                   ENDSR
052400070306     C/EJECT
052500070306     C************************************************************
052600070321      *?  Quale Visualizzazione deve impostare        ?
052700070306     C************************************************************
052800070312     C     DSP_seguente  BEGSR
052900070306     C*
053000070312     C*  Se non immesso il Nr.Foglio
053100070312if  0c                   if        d1CNFV = 0
053200070312     C*
053300070309     C*  Quale SFL deve mostrare in base numero Foglio
053400070309     C*  Emissione SFL di elenco solo se non richiamato
053500070419if  1c                   if        FDGtfv  = '3'
053600070419     c                   eval      $GEST ='S1'
053700070419     C                   MOVE      *ON           $INZS1
053800070419     c                   eval      $VISS1 = 'N'
053900070419     c                   else
054000070309      *
054100070321if  1c                   if        d1tipv = 'P'
054200070309     c                   eval      $GEST ='S2'
054300070309     C                   MOVE      *ON           $INZS2
054400070326     c                   seton                                        09
054500070321     c                   else
054600070321     c                   eval      $GEST ='S1'
054700070321     C                   MOVE      *ON           $INZS1
054800070326     c                   setoff                                       09
054900070321     c                   end
055000070309     C*
055100070309e   1c                   endif
055200070309      *
055300070312x   0c                   else
055400070312     C*
055500070312     C*  Dettaglio della Distinta
055600070312     c                   eval      $GEST ='D2'
055700070312     C                   MOVE      *ON           $INZD2
055800070312     C                   exsr      GESD2
055900070312     C*
056000070312e   0c                   endif
056100070312      *
056200070306     C                   ENDSR
056300070227     C/EJECT
056400070227     C************************************************************
056500070321      *?  INIZIALIZZAZIONE Primo video      ?
056600070227     C************************************************************
056700070227     C     INZD1         BEGSR
056800070227     C*
056900070227     C* Pulizia campi e indicatori
057000070308     c                   exsr      INZ_indERR
057100070309     C*
057200070309     C* Pulizia formato record   campi video
057300070309     c                   clear                   DG31D01
057400070309     C* campi titolo
057500070312     C                   MOVEL     TES(1)        d1CTES
057600080715     c                   if        fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR'
057700070606     C                   MOVEL(p)  TES(2)        d1CTES
057800070606     c                   end
057900070312     C                   MOVEL     dsPGM         d1CPGM
058000070312     C                   MOVEL     RAGUT         D1CDSA
058100070312     C                   MOVEL     knsif         d1nsif
058200070312     C                   MOVEL     knmus         d1nmus
058300070321      *
058400070822     C                   Z-ADD     *date         d1CDIN
058500070822     C                   Z-ADD     31129999      d1CDFI
058600070321     c                   move      'N'           d1IMPO
058700070822     c                   move      ' '           d1STP
058800070309     C* Solo Fogli (4)
058900070312     C                   MOVEL     wnpg          d1cnpg
059000070330     c                   move      'T'           d1TIPV
059100070830     c                   move      '2'           d1ctfv
059200080610     c                   move      'I'           d1ieFA
059300160503     C                   MOVEL     '1'           d1Ctur1
059400160503     C                   MOVEL     '2'           d1Ctur2
059500160505     C                   MOVEL     ' '           d1Ctpm
059600070309     C***
059700070309     C* ??????????????????????????
059800070309     C* Il p.o.deve essere impostato in base al profilo
059900070312     C                   MOVEL     x_dutpou      d1CFGS                         *FIL.GESTIONE
060000070309     C***
060100070309     C* VEDO SE SONO SIMFEL O REMOTO
060200070309     c     simtpp        ifeq      '2'
060300070309     c     simtpp        oreq      *blanks
060400070312     c                   movel     simpou        d1cfgs
060500070309     C                   ELSE
060600070312     C                   MOVEL     SIMFEL        d1CFGS
060700070309     C                   END
060800070309      *
060900070309      * ? memorizzo che è richiamato anche con 01 = *on -> pgm RICHIAMATO
061000070313     C     FDGRIC        IFNE      ' '
061100070312     c                   move      'S'           pgm_da_call       1
061200070404      *
061300070404      *  Se richiamato da Interrogazione
061400070404     c                   setoFF                                       17
061500070404     C     FDGRIC        IFeq      'I'
061600070404     c                   setoN                                        17
061700070404     c                   enD
061800070322      *
061900070322      * al momento deve ricevere a 0 questo parametro
062000070406     c********           if        FDGNPG <> 0
062100070406     C********           MOVEL     FDGNPG        d1cnpg
062200070406     c********           end
062300070322      *
062400070322     c                   if        FDGTPV <> *blank
062500070322     C                   MOVEL     FDGTPV        d1tipv
062600070322     c                   end
062700070309      *
062800070309      * ? Condiziono l'emissione del campo "1=Selezione" nei sfl
062900070404     c                   if        $FV6(1) = 999999 and $FV6(600) = 999999
063000070404     c                   setoFF                                       19
063100070404     c                   else
063200070404     c                   setoN                                        19
063300070404     c                   end
063400070313     c                   move      *in19         sav_in19
063500070419      *
063600070419      * Ripulisco eventuale flag di ritorno per FIMS03R
063700070419      * (se FNLR02R richiamato per sapere se ci sono distinte aperte
063800070419      *  non inserite nel piano a video di FIMS03R)
063900070419     c                   if        FDGtfv = '3'
064000070419     c                   clear                   FDGfs1
064100070419     c                   endif
064200070309      *
064300070321     C* IMPOSTO I CAMPI da A/M/G
064400070313     C                   MOVEL     FDGGDA        DATINW
064500070313     C                   MOVE      DATINW        G02INV
064600070313     C                   MOVEL     '3'           G02ERR
064700070313     C                   CALL      'XSRDA8'
064800070313     C                   PARM                    WLBDAT
064900070313     C                   Z-ADD     G02DAT        d1CDIN
065000070321      *
065100070313     C                   MOVEL     FDGGAL        DATFIW
065200070313     C                   MOVE      DATFIW        G02INV
065300070313     C                   MOVEL     '3'           G02ERR
065400070313     C                   CALL      'XSRDA8'
065500070313     C                   PARM                    WLBDAT
065600070313     C                   Z-ADD     G02DAT        d1CDFI
065700070321      *
065800070313     C                   MOVEL     FDGTFV        d1CTFV
065900070313     C                   MOVEL     FDGCGI        d1Ccgi
066000070313     C                   MOVEL     FDGFGS        d1CFGS
066100070313     C                   MOVEL     FDGNFV        d1CNFV
066200070625     C                   MOVEL     FDGtdis       d1Ctpm
066300070625     C                   MOVEL     FDGtusc       d1Tusc
066400080611     C*
066500070822     C                   MOVEL     FDGstp        d1stp
066600160512     C*
066700160512     C* se chiamato come UNICO deve far vedere tutto dei turni
066800160512     c                   if        FDGtur1 ='U' or FDGtur2 ='U'
066900160512     C                   MOVEL     '1'           FDGtur1
067000160512     C                   MOVEL     '2'           FDGtur2
067100160512     c                   end
067200160503     C*
067300160503     c                   if        FDGtdis <>'M'
067400160504     C                   MOVEL     fdgtur1       d1Ctur1
067500160504     C                   MOVEL     fdgtur2       d1Ctur2
067600160504     c                   end
067700070309     C*
067800160504     c                   if        FDGtdis <>'M'  and
067900160504     c                             fdgtur1 = ' '  and fdgtur2 = ' '
068000160504     C                   MOVEL     '1'           d1Ctur1
068100160504     C                   MOVEL     '2'           d1Ctur2
068200160504     c                   end
068300160504     C*
068400070309     C* VEDO SE VISUALIZZARE LA PRIMA VIDEATA (SCELTA PARAMETRI) O NO
068500070313     C     FDGRIC        IFNE      '§'                                          '§' SI 1° VIDEO
068600070309      * Non visualizzare il primo video
068700070309     C                   move      'N'           $VISD1
068800070309     C                   ENDIF
068900070309      *
069000070309     C                   END
069100070309     C*
069200070309     C* Aggancia Organigramma x picking su messaggerie
069300070309     c                   clear                   og150
069400070830     c                   clear                   og147
069500070927     c                   clear                   Abilita_ORM       1
069600160503     c                   setoff                                       083802
069700160506     c                   eval      sav_in38 = *in38
069800070312     C     d1CFGS        chain     AZORG01l
069900070309    1C                   IF        %found(azorg01l)
070000070312     C                   MOVEL     ORGDES        d1DFGS
070100070309     c                   movel     orgdf0        og150
070200070830     c                   movel     orgde7        og147
070300080207     c                   movel     orgde8        og148
070400070927      * Abilitazione a trattare gli ORM
070500070927     c                   movel     §ogcGIO       Abilita_ORM
070600070830      *
070700070830     c* p.o. con ORM
070800070830     c                   if        §ogDDAO <> *blank
070900070830     c                   move      §ogDDAO       data_OrM          8 0
071000070830     c                   seton                                        02
071100070830     C                   MOVE      *BLANK        D1DATI
071200070904     C                   MOVE      Tipo_Prestaz  D1DATI
071300070830     c                   eLSE
071400070830     c                   setoFF                                       02
071500070830     C                   MOVE      'C'           D1DATI
071600070830     c                   end
071700070312      *
071800070309     c* p.o. con piking messaggerie
071900070309     c                   if        §ogpkm='S'
072000070309     c                   seton                                        08
072100070309     c                   endif
072200070309     c                   eval      fgspkm = §ogpkm
072300160503      *
072400160503     c* p.o. con TURNO
072500160503     c                   if        §OGTURNO='S'
072600160503     c                   seton                                        38
072700160506     c                   eval      sav_in38 = *in38
072800160503     c                   endif
072900160503     c                   eval      fgsturno = §OGTURNO
073000070309     c                   end
073100070309      *
073200080219     c* in test o attivo x PDA
073300080219     c                   setoff                                       15
073400080725     c                   eval      Filiale_con_PDA = 'N'
073500080611     c                   clear                   PDAconsegne
073600080623     c                   clear                   PDAritiri
073700080623      *
073800080623      * se attivo PDA x Consegne o Ritiri
073900080219     c                   if        §OGPDAORM <> *blank or
074000080219     c                             §OGPDACON <> *blank
074100080725     c                   eval      Filiale_con_PDA = 'S'
074200080623     c                   seton                                        15
074300080611     c                   eval        PDAconsegne = §OGPDACON
074400080623     c                   eval        PDAritiri   = §OGPDAORM
074500080724      *
074600080724     c                   end
074700080616      *
074800080611      *  se richiesta/o non richiesta Fase "Stampata":
074900080724      *   e se congruente con eventuali altre selezioni di fasi
075000080724      *    in Inclusione o Eslusione altrimenti comunque durante la
075100080724      *    selezione dei records esegue il controllo x le Stampate
075200080724     c                   if          fdgIEfas = *blank or
075300080724     c                               (fdgIEfas ='I' and d1STP = 'S') or
075400080724     c                               (fdgIEfas ='E' and d1STP = 'N')
075500080623     c                   If        d1STP = 'S'
075600080611     c                   move      'I'           d1ieFA
075700080611     c                   eval      d1FA01 ='STP'
075800080623     c                   elseIf    d1STP = 'N'
075900080611     c                   move      'E'           d1ieFA
076000080611     c                   eval      d1FA01 ='STP'
076100080611     c                   endIf
076200080724      *
076300080724     c                   end
076400080623      *
076500080623      *  Attenzione se però sono passate le FASI a livello di
076600080623      *    Inclusione/Esclusione
076700080623      *  queste prevalgono sul flag di Stampato:
076800080724      * SARA' LA ROUTINE DI CONTROLLO, che filtra i records letti,
076900080724      *  a selezionare i records in base ALLE FASI passate e
077000080724      *   alle STAMPATE si/no.
077100080724      *
077200080623     c                   If        fdgFASIds <>*blank
077300080623      * includi/escludi
077400080623     c                   move      fdgIEfas      d1ieFA
077500080623      * Se invece ha passato delle Fasi le imposta
077600080623     c                   clear                   fs
077700080623     c                   Do        10            fas               3 0
077800080623     c                   if        fdgFASI(fas) <> *blank
077900080623     c                   add       1             fs
078000080623     c                   eval      d1fasi(fs) = fdgFASI(fas)
078100080623     c                   end
078200080623     c                   endDo
078300080616     c                   endIf
078400080616      *
078500070309     c* salvo campi di comodo
078600070312     c                   eval      savfgs = d1cfgs
078700070313     c                   eval      sav_in08 = *in08
078800160601     c                   eval      sav_in11 = *off
078900160505     c                   eval      sav_in10 = *off
079000080219     c                   eval      sav_in15 = *in15
079100070227     C*
079200070227     C                   ENDSR
079300940127     C************************************************************
079400070321      *? Controlli primo video di SCELTA Operazione da eseguire  ?
079500940127     C************************************************************
079600070227     C     CTRD1         BEGSR
079700030113     C*
079800070227     C*  Spegne indicatori di errore
079900070308     c                   exsr      INZ_indERR
080000080715     c                   clear                   d1cmsg
080100070309     C*
080200070309     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
080300160503     c                   setoff                                       083899
080400160506     c                   eval      sav_in38 = *in38
080500070309     C                   MOVE      *BLANK        test_Parallelo    1
080600070830     C                   MOVE      *BLANK        test_Paral_ORM    1
080700080222     C*
080800080222     C* RESETTA l'emissione del SFL di scelta inzializzandolo
080900080222     C*   ordinato x Numero:
081000160601     c                   eval      sav_in11 = *off
081100160505     c                   eval      sav_in10 = *off
081200070309     C*
081300070309      * Categoria Foglio x controllo nr.distinta
081400070312     C                   move      d1cnpg        wNPG
081500070309     C*------------------------
081600070309     C* SE IMMESSA DEVE ESSERE FILIALE VALIDA
081700070309     c                   clear                   fgspkm
081800160503     c                   clear                   fgsturno
081900070312     c                   clear                   d1DFGS
082000070309      *
082100070406     c* Filiale apertura obbligatoria
082200070406     c*    x Categoria 4 è sempre obbligatoria
082300070312     c                   if        d1cfgs=0
082400070312     C                   MOVEL     MSG(2)        d1Cmsg
082500070309     C                   SETON                                        51  99
082600070312     C                   GOTO      end_CT1
082700070309     c                   endif
082800070309      *
082900070309      *  FGS impostata
083000070312     C     d1CFGS        IFNE      0
083100070309     C*------------------------
083200070309      * ?- Controlla la filiale in gestione
083300070309     C                   CLEAR                   DSLV50
083400070309     C                   MOVEL     KNMUS         D50PRU
083500070312     C                   z-add     d1cFGS        D50FGS
083600070309     C                   CALL      'FNLV50R'
083700070309     C                   PARM                    DSLV50
083800070309    1C     D50ERR        IFNE      ' '
083900070312     C                   MOVEL     MSG(1)        d1Cmsg
084000070309     C                   SETON                                        51  99
084100070312     C                   goto      end_CT1
084200070309    1C                   ENDIF
084300070309      *
084400070309      * Esistenza su Organigramma
084500070312     c                   clear                   wfgs
084600070312     C     d1CFGS        chain     AZORG01l
084700070309    1C                   IF        not %found(azorg01l)
084800070312      *
084900070312     C                   MOVEL     MSG(1)        d1Cmsg
085000070309     C                   SETON                                        51  99
085100070312     C                   GOTO      end_CT1
085200070312      *
085300070309     c                   else
085400070312      *
085500070312     c                   z-add     d1CFGS        wfgs
085600070309     c                   movel     orgdf0        og150
085700070309     c                   movel     §ogpkm        fgspkm
085800160503     c                   movel     §ogturno      fgsturno
085900070312     c                   movel     orgdes        d1DFGS
086000070309     C                   MOVEL     ORGde6        og146
086100070830     C                   MOVEL     ORGde7        og147
086200080207     C                   MOVEL     ORGde8        og148
086300080219      *
086400080219      * in test o attivo x PDA
086500080219     c                   setoff                                       15
086600080725     c                   eval      Filiale_con_PDA = 'N'
086700080611     c                   clear                   PDAconsegne
086800080623     c                   clear                   PDAritiri
086900080623      *
087000080219     c                   if        §OGPDAORM <> *blank or
087100080219     c                             §OGPDACON <> *blank
087200080725     c                   eval      Filiale_con_PDA = 'S'
087300080623     c                   seton                                        15
087400080611     c                   eval        PDAconsegne = §OGPDACON
087500080623     c                   eval        PDAritiri   = §OGPDAORM
087600080219     c                   end
087700080623      *
087800080219     c                   eval      sav_in15 = *in15
087900080702      *
088000080702      *
088100080724      *  Controlli sulle fasi  del Video da fare solo se la filiale
088200080724      *   è partita con il PDA
088300080724     c                   if        (PDAconsegne <> ' ' or PDAritiri <> ' ')
088400080724     c                               and d1Fasids <> *blank
088500080702      *
088600080702      * Controlla se richiesta una fase non presente in tabella
088700080702     c                   Do        10            fas               3 0
088800080702     c                   if        d1fasi(fas) <> *blank
088900080702     c     '?'           scan      d1fasi(fas)
089000080702     c                   if        %Found
089100080702     c                   eval      kpjbus = kpjbu
089200080702     c                   movel(p)  '?'           kpjbu
089300080702     c                   call      'TNTB68R'
089400080702     c                   parm                    kpjba
089500080702     c                   movel     kpjbu         d1fasi(fas)
089600080702     c                   eval      kpjbu  = kpjbus
089700080702     c                   end
089800080702      *
089900080702      *  se la fase non è vuota
090000080702     c                   if        d1fasi(fas) <> *blank
090100080702      *
090200080702      *  se la filiale è partita con PDA la scelta "STAMPATE - S/N Tutte"
090300080702      *   è sostituita dalle fasi da scegliere:
090400080702      *    ma se si vogliono vedere solo distinte senza PDA
090500080702      *   x qualsiasi altra fase NON "STP" deve essere dato l'errore.
090600080702     c                   if        d1cPDA ='N' and d1fasi(fas)<>'STP'
090700080702     c                   eval      indicatore = 60 + fas
090800080702     c                   eval      *IN(indicatore) = *on
090900080702     c                   seton                                        99
091000080702     c                   leave
091100080702     c                   end
091200080702      *
091300080702      * controlla se la fase esiste sulla tabella DSF
091400080702     C                   movel(p)  'DSF'         TBEcod
091500080702     C                   movel(p)  d1fasi(fas)   TBEke1
091600080702     c     ktbe1         setll     tntbe01l
091700080702     c                   if        not %equal
091800080702     c                   eval      indicatore = 70 + fas
091900080702     c                   eval      *IN(indicatore) = *on
092000080702     c                   seton                                        99
092100080702     c                   leave
092200080702     c                   end
092300080702     c                   end
092400080702     c                   end
092500080702     c                   endDo
092600080702      *
092700080702     c                   end
092800080616      *
092900070309      * controllo sulla gestibilità del P.O.
093000070309     c                   if        §ogdda = *blank or §ogdda = *all'0'
093100070309     C                   MOVE      *ON           $FINE
093200070312     C                   goto      end_CT1
093300070309     c                   else
093400070309      *
093500070309      * se c'è una data in organigramma
093600070309     c                   move      §ogdda        data_org          8 0
093700070830     c                   move      §ogDDAO       data_orM
093800070927      * Abilitazione a trattare gli ORM
093900070927     c                   movel     §ogcGIO       Abilita_ORM
094000070309      *
094100070309     C*  Controlla Con il P.O. la data abilitazione
094200070309      * se siamo in fase di Parallelo prima di essere partiti
094300070426     c                   if        data_org = 20391231  or
094400070426     c                             data_org >  dateu8
094500070309     C                   eval      test_Parallelo = 'S'
094600070309     c                   end
094700070309      *
094800070830     C*  Controlla Con il P.O. la data abilitazione
094900070830      * se siamo in fase di Parallelo prima di essere partiti
095000070830     c                   if        data_OrM = 20391231  or
095100070830     c                             data_OrM >  dateu8
095200070830     C                   eval      test_Paral_ORM = 'S'
095300070830     c                   end
095400070830      *
095500070309      * se data distinte automatiche NON è
095600070309      * né = 31/12/2039 che significa p.o. in fase di test o parallelo
095700070309      * né < oggi che significa p.o. partito
095800070309      * allora fuori
095900070426     c******             if        data_org <> 20391231 and dateu8 < data_org
096000070426     C******             MOVE      *ON           $FINE
096100070426     C******             goto      end_CT1
096200070426     c******             end
096300070309     c                   end
096400070309      *
096500070309     c* P.o. con picking, messaggerie: visualizzo parzializzazione
096600070309     c                   if        fgspkm='S'
096700070309     c                   seton                                        08
096800070309    1C                   ENDif
096900160503      *
097000160503     c* P.o. con TURNO
097100160503     c                   if        fgsturno ='S'
097200160503     c                   seton                                        38
097300160506     c                   eval      sav_in38 = *in38
097400160503    1C                   ENDif
097500160503      *
097600070309    1C                   END
097700070309    1C                   END
097800070309      *
097900070309     C*------------------------
098000070309     c* per cambio p.o. gestione riemetto video 1 senza errori
098100070309     c*  può cambiare l'indicatore del picking messaggerie S/N
098200070312     c                   if        d1CFGS <> savfgs
098300160503     c**********         if        (sav_in08='0' AND *IN08)   or
098400160503     c**********                   (sav_in08='1' and not *in08)
098500160503     c**********         seton                                            99
098600160503     c**********         endif
098700080715      * si vuole forzare la riemissione del video al cambio della filiale
098800080715      * di gestione --> quindi 99 sempre ON
098900080715     c                   seton                                            99
099000080715      *
099100070313     c                   eval      sav_in08 = *in08
099200070312     c                   eval      savfgs  = d1CFGS
099300070309     c                   endif
099400070309     C*------------------------
099500070309      * CONTROLLO DATA INIZIALE
099600070309     C                   Z-ADD     0             DATINW
099700070312    1C     d1CDIN        IFNE      0
099800070312     C                   MOVE      d1CDIN        G02DAT
099900070309     C                   MOVEL     *BLANK        G02ERR
100000070309     C                   CALL      'XSRDA8'
100100070309     C                   PARM                    WLBDAT
100200070309    2C     G02ERR        IFEQ      '1'
100300070309     C                   SETON                                        57  99
100400070312     C                   GOTO      end_CT1
100500070309    2C                   END
100600070309     C                   Z-ADD     G02INV        DATINW
100700070312     C                   Z-ADD     G02DAT        d1CDIN
100800070309    1C                   END
100900070309      *
101000070309      * CONTROLLO DATA FINALE
101100070312    1C     d1CDFI        IFEQ      0
101200070312     C                   Z-ADD     31129999      d1CDFI
101300070309     C                   Z-ADD     99991231      DATFIW
101400070309   X1C                   ELSE
101500070312     C                   MOVE      d1CDFI        G02DAT
101600070309     C                   MOVEL     *BLANK        G02ERR
101700070309     C                   CALL      'XSRDA8'
101800070309     C                   PARM                    WLBDAT
101900070309    2C     G02ERR        IFEQ      '1'
102000070309     C                   SETON                                        58  99
102100070312     C                   GOTO      end_CT1
102200070309    2C                   END
102300070309     C*
102400070309     C                   Z-ADD     G02INV        DATFIW
102500070312     C                   Z-ADD     G02DAT        d1CDFI
102600070309      *
102700070309     C*------------------------
102800070309      * DATA INIZIALE MAGGIORE DATA FINALE
102900070309    2C     DATINW        IFGT      DATFIW
103000070309     C                   SETON                                        54  99
103100070312     C                   GOTO      end_CT1
103200070309    2C                   END
103300070309    1C                   END
103400070402      *?  Fare attenzione: si Abilita il posizionamento x Distinta...  ?
103500070402      *?   Solo se il range di date indica lo stesso giorno.
103600070402      *?   Si utilizza l'indicatore 13 per abilitare o meno il campo.
103700070402     c                   setoff                                       13
103800070403     c                   eval      cur_su_SFL = 'S'
103900070404    2C                   IF        DATFIW = DATINW  and
104000070404     c                                pgm_da_call <> 'S'
104100070403     c                   eval      cur_su_SFL = 'N'
104200070402     c                   seton                                        13
104300070402     c                   end
104400070309     C*------------------------
104500070309      * NUMERO FOGLIO
104600070312     c                   clear                   wnfv
104700070312    1C     d1CNFV        IFGT      0
104800070309     C* VEDO SE E' FOGLIO DI FILIALE GESTIBILE
104900070312     c                   z-add     d1CNFV        wnfv
105000070406     C     Kdst9         CHAIN     Fidst09L                           59
105100070309     C* FOGLIO INESISTENTE
105200070309    2C     *IN59         IFEQ      *ON
105300070309     C                   SETON                                          99
105400070312     C                   GOTO      end_CT1
105500070309    1C                   ENDIF
105600070312    1C                   ENDIF
105700070309     C*
105800070309     C*------------------------
105900070309     c                   move      'N'           Solo_aperte       1
106000070309     C* RICHIESTI SOLO FOGLI APERTI
106100070312     C     d1CTFV        IFEQ      '2'
106200070419     C     d1CTFV        orEQ      '3'
106300070309     c                   move      'S'           Solo_aperte
106400070309     C                   END
106500070309     C*------------------------
106600070309      * ?- Codice Giro
106700070309     c                   clear                   con_giro
106800070312     c                   clear                   d1dcgi
106900070312     c     '?'           scan      D1CCGI
107000070309     C*   ricerca codice giro
107100070309     c                   if        %Found
107200070309     C                   SETON                                            99
107300070309     c                   clear                   fidg09ds
107400070309     c                   move      'P01'         d09iop0
107500070312     c                   movel     d1CFGS        d09ifgs
107600071010     c*****              move      'C'           d09itug
107700071010     c                   move      d1DATI        d09itug
107800070309     c                   z-add     DATEU8        d09idat
107900070313     c                   eval      kpjbus = kpjbu
108000070309     c                   movel(p)  fidg09ds      kpjbu
108100070309     c                   call      'FIDG09R'
108200070309     c                   parm                    kpjba
108300070309     c                   movel     kpjbu         fidg09ds
108400070313     c                   eval      kpjbu = kpjbus
108500070320     c                   movel     d09oflo       dGRGflo
108600070320      * se il giro può essere messo in distinta
108700070320     c                   if        §grgDST <> 'N'
108800070312     c                   movel     d09ocgi       D1CCGI
108900070320     c                   end
109000070309     c                   end
109100070309      *
109200070309      *  controllo validità giro
109300070312     c                   if        D1CCGI <> *blank
109400070309     c                   clear                   fidg09ds
109500070309     c                   eval      D09IOP0 = '001'
109600070312     c                   eval      d09ifgs = d1CFGS
109700071010     c******             eval      d09itug = 'C'
109800071010     c                   eval      d09itug = d1DATI
109900070312     c                   eval      d09icgi = D1CCGI
110000070309     c                   move      dateu8        d09idat
110100070313     c                   eval      kpjbus = kpjbu
110200070309     c                   eval      kpjbu = fidg09ds
110300070309     c                   call      'FIDG09R'
110400070309     c                   parm                    kpjba
110500070309     c                   eval      fidg09ds = kpjbu
110600070320     c                   movel     d09oflo       dGRGflo
110700070313     c                   eval      kpjbu = kpjbus
110800070320     c                   if        d09oerr = '1' or §grgDST = 'N'
110900070309     C                   SETON                                        55  99
111000070312     C                   goto      end_CT1
111100070309     c                   else
111200070309     c                   if        d09ocgi <> ' '
111300070312     c                   eval      d1dcgi = d09odes
111400070309     c                   move      'S'           con_giro
111500070312      *
111600070312      **  Se impostato un Foglio e non ci sono giri sul foglio
111700070312      **   deve segnalare l'errore sul giro scelto
111800070312     c                   if        D1Cnfv  > *zero
111900070312     c                   move      'N'           Giro_in_NFV       1
112000080715     c                   clear                   Prior_GiroCon     1 0
112100080715     c                   clear                   Prior_GiroRit     1 0
112200070910     c                   if        d1dati = 'C'
112300070910      **
112400070312     c     kdst31        setll     fidst31l
112500070312     c     kdst31        reade     fidst31l
112600070910     c                   dow       not %Eof(fidst31l)
112700070312      **
112800070312      **  Controlla presenza del giro richiesto
112900070312     c                   if        %char(DST3NPG) = d1cNPG and
113000070312     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
113100070312     c                             and DST3CGI = d1cCGI
113200070312     c                   move      'S'           Giro_in_NFV       1
113300080715     c                   z-add     dst3prgG      Prior_GiroCon
113400070312     c                   leave
113500070312     c                   end
113600070312      **
113700070312     c     kdst31        reade     fidst31l
113800070312     c                   endDo
113900070312      **
114000070910     c                   else
114100070910      **
114200070910     c     kdst51        setll     fidst51l
114300070910     c     kdst51        reade     fidst51l
114400070910     c                   dow       not %Eof(fidst51l)
114500070910      **
114600070910      **  Controlla presenza del giro richiesto
114700070910     c                   if        %char(DST5NPG) = d1cNPG and
114800070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
114900070910     c                             and DST5CGI = d1cCGI
115000070910     c                   move      'S'           Giro_in_NFV       1
115100080715     c                   z-add     dst5prgG      Prior_GiroRit
115200070910     c                   leave
115300070910     c                   end
115400070910      **
115500070910     c     kdst51        reade     fidst51l
115600070910     c                   endDo
115700070910      **
115800070910     c                   end
115900070910      **
116000070312      ** Il giro selezionato non è presente nella distinta
116100070312     c                   if        Giro_in_NFV = 'N'
116200070312     C                   SETON                                        52  99
116300070312     C                   goto      end_CT1
116400070312     c                   end
116500070312      **
116600070312     c                   end
116700070312      **
116800070309     c                   end
116900070309     c                   end
117000070309     c                   end
117100070309      *
117200070309     C*------------------------
117300070309     c* Per visualizzare gli importi rchiedere tipi fogli = 2
117400070312     c                   if        d1IMPO='S' and d1CTFV<>'2'
117500070312     C                   MOVEL     MSG(3)        d1Cmsg
117600070309     C                   SETON                                        56  99
117700070312     C                   GOTO      end_CT1
117800070309     c                   endif
117900160503     C*------------------------
118000160506    1C                   IF        *in08 and sav_in38 = *on
118100160503      *
118200160503     c                   if        d1Ctpm ='M'  and
118300160503     c                              (d1ctur1 <>' ' or d1ctur2 <>' ')
118400160504     C*********          MOVEL     MSG(4)        d1Cmsg
118500160504     C*********          SETON                                        53  99
118600160509     C*********          GOTO      end_CT1
118700160503      *
118800160503     c                   elseIF    d1ctur1=' ' and d1ctur2 = ' ' and d1Ctpm<>'M'
118900160503     C                   MOVEL     MSG(5)        d1Cmsg
119000160503     C                   SETON                                        60  99
119100160503     C                   GOTO      end_CT1
119200160503      *
119300160503     c                   end
119400160503      *
119500160503     c                   end
119600160503      *
119700070309     C*------------------------
119800070312     C     end_CT1       TAG
119900070309     c   99              move      'S'           $VISD1
120000080715      *
120100080715      * Solo se ci sono messaggi di errore attiva il 28 per
120200080715      *  visualizzarli.
120300080715     c******             move      *in99         *in28
120400080715     c                   if        d1Cmsg <> *blank
120500080715     c                   move      *in99         *in28
120600080715     c                   end
120700160505     c**
120800160505     c                   clear                   sav_c1aut
120900160505     c                   clear                   sav_c1nfv
121000160505     c                   clear                   sav_c2aut
121100160505     c                   clear                   sav_c2nfv
121200070309     C*
121300160505     c                   clear                   c1daut
121400160505     c                   clear                   c1cnfv
121500160505     c                   clear                   c2daut
121600160505     c                   clear                   c2cnfv
121700160505     C*
121800070309     C                   ENDSR
121900070301     C/EJECT
122000070312      *---------------------------------------------------------------*
122100070321      *?  GESTIONE SFL 1                       ?
122200070312      *---------------------------------------------------------------*
122300070321     C     GESS1         BEGSR
122400070312     C*
122500070312     C* inizializzazione videata
122600070321     C     $INZS1        IFEQ      *ON
122700070321     C                   EXSR      INZS1
122800070419     C*
122900070419     C* Se richiamato x il test del FIMS03R esce forzatamente
123000070419     c                   if        pgm_da_call = 'S' and sav_in19 = *on
123100070419     c                             and  FDGtfv = '3'
123200070419     c                   LeaveSr
123300070419     c                   end
123400070419     C*
123500070321     C                   MOVE      *OFF          $INZS1
123600070312     C                   ENDIF
123700070312     C*
123800070312     C* Inizializza e imposta piede videata
123900070321     C                   EXSR      INZZ1
124000070312     C* emissione piede videata
124100070312      * ?            *-----------------------*
124200070321     C                   WRITE     DG31Z02
124300070312      * ?            *-----------------------*
124400070312     C*
124500070312     C     WMAX          IFEQ      0
124600070312     C* Non ci sono records
124700070312      * ?            *-----------------------*
124800070312     C                   WRITE     DG31V02
124900070312      * ?            *-----------------------*
125000070312     c                   setoff                                       30
125100070312     C                   Else
125200070312     c                   seton                                        30
125300070312      *
125400070312     C     Wsfl          IFgt      0
125500070321     C                   Z-ADD     wsfl          C1RCD
125600070312     C                   Else
125700070312     C     Wpag          IFgt      0
125800070321     C                   Z-ADD     wpag          C1RCD
125900070312     C                   EndIF
126000070312     C                   EndIF
126100070312     C                   ENDIF
126200071026      * -----------------------
126300071026      * ?  reimposta su quale riga deve posizionarsi il cursore
126400071026      * ?    se non si è chiesto l'ordinamento
126500071026     C                   if        $TASTO   <>  F11
126600071026     C                   z-add     Su_Quale_rec  h1riga
126700071026     C                   EndIF
126800071026      * -----------------------
126900080703      * solo se richiamato dal MONITOR e solo la prima volta
127000080703     c                   if        fdgpgm = 'MONITOR' and Apri_DROP ='S'
127100080703     c                   clear                   Apri_DROP
127200080703     c                   eval      *in16 = *ON
127300080703     c                   end
127400071026      *
127500070312      *  Emissione SFL dei GIRI
127600070312     c                   setoff                                       99
127700070419     c                   if        $VISS1 = 'S'
127800070312      * ?            *-----------------------*
127900070321     C                   EXFMT     DG31CT1
128000070312      * ?            *-----------------------*
128100070321     C                   movel     'S1'          Sav_$GEST
128200070419     c                   end
128300070312      *
128400070321     C     C1NRR         IFNE      0
128500070321     C                   Z-ADD     C1NRR         WSFL
128600070320     C                   Else
128700070320     C     SFLNRR        IFgt      0
128800070320     C                   Z-ADD     SFLNRR        WSFL
128900070320     c                   EndIF
129000070320     C                   ENDIF
129100070321     C                   Z-ADD     SFLNRR        C1RCD
129200070312     C* Selezioni
1293000703121    C                   SELECT
129400070313     C* F11=Cambia Ordinamento del SFL
129500070313     C                   WHEN      $TASTO   =   F11
129600160505     c                   if        sav_c1aut <> *blank or
129700160505     c                             sav_c1nfv  > 0
129800160505     c                   move      *on           $inzs1
129900160505     C                   clear                   C1DAUT
130000160505     C                   clear                   C1CNFV
130100160505     C                   clear                   sav_c1nfv
130200160505     C                   clear                   sav_c1aut
130300160505     c                   else
130400070321     C                   clear                   C1DAUT
130500070321     C                   clear                   C1CNFV
130600070321     C                   clear                   sav_c1nfv
130700070321     C                   clear                   sav_c1aut
130800070403     C                   EXSR      F11_ordina1
130900160505     c                   end
131000070312     C* F3=Fine
131100070312     C     $TASTO        WHENEQ    F03
131200080606     c                   move      '03'          fdgtfu
131300070312     C                   EXSR      F03_exit
131400070321     c                   goto      end_s01
131500070312     C* F12=Ritorno
131600070312     C     $TASTO        WHENEQ    F12
131700070312     c                   move      'D1'          $GEST
131800070330     C                   clear                   C1DAUT
131900070330     C                   clear                   C1CNFV
132000070330     C                   clear                   sav_c1nfv
132100070330     C                   clear                   sav_c1aut
132200070326     c                   clear                   d1ccgi
132300070312     C*    deve andare a fine se richiamato
132400070312     c                   if        pgm_da_call = 'S'
132500070326     c                   clear                   fdgcgi
132600070326     c                   clear                   fdgnfv
132700080606     c                   move      '12'          fdgtfu
132800070312     C                   EXSR      F03_exit
132900070312     c                   end
133000070321     c                   goto      end_s01
133100070312      *
1332000703121O   C                   OTHER
133300070312     C* CONTROLLO testata
133400070321     C                   EXSR      CTRC1
133500070312     C* CONTROLLO subfile
133600070312     C     *IN99         IFEQ      *OFF
133700070321     C                   EXSR      CTRS1
133800070320
133900070312     C                   END
134000070312      *
134100070312      * ?Emette la videata di Dettaglio Distinta
134200070312     C     *IN99         IFEQ      *OFF
134300070321     C********           MOVE      *ON           $INZD2
134400070312     C********           movel     'D2'          $GEST
134500070312     C                   END
134600070312      *
1347000703121-   C                   ENDSL
134800070312     C*
134900070321     c     end_s01       tag
135000070312     C                   ENDSR
135100070312     C/EJECT
135200070312      *---------------------------------------------------------------*
135300070321      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL ?
135400070312      *---------------------------------------------------------------*
135500070321     C     INZZ1         BEGSR
135600070312     C*
135700070312     C*
135800070312     C                   ENDSR
135900070312     C/EJECT
136000070312      *---------------------------------------------------------------*
136100070321      *?  INIZIALIZZAZIONE LISTA dei GIRI da selezionare ?
136200070312      *---------------------------------------------------------------*
136300070321     C     INZS1         BEGSR
136400070314     C*
136500070312     C* reset indicatori DSPATR
136600070312     c                   exsr      INZ_indERR
136700070312     C* pulizia SFL
136800070312     C                   SETOFF                                         3031
136900070321     C                   WRITE     DG31CT1
137000070314     C                   SETON                                          31
137100070320      *
137200070314     C* Posiziona il cursore in testata x 12 = *on
137300070320     c                   if        cur_su_SFL = 'N'
137400070321     c     $PcurS1       comp      *on                                    12
137500070320     C* imposto pos. del cursore
137600070320     C                   eval           H1RIGA = 3
137700070528     C                   eval           H1COLO = 17
137800070315     c                   else
137900070320     c                   move      'N'           cur_su_SFL        1
138000070315     c                   setoff                                           12
138100070320     C* imposto pos. del cursore
138200070320     C                   eval           H1RIGA = 7
138300070320     C                   eval           H1COLO = 2
138400070315     c                   end
138500071026     C* se siamo al caricamento del SFL si deve posizionare sulla prima riga disponibile
138600071026     c                   z-add     1             Che_NRec_sfl
138700071026     C                   z-add     h1riga        Su_Quale_rec
138800071026     C                   move      'S'           prima_volta
138900080219     c                   eval      *in15 = sav_in15
139000160506     c                   eval      *in38 = sav_in38
139100070320     C*
139200070314     C* Se richiamato attivo 04
139300070528     C*   se però è richiamato da Assegnazione attivo 05
139400070620     c                   setoff                                       0503
139500080625     c                   move      *off          sav_in05
139600080625     C*
139700070314     c                   if        pgm_da_call = 'S'
139800070314     C                   SETON                                          04
139900070404     c                   move      *on           sav_in04
140000080715     c                   if        fdgpgm = 'ASSMANUALE' or fdgpgm = 'MONITOR'
140100070620     C                   SETON                                          03
140200070620     c                   move      *on           sav_in03
140300070620     c                   end
140400070528     c                   if        fdgOPZ = '1'
140500070528     C                   SETON                                          05
140600070528     c                   move      *on           sav_in05
140700070314     c                   end
140800070528     c                   end
140900070312     C*
141000070312     C* Righe x Pagina del 1° SFL
141100071026     c                   z-add     16            s1pag
141200070312      *
141300070312     C* CARICAMENTO SFL totale
141400070321     C                   Z-ADD     0             S1NRR
141500070321     C                   Z-ADD     1             C1RCD
141600070312     C                   Z-ADD     0             WMAX
141700070312     C                   Z-ADD     0             Wsfl
141800070312     C* campi titolo
141900070321     C                   MOVEL     TES(1)        c1CTES
142000080715     c                   if        fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR'
142100070606     C                   MOVEL(p)  TES(2)        c1CTES
142200070606     c                   end
142300070321     C                   MOVEL     dsPGM         c1CPGM
142400070321     C                   MOVEL     RAGUT         c1CDSA
142500070321     C                   MOVEL     knsif         c1nsif
142600070321     C                   MOVEL     knmus         c1nmus
142700070321     C                   z-add     d1CFGS        c1cfgs
142800070321     C                   movel     d1DFGS        c1dfgs
142900070321     C                   movel     d1ccgi        c1ccgi
143000070830     c   08              movel     'Prc'         c1PRC
143100160503     c   38              movel     'T'           c1TUR1
143200160503     c   38              movel     'r'           c1TUR2
143300070312     C*
143400070312     C* Posizionamento su file pilota e impostazione primo record da leggere
143500070312     c                   clear                   Prima_Lettura     1
143600070312      *
143700070406     c                   if        Solo_aperte = 'N'
143800070312     c     Kdat1         setll     fidst008
143900070406     c                   else
144000070312     C     Kdst3         SETLL     Fidst003
144100070406     c                   End
144200070312     C* Carico il SFL
144300070321     C                   EXSR      ROLS1
144400070419     C*
144500070419     C* Se richiamato x il test del FIMS03R esce forzatamente
144600070419     c                   if        pgm_da_call = 'S' and sav_in19 = *on
144700070419     c                             and  FDGtfv = '3'
144800070419     c                   LeaveSr
144900070419     c                   end
145000070312     C*
145100070312     C                   Z-ADD     1             WPAG
145200070312     C*
145300070313     ** Al primo F11 ordino per Autista
145400070321     C                   EVAL      RrnLast = S1Nrr
145500160504      **
145600160505     c                   if          sav_in10 = *off
145700160504     C*
145800160601     c                   if        sav_in11 = *off
145900070417     C                   EVAL      WrkSort = Numero
146000070417     c                   exsr      F11_ordina1
146100160601     c                   setoff                                       11
146200160601     c                   elseif    sav_in11 = *on
146300070313     C                   EVAL      WrkSort = Autista
146400070402     c                   exsr      F11_ordina1
146500070313     c                   end
146600160504     C*
146700160505     c                   elseif      sav_in10 <> *off
146800160504     C                   EVAL      WrkSort = Turno
146900160504     c                   exsr      F11_ordina1
147000160504     c                   end
147100070320     C*
147200070312     C                   ENDSR
147300070312     C/EJECT
147400070301      *---------------------------------------------------------------*
147500070321      *?  CARICAMENTO PAGINA                  ?
147600070301      *---------------------------------------------------------------*
147700070321     C     ROLS1         BEGSR
147800070301     C*
147900070301     C                   SETOFF                                       32
148000070301     C                   Z-ADD     0             Y
148100070321     C                   Z-ADD     WMAX          S1NRR
148200070301     C*
148300070312     C* Caricamento del Sfl x Giro
148400070321     c                   Exsr      Write_S1
148500070301     C* Fine Caricamento
148600070321     C                   Z-ADD     S1NRR         WMAX                 30
148700070419     C*
148800070419     C* Se il programma è richiamato e non ha trovato dei records
148900070419     c                   if        Wmax = 0 and pgm_da_call = 'S'
149000070419     c                   move      '9'           FDGerr
149100070419      * esce forzatamente
149200080821     c                   move      *on           $ESCI
149300070419     C                   MOVE      *ON           $FINE
149400070419     c                   end
149500070419     C*
149600070302     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
149700070321     C     S1NRR         DIV       S1pag         PAGINE            4 0
149800070301     C                   MVR                     RESTO             3 0
149900070321     C     PAGINE        MULT      S1pag         C1RCD
1500000703011    C     RESTO         IFGT      0
150100070321     C                   ADD       1             C1RCD
1502000703011E   C                   ELSE
150300070321     C                   SUB       S1pag         C1RCD
150400070321     C                   ADD       1             C1RCD
1505000703011-   C                   ENDIF
150600070301     C*
150700070301     C                   ENDSR
150800070312      *---------------------------------------------------------------*
150900070321      *?  Scrive il SFL   ?
151000070312      *---------------------------------------------------------------*
151100070321     C     Write_S1      BEGSR
151200070312     C*
151300070312     c                   exsr      Legge_rec
151400070312     C*
1515000703121    C     $EFILE        DowEQ     *OFF
151600070312     C*
151700070321     C                   clear                   DG31SF1
151800070404     c                   move      sav_in04      *in04
151900070528     c                   move      sav_in05      *in05
152000080219     c                   move      sav_in15      *in15
152100160506     c                   move      sav_in38      *in38
152200070510     C* Priorità
152300080715     C*    Consegne/Ritiri/Entrambe
152400080715     c                   if        d1dati = 'C'
152500080715     C*****              z-add     dst3prgg      h1prgg
152600080715     c                   z-add     prior_giroCon h1prgg
152700080715      *
152800080715     c                   elseIf    d1dati = 'R'
152900080715     c                   z-add     prior_giroRit h1prgg
153000080715      *
153100080715     c                   end
153200080715      *
153300070510     C                   z-add     dst3sql       h1sql
153400070313     C* GIRO DATA Distinta
153500070417     C                   MOVE      dstDFV        h1CDFV
153600070312     C                   MOVE      dstDFV        G02INV
153700070312     C                   MOVEL     '3'           G02ERR
153800070312     C                   CALL      'XSRDA8'
153900070312     C                   PARM                    WLBDAT
154000070312     C*
154100070313     C                   movel     G02DAT        campo4            4
154200070321     C                   move      G02DAT        s1CDFV
154300070321     C                   movel     campo4        s1CDFV
154400070312      *
154500070321     C                   Z-ADD     dstNFV        s1CNFV
154600070312     c     kapd          chain     fiapd01l
154700070312     c                   if        %found(fiapd01l)
154800070321     c                   eval      s1DAUT = apdrsc
154900070312     c                   else
155000070321     c                   clear                   s1DAUT
155100070312     c                   end
155200070312      *
155300070321     c                   z-add     DSTPKL        s1cpem
155400070321     c                   z-add     DSTVLU        s1cvom
155500070326      *
155600070326     c                   if        s1cpem > 0 or s1cvom > 0
155700070326     c                   movel     'Max'         s1max
155800070326     c                   end
155900070313      *
156000070321     c                   z-add     DSTPKLa       s1cpea
156100070321     c                   z-add     DSTVLUa       s1cvoa
156200070905     c                   if        DSTfpp  ='M'
156300070905     c                   movel     'Mat'         s1Tusc
156400070905     c                   else
156500070905     c                   move      'Pom'         s1Tusc
156600070905     c                   end
156700070820     c                   clear                   s1STP
156800080623     c                   if        DSTfasd ='STP' or
156900080623     c                             Fase_STP = 'S'
157000080625     c                   movel     'St'          s1STP
157100070820     c                   endif
157200070313      *
157300080625      * Controlla se ci sono delle Fasi presenti da segnalare
157400080625     c                   clear                   fidg39dS
157500080625     c                   move      d1CNPG        fdg39NPG
157600080625     c                   z-add     s1CNFV        fdg39NFV
157700080625     c                   z-add     c1cfgs        fdg39FGS
157800080625     c                   move      'N'           fdg39VIDno
157900080625     c                   eval      kpjbus = kpjbu
158000080625     c                   movel(p)  fidg39ds      KPJBU
158100080625     c                   call      'FIDG39R'
158200080625     c                   parm                    kpjba
158300080625     c                   eval      fidg39ds = KPJBU
158400080625     c                   eval      kpjbu = kpjbus
158500080625      * se ci sono delle Fasi da vedere imposta l'asterisco nel primo Byte
158600080625      *  del flag di stampato.
158700080625     c                   if        fdg39FASI = 'S'
158800080625     c                   move      '*'           s1STP
158900080625     c                   endif
159000080613      *
159100070312     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
159200070625     c                   if        *in08
159300070312     c                   if        dsttpm='M'
159400070321     c                   movel     'Mes'         s1CTPM
159500070312     c                   endif
159600070312     c                   if        dsttpm<>'M'
159700070321     c                   movel     'Prc'         s1CTPM
159800070312     c                   endif
159900070312     c                   endif
160000160503      **  nel filler
160100160503     C                   movel     dstflr        DDStflr
160200160503     c* x il TURNO
160300160503     c                   if        *in38
160400160503     c                   movel     §DSTTURNO     s1CTUR
160500160504     c                   if        dsttpm <> 'M' and
160600160504     c                               s1CTUR = *blank
160700160504     c                   eval        s1CTUR = '1'
160800160503     c                   endif
160900160504     c                   endif
161000160503     c
161100070312      * Flag tipo distinta (solo consegna, orm o entrambi)
161200070312     C                   CLEAR                   WBOL
161300070312     C                   CLEAR                   WORM
161400070312     c                   Z-Add     d1CFGS        Wifp
161500070321     C                   Z-ADD     s1CNFV        kfnvv
161600070321     C                   Z-ADD     s1CNFV        WnFV
161700070312     c     Karb78        Setll     Fnarb78l
161800070312if  5c                   if        %equal(fnarb78l)
161900070312     c                   movel     'S'           wbol              1
162000070312x   5c                   else
162100070312     C     Kdst          SETLL     FnBRV01L
162200070312     c                   if        %equal(fnbrv01l)
162300070312     c                   movel     'S'           wbol              1
162400070312     c                   endif
162500070312e   5c                   endif
162600070312     C     KORM          SETLL     FNORM06L
162700070312do  5c                   do        *hival
162800070312     C     KORM          reade     FNORM06L
162900070312     c                   if        %eof(fnorm06l)
163000070312     c                   leave
163100070312     c                   endif
163200070312     c                   if        ormatb = *blanks and ormfao < 999
163300070312     C                   move      'S'           worm              1
163400070312     c                   endif
163500070312e   5c                   enddo
163600070312     C                   if        wbol = 'S' and worm <> 'S'
163700070321     C                   movel     'CON'         s1CTPD
163800070312     C                   endif
163900070312     C                   if        wbol <> 'S' and worm = 'S'
164000070321     C                   movel     'ORM'         s1CTPD
164100070312     C                   endif
164200070312     C                   if        wbol = 'S' and worm = 'S'
164300070321     C                   movel     'C/O'         s1CTPD
164400070312     C                   endif
164500070312      * Calcolo importi se non già fatto prima
164600070312if  5c                   if        d1CTFV = '2' and d1IMPO = 'S'
164700070312     c                   exsr      totalizzo
164800070321     c                   z-add     w_imas        s1IMAS
164900070321     c                   z-add     w_imca        s1IMCA
165000070312     c                   endif
165100070321      * Pda
165200070321     C                   movel     dstPDA        s1CPDA
165300080208     c                   setoff                                       14
165400080208     c                   if        dstDFV >= DateU8
165500080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
165600080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
165700080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
165800080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
165900080208     c                   seton                                        14
166000080208     c                   endIf
166100080208     c                   endIf
166200080222     c                   eval      h1in14 = *in14
166300080222      *
166400080208      *  lo Minuscolizza sempre a video
166500080207     C     Up:Lo         XLATE     §DSTtstPDA    s1CTSTPDA
166600070321      * ANNULLATO
166700070312     C     dstATB        IFEQ      'A'
166800070321     C                   MOVEL     'ANN'         s1CANN
166900070312     C                   END
167000070312if  5C     dstFCF        IFNE      ' '
167100070312      * CHIUSO
167200070312     C     dstFCF        IFEQ      'S'
167300070403     C                   MOVEL     'C   '        s1CFCF
167400070312     C                   ELSE
167500070312     C* BLOCCATO
167600070403     C                   MOVEL     'BL  '        s1CFCF
167700070312e   5C                   ENDIF
167800070312e   4C                   ENDIF
167900070315      *
168000070315      * Intestazioni seconda riga
168100070830     c                   setoFF                                       18
168200070321     c                   if        s1imca <> 0
168300070321     c                   movel     Imp_C_A       s1dica
168400070830     c                   setoN                                        18
168500070315     c                   end
168600070315      *
168700070830     c                   setoFF                                       20
168800070321     c                   if        s1imas <> 0
168900070321     c                   movel     Imp_ASS       s1dias
169000070830     c                   setoN                                        20
169100070315     c                   end
169200070510      *
169300070510     c                   setoff                                       40
169400070510if  4c                   if        h1prgg = 1
169500070510     c                   seton                                        40
169600070510     c                   end
169700070312      *---------
169800070313if  4c                   if        FDGtfv <> '3'
169900160601     c                   if        (sav_c1nfv <= s1cnfv and *in11 = *off) or
170000160601     c                             (sav_c1aut <= s1daut and *in11 = *on ) or
170100070321     c                             (sav_c1aut = *blank and sav_c1nfv =0)
170200070404     C* Imposta indicatore x abilitare campo scelta
170300070404     c                   move      sav_in19      *in19
170400070312     c********           seton                                        32
170500070321     C                   ADD       1             S1NRR
170600070312     C                   ADD       1             Y
170700070321     C                   WRITE     DG31SF1
170800070313     c                   end
170900070312e   4c                   endif
171000070312      *---------
171100070312      *  Esce da qui con un record di Totale x GIRO
171200070312     c                   exsr      Legge_rec
171300070312     c                   EndDO
171400070321      *
171500070312     C                   ENDSR
171600070321     C/EJECT
171700070321      *---------------------------------------------------------------*
171800070321      *?  CONTROLLO TESTATA LISTA   ?
171900070321      *---------------------------------------------------------------*
172000070321     C     CTRC1         BEGSR
172100070321      *
172200070321     C                   MOVE      *OFF          *IN99
172300070321      *
172400070321     C*------------------------
172500070321      *  interrogazione Giro
172600070321     c                   clear                   c1dcgi
172700070321     c     '?'           scan      c1cCGI
172800070321     C*   ricerca codice giro
172900070321     c                   if        %Found
173000070321     c                   clear                   fidg09ds
173100070321     c                   move      'P01'         d09iop0
173200070321     c                   movel     c1cFGS        d09ifgs
173300071010     c*****              movel     'C'           d09itug
173400071010     c                   movel     d1DATI        d09itug
173500070321     c                   z-add     DATEU8        d09idat
173600070321     c                   eval      kpjbus = kpjbu
173700070321     c                   movel(p)  fidg09ds      kpjbu
173800070321     c                   call      'FIDG09R'
173900070321     c                   parm                    kpjba
174000070321     c                   movel     kpjbu         fidg09ds
174100070321     c                   eval      kpjbu = kpjbus
174200070321     c                   movel     d09ocgi       c1cCGI
174300070321     c                   end
174400070321     C*------------------------
174500070321      *  controllo validità giro
174600070321     c                   if        c1cCGI <> *blank
174700070321     c                   clear                   fidg09ds
174800070321     c                   eval      D09IOP0 = '001'
174900070321     c                   eval      d09ifgs = c1cFGS
175000071010     c****               eval      d09itug = 'C'
175100071010     c                   eval      d09itug = d1DATI
175200070321     c                   eval      d09icgi = c1cCGI
175300070321     c                   move      dateu8        d09idat
175400070321     c                   eval      kpjbus = kpjbu
175500070321     c                   eval      kpjbu = fidg09ds
175600070321     c                   call      'FIDG09R'
175700070321     c                   parm                    kpjba
175800070321     c                   eval      fidg09ds = kpjbu
175900070321     c                   eval      kpjbu = kpjbus
176000070321     c                   if        d09oerr = '1'
176100070321     C                   SETON                                        54  99
176200070322     C                   goto      ENDC1
176300070321     c                   else
176400070321     c                   if        d09ocgi <> ' '
176500070321     c                   eval      c1dcgi = d09odes
176600070321     c                   move      'S'           con_giro
176700070321     c                   end
176800070321     c                   end
176900070321      *
177000070321     c                   end
177100070321     C*------------------------
177200070321      *  interrogazione AUT
177300070321      *
177400070321     c     '?'           scan      c1daut
177500070321     C*   ricerca codice giro
177600070321     c                   if        %Found
177700070321     c                   clear                   fnlv24ds
177800070321     C                   Z-ADD     c1cFGS        d24FIL
177900070321     C                   MOVEL     'R'           d24FLG
178000070321     C                   MOVEL     'A'           d24tip
178100070321     c                   eval      kpjbus = kpjbu
178200070321     C                   MOVEL(p)  fnlv24ds      KPJBU
178300070321     C                   CALL      'FNLV24R'
178400070321     C                   PARM                    KPJBA
178500070321     C                   MOVEL     KPJBU         fnlv24ds
178600070321     c                   eval      kpjbu = kpjbus
178700070321     C                   MOVEL     D24RSC        c1daut
178800070321     c                   end
178900070321     C*
179000070321     C*------------------------
179100070321     c                   move      *on           $PcurS1
179200070321     C* Riposizionamento x AUT o x Numero
179300160601     c                   if        (*in11 = *off and c1cnfv <> sav_c1nfv) or
179400160601     c                             (*in11 = *on  and c1daut <> sav_c1aut)
179500070321     c                   move      *off          $PcurS1
179600070321     c                   move      *on           $inzs1
179700070321     c                   move      'S1'          $GEST
179800160601     c   11              movel     c1daut        sav_c1aut
179900160601     c  n11              z-add     c1cnfv        sav_c1nfv
180000070321     c                   end
180100070321     C*
180200070321     C* Posiziona il cursore in testata x 12 = *on
180300070321     c     $PcurS1       comp      *on                                    12
180400070321     C  n12              eval           H1RIGA = 3
180500070528     C  n12              eval           H1COLO = 17
180600070321     C   12              eval           H1RIGA = 7
180700070321     C   12              eval           H1COLO = 2
180800070321      *
180900070321     C*------------------------
181000070321      *  se tutto OK
181100070321     c                   if        c1ccgi <> d1ccgi
181200070321     c                   move      c1ccgi        d1ccgi
181300070321     c                   move      *on           $inzs1
181400070321     c                   move      'S1'          $GEST
181500070321     c                   end
181600070321      *
181700070321     C*------------------------
181800070321      *
181900070321     c     endc1         tag
182000070321     C                   ENDSR
182100070321      *---------------------------------------------------------------*
182200070321      *?  CONTROLLO OPZIONI LISTA   ?
182300070321      *---------------------------------------------------------------*
182400070321     C     CTRS1         BEGSR
182500070321     C*
182600070321     C                   MOVEL     *OFF          $ESCI
182700070321     C                   SETOFF                                       99
182800070321     C*
182900070321     C* Leggo il sfl solo se ci sono rcd
1830000703211    C     WMAX          IFGT      0
183100070321     C                   READC     DG31SF1                                21
183200070321     C*
183300070321     C* esce se fine sfl o errore che richiede l'uscita
1834000703212    C     *IN21         DOWEQ     *OFF
183500070321     C     $ESCI         ANDEQ     *OFF
183600070321     C                   Z-ADD     S1NRR         C1RCD
183700071026     c                   eval      Che_NRec_sfl = s1nrr
183800070321     C* ctrl su riga
183900070321     C                   EXSR      RECS1
184000070321     C*
184100070321     C* gestione opzioni
1842000704043    C     s1SCE         IFNE      *blank
184300070321     C     *IN99         ANDEQ     *OFF
184400070321     C                   EXSR      SCE_S1
1845000703213-   C                   ENDIF
184600070321      *
184700070321     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1848000703213    C     *IN99         IFEQ      *ON
184900070321     C     $ESCI         OREQ      *ON
185000070321     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
185100070321     C* la ripristinerò a conclusione del ciclo di READC
185200070321     C                   MOVE      *OFF          $INZS1
1853000703213-   C                   ENDIF
185400070321     C*
185500070321      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
185600070404     c                   if        s1SCE  <> *blank
185700070321     C******             MOVE      *ON           *IN32
185800070404     c                   clear                   s1SCE
185900070321     c                   end
186000070510      *
186100070510     c                   setoff                                       40
186200070510if  4c                   if        h1prgg = 1
186300070510     c                   seton                                        40
186400070510     c                   end
186500080222     c                   eval      *in14 = h1in14
186600070510      *
186700070321     C                   UPDATE    DG31SF1
186800070321     C*
186900070321     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1870000703213    C     $ESCI         IFEQ      *OFF
187100070321     C                   READC     DG31SF1                                21
187200070321      *
187300070321     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1874000703214    C     *IN21         IFEQ      *ON
187500070321     C* calcolo pagina a cui deve posizionarsi
187600070321     c                   z-add     s1pag         Wpagine
187700070321     C                   EXSR      CLCPAG
1878000703214-   C                   ENDIF
1879000703213-   C                   ENDIF
188000070321     C*
1881000703212-   C                   ENDDO
188200071026     C*
188300071026      * -----------------------
188400071026      * ? calcola "su quale record" riposizionarsi tenendo conto che
188500071026      * ?  il SFL ha una testata di 6 righe ed inizia dalla 7°.
188600071026     c     Che_NRec_sfl  div       s1pag         Su_Quale_rec
188700071026     c                   mvr                     resto
188800071026     C     Resto         IFeq      0
188900071026     C                   z-add     16            Su_Quale_rec
189000071026     C                   Else
189100071026     C                   z-add     resto         Su_Quale_rec
189200071026     C                   ENDIF
189300071026      * ?  aggiunge le 6 righe di testata.
189400071026     C                   add       6             Su_Quale_rec
189500071026      * -----------------------
189600070321     C*
1897000703211-   C                   ENDIF
189800070321     C*
189900070321     C                   ENDSR
190000070321     C/EJECT
190100070321      *---------------------------------------------------------------*
190200070321      *?  CONTROLLO CAMPI I/O RIGA LISTA  ?
190300070321      *---------------------------------------------------------------*
190400070321     C     RECS1         BEGSR
190500070321     C*
190600070321     C* reset indicatori DSPATR
190700070321     c                   exsr      INZ_indERR
190800070528     C*
190900070528     C* se non Richiamato non sono abilitate le opzioni 2 e 3
191000070528     c                   if        pgm_da_call <> 'S'
191100070528     c                   if        s1sce = '2' or s1sce = '3'
191200070528     c                   seton                                        99  41
191300070528     c                   end
191400070528     c                   end
191500070528     C* Se richiamato in assegnazione non deve accettare opz 2,3
191600070620     c                   if        (pgm_da_call =  'S' and fdg31_opz = '1' and
191700080715     c                             fdgPGM = 'ASSMANUALE')
191800080715     c                             or
191900080715     c                             (fdgPGM<>'ASSMANUALE' and fdgPGM <>'MONITOR')
192000070606     c                   if        s1sce = '2' or s1sce = '3'
192100070528     c                   seton                                        99  41
192200070528     c                   end
192300070528     c                   end
192400070321     C*
192500070620     c                   if        pgm_da_call =  'S' and fdg31_opz <> '1' and
192600080715     c                             (fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR')
192700070620     c                   if        s1sce = '1'
192800070620     c                   seton                                        99  41
192900070620     c                   end
193000070620     c                   end
193100080624      * se la filiale non è partita con il PDA non si può chiedere di
193200080624      * vedere le Fasi della distinta
193300080725     c*******            if        s1sce = 'F' and not *in15 or
193400080725     c*******                      s1sce = 'F' and S1CPDA = 'N' and
193500080725     c*******                      %subst(s1stp:3:1)= ' '
193600080725     c*******            seton                                        99  41
193700080725     c*******            end
193800070620     C*
193900070321     C                   ENDSR
194000070321      *---------------------------------------------------------------*
194100070321      *?  GESTIONE OPZIONI LISTA       ?
194200070321      *---------------------------------------------------------------*
194300070321     C     SCE_S1        BEGSR
194400070321     C*
194500070321     C* determino Riga/Colonna del cursore
194600070321     C                   MOVE      NRG           R$$
194700070321     C                   MOVE      NCL           C$$
194800070321     C                   Z-ADD     RIRI          CURR
194900070321     C                   Z-ADD     COCO          CURC
195000070321     C*
195100070404     c                   If        s1SCE  = '1'
195200070405     c                   if        pgm_da_call <> *Blank
195300070404     c                   move      *on           $ESCI
195400070404     c                   move      *on           $FINE
195500070404     c                   z-add     s1CNFV        FDGNFV
195600070528     c                   move      d1cnpg        FDGNPG
195700070528     c                   eval      fdgOPZ = s1SCE
195800070405     c                   else
195900070405     c                   z-add     s1CNFV        d1cNFV
196000070405     c                   move      'D1'          $GEST
196100070405     c                   end
196200070405     c                   end
196300070321      *
196400070528     c                   If        s1SCE  = '2' or s1SCE  = '3'
196500070528     c                   if        pgm_da_call <> *Blank
196600070528     c                   move      *on           $ESCI
196700070528     c                   move      *on           $FINE
196800070528     c                   z-add     s1CNFV        FDGNFV
196900070528     c                   move      d1cnpg        FDGNPG
197000070528     c                   eval      fdgOPZ = s1SCE
197100070528     c                   clear                   fdgCGI
197200070528     c                   end
197300070528     c                   end
197400070528      *
197500070404     c                   If        s1SCE  = '5'
197600070404      *
197700080701      * Se la distinta ha delle Fasi da mostrare -> abilita il tasto
197800080701      *  funzionale per poter richiamare la window di visualizzazione
197900080701     c                   move      *oFf          d2fasi
198000080704     c                   if        %subst(s1stp:3:1)='*'
198100080701     c                   move      *oN           d2fasi
198200080701     c                   end
198300080701      *
198400070321     c                   z-add     s1CNFV        d1CNFV
198500070321     c                   z-add     c1cfgs        d1CFGS
198600070321     c                   z-add     s1CNFV        wnfv
198700070330     c                   move      'D2'          $GEST
198800070330     C                   move      'S'           $VISD2
198900070330     c                   move      *on           $INZD2
199000070321      *
199100070321     c                   end
199200070321     C*
199300070621     c                   If        s1SCE  = '7'
199400070427     c                   clear                   fidg35ds
199500070528     c                   eval      D35FGSi = wfgs
199600070528     c                   eval      D35DDCi = H1CDFV
199700070528     c                   eval      D35NDCi = S1CNFV
199800070528     c                   eval      D35NPGi = wnpg
199900070831     c                   eval      D35PGMi = fdgPGM
200000070903      * si vuole fare vedere tutto
200100070903     c*****              eval      D35TPi  = 'C'
200200070905     c                   If        pgm_da_call <> *blank and FDGOPZ <> '1' and
200300080715     c                             (fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR')
200400070528     c                   eval      D35opzi = 'S'
200500070528     c                   end
200600070427     c                   eval      kpjbus = kpjbu
200700070427     c                   movel(p)  fidg35ds      kpjbu
200800070427     c                   call      'FIDG35R'
200900070427     c                   parm                    kpjba
201000070427     c                   movel     kpjbu         fidg35ds
201100070427     c                   eval      kpjbu = kpjbus
201200070611     c                   if        pgm_da_call <> *Blank and fdgOPZ<>'1'
201300070528     c                   move      *on           $ESCI
201400070905     c                   if        d35FunO = '03'
201500070528     c                   move      *on           $FINE
201600070905     c                   end
201700070905     c                   if        d35opzO = '2' or d35opzO = '3'
201800070528     c                   z-add     s1CNFV        FDGNFV
201900070905     c                   end
202000070528     c                   move      d1cnpg        FDGNPG
202100070528     c                   eval      fdgOPZ = d35opzO
202200070528     c                   eval      fdgCGI = d35cgiO
202300070528     c                   end
202400070427     c                   end
202500070427     C*
202600070404     c                   If        s1SCE  = '9'
202700070321     c                   z-add     s1CNFV        wnfv
202800070321     c*******            move      *on           $ESCI
202900070321     c                   move      'D2'          $GEST
203000070330     C                   move      'N'           $VISD2
203100070321     c                   move      *on           $INZD2
203200070321     C                   exsr      GESD2
203300070321     c                   endIf
203400080624      *
203500080624      *  Visionare le Fasi della Distinta
203600080624     c                   If        s1SCE  = 'F'
203700080624     c                   write     FRCDTA
203800080624      *
203900080625     c                   clear                   fidg39dS
204000080624     c                   move      d1CNPG        fdg39NPG
204100080624     c                   z-add     s1CNFV        fdg39NFV
204200080624     c                   z-add     c1cfgs        fdg39FGS
204300080625     c                   move      'N'           fdg39noREC
204400080625     c                   clear                   fdg39VIDno
204500080624     c                   eval      kpjbus = kpjbu
204600080624     c                   movel(p)  fidg39ds      KPJBU
204700080624     c                   call      'FIDG39R'
204800080624     c                   parm                    kpjba
204900080624     c                   eval      kpjbu = kpjbus
205000080624      *
205100080624     c                   end
205200070321     C*
205300080820      *  Dettaglio Distinta
205400080820     c                   If        s1SCE  = 'D'
205500080820     c                   write     FRCDTA
205600080820      *
205700080820     c                   clear                   fidg311dS
205800080820     c                   z-add     c1cfgs        f31FGS
205900080821     c                   z-add     Wnpg          f31Npg
206000080820     c                   z-add     s1CNFV        f31NumDis
206100080820     c                   z-add     h1CDFV        f31DatDis
206200080820     c                   move      d1Dati        f31PriDati
206300081111     c**********         if        D1CPDA <> 'C' and D1CPDA <> 'O'
206400081111     c**********         move      'E'           f31DatiVis
206500081111     c**********         else
206600080820     c                   move      D1CPDA        f31DatiVis
206700081111     c**********         end
206800080821      * x gestione test/parallelo
206900080821     c                   if        Test_parallelo = 'S' and test_Paral_ORM = 'S'
207000080826     c******             move      'E'           f31Test
207100080821     c                   elseIf    Test_parallelo = 'S' and test_Paral_ORM <>'S'
207200080826     c******             move      'C'           f31Test
207300080821     c                   elseIf    Test_parallelo <>'S' and test_Paral_ORM = 'S'
207400080826     c******             move      'R'           f31Test
207500080821     c                   end
207600080821
207700080820     c                   eval      kpjbus = kpjbu
207800080820     c                   movel(p)  fidg311ds     KPJBU
207900080820     c                   call      'FIDG311R'
208000080820     c                   parm                    kpjba
208100080821     c                   eval      fidg311ds =   KPJBU
208200080821     c                   if        F31Tfunz = '03'
208300080821     c                   move      *on           $ESCI
208400080821     C                   EXSR      F03_exit
208500080821     c                   end
208600080820     c                   eval      kpjbu = kpjbus
208700080820      *
208800080820     c                   end
208900080820     C*
209000070321     C                   ENDSR
209100070321     C/EJECT
209200070321      *---------------------------------------------------------------*
209300070321      *?  GESTIONE SFL 2                       ?
209400070321      *---------------------------------------------------------------*
209500070321     C     GESS2         BEGSR
209600070321     C*
209700070321     C* inizializzazione videata
209800070321     C     $INZS2        IFEQ      *ON
209900070321     C                   EXSR      INZS2
210000070321     C                   MOVE      *OFF          $INZS2
210100070321     C                   ENDIF
210200070321     C*
210300070321     C* Inizializza e imposta piede videata
210400070321     C                   EXSR      INZZ2
210500070321     C* emissione piede videata
210600070321      * ?            *-----------------------*
210700070321     C                   WRITE     DG31Z02
210800070321      * ?            *-----------------------*
210900070321     C*
211000070321     C     WMAX          IFEQ      0
211100070321     C* Non ci sono records
211200070321      * ?            *-----------------------*
211300070321     C                   WRITE     DG31V02
211400070321      * ?            *-----------------------*
211500070321     c                   setoff                                       30
211600070321     C                   Else
211700070321     c                   seton                                        30
211800070321      *
211900070321     C     Wsfl          IFgt      0
212000070321     C                   Z-ADD     wsfl          C2RCD
212100070321     C                   Else
212200070321     C     Wpag          IFgt      0
212300070321     C                   Z-ADD     wpag          C2RCD
212400070321     C                   EndIF
212500070321     C                   EndIF
212600070321     C                   ENDIF
212700071029      * se si deve riposizionare
212800071029     c                   if        nrec_riposiz > 0
212900071029     C                   Z-ADD     nrec_riposiz  C2RCD
213000071029     C                   EndIF
213100071026      * -----------------------
213200071026      * ?  reimposta su quale riga deve posizionarsi il cursore
213300071026      * ?    se non si è chiesto l'ordinamento
213400071026     C                   if        $TASTO   <>  F11
213500071026     C                   z-add     Su_Quale_rec  h1riga
213600071026     C                   EndIF
213700071026      * -----------------------
213800080703      * solo se richiamato dal MONITOR e solo la prima volta
213900080703     c                   if        fdgpgm = 'MONITOR' and Apri_DROP ='S'
214000080703     c                   clear                   Apri_DROP
214100080703     c                   eval      *in16 = *ON
214200080703     c                   end
214300070321      *
214400070321      *  Emissione SFL dei GIRI
214500070321     c                   setoff                                       99
214600070321      * ?            *-----------------------*
214700070321     C                   EXFMT     DG31CT2
214800070321      * ?            *-----------------------*
214900070321     C                   movel     'S2'          Sav_$GEST
215000071026      *
215100070321     C     C2NRR         IFNE      0
215200070321     C                   Z-ADD     C2NRR         WSFL
215300070321     C                   Else
215400070321     C     SFLNRR        IFgt      0
215500070321     C                   Z-ADD     SFLNRR        WSFL
215600070321     c                   EndIF
215700070321     C                   ENDIF
215800070321     C                   Z-ADD     SFLNRR        C2RCD
215900070321     C* Selezioni
2160000703211    C                   SELECT
216100070321     C* F11=Cambia Ordinamento del SFL
216200070321     C                   WHEN      $TASTO   =   F11
216300160505     c                   if        sav_c2aut <> *blank or
216400160505     c                             sav_c2nfv  > 0
216500160505     c                   move      *on           $inzs2
216600160505     C                   clear                   C2DAUT
216700160505     C                   clear                   C2CNFV
216800160505     C                   clear                   sav_c2nfv
216900160505     C                   clear                   sav_c2aut
217000160505     c                   else
217100070322     C                   EXSR      F11_ordina2
217200160505     c                   end
217300070321     C* F3=Fine
217400070321     C     $TASTO        WHENEQ    F03
217500080606     c                   move      '03'          fdgtfu
217600070321     C                   EXSR      F03_exit
217700070321     c                   goto      end_s02
217800070321     C* F12=Ritorno
217900070321     C     $TASTO        WHENEQ    F12
218000070321     c                   move      'D1'          $GEST
218100070326     c                   clear                   d1ccgi
218200070321     C*    deve andare a fine se richiamato
218300070321     c                   if        pgm_da_call = 'S'
218400070326     c                   clear                   fdgcgi
218500070326     c                   clear                   fdgnfv
218600071008     c                   clear                   fdgopz
218700071008     c                   move      '12'          fdgtfu
218800070321     C                   EXSR      F03_exit
218900070321     c                   end
219000070321     c                   goto      end_s02
219100070321      *
2192000703211O   C                   OTHER
219300070321     C* CONTROLLO testata
219400070321     C                   EXSR      CTRC2
219500070321     C* CONTROLLO subfile
219600070321     C     *IN99         IFEQ      *OFF
219700070321     C                   EXSR      CTRS2
219800070321
219900070321     C                   END
220000070321      *
220100070321      * ?Emette la videata di Dettaglio Distinta
220200070321     C     *IN99         IFEQ      *OFF
220300070321     C********           MOVE      *ON           $INZD2
220400070321     C********           movel     'D2'          $GEST
220500070321     C                   END
220600070321      *
2207000703211-   C                   ENDSL
220800070321     C*
220900070321     c     end_s02       tag
221000070321     C                   ENDSR
221100070321     C/EJECT
221200070321      *---------------------------------------------------------------*
221300070321      *?  INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL    ?
221400070321      *---------------------------------------------------------------*
221500070321     C     INZZ2         BEGSR
221600070321     C*
221700070321     C*
221800070321     C                   ENDSR
221900070321     C/EJECT
222000070321      *---------------------------------------------------------------*
222100070321      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare  ?
222200070321      *---------------------------------------------------------------*
222300070321     C     INZS2         BEGSR
222400070321     C*
222500160505     C* reset indicatori DSPATR   10 -> ord.x Turno inesistente
222600160505     c                   setoff                                         10
222700160505     c                   eval      sav_in10 = *in10
222800070321     c                   exsr      INZ_indERR
222900070321     C* pulizia SFL
223000070321     C                   SETOFF                                         3031
223100070321     C                   WRITE     DG31CT2
223200070321     C                   SETON                                          31
223300160601     C                   SETON                                          11
223400160601     c                   eval      sav_in11 = *in11
223500070321      *
223600070321     C* Posiziona il cursore in testata x 12 = *on
223700070321     c                   if        cur_su_SFL = 'N'
223800070321     c     $PcurS2       comp      *on                                    12
223900070321     C* imposto pos. del cursore
224000070321     C                   eval           H1RIGA = 3
224100070528     C                   eval           H1COLO = 17
224200070321     c                   else
224300070321     c                   move      'N'           cur_su_SFL        1
224400070321     c                   setoff                                           12
224500070321     C* imposto pos. del cursore
224600070321     C                   eval           H1RIGA = 7
224700070321     C                   eval           H1COLO = 2
224800070321     c                   end
224900071026     C* se siamo al caricamento del SFL si deve posizionare sulla prima riga disponibile
225000071026     c                   z-add     1             Che_NRec_sfl
225100071026     C                   z-add     h1riga        Su_Quale_rec
225200071026     C                   move      'S'           prima_volta
225300080219     c                   eval      *in15 = sav_in15
225400071026     C*
225500070321     C* Se richiamato attivo 04
225600070528     C*   se però è richiamato da Assegnazione attivo 05
225700070528     c                   setoff                                       05
225800080625     c                   move      *off          sav_in05
225900080625     C*
226000070321     c                   if        pgm_da_call = 'S'
226100070321     C                   SETON                                          04
226200070404     c                   move      *on           sav_in04
226300080715     c                   if        fdgpgm ='ASSMANUALE' or fdgPGM = 'MONITOR'
226400070620     C                   SETON                                          03
226500070620     c                   move      *on           sav_in03
226600070620     c                   end
226700070528     c                   if        fdgOPZ = '1'
226800070528     C                   SETON                                          05
226900070528     c                   move      *on           sav_in05
227000070528     c                   end
227100070321     c                   end
227200070321     C*
227300070321     C* Righe x Pagina del 1° SFL
227400071026     c                   z-add     15            s2pag
227500070321      *
227600070321     C* CARICAMENTO SFL totale
227700070321     C                   Z-ADD     0             S2NRR
227800070321     C                   Z-ADD     1             C2RCD
227900070321     C                   Z-ADD     0             WMAX
228000070321     C                   Z-ADD     0             Wsfl
228100070321     C* campi titolo
228200070321     C                   MOVEL     TES(1)        c2CTES
228300080715     c                   if        fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR'
228400070606     C                   MOVEL(p)  TES(2)        c2CTES
228500070606     c                   end
228600070903
228700070830     c                   SeLect
228800070903
228900070830     c                   whEn      d1dati= ' '
229000070830     C                   MOVEL(p)  TES(7)        c2TES2
229100070903     C                   MOVEL(p)  'Sped.'       c2DesN
229200070903
229300070830     c                   whEn      d1dati= 'C'
229400070830     C                   MOVEL(p)  TES(5)        c2TES2
229500070903     C                   MOVEL(p)  'Sped.'       c2DesN
229600070903
229700070830     c                   whEn      d1dati= 'R'
229800070830     C                   MOVEL(p)  TES(6)        c2TES2
229900070903     C                   MOVEL(p)  ' Orm.'       c2DesN
230000070903
230100070830     c                   endSL
230200070903
230300070321     C                   MOVEL     dsPGM         c2CPGM
230400070321     C                   MOVEL     RAGUT         c2CDSA
230500070321     C                   MOVEL     knsif         c2nsif
230600070321     C                   MOVEL     knmus         c2nmus
230700070321     C                   z-add     d1CFGS        c2cfgs
230800070321     C                   movel     d1DFGS        c2dfgs
230900070321     C                   movel     d1ccgi        c2ccgi
231000070321     C*
231100070321     C* Posizionamento su file pilota e impostazione primo record da leggere
231200070321     c                   clear                   Prima_Lettura     1
231300070321      *
231400070406     c                   if        Solo_aperte = 'N'
231500070321     c     Kdat1         setll     fidst008
231600070406     c                   else
231700070321     C     Kdst3         SETLL     Fidst003
231800070406     c                   End
231900070321     C* Carico il SFL
232000070321     C                   EXSR      ROLS2
232100070321     C*
232200070321     C                   Z-ADD     1             WPAG
232300070321     C*
232400070321     ** Al primo F11 ordino per Autista
232500070321     C                   EVAL      RrnLast = S2Nrr
232600160504     C*
232700160505     c                   if        sav_in10 = *off
232800160504     C*
232900160601     c                   if        sav_in11 = *off
233000070417     C                   EVAL      WrkSort = Numero
233100070417     c                   exsr      F11_ordina2
233200160601     c                   setoff                                       11
233300160601     c                   elseIF    sav_in11 = *on
233400070321     C                   EVAL      WrkSort = Autista
233500070322     c                   exsr      F11_ordina2
233600070321     c                   end
233700160504     C*
233800160505     c                   elseif    sav_in10 <>*off
233900160504     C                   EVAL      WrkSort = Turno
234000160504     c                   exsr      F11_ordina2
234100160504     c                   end
234200160504     C*
234300070321     C                   ENDSR
234400070321     C/EJECT
234500070321      *---------------------------------------------------------------*
234600070321      *?  CARICAMENTO PAGINA da SFL dei GIRI             ?
234700070321      *---------------------------------------------------------------*
234800070321     C     ROLS2         BEGSR
234900070321     C*
235000070321     C                   SETOFF                                       32
235100070321     C                   Z-ADD     0             Y
235200070321     C                   Z-ADD     WMAX          S2NRR
235300070321     C*
235400070321     C* Caricamento del Sfl x Giro
235500070321     c                   Exsr      Write_S2
235600070321     C* Fine Caricamento
235700070321     C*
235800070321     C                   Z-ADD     S2NRR         WMAX                 30
235900070321     C*
236000070321     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
236100070321     C     S2NRR         DIV       S2pag         PAGINE            4 0
236200070321     C                   MVR                     RESTO             3 0
236300070321     C     PAGINE        MULT      S2pag         C2RCD
2364000703211    C     RESTO         IFGT      0
236500070321     C                   ADD       1             C2RCD
2366000703211E   C                   ELSE
236700070321     C                   SUB       S2pag         C2RCD
236800070321     C                   ADD       1             C2RCD
2369000703211-   C                   ENDIF
237000070321     C*
237100070321     C                   ENDSR
237200070321      *---------------------------------------------------------------*
237300070321      *?  Scrive il SFL       ?
237400070321      *---------------------------------------------------------------*
237500070321     C     Write_S2      BEGSR
237600071029     C*
237700071029     C* Pulisce il Nrecord di riposizionamento se richiesto da pgm chiamante
237800071029     c                   clear                   NRec_riposiz
237900071029     C*
238000070321     c                   exsr      Legge_rec
238100070321     C*
2382000703211    C     $EFILE        DowEQ     *OFF
238300070321     C*
238400070321     C                   clear                   DG31SF2
238500070404     c                   move      sav_in04      *in04
238600070528     c                   move      sav_in05      *in05
238700080219     c                   move      sav_in15      *in15
238800160506     c                   move      sav_in38      *in38
238900070510     C* Priorità
239000080715     C*    Consegne/Ritiri/Entrambe
239100080715     c                   if        d1dati = 'C'
239200080715     C*****              z-add     dst3prgg      h2prgg
239300080715     c                   z-add     prior_giroCon h2prgg
239400080715      *
239500080715     c                   elseIf    d1dati = 'R'
239600080715     c                   z-add     prior_giroRit h2prgg
239700080715      *
239800080715     c                   end
239900080715      *
240000070510     C                   z-add     dst3sql       h2sql
240100070321     C* GIRO DATA Distinta
240200070417     C                   MOVE      dstDFV        h2CDFV
240300070321     C                   MOVE      dstDFV        G02INV
240400070321     C                   MOVEL     '3'           G02ERR
240500070321     C                   CALL      'XSRDA8'
240600070321     C                   PARM                    WLBDAT
240700070321     C*
240800070321     C                   movel     G02DAT        campo4            4
240900070321     C                   move      G02DAT        s2CDFV
241000070321     C                   movel     campo4        s2CDFV
241100070321      *
241200070321     C                   Z-ADD     dstNFV        s2CNFV
241300070321     c     kapd          chain     fiapd01l
241400070321     c                   if        %found(fiapd01l)
241500070321     c                   eval      s2DAUT = apdrsc
241600070321     c                   else
241700070321     c                   clear                   s2DAUT
241800070321     c                   end
241900070321      *
242000070904     c                   if        d1dati= ' ' or d1dati= 'C'
242100070904     c                   z-add     DSTnbla       s2nbla
242200070419     c                   z-add     DSTnftl       s2nftl
242300070904     c                   z-add     DSTstpa       s2stpa
242400070904     c                   z-add     DSTPKL        s2cpem
242500070321     c                   z-add     DSTPKLa       s2cpea
242600070904     c                   z-add     DSTVLU        s2cvom
242700070321     c                   z-add     DSTVLUa       s2cvoa
242800070904
242900070904     c*****              if        d1dati= ' '
243000070927     c                   if        Abilita_ORM = 'S'
243100070904     c                   z-add     DSTRnbla      s2nbla1
243200070904     c                   z-add     DSTRnftl      s2nftl1
243300070904     c                   z-add     DSTRstpa      s2stpa1
243400070904     c                   z-add     DSTRPKL       s2cpem1
243500070904     c                   z-add     DSTRPKLa      s2cpea1
243600070904     c                   z-add     DSTRVLU       s2cvom1
243700070904     c                   z-add     DSTRVLUa      s2cvoa1
243800070927     c                   end
243900070904     c*****              end
244000070904
244100070904     c                   else
244200070904      *
244300070904     c                   z-add     DSTRnbla      s2nbla
244400070904     c                   z-add     DSTRnftl      s2nftl
244500070904     c                   z-add     DSTRstpa      s2stpa
244600070904     c                   z-add     DSTRPKL       s2cpem
244700070904     c                   z-add     DSTRPKLa      s2cpea
244800070904     c                   z-add     DSTRVLU       s2cvom
244900070904     c                   z-add     DSTRVLUa      s2cvoa
245000070904
245100070904     c******             if        d1dati= ' '
245200070904     c                   z-add     DSTnbla       s2nbla1
245300070904     c                   z-add     DSTnftl       s2nftl1
245400070904     c                   z-add     DSTstpa       s2stpa1
245500070904     c                   z-add     DSTPKL        s2cpem1
245600070904     c                   z-add     DSTPKLa       s2cpea1
245700070904     c                   z-add     DSTVLU        s2cvom1
245800070904     c                   z-add     DSTVLUa       s2cvoa1
245900070904     c******             end
246000070904
246100070904     c                   end
246200070904      *
246300070927     c*******            if        s2nbla1 >0 or s2cpem1 >0 or s2cvom1 >0 or
246400070927     c*******                      s2nftl1 >0 or s2cpea1 >0 or s2cvoa1 >0 or
246500070927     c*******                      s2stpa1 >0
246600070904
246700070904     c                   if        d1dati= ' ' or d1dati= 'C'
246800070927     c                   if        Abilita_ORM = 'S'
246900070904     C                   MOVE (p)  'Ritiri'      s2DDes1
247000070904     C                   MOVEL(p)  'Nr.Orm.'     s2DesN1
247100070927     c                   end
247200070904     c                   else
247300070904     C                   MOVE (p)  'Consegne'    s2DDes1
247400070904     C                   MOVEL(p)  'Nr.Sped'     s2DesN1
247500070904     c                   end
247600070904
247700070927     c*******            end
247800070904
247900070905     c                   if        DSTfpp  ='M'
248000070905     c                   movel     'Mat'         s2Tusc
248100070905     c                   else
248200070905     c                   movel     'Pom'         s2Tusc
248300070905     c                   end
248400070820     c                   clear                   s2STP
248500070820     c                   if        DSTfasd ='STP'
248600080625     c                   movel     'St'          s2STP
248700070820     c                   endif
248800070321      *
248900080625      * Controlla se ci sono delle Fasi presenti da segnalare
249000080625     c                   clear                   fidg39dS
249100080625     c                   move      d1CNPG        fdg39NPG
249200080625     c                   z-add     s2CNFV        fdg39NFV
249300080625     c                   z-add     c2cfgs        fdg39FGS
249400080625     c                   move      'N'           fdg39VIDno
249500080625     c                   eval      kpjbus = kpjbu
249600080625     c                   movel(p)  fidg39ds      KPJBU
249700080625     c                   call      'FIDG39R'
249800080625     c                   parm                    kpjba
249900080625     c                   eval      fidg39ds = KPJBU
250000080625     c                   eval      kpjbu = kpjbus
250100080625      * se ci sono delle Fasi da vedere imposta l'asterisco nel primo Byte
250200080625      *  del flag di stampato.
250300080625     c                   if        fdg39FASI = 'S'
250400080625     c                   move      '*'           s2STP
250500080625     c                   endif
250600080613      *
250700070321     c* Visualizzo tipo distinta parcel/messaggerie, se p.o. abilitato
250800070321     c                   if        *in08
250900070321     c                   if        dsttpm='M'
251000070321     c                   movel     'Mes'         s2CTPM
251100070321     c                   endif
251200070321     c                   if        dsttpm<>'M'
251300070321     c                   movel     'Prc'         s2CTPM
251400070321     c                   endif
251500070321     c                   endif
251600160503     c** dal filler
251700160503     C                   movel     dstflr        DDStflr
251800160503     c* x il TURNO
251900160503     c                   if        *in38
252000160505     c                   if        dsttpm <> 'M'
252100160505     c                   movel     'Turno'       s2turno
252200160505     c                   endif
252300160505     c                   movel     §DSTTURNO     s2CTUR
252400160504     c                   if        dsttpm <> 'M' and
252500160505     c                             s2CTUR = *blank
252600160504     c                   eval        s2CTUR = '1'
252700160504     c                   endif
252800160503     c                   endif
252900070321      * Flag tipo distinta (solo consegna, orm o entrambi)
253000070321     C                   CLEAR                   WBOL
253100070321     C                   CLEAR                   WORM
253200070321     c                   Z-Add     d1CFGS        Wifp
253300070321     C                   Z-ADD     s2CNFV        kfnvv
253400070321     C                   Z-ADD     s2CNFV        WnFV
253500070321     c     Karb78        Setll     Fnarb78l
253600070321if  5c                   if        %equal(fnarb78l)
253700070321     c                   movel     'S'           wbol              1
253800070321x   5c                   else
253900070321     C     Kdst          SETLL     FnBRV01L
254000070321     c                   if        %equal(fnbrv01l)
254100070321     c                   movel     'S'           wbol              1
254200070321     c                   endif
254300070321e   5c                   endif
254400070321     C     KORM          SETLL     FNORM06L
254500070321do  5c                   do        *hival
254600070321     C     KORM          reade     FNORM06L
254700070321     c                   if        %eof(fnorm06l)
254800070321     c                   leave
254900070321     c                   endif
255000070321     c                   if        ormatb = *blanks and ormfao < 999
255100070321     C                   move      'S'           worm              1
255200070321     c                   endif
255300070321e   5c                   enddo
255400070321     C                   if        wbol = 'S' and worm <> 'S'
255500070321     C                   movel     'CON'         s2CTPD
255600070321     C                   movel     'TpD'         s2dTPD
255700070321     C                   endif
255800070321     C                   if        wbol <> 'S' and worm = 'S'
255900070321     C                   movel     'ORM'         s2CTPD
256000070321     C                   movel     'TpD'         s2dTPD
256100070321     C                   endif
256200070321     C                   if        wbol = 'S' and worm = 'S'
256300070321     C                   movel     'C/O'         s2CTPD
256400070321     C                   movel     'TpD'         s2dTPD
256500070321     C                   endif
256600070321      * Calcolo importi se non già fatto prima
256700070321if  5c                   if        d1CTFV = '2' and d1IMPO = 'S'
256800070321     c                   exsr      totalizzo
256900070321     c                   z-add     w_imas        s2IMAS
257000070321     c                   z-add     w_imca        s2IMCA
257100070321     c                   endif
257200070321      * Pda
257300080208     c                   setoff                                       14
257400080208     c                   if        dstDFV >= DateU8
257500080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
257600080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
257700080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
257800080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
257900080208     c                   seton                                        14
258000080208     c                   endIf
258100080208     c                   endIf
258200080222     c                   eval      h2in14 = *in14
258300080222      *
258400080208      *  lo Minuscolizza sempre a video
258500080207     C     Up:Lo         XLATE     §DSTtstPDA    s2CTSTPDA
258600080208      *
258700070321     C                   movel     dstPDA        s2CPDA
258800070419     C                   if        s2CPDA <> *blank
258900070419     C                   movel     'PDA'         s2DPDA
259000070419     c                   end
259100070321      * ANNULLATO
259200070321     C     dstATB        IFEQ      'A'
259300070321     C                   MOVEL     'ANN'         s2CANN
259400070321     C                   END
259500070321if  5C     dstFCF        IFNE      ' '
259600070321      * CHIUSO
259700070321     C     dstFCF        IFEQ      'S'
259800070821     C                   MOVEL     'Chiu'        s2CFCF
259900070321     C                   ELSE
260000070321     C* BLOCCATO
260100070322     C                   MOVEL     'BLoc'        s2CFCF
260200070321e   5C                   ENDIF
260300070321e   4C                   ENDIF
260400070904
260500070904      *   Se richiamato come Ritiri o Consegne o Entrambe
260600070321      *
260700070321      * Intestazioni seconda riga
260800070830     c                   setoFF                                       18
260900070321     c                   if        s2imca <> 0
261000070321     c                   movel     Imp_C_A       s2dica
261100070830     c                   setoN                                        18
261200070321     c                   end
261300070321      *
261400070830     c                   setoFF                                       20
261500070321     c                   if        s2imas <> 0
261600070321     c                   movel     Imp_ASS       s2dias
261700070830     c                   setoN                                        20
261800070321     c                   end
261900070321      *
262000070510      *  Illumina diversamente il campo x le distinte con priorità (1)
262100070510     c                   setoff                                       40
262200070510if  4c                   if        h2prgg = 1
262300070510     c                   seton                                        40
262400070510     c                   end
262500070321      *---------
262600070321if  4c                   if        FDGtfv <> '3'
262700160601     c                   if        (sav_c2nfv <= s2cnfv and *in11 = *off) or
262800160601     c                             (sav_c2aut <= s2daut and *in11 = *on ) or
262900070321     c                             (sav_c2aut = *blank and sav_c2nfv =0)
263000070404     C* Imposta indicatore x abilitare campo scelta
263100070404     c                   move      sav_in19      *in19
263200070321     c********           seton                                        32
263300071029      *
263400070321     C                   ADD       1             S2NRR
263500070321     C                   ADD       1             Y
263600071029      *
263700071029      *  se deve posizionare il cursore sul record se il pgm è
263800071029      *  chiamato per assegnare/disassegnare accende il PC sulla scelta
263900071029      *  solo se ha il nr. Foglio di riferimento per il posizionamento sfl.
264000071029     c                   if        fdgNFpos <> *blank
264100071029     c                   move      s2cNFV        sav2NFV           6
264200071029     c                   if        fdgNFpos = sav2NFV
264300071029     c                   seton                                        42
264400071029     c                   move      'X'           H2recPos
264500071029     c                   end
264600071029     c                   end
264700071029      *
264800070321     C                   WRITE     DG31SF2
264900071029     c                   setoFF                                       42
265000070321     c                   end
265100070321e   4c                   endif
265200070321      *---------
265300070321      *  Esce da qui con un record di Totale x GIRO
265400070321     c                   exsr      Legge_rec
265500070321     c                   EndDO
265600070321     C*
265700070321     C                   ENDSR
265800070321      *---------------------------------------------------------------*
265900070321      *?  CONTROLLO TESTATA LISTA    ?
266000070321      *---------------------------------------------------------------*
266100070321     C     CTRC2         BEGSR
266200070321      *
266300070321     C                   MOVE      *OFF          *IN99
266400070321      *
266500070321     C*------------------------
266600070321      *  interrogazione Giro
266700070321     c                   clear                   c2dcgi
266800070321     c     '?'           scan      c2cCGI
266900070321     C*   ricerca codice giro
267000070321     c                   if        %Found
267100070321     c                   clear                   fidg09ds
267200070321     c                   move      'P01'         d09iop0
267300070321     c                   movel     c2cFGS        d09ifgs
267400071010     c****               movel     'C'           d09itug
267500071010     c                   movel     d1DATI        d09itug
267600070321     c                   z-add     DATEU8        d09idat
267700070321     c                   eval      kpjbus = kpjbu
267800070321     c                   movel(p)  fidg09ds      kpjbu
267900070321     c                   call      'FIDG09R'
268000070321     c                   parm                    kpjba
268100070321     c                   movel     kpjbu         fidg09ds
268200070321     c                   eval      kpjbu = kpjbus
268300070321     c                   movel     d09ocgi       c2cCGI
268400070321     c                   end
268500070321     C*------------------------
268600070321      *  controllo validità giro
268700070321     c                   if        c2cCGI <> *blank
268800070321     c                   clear                   fidg09ds
268900070321     c                   eval      D09IOP0 = '001'
269000070321     c                   eval      d09ifgs = c2cFGS
269100071010     c*****              eval      d09itug = 'C'
269200071010     c                   eval      d09itug = d1DATI
269300070321     c                   eval      d09icgi = c2cCGI
269400070321     c                   move      dateu8        d09idat
269500070321     c                   eval      kpjbus = kpjbu
269600070321     c                   eval      kpjbu = fidg09ds
269700070321     c                   call      'FIDG09R'
269800070321     c                   parm                    kpjba
269900070321     c                   eval      fidg09ds = kpjbu
270000070321     c                   eval      kpjbu = kpjbus
270100070321     c                   if        d09oerr = '1'
270200070321     C                   SETON                                        54  99
270300070321     C                   goto      ENDC2
270400070321     c                   else
270500070321     c                   if        d09ocgi <> ' '
270600070321     c                   eval      c2dcgi = d09odes
270700070321     c                   move      'S'           con_giro
270800070321     c                   end
270900070321     c                   end
271000070321      *
271100070321     c                   end
271200070321     C*------------------------
271300070321      *  interrogazione AUT
271400070321      *
271500070321     c     '?'           scan      c2daut
271600070321     C*   ricerca codice giro
271700070321     c                   if        %Found
271800070321     c                   clear                   fnlv24ds
271900070321     C                   Z-ADD     c2cFGS        d24FIL
272000070321     C                   MOVEL     'R'           d24FLG
272100070321     C                   MOVEL     'A'           d24tip
272200070321     c                   eval      kpjbus = kpjbu
272300070321     C                   MOVEL(p)  fnlv24ds      KPJBU
272400070321     C                   CALL      'FNLV24R'
272500070321     C                   PARM                    KPJBA
272600070321     C                   MOVEL     KPJBU         fnlv24ds
272700070321     c                   eval      kpjbu = kpjbus
272800070321     C                   MOVEL     D24RSC        c2daut
272900070321     c                   end
273000070321     C*
273100070321     C*------------------------
273200070321     c                   move      *on           $PcurS2
273300070321     C* Riposizionamento x AUT o x Numero
273400160601     c                   if        (*in11 = *off and c2cnfv <> sav_c2nfv) or
273500160601     c                             (*in11 = *on  and c2daut <> sav_c2aut)
273600070321     c                   move      *off          $PcurS2
273700070321     c                   move      *on           $inzs2
273800070321     c                   move      'S2'          $GEST
273900160601     c   11              movel     c2daut        sav_c2aut
274000160601     c  n11              z-add     c1cnfv        sav_c1nfv
274100070321     c                   end
274200070321     C*
274300070321     C* Posiziona il cursore in testata x 12 = *on
274400070321     c     $PcurS2       comp      *on                                    12
274500070321     C  n12              eval           H1RIGA = 3
274600070528     C  n12              eval           H1COLO = 17
274700070321     C   12              eval           H1RIGA = 7
274800070321     C   12              eval           H1COLO = 2
274900070321      *
275000070321     C*------------------------
275100070321      *  se tutto OK
275200070321     c                   if        c2ccgi <> d1ccgi
275300070321     c                   move      c2ccgi        d1ccgi
275400070321     c                   move      *on           $inzs2
275500070321     c                   move      'S2'          $GEST
275600070321     c                   end
275700070321      *
275800070321     C*------------------------
275900070321      *
276000070321     c     endc2         tag
276100070321     C                   ENDSR
276200070321      *---------------------------------------------------------------*
276300070321      *? CONTROLLO OPZIONI LISTA     ?
276400070321      *---------------------------------------------------------------*
276500070321     C     CTRS2         BEGSR
276600070321     C*
276700070321     C                   MOVEL     *OFF          $ESCI
276800070321     C                   SETOFF                                       99
276900070321     C*
277000070321     C* Leggo il sfl solo se ci sono rcd
2771000703211    C     WMAX          IFGT      0
277200070321     C                   READC     DG31SF2                                21
277300071026     C*
277400070321     C* esce se fine sfl o errore che richiede l'uscita
2775000703212    C     *IN21         DOWEQ     *OFF
277600070321     C     $ESCI         ANDEQ     *OFF
277700070321     C                   Z-ADD     S2NRR         C2RCD
277800071026     c                   eval      Che_NRec_sfl = s2nrr
277900070321     C* ctrl su riga
278000070321     C                   EXSR      RECS2
278100070321     C*
278200070321     C* gestione opzioni
2783000704043    C     s2SCE         IFNE      *blank
278400070321     C     *IN99         ANDEQ     *OFF
278500070321     C                   EXSR      SCE_S2
2786000703213-   C                   ENDIF
278700070321      *
278800070321     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
2789000703213    C     *IN99         IFEQ      *ON
279000070321     C     $ESCI         OREQ      *ON
279100070321     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
279200070321     C* la ripristinerò a conclusione del ciclo di READC
279300070321     C                   MOVE      *OFF          $INZS2
2794000703213-   C                   ENDIF
279500070321     C*
279600070321      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
279700070404     c                   if        s2SCE  <> *blank
279800070321     C******             MOVE      *ON           *IN32
279900070404     c                   clear                   s2SCE
280000070321     c                   end
280100070510      *
280200070510     c                   setoff                                       40
280300070510if  4c                   if        h2prgg = 1
280400070510     c                   seton                                        40
280500070510     c                   end
280600070510      *
280700080222     c                   eval      *in14 = h2in14
280800070321     C                   UPDATE    DG31SF2
280900070321     C*
281000070321     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
2811000703213    C     $ESCI         IFEQ      *OFF
281200070321     C                   READC     DG31SF2                                21
281300070321      *
281400070321     C* a fine ciclo ripristino il flag richiesta iniz.sfl
2815000703214    C     *IN21         IFEQ      *ON
281600070321     C* calcolo pagina a cui deve posizionarsi
281700070321     c                   z-add     s2pag         Wpagine
281800070321     C                   EXSR      CLCPAG
2819000703214-   C                   ENDIF
2820000703213-   C                   ENDIF
282100070321     C*
2822000703212-   C                   ENDDO
282300071026     C*
282400071026      * -----------------------
282500071026      * ? calcola "su quale record" riposizionarsi tenendo conto che
282600071026      * ?  il SFL ha una testata di 6 righe ed inizia dalla 7°.
282700071026     c     Che_NRec_sfl  div       s2pag         Su_Quale_rec
282800071026     c                   mvr                     resto
282900071026     C     Resto         IFeq      0
283000071026     C                   z-add     15            Su_Quale_rec
283100071026     C                   Else
283200071026     C                   z-add     resto         Su_Quale_rec
283300071026     C                   ENDIF
283400071026      * ?  aggiunge le 6 righe di testata.
283500071026     C                   add       6             Su_Quale_rec
283600071026      * -----------------------
283700070321     C*
2838000703211-   C                   ENDIF
283900070321     C*
284000070321     C                   ENDSR
284100070321     C/EJECT
284200070321      *---------------------------------------------------------------*
284300070321      *? CONTROLLO CAMPI I/O RIGA LISTA  ?
284400070321      *---------------------------------------------------------------*
284500070321     C     RECS2         BEGSR
284600070321     C*
284700070321     C* reset indicatori DSPATR
284800070321     c                   exsr      INZ_indERR
284900070528     C*
285000070528     C* se non Richiamato non sono abilitate le opzioni 2 e 3
285100070528     c                   if        pgm_da_call <> 'S'
285200070528     c                   if        s2sce = '2' or s2sce = '3'
285300070528     c                   seton                                        99  41
285400070528     c                   end
285500070528     c                   end
285600070528     C* Se richiamato in assegnazione non deve accettare opz 2,3
285700070620     c                   if        (pgm_da_call =  'S' and fdg31_opz = '1' and
285800080715     c                             fdgPGM = 'ASSMANUALE')
285900080715     c                             or
286000080715     c                             (fdgPGM<>'ASSMANUALE' and fdgPGM<>'MONITOR')
286100070528     c                   if        s2sce = '2' or s2sce = '3'
286200070528     c                   seton                                        99  41
286300070528     c                   end
286400070528     c                   end
286500070620     C*
286600070620     c                   if        pgm_da_call =  'S' and fdg31_opz <> '1' and
286700080715     c                             (fdgPGM ='ASSMANUALE' or fdgPGM ='MONITOR')
286800070620     c                   if        s2sce = '1'
286900070620     c                   seton                                        99  41
287000070620     c                   end
287100070620     c                   end
287200080625      * se la filiale non è partita con il PDA non si può chiedere di
287300080625      * vedere le Fasi della distinta
287400080725     c*******            if        s2sce = 'F' and not *in15 or
287500080725     c*******                      s2sce = 'F' and S2CPDA = 'N'
287600080725     c*******            seton                                        99  41
287700080725     c*******            end
287800070528     C*
287900070321     C                   ENDSR
288000070321      *---------------------------------------------------------------*
288100070321      *? GESTIONE OPZIONI LISTA          ?
288200070321      *---------------------------------------------------------------*
288300070321     C     SCE_S2        BEGSR
288400070321     C*
288500070321     C* determino Riga/Colonna del cursore
288600070321     C                   MOVE      NRG           R$$
288700070321     C                   MOVE      NCL           C$$
288800070321     C                   Z-ADD     RIRI          CURR
288900070321     C                   Z-ADD     COCO          CURC
289000071029     C*
289100071029     c                   If        s2SCE <> *blank
289200071029     c                   move (p)  s2CNFV        FDGNFPOS
289300071029     c                   end
289400070404     C*
289500070404     c                   If        s2SCE  = '1'
289600070405     c                   If        pgm_da_call <> *blank
289700070404     c                   move      *on           $FINE
289800070404     c                   move      *on           $ESCI
289900070404     c                   z-add     s2CNFV        FDGNFV
290000070404     c                   move      d1cnpg        FDGNPG
290100070528     c                   eval      fdgOPZ = s1SCE
290200070405     c                   else
290300070405     c                   z-add     s2CNFV        d1cNFV
290400070405     c                   move      'D1'          $GEST
290500070404     c                   end
290600070405     c                   end
290700070528      *
290800070528     c                   If        s2SCE  = '2' or s2SCE = '3'
290900070528     c                   if        pgm_da_call <> *Blank
291000080704      *
291100080704      * Forza la scrittura per non far cancellare la pagina emessa
291200070528     c                   move      *on           $ESCI
291300070528     c                   move      *on           $FINE
291400070528     c                   z-add     s2CNFV        FDGNFV
291500070528     c                   move      d1cnpg        FDGNPG
291600070528     c                   eval      fdgOPZ = s2SCE
291700070528     c                   clear                   fdgCGI
291800070528     c                   end
291900070528     c                   end
292000070528      *
292100070404     c                   If        s2SCE  = '5'
292200080701      *
292300080701      * Se la distinta ha delle Fasi da mostrare -> abilita il tasto
292400080701      *  funzionale per poter richiamare la window di visualizzazione
292500080701     c                   move      *oFf          d2fasi
292600080704     c                   if        %subst(s2stp:3:1)='*'
292700080701     c                   move      *oN           d2fasi
292800080701     c                   end
292900080701      *
293000070321     c                   z-add     s2CNFV        d1CNFV
293100070321     c                   z-add     c2cfgs        d1CFGS
293200070321     c                   z-add     s2CNFV        wnfv
293300070403     c                   move      'D2'          $GEST
293400070403     C                   move      'S'           $VISD2
293500070403     c                   move      *on           $INZD2
293600070404      *
293700070404     c                   end
293800070321     C*
293900070427     C*
294000070621     c                   If        s2SCE  = '7'
294100070427     c                   clear                   fidg35ds
294200070528     c                   eval      D35FGSi = wfgs
294300070528     c                   eval      D35DDCi = H2CDFV
294400070528     c                   eval      D35NDCi = S2CNFV
294500070528     c                   eval      D35NPGi = wnpg
294600070831     c                   eval      D35PGMi = fdgPGM
294700070903      * si vuole fare vedere tutto
294800070903     c******             eval      D35TPi  = 'C'
294900070905     c                   If        pgm_da_call <> *blank and FDGOPZ <> '1' and
295000080715     c                             (fdgPGM ='ASSMANUALE' or fdgPGM ='MONITOR')
295100070528     c                   eval      D35opzi = 'S'
295200070528     c                   end
295300070427     c                   eval      kpjbus = kpjbu
295400070427     c                   movel(p)  fidg35ds      kpjbu
295500070427     c                   call      'FIDG35R'
295600070427     c                   parm                    kpjba
295700070427     c                   movel     kpjbu         fidg35ds
295800070427     c                   eval      kpjbu = kpjbus
295900070621     c                   if        s2nbla >0
296000070611     c                   if        pgm_da_call <> *Blank and fdgOPZ<>'1'
296100070528     c                   move      *on           $ESCI
296200070831     c                   if        d35funO <>'12'
296300070528     c                   move      *on           $FINE
296400070831     c                   end
296500070528     c                   z-add     s2CNFV        FDGNFV
296600070831     c                   if        d35funO = '12' or d35funO = '03'
296700070831     c                   z-add     0             FDGNFV
296800070831     c                   end
296900070528     c                   move      d1cnpg        FDGNPG
297000070528     c                   eval      fdgOPZ = d35opzO
297100070528     c                   eval      fdgCGI = d35cgiO
297200070528     c                   end
297300070621     c                   end
297400070427     c                   end
297500070427     C*
297600070621     c                   If        s2SCE  = '9'
297700070321     c                   z-add     s2CNFV        wnfv
297800070321     c*******            move      *on           $ESCI
297900070321     c                   move      'D2'          $GEST
298000070403     C                   move      'N'           $VISD2
298100070321     c                   move      *on           $INZD2
298200070321     C                   exsr      GESD2
298300070321     c                   endIf
298400080625      *
298500080625      *  Visionare le Fasi della Distinta
298600080625     c                   If        s2SCE  = 'F'
298700080625     c                   write     FRCDTA
298800080625      *
298900080625     c                   clear                   fidg39dS
299000080625     c                   move      d1CNPG        fdg39NPG
299100080625     c                   z-add     s2CNFV        fdg39NFV
299200080625     c                   z-add     c2cfgs        fdg39FGS
299300080625     c                   move      'N'           fdg39noREC
299400080625     c                   clear                   fdg39VIDno
299500080625     c                   eval      kpjbus = kpjbu
299600080625     c                   movel(p)  fidg39ds      KPJBU
299700080625     c                   call      'FIDG39R'
299800080625     c                   parm                    kpjba
299900080625     c                   eval      kpjbu = kpjbus
300000080625      *
300100080625     c                   end
300200080625     C*
300300080820      *  Dettaglio Distinta
300400080820     c                   If        s2SCE  = 'D'
300500080820     c                   write     FRCDTA
300600080820      *
300700080820     c                   clear                   fidg311dS
300800080820     c                   z-add     c2cfgs        f31FGS
300900080820     c                   z-add     s2CNFV        f31NumDis
301000080820     c                   z-add     h2CDFV        f31DatDis
301100080820     c                   move      d1Dati        f31PriDati
301200081111     c**********         if        D1CPDA <> 'C' and D1CPDA <> 'O'
301300081111     c**********         move      'E'           f31DatiVis
301400081111     c**********         else
301500080820     c                   move      D1CPDA        f31DatiVis
301600081111     c**********         end
301700080820     c                   eval      kpjbus = kpjbu
301800080820     c                   movel(p)  fidg311ds     KPJBU
301900080820     c                   call      'FIDG311R'
302000080820     c                   parm                    kpjba
302100080821     c                   eval      fidg311ds =   KPJBU
302200080821     c                   if        F31Tfunz = '03'
302300080821     c                   move      *on           $ESCI
302400080821     C                   EXSR      F03_exit
302500080821     c                   end
302600080820     c                   eval      kpjbu = kpjbus
302700080820      *
302800080820     c                   end
302900080820     C*
303000070321     C*
303100070321     C                   ENDSR
303200070321     C/EJECT
303300070312      *---------------------------------------------------------------*
303400070321      *?  LETTURA RCD ARCHIVIO PILOTA         ?
303500070312      *---------------------------------------------------------------*
303600070312     C     LEGGE_REC     BEGSR
303700070312     C*
303800070312     C*  Per EoF deve uscire
303900070312     C                   MOVEL     *OFF          $EFILE
304000070312     C*  Per Record Ok deve uscire riportando i dati
304100070312     C                   MOVEL     *OFF          $RCDOK
304200070312     C*
3043000703121    C     $EFILE        DOUEQ     *ON
304400070312     C     $RCDOK        OREQ      *ON
304500070312     C*
304600070312if  2C                   IF        dstDFV <= DATFIW or Prima_Lettura <> 'N'
304700070312     c                   eval      Prima_Lettura = 'N'
304800070312      *
304900070406     c                   if        Solo_aperte = 'N'
305000070312     c     Kdat2         Reade     Fidst008                               33
305100070406     c                   else
305200070312     C     Kdst3         READE     Fidst003                               33
305300070406     c                   End
305400070312      *
305500070312    2C                   END
305600070312      *
305700070312      * solo se superato il limite del range di date
305800070312if  2C                   IF        dstDFV > DATFIW
305900070312     c                   If        Solo_aperte = 'N'
306000070312     C                   SETON                                        33
306100070312     c                   Else
306200070312     C     Kdst3         READE     Fidst003                               33
306300070312     c                   End
306400070312e   2C                   ENDif
306500070312      *
306600070312     C                   select
306700070312      * Fine File
306800070312     C                   when      *in33 = *on
306900070312     C                   MOVEL     *ON           $EFILE
307000070312      *
307100070312     C                   other
307200070312      *
307300070312      * ?Controlla la Spedizione se è da considerare
307400070312     C                   exsr      Check_REC
307500070312      *
307600070312      * ?     se Non è Esclusa
307700070312     c                   if        *in96 = *on
307800070312     C                   MOVE      *ON           $RCDOK
307900070312     c                   end
308000070312      *
308100070312     C                   endsl
308200070312     C*
3083000703121-   C                   ENDDO
308400070312     C*
308500070312     C                   ENDSR
308600070312      *---------------------------------------------------------------*
308700070321      *?  CONTROLLA la riga se deve essere presa in considerazione    ?
308800070312      *---------------------------------------------------------------*
308900070312     C     Check_REC     BEGSR
309000070312     C*
309100070312     C*  96 = *on x Record OK
309200070312     C                   SETOFF                                       96
309300070312      *
309400080724      *  pro-memoria
309500080724     c                   clear                   Fase_STP
309600080613     c                   clear                   Fasi_DSF
309700080613     c                   clear                   Trovato_DSF
309800080613      *
309900080724      *  Controlla se la distinta è stata fatta con PDA
310000080613     c                   eval      Dist_consegne_PDA = ' '
310100080623     c                   eval      Dist_ritiri_PDA = ' '
310200080623      *
310300160503      *  se in test nel filler (anche per il TURNO)
310400080613     c                   eval      ddstFLR = dstFLR
310500160503      *
310600080613     c                   If          dstPDA     = 'C' or dstPDA     = 'E' or
310700080613     c                               §DSTTSTPDA = 'C' or §DSTTSTPDA = 'E'
310800080613     c                   eval      Dist_consegne_PDA = 'S'
310900080613     c                   end
311000080613      *
311100080623     c                   If          dstPDA     = 'O' or dstPDA     = 'E' or
311200080623     c                               §DSTTSTPDA = 'O' or §DSTTSTPDA = 'E'
311300080623     c                   eval      Dist_ritiri_PDA = 'S'
311400080623     c                   end
311500080624      *
311600080624      * ?Al momento si tiene in considerazione anche la fase presente in testata
311700080624     c                   if        dstFASD = 'STP'
311800080624     c                   eval      Fase_STP = 'S'
311900080624     c                   eval      Fasi_DSF(1) = dstFASD
312000080624     c                   end
312100080624      *
312200080616      *  poi controllo sul dettaglio delle Fasi
312300080613     C     Kdsf_01       setll     fiDSF01l
312400080613     C     Kdsf_01       reade     fiDSF01l
312500080613      *
312600080613     c                   dow       not %Eof(fiDSF01l)
312700080624      *
312800080613     c                   if        dsfTRD <> *BLANK
312900080613     c                   eval      Trovato_DSF = 'S'
313000080624      * Se c'è la fase STP.
313100080624     c                   if        dsfTRD = 'STP'
313200080624     c                   eval       Fase_STP = 'S'
313300080624     c                   end
313400080623      *
313500080613      * carica le fasi su una schiera
313600080613     c                   z-add     1             dsf               3 0
313700080613     c     dsfTRD        lookup    Fasi_DSF(dsf)                          21
313800080613     c                   if         not *in21
313900080613     c                   z-add     1             dsf
314000080613     c     *blank        lookup    Fasi_DSF(dsf)                          21
314100080613      * imposta nel primo libero
314200080613     c                   if         *in21
314300080613     c                   eval      Fasi_DSF(dsf) = dsfTRD
314400080613     c                   end
314500080613     c                   end
314600080613      *
314700080613     c                   end
314800080613     C     Kdsf_01       reade     fiDSF01l
314900080613     c                   end
315000080613      *
315100070312      * Se richieste Distinte Aperte e data foglio > data inizio (range)   oppure
315200070312      *  Se richieste Distinte Chiuse                                      oppure
315300070312      *   Tutte
315400080624      *?  Abilita il record  per  essere caricato sul SFL     --> 96=ON     ?
315500070312     c                   If        (Solo_aperte = 'S' and dstDFV >= DATINW) or
315600070312     c                             (d1CTFV = '1' and dstFCF <> ' ') or
315700070312     c                             (d1CTFV = ' ')
315800070312     C                   SETON                                        96
315900070312     C                   END
316000080624      *?  Poi Controlla con i Parametri se SCARTARE il record  --> 96 OFF   ?
316100070312      *
316200070312      **  Se impostato il Giro deve caricare le distinte che hanno il Giro
316300070312     c                   if        d1ccgi <> *blank
316400070312     C                   Z-ADD     dstNFV        WnFV
316500070312     c                   move      'N'           Giro_in_NFV       1
316600080715     c                   clear                   Prior_GiroCon     1 0
316700080715     c                   clear                   Prior_GiroRit     1 0
316800070910      **
316900070910     c                   if        d1dati = 'C'
317000070910      **
317100070312     c     kdst31        setll     fidst31l
317200070312     c     kdst31        reade     fidst31l
317300070312     c                   dow       not %Eof(fidst31l)
317400070312      **
317500070312      **  Controlla presenza del giro richiesto
317600070312     c                   if        %char(DST3NPG) = d1cNPG and
317700070312     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
317800070312     c                             and DST3CGI = d1cCGI
317900070312     c                   move      'S'           Giro_in_NFV       1
318000080715     c                   z-add     dst3prgG      Prior_GiroCon
318100070312     c                   leave
318200070312     c                   end
318300070312      **
318400070312     c     kdst31        reade     fidst31l
318500070312     c                   endDo
318600070910      **
318700070910     c                   else
318800070910      **
318900070910     c     kdst51        setll     fidst51l
319000070910     c     kdst51        reade     fidst51l
319100070910     c                   dow       not %Eof(fidst51l)
319200070910      **
319300070910      **  Controlla presenza del giro richiesto
319400070910     c                   if        %char(DST5NPG) = d1cNPG and
319500070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
319600070910     c                             and DST5CGI = d1cCGI
319700070910     c                   move      'S'           Giro_in_NFV       1
319800080715     c                   z-add     dst5prgG      Prior_GiroRit
319900070910     c                   leave
320000070910     c                   end
320100070910      **
320200070910     c     kdst51        reade     fidst51l
320300070910     c                   endDo
320400070910      **
320500070910     c                   end
320600070312      **
320700070312      ** Il giro selezionato non è presente nella distinta
320800070312     c                   if        Giro_in_NFV = 'N'
320900070312     C                   SETOff                                       96
321000070312     c                   end
321100070312      *
321200070312     c                   end
321300070312     C*
321400070312     C* CONTROLLO CHE SIA FOGLIO DI FILIALE IN GESTIONE
321500070312if  3C   96d1CFGS        IFNE      0
321600070312      *
321700070312     C     dstFGS        IFNE      d1CFGS
321800070312     C                   SETOFF                                       96
321900070312     C                   ENDIF
322000070312     C*
322100070312x   3C                   ELSE
322200070312     C*
322300070312     C* CARICO SOLO LE FILIALE NON DELL'AREA
322400070312     C     dstFGS        LOOKUP    L1                                     21
322500070312     C   21              SETOFF                                       96
322600070312e   3C                   ENDIF
322700070312
322800070312      * richiamato da FIMS03R, che passa i F.V. già inseriti
322900070504if  3c                   if        sav_in19 = *ON and *in96
323000070312     c                   eval      *in21  = *off
323100070404     c     dstnfv        lookup    $FV6                                   21
323200070504      * Per visualizzare le Distinte non presenti sul piano Packing List
323300070504if  3c                   IF        *in21 = *oN and fdgTFV = '2'
323400070504     C                   SETOFF                                       96
323500070504     c                   end
323600070312      * richiamato SOLO per sapere se esistono distinte aperte
323700070312      *   non inserito nel piano di cui sono stati passati i F.V.
323800070504if  3c                   IF        *in21 = *oFF and fdgTFV = '3'
323900070419     C                   eval      FDGfs1 = *on
324000070312     C                   MOVE      *ON           $FINE
324100070312     C                   MOVEL     *ON           $EFILE
324200070419     c                   LeaveSR
324300070312e   3c                   endif
324400070419e   3c                   endif
324500070312      *
324600070312     c                   if        *in08 and d1CTPM<>' '
324700070312     c                   if        (d1CTPM='M' and dsttpm<>'M') or
324800070312     c                             (d1CTPM='P' and dsttpm ='M')
324900070312     c                   eval      *in96  = *off
325000070312     c                   endif
325100070312     c                   endif
325200070312      *
325300160503      *   TURNO  solo per PARCEL e NON messaggerie
325400160504      *    aggiunto controllo se c'è "M" Messaggerie non controlla nulla
325500160503     c                   if        *in38 and d1ctpm <> 'M'
325600160503     c                   if        ((d1Ctur1='1'  or d1Ctur2='1')   and
325700160503     c                             ((§dstturno=' ' and dsttpm<>'M') or
325800160503     c                              §dstturno='1'))
325900160503     c                                or
326000160503     c                             ((d1Ctur1='2'  or d1Ctur2='2')   and
326100160503     c                                               §dstturno ='2')
326200160503     c                                or
326300160503     c                              (d1Ctur1=' ' and d1Ctur2=' ')
326400160504     c                                or
326500160504     c                              dsttpm ='M'
326600160503      * qui sarebbe OK
326700160503     c                   else
326800160503      * da scartare
326900160503     c                   eval      *in96  = *off
327000160503     c                   endif
327100160503     c                   endif
327200160503      *
327300070625      * Tipo Uscita Mattina/Pomeriggio
327400070625     c                   if        d1Tusc<>' '
327500070625     c                   if        (d1Tusc='M' and dstFPP<>'M') or
327600070625     c                             (d1Tusc='P' and dstFPP<>'P')
327700070625     c                   eval      *in96  = *off
327800070625     c                   endif
327900070625     c                   endif
328000070625      *
328100070417      *   Range di date entro i limiti
328200070417     C     dstDFV        IFgt      DATfiw
328300070417     C     dstDFV        ORlt      DATinw
328400070417     C                   SETOFF                                       96
328500070417     C                   ENDIF
328600070312      *
328700080623      *   se si vuole vedere solo un Tipo di distinte con PDA o senza o tutte
328800080701      * Senza PDA
328900080701     C                   IF        D1cPDA = 'N' and (
329000080701     c                             Dist_Consegne_PDA <>' ' or
329100080701     c                             Dist_Ritiri_PDA <>' ' )
329200080701     C                   SETOFF                                       96
329300080701     C                   ENDIF
329400080701      * PDA con Ritiri e Consegne
329500081111     C                   IF        D1cPDA = 'S' and (
329600080915     c                             Dist_Consegne_PDA = ' ' and
329700080702     c                             Dist_Ritiri_PDA = ' ' )
329800080623     C                   SETOFF                                       96
329900080623     C                   ENDIF
330000080701      * PDA con Consegne
330100081111     C**********         IF        D1cPDA = 'C' and
330200081111     c**********                   Dist_Consegne_PDA = ' '
330300081111     C**********         SETOFF                                       96
330400081111     C**********         ENDIF
330500080701      * PDA con Ritiri
330600081111     C**********         IF        D1cPDA = 'O' and
330700081111     c**********                   Dist_Ritiri_PDA = ' '
330800081111     C**********         SETOFF                                       96
330900081111     C**********         ENDIF
331000080701      *
331100080701      *  se si vuole vedere solo quelle senza PDA pulisco la lista
331200080724      *   delle fasi scelte tranne per la fase stampata
331300080701     C                   IF        D1cPDA = 'N'
331400080724     c                   do        10            Fas
331500080724     c                   if        d1Fasi(fas) <> 'STP'
331600080724     c                   clear                   d1Fasi(fas)
331700080724     c                   end
331800080724     c                   endDo
331900080623     C                   ENDIF
332000080623      *
332100070607      *? Controllo se richiesto il trasferimento da pgm richiamato    ?
332200070607      *?  da assegnazione manuale e se devo impostare la distinta     ?
332300070607      *?  su cui trasferire non voglio far vedere la distinta dalla   ?
332400070607      *?  quale sto trasferendo le spedizioni.                        ?
332500080715     c                   if        (fdgPGM ='ASSMANUALE' or fdgPGM = 'MONITOR')
332600080701     c                             and pgm_da_call =  'S' and fdg31_opz = '1'
332700070606     c                             and fdgNF1 = dstNFV
332800070606     C                   SETOFF                                       96
332900070606     c                   end
333000070606      *
333100070607      *?  Se sto Trasferendo o Togliendo  non voglio caricare le      ?
333200070607      *?  Distinte vuote  (senza spedizioni - non mi servono)         ?
333300070607      *?  [nel trasferimento solo quando scelgo da dove trasferire]   ?
333400080715     c                   if        (fdgPGM ='ASSMANUALE' or fdgPGM ='MONITOR')
333500080606     c                               and pgm_da_call =  'S'
333600070607     c                   if        fdg31_opz = '2' and DSTnbla =0  or
333700070607     c                             fdg31_opz = '3' and DSTnbla =0 and fdgNF1 =0
333800070621      *  ?  Asteriscato in quanto si vuole continuare a vedere comunque tutto...  ?
333900070621     C***********        SETOFF                                       96
334000070607     c                   end
334100070607     c                   end
334200080613      *
334300080606      *  ?  se Richiamato da MONITOR deve caricare la sola Distinta...  ?
334400080606      *? Passata e nascosta nel SAVfdgNFV.    ?
334500080606     c                   if        fdgPGM = 'MONITOR'  and
334600080606     c                             pgm_da_call =  'S' and
334700080606     c                             SAVfdgNFV <> dstNFV
334800110309      *********
334900110309      *********  avendo eliminato sul chiamante FIDG30R occorre filtrare qui
335000110309      *********   con il parametro inerente all'azione da compiere su questo pgm.
335100110309     c                             and fdg31_opz  ='2'
335200110309      ***
335300080606     C                   SETOFF                                       96
335400080606     c                   end
335500080606      *
335600080613      * >>>>>>>>>
335700080613      *?  Controlla se stampate o no oppure tutte.      ?
335800080623      *  si deve controllare se la distinta ha la fase STAMPATA.
335900110308      *    solo se non richiamato dal MONITOR.
336000110308     c                   if        fdgPGM <>'MONITOR'
336100080624     c                   if        d1stp = 'N' and fase_STP ='S' or
336200080703     c                             d1stp = 'S' and fase_STP =' '
336300080613     C                   SETOFF                                       96
336400080613     c                   end
336500110308     c                   end
336600080624      * >>>>>>>>>
336700080716      * >>>>>>>>>   Filtro mediante le FASI
336800080716      * >>>>>>>>>
336900080701      *
337000080724      * Altrimenti x le altre Fasi in schiera:
337100080624     c                   move      'N'           trovata_fase      1
337200080624      *   e se almeno 1 fase è stata selezionata:
337300080624     c                   if        d1fasiDS <> *blank
337400080613      *
337500080611     c                   do        10            Fs                3 0
337600080623     c                   if        d1fasi(fs) <> *blank
337700080613     c                   z-add     1             dsf               3 0
337800080623     c     d1fasi(fs)    lookup    Fasi_dsf(dsf)                          21
337900080613     c                   if        *in21
338000080611     c                   move      'S'           trovata_fase
338100080611     c                   leave
338200080611     c                   end
338300080613     c                   end
338400080611     c                   enddo
338500080611      * include o esclude
338600080613     c                   if        (D1IEFA = 'I' and trovata_fase = 'N') or
338700080613     c                             (D1IEFA = 'E' and trovata_fase = 'S')
338800080611     C                   SETOFF                                       96
338900080611     c                   end
339000080611      *
339100080716     c                   end
339200080701      *
339300070312     C                   ENDSR
339400070301      *---------------------------------------------------------------*
339500070321      *?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL      ?
339600070301      *---------------------------------------------------------------*
339700070228     C     CLCPAG        BEGSR
339800940224     C* Input :
339900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
340000940224     C*          cursore
340100070301     C* - Wpagine= numero rcd per pagina sfl
340200940224     C* Output :
340300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
340400940224     C*
340500070301     C     WSFL          DIV       Wpagine       PAGINE            4 0
340600940224     C                   MVR                     RESTO             3 0
340700940224     C     RESTO         IFGT      0
340800940224     C                   ADD       1             PAGINE
340900940224     C                   ENDIF
341000070301     C     PAGINE        MULT      Wpagine       WPAG
341100940224     C*
341200940224     C                   ENDSR
341300070302     C/EJECT
341400070302     C************************************************************
341500070321      *?  INIZIALIZZA indicatori di errore    ?
341600070302     C************************************************************
341700070308     C     INZ_indERR    BEGSR
341800070308      *
341900070308     C                   move      *ALL'0'       IN5190           40
342000070308     C                   moveA     IN5190        *IN(51)
342100070308      *
342200070308     C                   ENDSR
342300070308     C/EJECT
342400070301      *---------------------------------------------------------------*
342500070321      *?  GESTIONE F03 x uscire LR     ?
342600070301      *---------------------------------------------------------------*
342700070227     C     F03_exit      BEGSR
342800070227     C* fine programma
342900940309     C                   MOVE      *ON           $FINE
343000070302      *
343100940309     C                   ENDSR
343200070312     C/EJECT
343300070312      *---------------------------------------------------------------*
343400070321      *? GESTIONE F11 alterna l'ordinamento x numero o Autotrasportatore  ?
343500070312      *---------------------------------------------------------------*
343600070322     C     F11_ordina1   BEGSR
343700070320     C*
343800160505     c                   setoff                                       22
343900160506     c                   eval      *in38 = sav_in38
344000070320     C* imposto pos. del cursore
344100070403     C                   if        WrkSort = Autista or
344200160512     C                             WrkSort = Numero and *in13 = *oN
344300160512     c                             or Prima_volta ='S'
344400160512     c                   eval      Prima_volta = 'N'
344500070403     c                   eval      cur_su_sfl = 'N'
344600070320     C                   eval           H1RIGA = 3
344700070528     C                   eval           H1COLO = 17
344800070402     c                   end
344900070402     C* imposto pos. del cursore
345000160512     C                   if        WrkSort = Numero and *in13 = *oFF
345100160505     c                             or WrkSort = Turno
345200070403     c                   eval      cur_su_sfl = 'S'
345300070402     C                   eval           H1RIGA = 7
345400070402     C                   eval           H1COLO = 2
345500070402     c                   end
345600160505      *
345700071026     C                   z-add     h1riga        Su_Quale_rec
345800071026     c                   z-add     1             Che_NRec_sfl
345900070313      *
346000070313      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
346100070313      * presente nel subfile -- Selected -- questo è aggiunto al record.
346200070313      * Il campo è usato per selezionare i records dando un ordine a quelli
346300070313      * selezionati davanti ai non selezionati.
346400070313     C                   CLEAR                   QLGSCB
346500070313     C                   CLEAR                   QLGSCB00
346600070313
346700070313     c                   eval      wsfl = 1
346800070313
346900070313     ** Gestione della scelta ordinamento.
347000070313     C                   SELECT
347100070313      *
347200070402      *?  Ordinamento per Autista.  ?
347300070313     C                   WHEN      WrkSort = Autista
347400070322     c                   clear                   c1cnfv
347500160505     c                   seton                                        22
347600070403     C*
347700070313     ** 1 campo chiave.
347800070313     C                   EVAL      QLGNBRK    = 1
347900070313     ** La descrizione è
348000070402     ** a posizione 1, xx byte, char, ascending.
348100070313     C                   EVAL      QLGSP      = 1
348200070322     C                   EVAL      QLGSS      = %SIZE(S1DAUT)
348300070313     C                   EVAL      QLGDT      = Carattere
348400070313     C                   EVAL      QLGSO      = Ascendente
348500070313     C                   EVAL      QLGKL(1)   = QLGSKL
348600070313
348700070313     ** Al prossimo F11 ordino per Numero Distinta
348800070313     C                   EVAL      WrkSort = Numero
348900070313
349000160601     C                   EVAL      *IN11 = *ON
349100160505     C                   EVAL      *IN10 = *OFF
349200070313     C                   EVAL      sav_in11 = *IN11
349300160504     C                   EVAL      sav_in10 = *IN10
349400070313
349500070402      *?  Ordinamento per Numero Distinta   ?
349600070313     C                   WHEN      WrkSort = Numero
349700070322     c                   clear                   c1daut
349800070403      *
349900070510     ** 3 campi chiave.  x DATA e NUMERO
350000070510     C                   EVAL      QLGNBRK    = 3
350100070313     ** Il gruppo è
350200070403     ** a posizione xx, 6 byte, char, ascending
350300070417     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
350400070417     C                                     %SIZE(S1CDFV)
350500070417     C                   EVAL      QLGSS    = %SIZE(H1CDFV)
350600070403     C                   EVAL      QLGDT    = Numerico
350700070403     C                   EVAL      QLGSO    = Ascendente
350800070403     C                   EVAL      QLGKL(1) = QLGSKL
350900070313
351000070510     ** Priorità  è
351100070510     ** a posizione xx, 6 byte, char, ascending
351200070510     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
351300070510     C                                     %SIZE(S1CDFV) + %SIZE(H1CDFV)
351400070510     C                   EVAL      QLGSS    = %SIZE(H1PRGG)
351500070510     C                   EVAL      QLGDT    = Numerico
351600070510     C                   EVAL      QLGSO    = Ascendente
351700070510     C                   EVAL      QLGKL(2) = QLGSKL
351800070510
351900070313     ** La descrizione è
352000070403     ** a posizione 1, xx byte, char, ascending
352100070403     C                   EVAL      QLGSP      = 1 + %SIZE(S1DAUT)
352200070403     C                   EVAL      QLGSS      = %SIZE(S1CNFV)
352300070403     C                   EVAL      QLGDT      = Numerico
352400070313     C                   EVAL      QLGSO      = Ascendente
352500070510     C                   EVAL      QLGKL(3)   = QLGSKL
352600070313
352700160601     C                   EVAL      *IN11 = *OFF
352800160505     C   38              EVAL      *IN10 = *ON
352900070313     C                   EVAL      sav_in11 = *IN11
353000160504     C                   EVAL      sav_in10 = *IN10
353100070313
353200070313     ** Al prossimo F11 ordino per descrizione.
353300070313     C                   EVAL      WrkSort = Autista
353400160505     C   38              EVAL      WrkSort = Turno
353500070403      *
353600070403      *   Se il range di date non è dello stesso giorno
353700070417     c******             if        *in13 = *off
353800070417     C******             MOVE      *On           $INZS1
353900070417     c******             goto      end_ordina1
354000070417     c******             end
354100160505
354200160505      *?  Ordinamento per Turno   ?
354300160505     C                   WHEN      WrkSort = Turno
354400160505     c                   clear                   c1daut
354500160505      *
354600160505     ** 4 campi chiave.  Turno  e NUMERO
354700160505     C                   EVAL      QLGNBRK    = 4
354800160505     ** Il turno  è
354900160505     ** a posizione xx, 6 byte, char, ascending
355000160505     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
355100160505     C                                     %SIZE(S1CDFV) + %SIZE(H1CDFV) +
355200160505     c                                     %SIZE(H1PRGG)
355300160505     C                   EVAL      QLGSS    = %SIZE(s1cTUR)
355400160505     C                   EVAL      QLGDT    = Numerico
355500160505     C                   EVAL      QLGSO    = Ascendente
355600160505     C                   EVAL      QLGKL(1) = QLGSKL
355700160505
355800160505     ** Il gruppo è
355900160505     ** a posizione xx, 6 byte, char, ascending
356000160505     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
356100160505     C                                     %SIZE(S1CDFV)
356200160505     C                   EVAL      QLGSS    = %SIZE(H1CDFV)
356300160505     C                   EVAL      QLGDT    = Numerico
356400160505     C                   EVAL      QLGSO    = Ascendente
356500160505     C                   EVAL      QLGKL(2) = QLGSKL
356600160505
356700160505     ** Priorità  è
356800160505     ** a posizione xx, 6 byte, char, ascending
356900160505     C                   EVAL      QLGSP   = 1 + %SIZE(S1DAUT) + %SIZE(S1CNFV) +
357000160505     C                                     %SIZE(S1CDFV) + %SIZE(H1CDFV)
357100160505     C                   EVAL      QLGSS    = %SIZE(H1PRGG)
357200160505     C                   EVAL      QLGDT    = Numerico
357300160505     C                   EVAL      QLGSO    = Ascendente
357400160505     C                   EVAL      QLGKL(3) = QLGSKL
357500160505
357600160505     ** La descrizione è
357700160505     ** a posizione 1, xx byte, char, ascending
357800160505     C                   EVAL      QLGSP      = 1 + %SIZE(S1DAUT)
357900160505     C                   EVAL      QLGSS      = %SIZE(S1CNFV)
358000160505     C                   EVAL      QLGDT      = Numerico
358100160505     C                   EVAL      QLGSO      = Ascendente
358200160505     C                   EVAL      QLGKL(4)   = QLGSKL
358300160505
358400160601     C                   EVAL      *IN11 = *OFF
358500160505     C                   EVAL      *IN10 = *OFF
358600160505     C                   EVAL      sav_in11 = *IN11
358700160505     C                   EVAL      sav_in10 = *IN10
358800160505
358900160505     ** Al prossimo F11 ordino per descrizione.
359000160505     C                   EVAL      WrkSort = Autista
359100160505      *
359200070313
359300070313     C                   ENDSL
359400070313
359500070313      * Load other sort parameters.
359600070313     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
359700070322     C                   EVAL      QLGRL     = %SIZE(SflRcd1) - 1
359800070313     C                   EVAL      QLGRT     = 8
359900070313     C                   EVAL      QLGOKL    = 80
360000070313     C                   EVAL      QLGLKE    = 16
360100070313     C                   EVAL      QLGLSS    = 290
360200070313
360300070313      * Initialize Sort I/O API fields.
360400070313     C                   EVAL      QLGRL00  = QLGRL
360500070313     C                   EVAL      QLGRC00  = 1
360600070313     C                   CLEAR                   QUSEI
360700070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
360800070313
360900070313      * First step - Initialize the sort routine.
361000070313     C                   CALL      'QLGSORT'
361100070313     C                   PARM                    QLGSCB
361200070313     C                   PARM                    NotUsed
361300070313     C                   PARM                    NotUsed
361400070313     C                   PARM                    SizeList
361500070313     C                   PARM                    ReturnSize
361600070313     C                   PARM                    QUSEC
361700070313
361800070313      * Next step - Write records to I/O routine.
361900070313     C                   EVAL      QLGRT00 = Put
362000070313
362100070322     C                   DO        RrnLast       S1nrr
362200070313
362300070322     C     S1nrr         CHAIN     DG31SF1
362400070313
362500070313     ** Solo le righe con Selected = 'Y' sono riordinate,
362600070313     ** quindi per fare un ordinamento di tutte le righe
362700070313     ** metto 'Y' sempre.
362800070322     C                   EVAL      Selected1 = 'Y'
362900070313
363000070313     C                   CLEAR                   QUSEI
363100070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
363200070322      *
363300070313     C                   CALL      'QLGSRTIO'
363400070313     C                   PARM                    QLGSCB00
363500070322     C                   PARM                    SflRcd1
363600070313     C                   PARM                    NotUsed
363700070313     C                   PARM                    SizeList
363800070313     C                   PARM                    NotUsed
363900070313     C                   PARM                    QUSEC
364000070313
364100070313     C                   ENDDO
364200070313
364300070313      * Next step - Signal end of input, clear subfile for reload.
364400070313     C                   EVAL      QLGRT00 = EndPut
364500070313     C                   CLEAR                   QUSEI
364600070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
364700070313
364800070313     C                   CALL      'QLGSRTIO'
364900070313     C                   PARM                    QLGSCB00
365000070322     C                   PARM                    SflRcd1
365100070313     C                   PARM                    NotUsed
365200070313     C                   PARM                    SizeList
365300070313     C                   PARM                    NotUsed
365400070313     C                   PARM                    QUSEC
365500070313
365600070313     C* pulizia SFL
365700070313     C                   SETOFF                                         3031
365800070322     C                   WRITE     DG31CT1
365900070313     C                   SETON                                          31
366000070313
366100070313      * Final step - Write the records back to the subfile.
366200070313     C                   EVAL      QLGRT00 = Get
366300070313
366400070322     C                   DO        RrnLast       s1nrr
366500070313     C                   CLEAR                   QUSEI
366600070313     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
366700070313     C                   CALL      'QLGSRTIO'
366800070313     C                   PARM                    QLGSCB00
366900070313     C                   PARM                    NotUsed
367000070322     C                   PARM                    SflRcd1
367100070313     C                   PARM                    QLGRL00
367200070313     C                   PARM                    NotUsed
367300070313     C                   PARM                    QUSEC
367400070510      *
367500080208      * Ripristina gli indicatori x i DSPATR
367600070510     c                   setoff                                       40
367700070510if  4c                   if        h1prgg = 1
367800070510     c                   seton                                        40
367900070510     c                   end
368000080208      * x fase di test
368100080219     c                   eval      *in15 = sav_in15
368200080208     c                   setoff                                       14
368300080208     c                   if        h1cDFV >= DateU8
368400080208     c                   if        §ogPDAorm ='S' and (S1CTSTPDA ='o') or
368500080208     c                             §ogPDAorm ='S' and (S1CTSTPDA ='e') or
368600080208     c                             §ogPDAcon ='S' and (S1CTSTPDA ='e') or
368700080208     c                             §ogPDAcon ='S' and (S1CTSTPDA ='c')
368800080208     c                   seton                                        14
368900080208     c                   endIf
369000080208     c                   endIf
369100080222     c                   eval      h1in14 = *in14
369200070510      *
369300070322     C                   WRITE     DG31SF1
369400070313     C                   ENDDO
369500070320      *
369600070403     C     end_ordina1   ENDSR
369700070322     C/EJECT
369800070322      *---------------------------------------------------------------*
369900070322      *? GESTIONE F11 alterna l'ordinamento x numero o Autotrasportatore  ?
370000070322      *---------------------------------------------------------------*
370100070322     C     F11_ordina2   BEGSR
370200070322     C*
370300070403     C* imposto pos. del cursore
370400070403     C                   if        WrkSort = Autista or
370500070403     C                             WrkSort = Numero and *in13 = *on
370600070403     c                   eval      cur_su_sfl = 'N'
370700070403     C                   eval           H1RIGA = 3
370800070528     C                   eval           H1COLO = 17
370900070403     c                   end
371000070403     C* imposto pos. del cursore
371100070403     C                   if        WrkSort = Numero and *in13 = *off
371200080715     c                             or Prima_volta ='S'
371300160505     c                             or WrkSort = Turno
371400070607     c                   eval      Prima_volta = 'N'
371500070403     c                   eval      cur_su_sfl = 'S'
371600070403     C                   eval           H1RIGA = 7
371700070403     C                   eval           H1COLO = 2
371800070403     c                   end
371900071026     C                   z-add     h1riga        Su_Quale_rec
372000071026     c                   z-add     1             Che_NRec_sfl
372100070322      *
372200070322      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
372300070322      * presente nel subfile -- Selected -- questo è aggiunto al record.
372400070322      * Il campo è usato per selezionare i records dando un ordine a quelli
372500070322      * selezionati davanti ai non selezionati.
372600070322     C                   CLEAR                   QLGSCB
372700070322     C                   CLEAR                   QLGSCB00
372800070322
372900070322     c                   eval      wsfl = 1
373000070322
373100070322     ** Gestione della scelta ordinamento.
373200070322     C                   SELECT
373300070322      *
373400070403      *?  Ordinamento per Autista.  ?
373500070322     C                   WHEN      WrkSort = Autista
373600070322     c                   clear                   c2cnfv
373700070403      *
373800070322     ** 1 campo chiave.
373900070322     C                   EVAL      QLGNBRK    = 1
374000070322     ** La descrizione è
374100070322     ** a posizione 1, 28 byte, char, ascending.
374200070322     C                   EVAL      QLGSP      = 1
374300070322     C                   EVAL      QLGSS      = %SIZE(S2DAUT)
374400070322     C                   EVAL      QLGDT      = Carattere
374500070322     C                   EVAL      QLGSO      = Ascendente
374600070322     C                   EVAL      QLGKL(1)   = QLGSKL
374700070322
374800070322     ** Al prossimo F11 ordino per Numero Distinta
374900070322     C                   EVAL      WrkSort = Numero
375000070322
375100160601     C                   EVAL      *IN11 = *ON
375200070322     C                   EVAL      sav_in11 = *IN11
375300160504     C                   EVAL      sav_in10 = *IN10
375400070322
375500070403      *?  Ordinamento per Numero Distinta   ?
375600070322     C                   WHEN      WrkSort = Numero
375700071026      ********
375800071026     c**********         clear                   c2daut
375900071026      ********
376000070510     ** 3 campi chiave.
376100070510     C                   EVAL      QLGNBRK    = 3
376200070322     ** Il gruppo è
376300070322     ** a posizione 29, 6 byte, char, ascending
376400070417     C                   EVAL      QLGSP   = 1 + %SIZE(S2DAUT) + %SIZE(S2CNFV) +
376500070417     C                                     %SIZE(S2CDFV)
376600070417     C                   EVAL      QLGSS    = %SIZE(h2CDFV)
376700070403     C                   EVAL      QLGDT    = Numerico
376800070403     C                   EVAL      QLGSO    = Ascendente
376900070403     C                   EVAL      QLGKL(1) = QLGSKL
377000070510     ** La priorità
377100070510     ** a posizione xx, x byte, char, ascending
377200070510     C                   EVAL      QLGSP   = 1 + %SIZE(S2DAUT) + %SIZE(S2CNFV) +
377300070510     C                                     %SIZE(S2CDFV) + %SIZE(h2CDFV)
377400070510     C                   EVAL      QLGSS    = %SIZE(h2PRGG)
377500070510     C                   EVAL      QLGDT    = Numerico
377600070510     C                   EVAL      QLGSO    = Ascendente
377700070510     C                   EVAL      QLGKL(2) = QLGSKL
377800070322     ** La descrizione è
377900070322     ** a posizione 5, 28 byte, char, ascending
378000070403     C                   EVAL      QLGSP      = 1 + %SIZE(S2DAUT)
378100070403     C                   EVAL      QLGSS      = %SIZE(S2CNFV)
378200070403     C                   EVAL      QLGDT      = Numerico
378300070322     C                   EVAL      QLGSO      = Ascendente
378400070510     C                   EVAL      QLGKL(3)   = QLGSKL
378500070322
378600160601     C                   EVAL      *IN11 = *OFF
378700070322     C                   EVAL      sav_in11 = *IN11
378800160504     C                   EVAL      sav_in10 = *IN10
378900070322
379000070322     ** Al prossimo F11 ordino per descrizione.
379100070322     C                   EVAL      WrkSort = Autista
379200070403      *
379300070403      *   Se il range di date non è dello stesso giorno
379400070417     c*******            if        *in13 = *off
379500070417     C*******            MOVE      *On           $INZS2
379600070417     c*******            goto      end_ordina2
379700070417     c*******            end
379800070322
379900070322     C                   ENDSL
380000070322
380100070322      * Load other sort parameters.
380200070322     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
380300070322     C                   EVAL      QLGRL     = %SIZE(SflRcd2) - 1
380400070322     C                   EVAL      QLGRT     = 8
380500070322     C                   EVAL      QLGOKL    = 80
380600070322     C                   EVAL      QLGLKE    = 16
380700070322     C                   EVAL      QLGLSS    = 290
380800070322
380900070322      * Initialize Sort I/O API fields.
381000070322     C                   EVAL      QLGRL00  = QLGRL
381100070322     C                   EVAL      QLGRC00  = 1
381200070322     C                   CLEAR                   QUSEI
381300070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
381400070322
381500070322      * First step - Initialize the sort routine.
381600070322     C                   CALL      'QLGSORT'
381700070322     C                   PARM                    QLGSCB
381800070322     C                   PARM                    NotUsed
381900070322     C                   PARM                    NotUsed
382000070322     C                   PARM                    SizeList
382100070322     C                   PARM                    ReturnSize
382200070322     C                   PARM                    QUSEC
382300070322
382400070322      * Next step - Write records to I/O routine.
382500070322     C                   EVAL      QLGRT00 = Put
382600070322
382700070322     C                   DO        RrnLast       S2nrr
382800070322
382900070322     C     S2nrr         CHAIN     DG31SF2
383000070322
383100070322     ** Solo le righe con Selected = 'Y' sono riordinate,
383200070322     ** quindi per fare un ordinamento di tutte le righe
383300070322     ** metto 'Y' sempre.
383400070322     C                   EVAL      Selected2 = 'Y'
383500070322
383600070322     C                   CLEAR                   QUSEI
383700070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
383800070322      *
383900070322     C                   CALL      'QLGSRTIO'
384000070322     C                   PARM                    QLGSCB00
384100070322     C                   PARM                    SflRcd2
384200070322     C                   PARM                    NotUsed
384300070322     C                   PARM                    SizeList
384400070322     C                   PARM                    NotUsed
384500070322     C                   PARM                    QUSEC
384600070322
384700070322     C                   ENDDO
384800070322
384900070322      * Next step - Signal end of input, clear subfile for reload.
385000070322     C                   EVAL      QLGRT00 = EndPut
385100070322     C                   CLEAR                   QUSEI
385200070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
385300070322
385400070322     C                   CALL      'QLGSRTIO'
385500070322     C                   PARM                    QLGSCB00
385600070322     C                   PARM                    SflRcd2
385700070322     C                   PARM                    NotUsed
385800070322     C                   PARM                    SizeList
385900070322     C                   PARM                    NotUsed
386000070322     C                   PARM                    QUSEC
386100070322
386200070322     C* pulizia SFL
386300070322     C                   SETOFF                                         3031
386400070322     C                   WRITE     DG31CT2
386500070322     C                   SETON                                          31
386600070322
386700070322      * Final step - Write the records back to the subfile.
386800070322     C                   EVAL      QLGRT00 = Get
386900070322
387000070322     C                   DO        RrnLast       s2nrr
387100070322     C                   CLEAR                   QUSEI
387200070322     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
387300070322     C                   CALL      'QLGSRTIO'
387400070322     C                   PARM                    QLGSCB00
387500070322     C                   PARM                    NotUsed
387600070322     C                   PARM                    SflRcd2
387700070322     C                   PARM                    QLGRL00
387800070322     C                   PARM                    NotUsed
387900070322     C                   PARM                    QUSEC
388000070510      *
388100080208      * Ripristina gli indicatori x i DSPATR
388200070510     c                   setoff                                       40
388300070510if  4c                   if        h2prgg = 1
388400070510     c                   seton                                        40
388500070510     c                   end
388600071029      *
388700071029     c                   setoff                                       42
388800071029if  4c                   if        h2recpos = 'X'
388900071029     c                   z-add     s2nrr         nrec_riposiz
389000071029     C                   z-add     riri          Su_Quale_rec
389100071029     c                   end
389200080208      * x fase di test
389300080219     c                   eval      *in15 = sav_in15
389400080208     c                   setoff                                       14
389500080208     c                   if        h2cDFV >= DateU8
389600080208     c                   if        §ogPDAorm ='S' and (S2CTSTPDA ='o') or
389700080208     c                             §ogPDAorm ='S' and (S2CTSTPDA ='e') or
389800080208     c                             §ogPDAcon ='S' and (S2CTSTPDA ='e') or
389900080208     c                             §ogPDAcon ='S' and (S2CTSTPDA ='c')
390000080208     c                   seton                                        14
390100080208     c                   endIf
390200080208     c                   endIf
390300080222     c                   eval      h2in14 = *in14
390400070510      *
390500070322     C                   WRITE     DG31SF2
390600070322     C                   ENDDO
390700070322      *
390800070403     C     End_ordina2   ENDSR
390900070313     C/EJECT
391000070313      *---------------------------------------------------------------*
391100070321      *? GESTIONE F21 interrogazione spunte    ?
391200070313      *---------------------------------------------------------------*
391300070313     C     F21_interroga BEGSR
391400070313     C*
391500070312     C                   CLEAR                   PARA04
391600070314     C                   Z-ADD     wNFV          P04NFV
391700070312     C                   Z-ADD     99            P04NRS
391800070312     C                   Z-ADD     99            P04NPS
391900070314     C                   MOVEL     wnpg          P04NPG
392000070312     C                   MOVEL     '1'           P04FLG
392100070312     C                   MOVEL     'FIDG04R'     P04PGM
392200070314     C                   Z-ADD     wfgs          P04FGS
392300070313     c                   eval      kpjbus = kpjbu
392400070312     C                   MOVEL     PARA04        KPJBU
392500070312     C                   CALL      'FNLR04R'
392600070312     C                   PARM                    KPJBA
392700070313     c                   eval      kpjbu = kpjbus
392800070312      *
392900070312     C                   ENDSR
393000070301     C/EJECT
393100070321     C************************************************************
393200070321      *?  Emissione video di Dettaglio           ?
393300070312     C************************************************************
393400070312     C     GESD2         BEGSR
393500070315     C*
393600070312     C* inizializzazione videata
393700070312     C     $INZD2        IFEQ      *ON
393800070312     C                   EXSR      INZD2
393900070312     C                   MOVE      *OFF          $INZD2
394000070312     C                   ENDIF
394100070312     C*
394200070312     c     Video_2       tag
394300070312     C*  Emissione Video
394400070312     c                   setoff                                       99
394500160506     c                   eval      *in38 = sav_in38
394600070312     c                   if        $VISD2 = 'S'
394700070320      *-----
394800070312      * ?            *-----------------------*
394900070312     C                   EXFMT     DG31D02
395000070312      * ?            *-----------------------*
395100070320      *-----
395200070320     c                   else
395300070320      *-----
395400070320      *  Richiama il programma di visualizzazione del dettaglio distinta
395500070402     c                   exsr      Detta_GIRI
395600070406
395700070406     c                   end
395800070320     C*
395900070320     C* imposto pos. del cursore
396000070320     C                   eval           H1RIGA = CURR
396100070320     C                   eval           H1COLO = CURC
396200070312     C* Selezioni
3963000703121    C                   SELECT
396400070402      *-----
396500070402     C                   WHEN      $TASTO   =   F02
396600070402     c                   exsr      Detta_GIRI
396700070402     C                   if        comando = 'KC'
396800070402     C                   clear                   comando
396900080821     c                   move      *on           $ESCI
397000080606     c                   move      '03'          fdgtfu
397100070402     C                   EXSR      F03_exit
397200070402     c                   goto      end_d02
397300070402     c                   end
397400070402     C*--------
397500070312     C* F3=Fine
397600070320     C                   WHEN      $TASTO   =   F03  or comando = 'KC'
397700070330     C                   clear                   comando
397800080821     c                   move      *on           $ESCI
397900080606     c                   move      '03'          fdgtfu
398000070312     C                   EXSR      F03_exit
398100070312     c                   goto      end_d02
398200070312     C* F12=Ritorno
398300070320     C                   WHEN      $TASTO   =   F12  or comando = 'KL'
398400070330     C                   clear                   comando
398500070329     c                   if        sav_$GEST <> *blank
398600070313     C                   movel     Sav_$GEST     $gest
398700070329     c                   else
398800080606     c                   move      '12'          fdgtfu
398900080821     c                   move      *on           $ESCI
399000070329     C                   EXSR      F03_exit
399100070329     c                   end
399200070312     c                   goto      end_d02
399300070312      *
399400070312     C* F21=Spunte del foglio richiesto
399500070312     C                   WHEN      $TASTO   =   F21
399600070312     C                   EXSR      F21_interroga
399700070312     c                   seton                                        99
399800070404     C* F6=Conferma Scelta
399900070404     C                   WHEN      $TASTO   =   F06
400000070404     c                   z-add     d2CNFV        FDGNFV
400100070404     c                   move      d1cnpg        FDGNPG
400200080821     c                   move      *on           $ESCI
400300080606     c                   move      '06'          fdgtfu
400400070404     C                   EXSR      F03_exit
400500070404     c                   goto      end_d02
400600080820      *
400700080701     C* F8=Visualizza Fasi
400800080701     C                   WHEN      $TASTO   =   F08
400900080701     c                   write     FRCDTA
401000080701      *
401100080701     c                   clear                   fidg39dS
401200080701     c                   move      d1CNPG        fdg39NPG
401300080701     c                   z-add     d2CNFV        fdg39NFV
401400080701     c                   z-add     d2cfgs        fdg39FGS
401500080701     c                   move      'N'           fdg39noREC
401600080701     c                   clear                   fdg39VIDno
401700080701     c                   eval      kpjbus = kpjbu
401800080701     c                   movel(p)  fidg39ds      KPJBU
401900080701     c                   call      'FIDG39R'
402000080701     c                   parm                    kpjba
402100080701     c                   eval      kpjbu = kpjbus
402200080820      *
402300080820     C* F9=Dettaglio Distinta
402400080820     C                   WHEN      $TASTO   =   F09
402500080820     c                   write     FRCDTA
402600080820      *
402700080820     c                   clear                   fidg311dS
402800080820     c                   z-add     d2cfgs        f31FGS
402900080827     c                   z-add     wnpg          f31npg
403000080820     c                   z-add     d2CNFV        f31NumDis
403100080820     c                   z-add     h2CDFV        f31DatDis
403200080820     c                   move      d1Dati        f31PriDati
403300081111     c**********         if        D1CPDA <> 'C' and D1CPDA <> 'O'
403400081111     c**********         move      'E'           f31DatiVis
403500081111     c**********         else
403600080820     c                   move      D1CPDA        f31DatiVis
403700081111     c**********         end
403800080820     c                   eval      kpjbus = kpjbu
403900080820     c                   movel(p)  fidg311ds     KPJBU
404000080820     c                   call      'FIDG311R'
404100080820     c                   parm                    kpjba
404200080821     c                   eval      fidg311ds =   KPJBU
404300080821     c                   if        F31Tfunz = '03'
404400080821     c                   move      *on           $ESCI
404500080821     C                   EXSR      F03_exit
404600080821     c                   end
404700080820     c                   eval      kpjbu = kpjbus
404800080820      *
404900070312      *
4050000703121O   C                   OTHER
405100070312     C* CONTROLLO DATI
405200070312     C                   EXSR      CTRD2
4053000703121-   C                   ENDSL
405400070320      *-----
405500070312     C*
405600070312     C* Per errori o decodifiche
405700070320     c                   if        $VISD2 = 'S'
405800070312     c   99              goto      Video_2
405900070320     c                   end
406000070312      *
406100070312     c     end_d02       tag
406200070312      *
406300070312     C                   ENDSR
406400070312     C/EJECT
406500070312     C************************************************************
406600070402      *?  Visualizza anche il dettaglio GIRI     ?
406700070312     C************************************************************
406800070402     C     Detta_GIRI    BEGSR
406900070320     C*
407000070402      *  Richiama il programma di visualizzazione del dettaglio distinta
407100070402     c                   clear                   fidg02ds
407200070402     C                   MOVEL     wfgs          D02FIL
407300070402     C                   MOVEL     'A'           D02TIPA
407400070402     C                   MOVEL     dstpdr        D02PDR
407500070402     C                   MOVEL     '05'          D02CMD
407600070402     C                   MOVEL     'N'           D02STRCMT
407700070402     C                   MOVEL     'N'           D02OPNCMT
407800070402     C                   MOVEL     'N'           D02COM
407900070402     C                   MOVEL     'FIDST00F'    D02FILE
408000070402     C                   MOVEL     dstDFV        D02DFV
408100070402     C                   MOVEL     dstNFV        D02NFV
408200070402     C                   MOVEL     '4'           D02NPG
408300070402     C                   MOVEL     dstFPP        D02FPP
408400070402     C                   MOVEL     dstTPM        D02TPM
408500070904     C                   MOVEL     d1dati        D02TP
408600070402     c                   eval      kpjbus = kpjbu
408700070402     C                   eval      KPJBU  = fidg02ds
408800070402     c                   call      'FIDG02C'
408900070402     c                   parm                    kpjba
409000070402     C                   MOVEL     KPJBU         fidg02ds
409100070905      *
409200070905      *  secondo giro per visualizzare le informazioni successive
409300070905      *   solo se NON F3
409400070905     C                   if        D02CMDE <> 'KC'
409500070905     C                   if        d1dati = 'C'
409600070905     C                   MOVEL     'R'           D02TP
409700070905     c                   else
409800070905     C                   MOVEL     'C'           D02TP
409900070905     c                   end
410000070905      *
410100070926     C                   if        test_Paral_ORM = 'S' and D02TP = 'R' or
410200070926     C                             test_Parallelo = 'S' and D02TP = 'C'
410300070905     c                   eval      kpjbus = kpjbu
410400070905     C                   eval      KPJBU  = fidg02ds
410500070905     c                   call      'FIDG02C'
410600070905     c                   parm                    kpjba
410700070905     C                   MOVEL     KPJBU         fidg02ds
410800070926     c                   endIf
410900070905     c                   end
411000070905      *
411100070402     c                   eval      kpjbu = kpjbus
411200070402     C                   MOVEL     D02CMDE       comando           2
411300070402     C                   MOVEL     D02ERR        errore            1
411400070402      *
411500070402     C                   ENDSR
411600070402     C/EJECT
411700070402     C************************************************************
411800070402      *?  INIZIALIZZAZIONE Vidoe di Dettaglio    ?
411900070402     C************************************************************
412000070402     C     INZD2         BEGSR
412100070402     C*
412200070312     C* Pulizia campi e indicatori
412300070312     c                   exsr      INZ_indERR
412400070312     C*
412500080701     C* Abilita il tasto F8 x visualizzare le FASI presenti su FIDSF
412600080701     c                   eval      *in18 = d2fasi
412700080701     c                   eval      sav_in18 = *in18
412800080701     C*
412900070312     C* Pulizia formato record   campi video
413000070312     c                   clear                   DG31D02
413100070403     c                   if        se_EDP = 'S'
413200070830     C***                SETON                                          15
413300070403     c                   end
413400070314     C*
413500070314     C* Se richiamato attivo 04
413600070314     c                   if        pgm_da_call = 'S'
413700070314     C                   SETON                                          04
413800070314     c                   end
413900070312     C*
414000070312     C* Imposta indicatore di selezione
414100080701     c                   move      sav_in18      *in18
414200070313     c                   move      sav_in19      *in19
414300080219     c                   move      sav_in15      *in15
414400080818     c                   move      sav_in08      *in08
414500070312     C* campi titolo
414600070312     C                   MOVEL     TES(1)        d2cTES
414700080715     c                   if        fdgPGM ='ASSMANUALE' or fdgPGM ='MONITOR'
414800070606     C                   MOVEL(p)  TES(2)        d2CTES
414900070606     c                   end
415000070312     C                   MOVEL     dsPGM         d2cPGM
415100070312     C                   MOVEL     RAGUT         d2cDSA
415200070312     C                   MOVEL     knsif         d2nsif
415300070312     C                   MOVEL     knmus         d2nmus
415400070312      *
415500070312     C*  Aggancia la distinta
415600070406     c     kDST9         chain     Fidst09L
415700070406     c                   if        %Found(Fidst09l)
415800080704     C*
415900080704     C* Controlla se ci sono delle Fasi
416000080704     C     Kdsf_01       setll     fiDSF01l
416100080704     c                   if        %Equal
416200080704     c                   eval      *in18 = *On
416300080704     c                   eval      sav_in18 = *in18
416400080704     c                   end
416500070312     C*
416600070312     C                   SETOFF                                       52  53
416700070312     C                   SETOFF                                       06  07
416800070312     C                   z-add     wnfv          d2cnfv
416900070312     C                   z-add     dstfgs        d2cfgs
417000070314     C                   z-add     dstpdr        d2caut
417100070314     c     kapd          chain     fiapd01l
417200070314     c                   if        %found(fiapd01l)
417300070314     c                   eval      d2DAUT = apdrsc
417400070314     c                   end
417500070314      *
417600070312     C* FILIALE GESTIONE
417700070312     C     dstFGS        CHAIN     AZORG                              21
417800070312     C  N21              MOVEL     ORGDES        d2DFGS
417900070312     C* DATA FOGLIO
418000070312     C                   MOVE      dstDFV        G02INV
418100070312     C                   MOVE      *ZEROS        G02DAT
418200070312     C                   MOVEL     '3'           G02ERR
418300070312     C                   CALL      'XSRDA8'
418400070312     C                   PARM                    WLBDAT
418500070312     C                   Z-ADD     G02DAT        d2CDFV
418600080820     C                   Z-ADD     dstDFV        h2CDFV
418700070312     c                   eval      d2cpkla= DSTPKLA
418800070312     c                   eval      d2cvlua= DSTVLUA
418900070312     c                   eval      d2cstpa= DSTSTPA
419000070312     c                   eval      d2cnbla= DSTnblA
419100070313     c                   eval      d2cpkl = DSTPKL
419200070313     c                   eval      d2cvlu = DSTVLU
419300070313     c                   eval      d2cstp = DSTNFTL
419400070313     c                   eval      d2cnbl = DSTnbl
419500070903     c                   eval      d2cPDA = DSTpda
419600080917     c                   clear                   d2ann
419700080917     c                   if        DSTATB ='A'
419800080917     c                   movel     'Annullata'   d2ann
419900080917     c                   end
420000080207      *
420100080207      * per Entrambe prima era segnalato con (S) ora invece si vuole vedere (E)
420200080207      * inoltre se non è attivo il PDA si deve visualizzare Blank invece che "N".
420300080710     c*******            if        d2cPDA = 'N'
420400080710     c*******            eval      d2cPDA = ' '
420500080710     c*******            end
420600080207      *
420700080207      *  nel campo filler c'è il tipo di attivazione
420800080207     c                   eval      DDSTFLR =  DstFLR
420900080208     c                   setoff                                       14
421000080208     c                   if        dstDFV >= DateU8
421100080208     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
421200080208     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
421300080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
421400080208     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
421500080208     c                   seton                                        14
421600080208     c                   endIf
421700080208     c                   endIf
421800080213      *
421900080222     c                   if        dstDFV >= dateu8
422000080213     c                   if        §ogPDAorm ='S' and (§DSTTSTPDA ='O') or
422100080213     c                             §ogPDAorm ='S' and (§DSTTSTPDA ='E') or
422200080213     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='E') or
422300080213     c                             §ogPDAcon ='S' and (§DSTTSTPDA ='C')
422400080213     C                   eval      D2msgv= 'ATT. gestione PDA attivata'
422500080213     c                   endIf
422600080222     c                   endIf
422700080212      *
422800080212     C                   movel     §DSTtstPDA    d2CTSTPDA
422900080207      ***
423000070312     C                   MOVEL     dstFPP        d2CFPP                         *MATT/POM
423100070312     C                   MOVEL     dstPEP        d2CPEP                         *PIKING(S/N)
423200070312     C                   MOVEL     dsttpm        d2ctpm                         *MATT/POM
423300070312     C                   MOVEL     dstfca        d2cfca                         *MATT/POM
423400160503     C                   MOVEL     §dstturno     d2ctur                         Turno
423500160505     c                   if        d2ctpm <> 'M'
423600160505     C                   MOVEL     'Turno'       d2turno
423700160505     c                   endif
423800160504     c                   if        d2ctpm <> 'M' and
423900160504     c                               d2CTUR = *blank
424000160504     c                   eval        d2CTUR = '1'
424100160504     c                   endif
424200070403     c                   eval      d2cnot  = dstnot
424300070312     c                   if        d2ctpm = ' '
424400070312     c                   eval      d2ctpm = 'P'
424500070312     c                   end
424600070403      * BANCALI
424700070403     C     Kqdt          CHAIN     Fiqdt01l
424800070403     c                   if        %found(Fiqdt01l)
424900070403     C                   MOVEL     qdtnbnaf      D2CNBN                         *NR.BANCALI
425000070403     C                   MOVEL     qdtfnbaf      D2CFNBN                        *NR.BANCALI
425100070403     c                   end
425200070312      * VEDO SE ANNULLATO
425300070312     C     dstATB        IFEQ      'A'
425400070312     C                   SETON                                        5206
425500070312     C                   MOVEL     CANNUL        d2DTFV
425600070312     C                   END
425700070312      * VEDO SE CHIUSO
425800070312     C     dstFCF        IFNE      *BLANKS
425900070312     C     dstFCF        IFEQ      'S'
426000070312     C                   SETON                                        53
426100070312     C                   ELSE
426200070312     C                   SETON                                        06
426300070312     C                   MOVEL     CBLOCC        d2DTFV
426400070312     C                   ENDIF
426500070312     C                   ENDIF
426600070312     C*
426700070312     C* 07 ON ESISTONO SPUNTE SUL FOGLIO
426800070312     C     Kdst          SETLL     FnBRV000                               07     ON=TROVATO
426900070312     C*
427000070910     c                   if        d1Dati = 'C'
427100070910     C*
427200070314     C* Carica i GIRI della Distinta
427300070314     c     kdst31        setll     fidst31l
427400070314     c     kdst31        reade     fidst31l
427500070314     c                   z-add     0             x                 3 0
427600070314      **
427700070314     c                   dow       not %Eof(fidst31l)
427800070314      **  Controlla presenza del giro richiesto
427900070314     c                   if        %char(DST3NPG) = d1cNPG and
428000070314     c                             DST3DFV >= DATinw and DST3DFV <= DATfiw
428100070314     c                   add       1             x
428200070315     c                   if        x > 16
428300070314     c                   leave
428400070314     c                   end
428500070314      *
428600070314     c                   movel     dst3cgi       cgi(x)
428700070314      *  controllo validità giro
428800070314     c                   clear                   fidg09ds
428900070314     c                   eval      D09IOP0 = '001'
429000070314     c                   eval      d09ifgs = wfgs
429100071010     c*****              eval      d09itug = 'C'
429200071010     c                   eval      d09itug = d1DATI
429300070314     c                   eval      d09icgi = dst3cgi
429400070314     c                   move      dateu8        d09idat
429500070314     c                   eval      kpjbu = fidg09ds
429600070315     c                   call      'FIDG09R'
429700070314     c                   parm                    kpjba
429800070314     c                   eval      fidg09ds = kpjbu
429900070314     c                   if        d09oerr <> '1' and d09ocgi <> ' '
430000070314     c                   eval      dgi(x) = d09odes
430100070314     c                   end
430200070314      *
430300070314     c                   end
430400070314     c     kdst31        reade     fidst31l
430500070314     c                   endDo
430600070910      **
430700070910     c                   else
430800070910      **
430900070910     C* Carica i GIRI della Distinta
431000070910     c     kdst51        setll     fidst51l
431100070910     c     kdst51        reade     fidst51l
431200070910     c                   z-add     0             x                 3 0
431300070910      **
431400070910     c                   dow       not %Eof(fidst51l)
431500070910      **  Controlla presenza del giro richiesto
431600070910     c                   if        %char(DST5NPG) = d1cNPG and
431700070910     c                             DST5DFV >= DATinw and DST5DFV <= DATfiw
431800070910     c                   add       1             x
431900070910     c                   if        x > 16
432000070910     c                   leave
432100070910     c                   end
432200070910      *
432300070910     c                   movel     dst5cgi       cgi(x)
432400070910      *  controllo validità giro
432500070910     c                   clear                   fidg09ds
432600070910     c                   eval      D09IOP0 = '001'
432700070910     c                   eval      d09ifgs = wfgs
432800071010     c****               eval      d09itug = 'C'
432900071010     c                   eval      d09itug = d1DATI
433000070910     c                   eval      d09icgi = dst5cgi
433100070910     c                   move      dateu8        d09idat
433200070910     c                   eval      kpjbu = fidg09ds
433300070910     c                   call      'FIDG09R'
433400070910     c                   parm                    kpjba
433500070910     c                   eval      fidg09ds = kpjbu
433600070910     c                   if        d09oerr <> '1' and d09ocgi <> ' '
433700070910     c                   eval      dgi(x) = d09odes
433800070910     c                   end
433900070910      *
434000070910     c                   end
434100070910     c     kdst51        reade     fidst51l
434200070910     c                   endDo
434300070910      **
434400070910      *
434500070910     c                   end
434600070910     C*
434700070406     c     d2cgi1        comp      *blank                                 65
434800070314      **
434900070312     c                   end
435000070312     C*
435100070312     C                   ENDSR
435200070312     C************************************************************
435300070321      *?  Controlli Video di Dettaglio                           ?
435400070312     C************************************************************
435500070312     C     CTRD2         BEGSR
435600070312     C*
435700070312     C*  Spegne indicatori di errore
435800070312     c                   exsr      INZ_indERR
435900070312     C*
436000070312     c                   seton                                        99
436100070406     c     d2cgi1        comp      *blank                                 65
436200070312      *
436300070312      * ? * Ritorno al primo video
436400070312     c                   move      'D1'          $gest
436500070312     C*------------------------
436600070312     C*
436700070312     C                   ENDSR
436800070301      *---------------------------------------------------------------*
436900070321      *
    OPERAZIONI INIZIALI        ?
437000070301      *---------------------------------------------------------------*
437100940131     C     *INZSR        BEGSR
437200030113     C*
437300030113     C* Reperimento parametri
437400030113     C     *ENTRY        PLIST
437500030113     C                   PARM                    KPJBA
437600070404     c                   parm                    $FV6
437700070309      *
437800070313     C                   MOVEL     KPJBU         FIDG31DS
437900080625     c                   if        fdgIEfas = *blank
438000080625     c                   eval      fdgIEfas = 'I'
438100080625     c                   end
438200080616      *
438300070904      * default -> Consegne
438400070904     C                   IF        fdgTpoP      = *blank
438500070904     c                   movel     'C'           fdgTpoP
438600070904     c                   end
438700070903     c                   movel     fdgTpoP       Tipo_Prestaz      1
438800070309      *
438900070606      * memorizza se richiamato da Assegnazione Manuale da quale tipo
439000070606      *  di operazione è stato chiamato Assegnazione oppure altro.
439100070606     c                   clear                   fdg31_opz         1
439200080715     c                   if        fdgPGM = 'ASSMANUALE' or fdgPGM = 'MONITOR'
439300080606     c                   z-add     fdgNFV        SAVfdgNFV
439400080606     c                   z-add     0             fdgNFV
439500070606     c                   move      fdgOPZ        fdg31_opz         1
439600110309     c*********
439700110309     c********* Solo nel caso si trattasse di Togliere da una distinta apro il DROP
439800110309     c*********   altrimenti NON mi serve aprire direttamente i dati sulle singole DISTINTE.
439900110309     c                   if        fdg31_opz = '2'
440000080703     c                   eval      Apri_Drop = 'S'
440100110309     c                   end
440200110309     c*********
440300070607     c                   eval      cur_su_SFL = 'S'
440400070606     c                   end
440500070606      *
440600070309     C                   Z-ADD     1             CODUT
440700070309     C                   CALL      'X§PARUT'
440800070309     C                   PARM                    UT§DSE
440900070309     C                   MOVEL     REC80         CNCR80
441000070309     C***
441100070309     C     Kdst          KLIST
441200070309     C                   KFLD                    WNPG
441300070312     C                   KFLD                    wNFV
441400070312     C                   KFLD                    wFGS
441500070312     C***
441600080613     C     Kdsf_01       KLIST
441700080613     C                   KFLD                    dstNPG
441800080613     C                   KFLD                    dstNFV
441900080613     C                   KFLD                    dstFGS
442000080613     C***
442100070406     C     Kdst9         KLIST
442200070406     C                   KFLD                    wFGS
442300070406     C                   KFLD                    WNPG
442400070406     C                   KFLD                    wNFV
442500070406     C***
442600070312     C     Kdst31        KLIST
442700070312     C                   KFLD                    wFGS
442800070312     C                   KFLD                    wNFV
442900070314      *
443000070910     C     Kdst51        KLIST
443100070910     C                   KFLD                    wFGS
443200070910     C                   KFLD                    wNFV
443300070910      *
443400070403     C     Kqdt          KLIST
443500070403     C                   KFLD                    wFGS
443600070403     C                   KFLD                    WNFV
443700070403      *
443800070314     C     KORM          KLIST
443900070314     C                   KFLD                    wFGS
444000070314     C                   KFLD                    WNFV
444100070309     C***
444200070309     C     Kapd          KLIST
444300070309     C                   KFLD                    dsttipa
444400070309     C                   KFLD                    dstpdr
444500070309      * Fnarb
444600070309     c     Karb78        Klist
444700070309     c                   Kfld                    Wifp
444800070309     c                   Kfld                    Kfnvv
444900070309      *
445000070309     C     KDAT          KLIST
445100070309     C                   KFLD                    WNPG
445200070309     C                   KFLD                    DATINW
445300070309
445400070309     c     Kdat1         Klist
445500070314     c                   Kfld                    wFGS
445600070309     c                   Kfld                    Wnpg
445700070309     c                   Kfld                    Datinw
445800070309
445900070309     c     Kdat2         Klist
446000070314     c                   Kfld                    wFGS
446100070309     c                   Kfld                    Wnpg
446200070309      *
446300070309     C     Kdst3         KLIST
446400070314     C                   KFLD                    wFGS
446500070309     C                   KFLD                    WNPG
446600070309      *
446700070309     C     Kimpo         Klist
446800070309     C                   Kfld                    arbaas
446900070309     C                   Kfld                    arblnp
447000070309     C                   Kfld                    arbnrs
447100070309     C                   Kfld                    arbnsp
447200080702      *
447300080702     C     KTbe1         KLIST
447400080702     C                   KFLD                    TBEcod
447500080702     C                   KFLD                    TBEke1
447600070309     C***
447700070309     C* CARICO TABELLA FILIALI GESTITE £1
447800070309     C                   CLEAR                   DSUL06
447900070309     C                   MOVE      '£1'          D06COD
448000070309     C                   MOVEL     SIMFEL        D06KEY
448100070309     C                   MOVEL     'L'           D06TLA
448200070313     c                   eval      kpjbus = kpjbu
448300070309     C                   MOVEL     DSUL06        KPJBU
448400070309     C                   CALL      'TRUL06R'
448500070309     C                   PARM                    KPJBA
448600070309     C                   MOVEL     KPJBU         DSUL06
448700070313     c                   eval      kpjbu = kpjbus
448800070309     C                   MOVEA     LIN           L1
448900070309     C*
449000070309      *  Se richiamata da EDP
449100070308     c                   CLEAR                   se_EDP            1
449200070308     c                   if        %subst(Knmus:1:3) = 'EDP'
449300070308     c                   MOVE      'S'           se_EDP
449400070307     c                   end
449500070307      *
449600070227      * Reperisco dati job
449700070227     c                   exsr      DatiJob
449800070227
449900030113     C* Variabili per gestione videate
450000070313     C                   MOVE      *BLANK        Sav_$GEST
450100070313     C                   MOVE      *BLANK        $GEST             2
450200030113     C                   MOVE      *BLANK        $FINE             1
450300070312     C                   MOVE      *off          $INZD1            1
450400070312     C                   MOVE      *off          $INZD2            1
450500070312     C                   MOVE      *off          $INZS1            1
450600070312     C                   MOVE      *off          $INZS2            1
450700070312     C                   MOVE      *off          $INZS3            1
450800070312     C                   clear                   $visD1            1
450900070312     C                   clear                   $visD2            1
451000070312     C                   clear                   $visS1            1
451100070312     C                   clear                   $visS2            1
451200070312     C                   clear                   $visS3            1
451300030113     C                   MOVE      *BLANK        $EFILE            1
451400030113     C                   MOVE      *BLANK        $ESCI             1
451500030113     C                   MOVE      *BLANK        $RCDOK            1
451600070314     C                   MOVE      *on           $PcurS2           1
451700070321     C                   MOVE      *on           $PcurS1           1
451800070320     C*
451900070320     C                   Z-ADD     0             CURR              2 0
452000070320     C                   Z-ADD     0             CURC              2 0
452100030113     C* Indici
452200030113     C                   Z-ADD     0             X                 3 0
452300030113     C                   Z-ADD     0             Y                 3 0
452400070227     C***
452500070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
452600070227     C***
452700070227     C                   TIME                    W0140            14 0
452800070227     C                   MOVE      W0140         UDATE8            8 0
452900070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
453000070227     C*
453100070227     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
453200070227     C                   MOVEL     *BLANK        G02ERR
453300070227     C                   CALL      'XSRDA8'
453400070227     C                   PARM                    WLBDAT
453500070301     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
453600940127     C*
453700940117     C                   ENDSR
453800070227      *---------------------------------------------------------------*
453900070321      *
  Reperimento Dati del job (Utente/Operativi)     ?
454000070227      *---------------------------------------------------------------*
454100070228     c     DatiJOB       BEGSR
454200070227      *
454300070227     c     *dtaara       define    §azute        azuteds
454400070227     c     *dtaara       define    §datiute      ddatiute
454500070227      *
454600070227     c                   in(E)     *dtaara
454700070312     c                   IF        %ERROR or X_RSUT = *blanks
454800070227     c                   clear                   Tibs34Ds
454900070227     c                   call      'TIBS34R'
455000070227     c                   parm                    Tibs34Ds
455100070227     c                   in        *dtaara
455200070227     c                   ENDIF
455300070227      *
455400070227     c                   ENDSR
455500070312     C*-----------------------------------------------------***********
455600070321      *
  Imposta i campi di totale   ?
455700070312     C*-----------------------------------------------------***********
455800070312     c     totalizzo     begsr
455900070312      *--
456000070321     c                   clear                   w_imca           13 3
456100070321     c                   clear                   w_imas           13 3
456200070321      *--
456300070312     c     Karb78        setll     fnarb78l
456400070312     c                   do        *hival
456500070321     c     karb78        reade     fnarb78l
456600070321     c                   if        %eof(fnarb78l)
456700070321     c                   leave
456800070321     c                   endif
456900070321
457000070312      * totalizzo gli assegnati
457100070312     c                   if        arbica = *blank or arbica = 'R'
457200070312     c     kimpo         chain     fiar601l                           21
457300070321     c  n21              add       ar6ift        w_imas
457400070312     c                   endif
457500070312      * totalizzo i contrassegni
457600070312     c                   if        arbicc = *blank or arbicc = 'R'
457700070312     c     kimpo         chain     fiar901l                           21
457800070312     c                   if        not *in21
457900070312     c                   if        ar9vca = 'EUR'
458000070321     c                   add       ar9cas        w_imca
458100070312     c                   else
458200070312      *se la divisa non è EUR traduco con yeurco l'importo
458300070312     c                   CLEAR                   YEURDS
458400070312     c                   MOVEL     ar9vca        YECDVI
458500070312     c                   Z-ADD     ar9cas        YECIMI
458600070312     c                   MOVEL     'EUR'         YECDVO
458700070312     c                   MOVEL     'H'           YECTAR
458800070312     c                   CALL      'YEURCO'
458900070312     c                   PARM                    YEURDS
459000070321     c                   add       YECIMO        w_imca
459100070312     c                   endif
459200070312     c                   endif
459300070312     c                   endif
459400070312     c                   enddo
459500070312      *
459600070312     c                   endsr
459700070309     C/EJECT
459800070309     C*-----------------------------------------------------***********
459900070309**  PGM RICHIAMANTI
460000070309FIDG31R
460100070309**  TES
460200070830     INTERROGAZIONE DISTINTE
460300070606 **    ASSEGNAZIONE MANUALE    **
460400070606 **  Interrogazione Distinte   **
460500070830 **                            **
460600070830 **    Dettaglio   Consegne    **
460700070830 **    Dettaglio    Ritiri     **
460800070830 **     Consegne e  Ritiri     **
460900070309**
461000070309Filiale in gestione errata o mancante
461100160503Filiale obbligatoria
461200070309Importi visualizzabili soltanto richiedendo Tipo fogli=2 non chiusi
461300160503I Turni possono essere soltanto su distinte PARCEL e NON messaggerie
461400160503I Turni sono una scelta obbligata su distinte PARCEL
