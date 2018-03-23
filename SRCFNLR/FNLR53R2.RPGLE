000100061108     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200920914     H DECEDIT('0,') DATEDIT(*DMY.)
000300930303      *---------*----------------------------------------------------*
000400000503      * FNLR53R2*                                                    *
000500930217      *---------*                                                    *
000600080709      *         - GESTIONE CHIUSURA DISTINTA -                       *
000700940112      *--------------------------------------------------------------*
000800941115     FFNLR53D   CF   E             WORKSTN
000900941114     F                                     SFILE(LR53S01:NRR1)
001000941114     F                                     SFILE(LR53S02:NRR2)
001100941114     F                                     SFILE(LR53S03:NRR3)
001200020822     FFNARB70L  IF   E           K DISK
001300941115     FFNARB01L  IF   E           K DISK    USROPN
001400941115     F                                     COMMIT
001500941114     F                                     RENAME(FNARB000:FNARB01)
001600990922     FFIAR601L  IF   E           K DISK
001700060214     FFiAR401L  IF   E           K DISK    USROPN
001800941115     F                                     COMMIT
001900990922     FFIARI01L  IF   E           K DISK
002000060419     FFIdci01l  uF   E           K DISK    usropn
002100051115     FFIAR901L  IF   E           K DISK    USROPN
002200941115     F                                     COMMIT
002300941115     FFNART01L  IF   E           K DISK    USROPN
002400941115     F                                     COMMIT
002500020822     FFNFVV01L  UF   E           K DISK    USROPN
002600930402     F                                     COMMIT
002700070410     Ffidst09L  uf   e           k disk    usropn
002800070319     F                                     commit infds(dstinf)
002900941114     FFNLBL01L  IF   E           K DISK
003000011121     Ffiftt02L  IF   E           K DISK
003100011121     Ffifst02L  IF   E           K DISK
003200011121     F                                     RENAME(fiftt000:fifst000)
003300021204     Ffiapd01L  IF   E           K DISK
003400941115     FTABEL00F  IF   E           K DISK
003500991006     FTNTBE01L  IF   E           K DISK
003600000602     FFNEVB05L  IF   E           K DISK
003700010402     FFNEVB04L  IF   E           K DISK
003800010402     F                                     RENAME(FNEVB000:FNEVB4)
003900031016     FFNEVb00f  o    E             DISK
004000031016     F                                     RENAME(FNEVB000:FNEVB0)prefix(x)
004100070122     FFNBRV01L  IF   E           K DISK
004200941114     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
004300120529     Ffnarbj0f  IF   E           K DISK    USROPN
004400080708     FFipct02l  IF   E           K DISK
004500080714     FFidsf01l  IF   E           K DISK
004600110330     FFiar501l  IF   E           K DISK
004700940317      *----------------------------------------------------------------
004800020509     D*UNI             S              3    DIM(29)                              LINEE ARRIVI
004900020509     D £1              S              3  0 DIM(30)                              FIL. GESTITE
005000020509     D £6              S              3  0 DIM(30)                              LINEE ARRIVI
005100941114     D G15             S              8  0 DIM(5)                               DATA DISTINTA - 15 G
005200120531     D D3A             S                   DIM(200)like(ds3a)                   SKI TAB. 3A
005300080903     D C3A             S              2    DIM(200)                             SKI CHIAVI 3A
005400120531     D D7R             S                   DIM(200)like(ds7r)                   SKI TAB. 7R
005500080903     D C7R             S              2    DIM(200)                             SKI CHIAVI 7R
005600080707     D ERR             S             70    DIM(22) CTDATA PERRCD(1)
005700120531     D D1A             S                   DIM(200) like(ds1a)                  SKI TAB. 1A
005800080903     D C1A             S              2    DIM(200)                             SKI CHIAVI 1A
005900080903     D COA             S              1    DIM(200)                             ANOMAL.MANC.CONS.
006000080903     D CMC             S              3    DIM(200)                             CODICI MANC.CONS.
006100080903     D CMB             S              3    DIM(200)                             COD.MANC.CONS.BAR
006200080903     D PMC             S              2    DIM(200)                             PRIORITA COD.M.C.
006300000503     D FIP             S              3  0 DIM(50)                              FILIALI POSTE
006400080903     D C2Z             S              3    DIM(200)                             CHIAVI EVENTI
006500120531     D D2Z             S                   DIM(200)  like(ds2z)                 DS EVENTI
006600101123     D C2a             S              3    DIM(200)                             CHIAVI EVENTI
006700120531     D D2a             S                   DIM(200)  like(ds2a)                 DS EVENTI
006800000211     D MSGT            S             70    DIM(1) CTDATA PERRCD(1)
006900000211     D MSG             S             66    DIM(3) CTDATA PERRCD(1)
007000021107      *
007100941115      *----------------------------------------------------------------
007200091216     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
007300941115     D FNLR53        E DS                  EXTNAME(FNLR53DS)
007400941115     D  NDC1                   1     35  0
007500941115     D                                     DIM(5)                               NUM.DISTINTA
007600941115     D  DTD                   36     75  0
007700941114     D                                     DIM(5)                               DATA DISTINTA
007800941115     D  DDC                   76    115  0
007900941114     D                                     DIM(5)                               DATA CHIUSURA
008000941115     D  HRC                  116    135  0
008100911028     D                                     DIM(5)                               ORA CHIUSURA
008200941115     D  FPP                  136    140
008300941114     D                                     DIM(5)                               TIPO USCITA
008400941115     D  PEP                  141    145
008500941114     D                                     DIM(5)                               PICKING S/N
008600941115     D  SET                  146    180  0
008700941115     D                                     DIM(5)                               ORA CHIUSURA
008800030204     D  CAR                  247    251
008900030204     D                                     DIM(5)                               Carico S/N
009000911028     D KPJBA         E DS
009100911028     D  LIBSYS                92    100
009200911028     D  MSGERR               453    502
009300030718     D trul90ds      E DS
009400911028      *
009500000211     D DSMSG           DS
009600000211     D  KCODM                 25     26
009700051215     D fnlv80ds      E DS
009800000531     D DSARBD        E DS
009900000531     D DSARBG        E DS
010000000531     D DARBT         E DS
010100000531     D DSARBK        E DS
010200000531     D DS48          E DS                  EXTNAME(FNLR48DS)
010300000602     D DS2Z          E DS
010400101123     D DS2a          E DS
010500990923     D DS1A          E DS
010600991006     D DSCV          E DS
010700991006     D DGED          E DS
010800020418     D Ddgd          E DS
010900990923     D DS3A          E DS
011000941115     D DS7R          E DS
011100000427     D DS3E          E DS
011200090121     d ddstflr       e ds
011300080714     d fipctcetds    e ds
011400080714     d fnlr53ds7     e ds
011500070319     d trul82ds      e ds
011600020822     D ddatiute      e ds
011700020822     D azuteds       e ds                  extname(AZUTE00F)
011800020822     D tibs34ds      E DS                  inz
011900000503     D OG143         E DS
012000000503     D DSBL4A        E DS
012100990927     D YEURDS        E DS                  EXTNAME(YEURCODS)
012200110330     D dar5gen       E DS
012300110330     d TRULINTds     e ds                  inz
012400110330     d   iINTtip     e                     inz('I')
012500020509     d trul06ds      e ds
012600020509     d  lin                    1     90  0 dim(30)
012700911028      *
012800941114     D DSLR01        E DS
012900941114     D WLBDA8          DS
013000941114     D  G02DAT                 1      8  0
013100941114     D  G02INV                 9     16  0
013200941114     D  G02ERR                17     17
013300941114     D  G02TGI                18     22  0
013400930407      *
013500930407     D WLBGIO          DS
013600941114     D  GIODAT                 1      8  0
013700941114     D  GIOINV                 9     16  0
013800941114     D  GIOTGI                17     21  0
013900020418      *
014000020418     d Wdata8          DS
014100020418     d  dadata                 1      8  0
014200020418     d  adata                  9     16  0
014300020418     d  GioLav                17     21  0
014400020418      *
014500911030      *
014600930202     D                 DS
014700930202     D  NDC                    1     35  0
014800911105     D                                     DIM(5)                               NUM.DISTINTA
014900941115     D  V2CND1                 1      7  0
015000941115     D  V2CND2                 8     14  0
015100941115     D  V2CND3                15     21  0
015200941115     D  V2CND4                22     28  0
015300941115     D  V2CND5                29     35  0
015400930331     D                 DS
015500930331     D  W1GC                   1      2
015600930331     D  DSG1                   1      1
015700930331     D  DSMP1                  2      2
015800011121      * X  CHKOBJ fiftdS0T
015900950221     D                 DS
016000950221     D  WFILC                 43     45  0
016100950221     D  WCMDC                  1     60
016200011121      * X  ALCOBJ fiftdS0T
016300950221     D                 DS
016400950221     D  WFILA                 37     39  0
016500950221     D  WCMDA                  1     60
016600131106     c*
016700131106     d dEVB01        e ds
016800131106     c*
016900070319      *
017000070319     d dstinf          ds
017100070319     d  dstnrr               397    400i 0
017200020822     c* ds x passaggio parametri a FNLR55R2
017300020822     D fnlr55          DS
017400020822     D  po55                          3  0
017500020822     D  ndc55                         5  0
017600090122     c* ds x passaggio parametri a FNLR53R8 cancellazione file pda per distinta chiusa
017700090122     D pardel          DS
017800090122     D  pardis                        6s 0
017900090122     D  parfil                        3s 0
018000030828     c* ds x passaggio parametri a FNLR55R2
018100030828     D parmwf          DS
018200030828     D  dis                    1     35  0
018300030828     D                                     DIM(5)                               NUM.DISTINTA
018400030828     D  filges                36     38  0
018500030901     d  errwf                 39     39
018600080708     d dataisoc        s               d   datfmt(*iso)
018700080708     d dataeurc        s               d   datfmt(*eur)
018800120312     d comlnp          s              3
018900120312     d comnrs          s              2
019000120312     d comnsp          s              7
019100941114      *----------------------------------------------------------------
019200940322     IAZORG01L  AA
019300940322     I                             P    4    5 0ORGFIL
019400940414     I                                  7    7  ORGFAG
019500000503     I                               3776 3800  ORGDE3
019600911028      *---------------------------------------------------------------*
019700911028      * FLUSSO PRINCIPALE                                             *
019800911028      *---------------------------------------------------------------*
019900941114      *  Azzeramento campi primo subfile
020000941114     C                   EXSR      INZS01
020100941114      *  Loop gestione videate
020200941114     C     WFINE         DOUEQ     'S'
020300941114     C     WTPVID        CASEQ     '1'           GESS01
020400941114     C                   END
020500941114     C                   END
020600941115     C                   EXSR      UPDFVV
020700941115     C     FINE          TAG
020800941124     C                   MOVEL     'U'           D53TRI
020900960326     C     WCAL53        IFEQ      'S'
021000941124     C                   EXSR      CAL53
021100960326     C                   END
021200941114     C                   SETON                                        LR
021300941114      *---------------------------------------------------------*
021400941114      *  Inizializzazione del primo subfile                     *
021500941114      *---------------------------------------------------------*
021600941114     C     INZS01        BEGSR
021700941114      *
021800941114     C                   SETON                                        6280
021900941114     C                   WRITE     LR53C01
022000941114     C                   CLEAR                   LR53S01
022100941114     C                   SETOFF                                       629028
022200941114     C                   SETOFF                                       404142
022300001006     C                   SETOFF                                       434450
022400000601     C                   MOVE      *BLANK        ERRCS
022500941124     C                   Z-ADD     0             NRR1
022600031013     C                   DO        5             H                 1 0
022700941115     C                   ADD       1             NRR1              4 0
022800941114     C                   WRITE     LR53S01
022900941114     C                   END
023000941114     C                   Z-ADD     *ZEROS        NDC
023100941115     C                   Z-ADD     1             NR1KEY
023200941114      *
023300941114     C                   ENDSR
023400941114      *---------------------------------------------------------*
023500941114      *  Gestione della prima videata                           *
023600941114      *---------------------------------------------------------*
023700941114     C     GESS01        BEGSR
023800060323     c* pulisco il file in qtemp del dettaglio contante
023900060323     c                   exsr      srtaglio
024000941114      *
024100941123     C                   WRITE     LR53Z01
024200941114     C                   EXFMT     LR53C01
024300941124     C                   SETOFF                                       902845
024400950124     C                   SETOFF                                       404142
024500950124     C                   SETOFF                                       4344
024600941114      *  Fine Lavoro
024700941114     C     *INKC         IFEQ      '1'
024800941114     C                   MOVEL     'S'           WFINE
024900941114     C                   GOTO      FINVD1
025000941114     C                   END
025100041013      * Richiamo pgm gestione inserimento bancali aff-rie/ e colli ritirati
025200941114      * Sblocco Distinte
025300941114     C     *INKK         IFEQ      '1'
025400950224     C                   MOVE      V1CLNA        KPJBU
025500941114     C                   CALL      'FNLR53R1'
025600950224     C                   PARM                    KPJBA
025700941114     C                   GOTO      FINVD1
025800941124     C                   END
025900060323      * mandati x cassaforte con feritoia
026000060323     C     *INKi         IFEQ      '1'
026100060323     C                   CALL      'CNC0W6R'
026200060323     C                   PARM                    KPJBA
026300060323     C                   GOTO      FINVD1
026400060323     C                   END
026500060323      * incassi clienti codificati
026600060323     C     *INKh         IFEQ      '1'
026700060323     C                   CALL      'YCOW0200R'
026800060323     C                   PARM                    KPJBA
026900060323     C                   GOTO      FINVD1
027000060323     C                   END
027100941124      * Se ho richiesto il cambio filiale ricarico £6
027200941124     C     *IN02         IFEQ      '1'
027300941124     C                   SETOFF                                       02
027400941124     C                   END
027500941114     C* Routine controlli
027600941114     C                   EXSR      CTR01
027700941124     C   90              GOTO      FINVD1
027800941124      * Cambio Filiale
027900941124     C     *INKS         IFEQ      '1'
028000941124     C                   SETOFF                                       80
028100941124     C                   SETON                                        02
028200941124     C                   GOTO      FINVD1
028300941124     C                   END
028400941114     C* Conferma
028500941114     C     *INKF         IFEQ      '1'
028600941115     C                   EXSR      INZS02
028700030926     C   90              GOTO      FINVD1
028800000601     C     ERRCS         IFNE      *BLANK
028900030901      *verifico l'azione se ritorno con f3 da pgm con lettore ottico non
029000030901      *emetto la videata dei contrassegni della sda
029100030901     c                   if        kcdaz <> 'XR53'
029200000601     C                   EXFMT     LR53W12
029300030901     c                   endif
029400000601     C                   EXSR      UPDFVV
029500000601     C                   EXSR      INZS01
029600000601     C                   ELSE
029700941115     C                   EXSR      CAL53
029800941124     C     D53F03        IFEQ      '1'
029900941124     C                   MOVEL     'S'           WFINE
030000941124     C                   ELSE
030100941124     C                   EXSR      UPDFVV
030200941124     C                   EXSR      INZS01
030300941124     C                   END
030400000601     C                   END
030500941114     C                   END
030600941114     C*
030700941114     C     FINVD1        ENDSR
030800941114      *---------------------------------------------------------*
030900060323      *  file dettaglio contante                                *
031000941114      *---------------------------------------------------------*
031100060323     C     srtaglio      BEGSR
031200060419     c                   setoff                                       88
031300060419     c                   if        not %open(fidci01l)
031400060419     c                   open      fidci01l                             88
031500060419     c                   end
031600060323     C*
031700060419     c                   if        not *in88
031800060323     c     knraz         setll     fidci01l
031900060323     c                   do        *hival
032000060323     c     knraz         reade     fidci01l
032100060323     c                   if        %eof(fidci01l)
032200060323     c                   leave
032300060323     c                   end
032400060323     c                   delete    fidci000
032500060323     c                   enddo
032600060323     c                   end
032700060323     C*
032800060323     C                   ENDSR
032900060323      *---------------------------------------------------------*
033000060323      *  Sblocco fogli viaggio                                  *
033100060323      *---------------------------------------------------------*
033200060323     C     UPDFVV        BEGSR
033300941114      *
033400941115     C                   DO        5             I                 3 0
033500941114     C     NDC(I)        IFNE      *ZEROS
033600941114     C                   MOVE      ' '           FVVFCF
033700941115     C                   Z-ADD     NDC(I)        KNFV
033800020822     C     KFVV          CHAIN     FNFVV01L                           96
033900941114     C* Controllo se il flag chiusura è uguale a 'F' lo pulisco
034000941114     C     FVVFCF        IFEQ      'F'
034100941114     C                   MOVE      ' '           FVVFCF
034200941114     C                   EXCEPT    AGGFVV
034300941114     C                   END
034400070316     C                   Z-ADD     NDC(I)        KNFV6
034500070410     C     Kdst          CHAIN     fidst09L                           96
034600070316     C* Controllo se la distinta è stata chiusa
034700070316     C                   IF        not*in96 and dstfcf = *blank
034800070316     C                   MOVE      *blank        dstfcf
034900070316     C                   update    fidst000
035000070316     C                   END
035100090122     C                   IF        not*in96 and dstfcf = 'S'
035200090122     C                   clear                   pardel
035300090122     c                   z-add     dstnfv        pardis
035400090122     c                   z-add     dstfgs        parfil
035500090122     C                   call      'FNLR53R8'
035600090122     C                   parm                    pardel
035700090122     C                   END
035800941114     C                   END
035900941114     C                   END
036000941114     C*
036100941114?    C                   COMMIT
036200941114     C*
036300941114     C                   ENDSR
036400941114      *---------------------------------------------------------*
036500941114      *  Caricamento £6                                         *
036600941114      *---------------------------------------------------------*
036700941114     C     CAR£6         BEGSR
036800941114      *
036900941114      * Aggancio la £6 e recupero le linee a loro associate
037000020509     c                   clear                   trul06ds
037100020509     c                   eval      d06cod = '£6'
037200020509     c                   movel     V1Clna        d06key
037300020509     c                   movel     trul06ds      kpjbu
037400020509     c                   call      'TRUL06R'
037500020509     c                   parm                    kpjba
037600020509     c                   movel     kpjbu         trul06ds
037700020509     c                   movea     lin           £6
037800941114     C*
037900941114     C                   ENDSR
038000941114      *---------------------------------------------------------*
038100941114      *  Controlli prima videata                                *
038200941114      *---------------------------------------------------------*
038300941114     C     CTR01         BEGSR
038400941114      *
038500941115     C                   Z-ADD     1             NR1KEY
038600941114     C     V1CLNA        IFNE      WSVLNA
038700941114      * Controllo se esiste la filiale da me digitata
038800941114     C     V1CLNA        LOOKUP    £1                                     96
038900941124     C     *IN96         IFEQ      '0'
039000941124     C                   SETON                                        289045
039100941114     C                   MOVEL     ERR(1)        $MSG
039200941114     C                   GOTO      FINCT1
039300941114     C                   END
039400941115     C                   MOVE      V1CLNA        WSVLNA            3 0
039500941124     C                   EXSR      CAR£6
039600941114     C                   END
039700941114     C*
039800941114     C                   MOVEL     *ZEROS        SAVPDC            7 0
039900941114     C                   MOVEL     *ZEROS        NDC
040000941114     C                   MOVEL     *ZEROS        DDC
040100941114     C                   MOVEL     *ZEROS        HRC
040200941114     C                   MOVEL     *ZEROS        DTD
040300941114     C                   MOVEL     *ZEROS        G15
040400941114     C                   Z-ADD     0             J                 1 0
040500080707     C                   MOVE      *blank        savpda            1
040600941114     C*
040700941114     C                   READC     LR53S01                                96
040800941114     C     *IN96         DOWEQ     '0'
040900941114     C     *IN28         ANDEQ     '0'
041000941114     C                   SETOFF                                       404142
041100941114     C                   SETOFF                                       4344
041200941114     C*  Controllo il numero distinta
041300941114     C     V1CNDC        IFNE      *ZEROS
041400941123     C     V1CNDC        ANDNE     *BLANKS
041500941124     C     '?'           SCAN      V1CNDC                                 95
041600941124     C     *IN95         IFEQ      '1'
041700941114     C                   CLEAR                   DSLR01
041800941124     C                   MOVEL     'FNLR53R2'    DLRPGM
041900941114     C                   MOVEL     '2'           DLRTFV
042000941114     C                   Z-ADD     4             DLRNPG
042100941114     C                   Z-ADD     V1CLNA        DLRFGS
042200941114     C                   MOVEL     'S'           DLRRIC
042300941114     C                   Z-ADD     OGGI          DLRADA
042400941114     C                   MOVEL     DSLR01        KPJBU
042500941114     C                   CALL      'FNLR02R'
042600941114     C                   PARM                    KPJBA
042700941114     C                   MOVEL     KPJBU         DSLR01
042800941114     C                   MOVE      DLRNFV        V1CNDC
042900941114     C                   END
043000941114     C*
043100941124     C                   TESTN                   V1CNDC               95
043200941124     C   95              MOVE      V1CNDC        §NDC              7 0
043300941114     C                   ELSE
043400941114     C                   Z-ADD     *ZEROS        §NDC
043500941123     C                   Z-ADD     *ZEROS        V1CHRC
043600941123     C                   Z-ADD     *ZEROS        V1CMNC
043700941123     C                   Z-ADD     *ZEROS        V1CDTC
043800941123     C                   MOVEL     *BLANKS       V1CFPP
043900941123     C                   MOVEL     *BLANKS       V1CPEP
044000030204     C                   MOVEL     *BLANKS       v1cCAR
044100941114     C                   END
044200941114     C*  Ricerco distinta su file bolle arrivi
044300941114     C     §NDC          IFNE      *ZEROS
044400070316     C*  Blocco record Fidst
044500070316     C                   Z-ADD     §ndc          KNFV6
044600070410     C     Kdst          chain(e)  fidst09L                           95
044700070319     c                   if        %error
044800070521     C     Kdst          chain(n)  fidst09L                           95
044900070319     c                   exsr      geslck
045000070410     C     Kdst          chain(e)  fidst09L                           95
045100070319     c                   endif
045200090121     c                   movel     dstflr        ddstflr
045300080707      * verifica se distinta PDA e chiusura senza lettore ottico
045400100225     c                   if        dstpda <> 'N' and dstpda <> 'O'
045500100225     c                             and kcdaz = 'LR53'
045600090217     C                   Z-ADD     NRR1          NR1KEY
045700090217     C                   SETON                                        902840
045800090217     C                   MOVEL     ERR(22)       $MSG
045900090217     C                   GOTO      AGGS01
046000090217     c                   endif
046100110217      * imposta distinta PDA
046200110217     c                   eval      wpda = 'N'
046300110217     c                   if        §DSTTSTPDA = *blank and
046400110217     c                             (dstpda = 'C' or dstpda = 'E') or
046500110217     c                             §DSTTSTPDA = 'O' and
046600110217     c                             (dstpda = 'C' or dstpda = 'E')
046700110217     c                   eval      wpda = 'S'
046800110217     c                   endif
046900080707      * verifica presenza fase CRI per chiudere la distinta
047000090121      * azione di caricamento da lettore ottico o da predistinta
047100090121      * solo se non è in TEST e la distinta comprende le consegne PDA
047200090121      * oppure se è in TEST x ORM e la distinta comprende le consegne PDA
047300090121     c                   if        kcdaz <> 'LR53'
047400101119     c                   if        wpda = 'S'
047500080707     c     kdsf          chain     fidsf01l
047600080707     c                   if        not %found(fidsf01l)
047700080707     C                   Z-ADD     NRR1          NR1KEY
047800080707     C                   SETON                                        902840
047900080707     C                   MOVEL     ERR(21)       $MSG
048000080707     C                   GOTO      AGGS01
048100080707     c                   endif
048200080707     c                   endif
048300090121     c                   endif
048400960702     C*  Ricerco distinta su foglio viaggio
048500960702     C                   SETOFF                                       95
048600960702     C                   Z-ADD     §NDC          KNFV
048700020822     C     KFVV          CHAIN     FNFVV01L                           9495
048800960702     C* RILASCIO
048900960702     C  N94
049000960702     CANN95              UPDATE    FNFVV000
049100960702     C     *IN94         IFEQ      '1'
049200960702     C                   Z-ADD     NRR1          NR1KEY
049300960702     C                   SETON                                        902840
049400960702     C                   MOVEL     ERR(2)        $MSG
049500960702     C                   GOTO      AGGS01
049600960702     C                   END
049700960702     C* Controllo se distinta già utilizzata da altri o già chiusa
049800960702     C     *IN95         IFEQ      '1'
049900960702     C     FVVFCF        ORNE      ' '
050000960702     C                   Z-ADD     NRR1          NR1KEY
050100960702     C                   SETON                                        902840
050200960702     C                   MOVEL     ERR(6)        $MSG
050300960702     C                   GOTO      AGGS01
050400960702     C                   END
050500960702     C                   Z-ADD     FVVDFV        KDDC
050600960702     C                   Z-ADD     §NDC          KNDC
050700020923     C                   Z-ADD     v1clna        KIFP
050800020822     C     KARB          CHAIN     FNARB70L                           95
050900941123     C     *IN95         IFEQ      '1'
051000941123     C                   Z-ADD     NRR1          NR1KEY
051100941114     C                   SETON                                        902840
051200941114     C                   MOVEL     ERR(2)        $MSG
051300941114     C                   GOTO      AGGS01
051400941114     C                   END
051500941114     C*  Distinta non abilitata
051600941124     C     ARBLNA        LOOKUP    £6                                     95
051700941124     C     *IN95         IFEQ      '0'
051800941123     C                   Z-ADD     NRR1          NR1KEY
051900941114     C                   SETON                                        902840
052000941123     C                   MOVEL     ERR(12)       $MSG
052100941114     C                   GOTO      AGGS01
052200941114     C                   END
052300941114     C*  Imposto dati da foglio viaggio
052400941114     C                   MOVE      FVVDFV        G02INV
052500941114     C                   MOVEL     '3'           G02ERR
052600941114     C                   CALL      'XSRDA8'
052700941114     C                   PARM                    WLBDA8
052800941114     C                   Z-ADD     FVVDFV        CTRDAT            8 0
052900941114     C                   Z-ADD     G02DAT        §DDC              8 0
053000941114     C     G02TGI        SUB       15            GIOTGI
053100941114     C                   MOVE      *ZEROS        GIODAT
053200941114     C                   MOVE      *ZEROS        GIOINV
053300941114     C                   CALL      'XSRGI8'
053400941114     C                   PARM                    WLBGIO
053500941114     C                   MOVE      GIOINV        §G15              8 0
053600941114     C*  Imposto data consegna, tipo uscita, flag picking
053700941114     C     V1CDTC        IFEQ      *ZEROS
053800941114     C                   Z-ADD     §DDC          V1CDTC
053900941114     C                   END
054000941114     C     V1CFPP        IFEQ      *BLANKS
054100941115     C                   MOVE      FVVFPP        V1CFPP
054200941114     C                   END
054300950602     C     V1CPEP        IFEQ      *BLANKS
054400950602     C                   MOVE      FVVPEP        V1CPEP
054500950602     C                   END
054600941114     C*  Reperisco flag etichettatura
054700941115     C     APDPDR        IFNE      ARBPDC
054800941114     C                   MOVEL     *BLANKS       APDPKM
054900941114     C                   MOVEL     *BLANKS       APDPKP
055000021204     C     kapd          CHAIN     fiapd01L                           95
055100941114     C                   END
055200941114     C*
055300941115     C                   MOVE      'N'           V1HPEX
055400941114     C     APDPKM        IFEQ      'S'
055500941114     C     V1CFPP        ANDEQ     'M'
055600941114     C     APDPKP        OREQ      'S'
055700941114     C     V1CFPP        ANDEQ     'P'
055800941115     C                   MOVE      'S'           V1HPEX
055900941114     C                   END
056000941114     C*
056100941114     C     V1CPEP        IFEQ      ' '
056200941115     C                   MOVE      V1HPEX        V1CPEP
056300941114     C                   END
056400030204     C*
056500030204     C* lo stesso per il flag di Carico
056600030204     C                   MOVE      'N'           v1hCaX
056700030204     C     APDcaM        IFEQ      'S'
056800030204     C     v1cFPP        ANDEQ     'M'
056900030204     C     APDcaP        OREQ      'S'
057000030204     C     v1cFPP        ANDEQ     'P'
057100030204     C                   MOVE      'S'           v1hCaX
057200030204     C                   END
057300030204     C*
057400030204     C     v1cCAR        IFEQ      ' '
057500030204     C                   MOVE      v1hCaX        v1cCAR
057600030204     C                   END
057700030204     C*
057800010302     C     V1CHRC        IFNE      *ZEROS
057900010302     C     V1CFPP        IFEQ      'P'
058000010302     C     V1CHRC        ANDLE     12
058100010302     C                   Z-ADD     NRR1          NR1KEY
058200010302     C                   SETON                                        902841
058300010302     C                   MOVEL     ERR(16)       $MSG
058400010302     C                   GOTO      AGGS01
058500010302     C                   END
058600010302     C                   END
058700941114     C*
058800941114     C* Controllo la data della distinta
058900941114     C                   MOVE      V1CDTC        G02DAT
059000941114     C                   MOVEL     *BLANK        G02ERR
059100941115     C                   CALL      'XSRDA8'
059200941115     C                   PARM                    WLBDA8
059300941114     C     G02ERR        IFEQ      '1'
059400941114     C     G02INV        ORGT      OGGI
059500941123     C                   Z-ADD     NRR1          NR1KEY
059600941123     C                   SETON                                        902842
059700941114     C                   MOVEL     ERR(7)        $MSG
059800941114     C                   GOTO      AGGS01
059900941114     C                   END
060000941115     C                   Z-ADD     G02INV        GGCON             8 0
060100941123     C                   Z-ADD     G02DAT        V1CDTC
060200050711     C* CONTROLLO SE LNA ITALIA O  ESTERO
060300050711     C                   MOVE      V1CLNA        ORGFIL
060400050711     C     ORGFIL        CHAIN     AZORG01L                           91
060500050711     c                   movel     orgde3        og143
060600050711      * se estero imposto il flag
060700050711     C  n91§OgNTW        ifeq      'FED'
060800050711     C     §OgNTW        oreq      'EEX'
060900050711     C     §OgNTW        oreq      'EUP'
061000050711     C                   MOVEL     'E'           d53ita
061100050711     c                   end
061200941114     C*
061300020418     C* controllo limiti minimi da tabella dgd per data consegna non per estero
061400020418     c                   if        d53ita <> 'E'
061500020418     c                   eval      dadata = ggcon
061600020418     c                   eval      adata  = oggi
061700020418     c                   CALL      'XSRLAV8'
061800020418     c                   PARM                    Wdata8
061900050225      * verifico se per l'aut non sono stati fissati dei giorni limite
062000050225      * fuori standard primi 2 byte del campo APDFLR
062100050225     c                   clear                   como2n
062200050516     c                   clear                   como2
062300050225     c                   if        apdflr <> *blank
062400050225     c                   movel     apdflr        como2             2
062500050225     c                   endif
062600050225     C                   TESTN                   como2                95
062700050225     C   95              MOVE      como2         como2n            2 0
062800050225
062900050225      * se l'autista ha una personalizzazione in anagrafico per i giorni
063000050225      * limite da controllare prevale sui valori standard della tabella
063100050225
063200050225     c                   if        como2n > 0
063300050225     c                   if        giolav > como2n
063400050225     C                   SETON                                        422890
063500050225     C                   MOVEL     ERR(10)       $MSG
063600100210     C                   GOTO      aggs01
063700050225     c                   end
063800050225     c                   else
063900050225      * limiti standard tabellati
064000020418     c                   if        giolav > §dgdgbar
064100021129     C                   SETON                                        422890
064200020418     C                   MOVEL     ERR(10)       $MSG
064300100210     C                   GOTO      aggs01
064400020418     c                   end
064500050225     c                   end
064600020418     c                   End
064700941129     C*
064800941129     C     FVVDFV        IFGT      GGCON
064900941129     C     GGCON         ANDNE     V1HDDC
065000941129     C                   MOVE      GGCON         V1HDDC
065100941129     C                   Z-ADD     NRR1          NR1KEY
065200941129     C                   SETON                                        902842
065300941129     C                   MOVEL     ERR(14)       $MSG
065400100210     C                   GOTO      aggs01
065500941129     C                   END
065600941114     C*
065700941114     C* Controllo l'ora
065800941114     C     V1CHRC        IFNE      *ZEROS
065900941114     C                   Z-ADD     V1CHRC        ORA               2 0
066000941114     C                   Z-ADD     V1CMNC        MINUTI            2 0
066100941114     C     ORA           IFGT      21
066200941114     C     ORA           ORLT      7
066300941114     C     MINUTI        ORGT      59
066400941123     C                   Z-ADD     NRR1          NR1KEY
066500941123     C                   SETON                                        902841
066600941114     C                   MOVEL     ERR(8)        $MSG
066700941114     C                   GOTO      AGGS01
066800941114     C                   END
066900941114     C                   ELSE
067000941123     C                   Z-ADD     NRR1          NR1KEY
067100941123     C                   SETON                                        902841
067200941114     C                   MOVEL     ERR(8)        $MSG
067300941114     C                   GOTO      AGGS01
067400941114     C                   SETON                                        9086
067500941114     C                   END
067600080707     C*  Controllo che le distinte siano congruenti PDA SI o NO
067700090121     c                   eval      wpda = 'N'
067800090121     c                   if        §DSTTSTPDA = *blank and
067900090121     c                             (dstpda = 'C' or dstpda = 'E') or
068000090121     c                             §DSTTSTPDA = 'O' and
068100090121     c                             (dstpda = 'C' or dstpda = 'E')
068200080707     c                   eval      wpda = 'S'
068300080707     c                   endif
068400080707      *
068500080707     C     savPDA        IFNE      *blank                                       No Prima Volta
068600080707     C     savPDA        IFNE      wpda                                         cod.padr.diversi
068700080707     C                   Z-ADD     NRR1          NR1KEY
068800080707     C                   SETON                                        902840
068900080707     C                   MOVEL     ERR(20)       $MSG
069000080707     C                   GOTO      AGGS01
069100080707     C                   END
069200080707     C                   ELSE
069300080714     C                   move      wpda          savPDA
069400080707     C                   END
069500941114     C*
069600941114     C*  Controllo che non siano utilizzati padroncini diversi
069700941114     C     SAVPDC        IFNE      0                                            No Prima Volta
069800941123     C     SAVPDC        IFNE      ARBPDC                                       cod.padr.diversi
069900941123     C                   Z-ADD     NRR1          NR1KEY
070000941114     C                   SETON                                        902840
070100941114     C                   MOVEL     ERR(11)       $MSG
070200941114     C                   GOTO      AGGS01
070300941123     C                   END
070400941123     C                   ELSE
070500941123     C                   Z-ADD     ARBPDC        SAVPDC
070600941123     C                   END
070700941114     C*  Controllo che non siano già stati effettuati i conteggi
070800941114     C*  se il padroncino è soggetto a tassazione
070900941114     C                   MOVE      'N'           CNTPDR            1
071000941114     C*
071100941114     C     APDPDD        IFNE      'S'
071200941114     C*  Se padroncino non è dipedente controllo se in filiale
071300941114     C*  di appartenza è attivata la gestione conteggi
071400020822     C                   MOVEL     APDPDR        ORGFIL
071500020822     C     ORGFIL        CHAIN     AZORG01L                           95
071600941114     C     *IN95         IFEQ      '0'
071700941114     C                   MOVE      'S'           CNTPDR            1
071800941115     C                   MOVE      ARBPDC        KPDR
071900941115     C                   MOVE      'C'           KFVL
072000941114     C                   MOVE      ' '           KTSR
072100941114     C                   MOVE      FVVDFV        KDDC
072200011121     C     KFTT          CHAIN     fiftt02L                           95
072300011121     C   95KFTT          CHAIN     fifst02L                           95
072400941114     C     *IN95         IFEQ      '0'
072500941123     C                   Z-ADD     NRR1          NR1KEY
072600941114     C                   SETON                                        902840
072700941114     C                   MOVEL     ERR(9)        $MSG
072800941114     C                   GOTO      AGGS01
072900941114     C                   END
073000941114     C                   END
073100941114     C                   END
073200971104     C*  Controllo flag calcolo picking
073300941114     C     V1CPEP        IFEQ      ' '
073400941114     C                   Z-ADD     NRR1          NR1KEY
073500941123     C                   SETON                                        902844
073600941114     C                   MOVEL     ERR(4)        $MSG
073700941114     C                   GOTO      AGGS01
073800941114     C                   END
073900941114     C*
074000941123     C     V1CPEP        IFNE      V1HPEX
074100941114     C     V1CPEP        ANDNE     SAVPEP
074200941115     C                   MOVE      V1CPEP        SAVPEP            1
074300941123     C                   Z-ADD     NRR1          NR1KEY
074400941114     C                   SETON                                        902844
074500941114     C                   MOVEL     ERR(5)        $MSG
074600941114     C                   GOTO      AGGS01
074700941114     C                   END
074800030204     C*
074900030204     C*  Controllo flag calcolo carico
075000030204     C     v1cCAR        IFEQ      ' '
075100030204     C                   Z-ADD     NRR1          NR1KEY
075200030204     C                   SETON                                        902845
075300030204     C                   MOVEL     ERR(17)       $MSG
075400030204     C                   GOTO      AGGS01
075500030204     C                   END
075600030204     C*
075700030204     C     v1cCAR        IFNE      v1hCaX
075800030204     C     v1cCAR        ANDNE     SAVcar
075900030204     C                   MOVE      v1cCAR        SAVcar            1
076000030204     C                   Z-ADD     NRR1          NR1KEY
076100030204     C                   SETON                                        902845
076200030204     C                   MOVEL     ERR(18)       $MSG
076300030204     C                   GOTO      AGGS01
076400030204     C                   END
076500100210     C*  Carico dati distinta in schiere
076600100210     C     §NDC          LOOKUP    NDC                                    95
076700100210     C     *IN95         IFEQ      '0'
076800100210     C                   ADD       1             J
076900100210     C                   Z-ADD     §G15          G15(J)
077000100210     C                   Z-ADD     §NDC          NDC(J)
077100100210     C                   Z-ADD     §DDC          DTD(J)
077200100210     C                   Z-ADD     V1CDTC        DDC(J)
077300100210     C                   MOVEL     V1CHRC        HRC(J)
077400100210     C                   MOVE      V1CMNC        HRC(J)
077500100210     C                   MOVE      V1CFPP        FPP(J)
077600100210     C                   MOVE      V1CPEP        PEP(J)
077700100210     C                   MOVE      v1cCAR        Car(J)
077800100210     C                   END
077900941114     C*
078000941114     C                   END
078100941114     C*
078200941114     C     AGGS01        TAG
078300941114     C                   SETON                                        22
078400941115     C                   UPDATE    LR53S01
078500941114     C                   SETOFF                                       22
078600941116     C  N28              READC     LR53S01                                96
078700941114     C                   END
078800941124     C*  Se richiesta conferma controllo se ci sono delle distinte
078900941124     C*  da chiudere
079000950323     C  N28*INKF         IFEQ      '1'
079100941124     C                   XFOOT     NDC           WCTNDC            8 0
079200941124     C     WCTNDC        IFEQ      0
079300941124     C                   SETON                                        2890
079400941124     C                   MOVEL     ERR(13)       $MSG
079500941124     C                   END
079600941124     C                   END
079700941114     C* Aggiorno su archivio
079800941114?    C                   COMMIT
079900941114     C*
080000941115?    C     FINCT1        ENDSR
080100941115      *---------------------------------------------------------*
080200941115      *  Caricamento dati subfile                               *
080300941115      *---------------------------------------------------------*
080400941115     C     INZS02        BEGSR
080500941115      *
080600941115     C                   SETON                                        62
080700941115     C                   WRITE     LR53C02
080800941115     C                   WRITE     LR53C03
080900941115     C                   SETOFF                                       6228
081000941115     C                   Z-ADD     0             SET
081100941115     C*
081200941115     C                   Z-ADD     0             NRR2              4 0
081300941115     C                   Z-ADD     0             NRR3              4 0
081400941115     C                   MOVEL     APDRSC        V2DRSC
081500941115     C                   DO        5             I
081600941115     C     NDC(I)        IFNE      0
081700000608      *RICHIAMO PROGRAMMA CONTROLLO CONGRUENZA SPUNTE RIENTRO
081800080715      * solo se distinte no PDA
081900000608     C     DISSDA        IFEQ      *BLANK
082000080715     C     wpda          andeq     'N'
082100000608     C                   MOVEL     KPJBU         SAVJBU
082200020822     C                   Z-ADD     NDC(I)        NDC55
082300020822     C                   Z-ADD     v1clna        po55
082400020822     C                   MOVEL(p)  fnlr55        KPJBU
082500000608     C                   CALL      'FNLR55R2'
082600000608     C                   PARM                    KPJBA
082700000608     C                   MOVEL     SAVJBU        KPJBU
082800000608     C                   END
082900000608     C*
083000941115     C*  Blocco record FVV
083100941115     C                   Z-ADD     NDC(I)        KNFV
083200020822     C     KFVV          CHAIN     FNFVV01L                           95
083300070316 1   C                   IF        *in95 = '0' and fvvatb <> '*'
083400070316 2   c                   if        fvvfcf = ' '
083500941115     C                   MOVEL     'F'           FVVFCF
083600070316 x2  C                   EXCEPT    AGGFVV
083700030926     c                   else
083800030926     c                   unlock    fnfvv01l
083900030926     C                   SETON                                        40
084000070316 e2  c                   end
084100070316 x1  c                   else
084200030926     c                   unlock    fnfvv01l
084300030926     C                   SETON                                        40
084400070316 e1  C                   END
084500070316 1   c                   if        *in40
084600030926     C                   SETON                                        9028
084700030926     C                   Z-ADD     NRR1          NR1KEY
084800030926     C                   MOVEL     ERR(6)        $MSG
084900030926     C                   leave
085000070316e1   c                   end
085100941115     C*
085200941115     C                   COMMIT
085300030828     C*  Carico dati distinta
085400030828     c                   if        kcdaz = 'LR53'
085500030828     C                   EXSR      CARDIS
085600030828     c                   endif
085700030828     c
085800030828     C                   END
085900030926     C                   ENDdo
086000070316     C*
086100070316     C*  Blocco record Fidst
086200070316     C                   DO        5             I
086300070316     C     NDC(I)        IFNE      0
086400070316     C                   Z-ADD     NDC(I)        KNFV6
086500070410     C     Kdst          chain(e)  fidst09L                           95
086600070319     c                   if        %error
086700070521     C     Kdst          chain(n)  fidst09L                           95
086800070319     c                   exsr      geslck
086900070410     C     Kdst          chain(e)  fidst09L                           95
087000070319     c                   endif
087100070508 1   C                   IF        *in95 = '0' and dstatb = ' '
087200070508     c                   update    fidst000
087300070508 e1  C                   END
087400070316     C                   END
087500070316     C                   ENDdo
087600030828      * controllo l'azione per sapere se richiamare pgm per caricare WF
087700030926     c                   if        kcdaz = 'XR53' and not *in90
087800030902      *
087900030828     c                   z-add     1             i
088000030828     c                   call      'FNLR53C5'
088100030828     c                   parm      'O'           parflag           1
088200120529     c* carica fnarbj per distinte senza PDA
088300101119     c                   if        wpda = 'N'
088400030828     c                   movea     ndc           dis
088500030828     c                   move      v1clna        filges
088600030828     c                   call      'FNLR53R5'
088700030828     c                   parm                    parmwf
088800030901     c                   if        errwf <>'1'
088900120529     c                   open      fnarbj0f
089000941115     C*  Carico dati distinta
089100941115     C                   EXSR      CARDIS
089200030901     c                   endif
089300120529     c* carica fnarbj per distinte con PDA
089400080707     c                   else
089500101027     c                   movea     ndc           dis
089600101027     c                   move      v1clna        filges
089700101027     c                   call      'FNLR53R5'
089800101027     c                   parm                    parmwf
089900101118     c                   if        errwf = '1'
090000101118     c                   goto      errore
090100101118     c                   endif
090200101118      *
090300080707     c                   clear                   fnlr53ds7
090400080707     c                   move      v1clna        D537FGSI
090500080707     c                   z-add     ndc(1)        D537ND1I
090600080707     c                   z-add     ndc(2)        D537ND2I
090700080707     c                   z-add     ndc(3)        D537ND3I
090800080707     c                   z-add     ndc(4)        D537ND4I
090900080707     c                   z-add     ndc(5)        D537ND5I
091000080707     c                   call      'FNLR53R7'
091100080707     c                   parm                    fnlr53ds7
091200101027     c                   if        D537ERRO = ' ' and errwf <>'1'
091300120529     c                   open      fnarbj0f
091400080707     C*  Carico dati distinta
091500080707     C                   EXSR      CARDIS
091600080716     c                   else
091700080716     c                   move      '1'           errcs
091800080707     c                   endif
091900080707     c                   endif
092000080707      *
092100030828     c                   if        kcdaz = 'XR53'
092200120529     c                   close     fnarbj0f
092300030828     c                   call      'FNLR53C5'
092400030828     c                   parm      'C'           parflag
092500030828     c                   endif
092600101118     c     errore        tag
092700030901     c                   if        errwf ='1'
092800030901     c                   move      errwf         errcs
092900030901     c                   endif
093000941115     C*
093100030901     c                   endif
093200941115     C                   ENDSR
093300941115      *---------------------------------------------------------*
093400941115      *  Caricamento singole distinte                           *
093500941115      *---------------------------------------------------------*
093600941115     C     CARDIS        BEGSR
093700941115     C*
093800020822     C                   Z-ADD     NDC(I)        KNDC
093900960702     C                   Z-ADD     DTD(I)        G02DAT
094000960702     C                   MOVEL     *BLANK        G02ERR
094100960702     C                   CALL      'XSRDA8'
094200960702     C                   PARM                    WLBDA8
094300960702     C                   Z-ADD     G02INV        KDDC
094400020923     C                   Z-ADD     v1clna        KIFP
094500030828      * controllo l'azione per sapere se leggere wf o arb
094600030828     c                   if        kcdaz <> 'XR53'
094700020822     C     KARB          CHAIN     FNARB70L                           96
094800030828     c                   else
094900120529     c     *loval        setll     fnarbj0f
095000120529     c                   read      fnarbj0f                               96
095100030828     c                   endif
095200941115     C     *IN96         DOWEQ     '0'
095300031007     c                   if        kcdaz = 'XR53'
095400031007     c                   z-add     1             i
095500031007     c     arbndc        lookup    ndc(i)                                 77
095600031007     c  n77              seton                                        96
095700031007     c  n77              iter
095800031007     c                   endif
095900941115     C* Pulisco campi subfile
096000990923     C                   SETOFF                                       06
096100941115     C                   SETOFF                                       404142
096200941115     C                   SETOFF                                       434445
096300941118     C                   SETOFF                                       464748
096400941124     C                   SETOFF                                       080522
096500941207     C                   SETOFF                                       790703
096600941115     C                   CLEAR                   LR53S02
096700941115     C                   CLEAR                   LR53S03
096800941115     C                   Z-ADD     0             WCAS
096900941115     C                   Z-ADD     0             WIMA
097000941115     C                   MOVEL     *BLANKS       WVCA
097100941115     C                   MOVEL     *BLANKS       WTIC
097200941115     C* Incremento totale spedizioni x distinta
097300941115     C                   ADD       1             SET(I)
097400941115     C                   Z-ADD     NDC(I)        VNMDOC
097500941115     C                   Z-ADD     G15(I)        VH2G15
097600941115     C                   Z-ADD     DDC(I)        G02DAT
097700941115     C                   MOVEL     ' '           G02ERR
097800941115     C                   CALL      'XSRDA8'
097900941115     C                   PARM                    WLBDA8
098000941115     C                   MOVE      G02INV        VH2DDC
098100941115     C                   Z-ADD     DTD(I)        G02DAT
098200941115     C                   MOVEL     ' '           G02ERR
098300941115     C                   CALL      'XSRDA8'
098400941115     C                   PARM                    WLBDA8
098500941115     C                   MOVE      G02INV        VH2DDT
098600100112     c                   if        v02dtc = 0
098700100112     c                   movel     DDC(I)        v02dtc
098800100112     c                   end
098900110214     C*mm                MOVE      DISSDA        V2SDA
099000030901     C* Determino se la spedizione è già stata consegnata
099100941115     C                   MOVEL     'NO'          V2CCON
099200941115     C     ARBDCM        IFGT      0
099300941115     C                   MOVEL     'SI'          V2CCON
099400941115     C* Se solo incasso protetto codice mancata consegna
099500941115     C                   SETON                                        79
099600941115     C                   END
099700091221     c* per priority calcolo l'affidabilità bolla per obbligare
099800091216     c* inserimento ora consegna e segnalare se ritardo
099900091216     c                   if        arbtsp ='E'
100000091216     c                   clear                   tnsd99
100100091216     C                   MOVE      ARBAAS        D98AAS
100200091216     C                   MOVE      ARBLNP        D98LNP
100300091216     C                   MOVE      ARBNRS        D98NRS
100400091216     C                   MOVE      ARBNSP        D98NSP
100500091216     C                   CALL      'TNSD99R'
100600091216     C                   PARM                    TNSD99
100700091216     c                   eval      v2hDCI = D98DCI
100800091216     c                   eval      v2hHCI = D98HCI
100900091221     c                   else
101000091221     c                   clear                   v2hdci
101100091221     c                   clear                   v2hhci
101200091216     c                   end
101300941115     C*
101400120312     C                   clear                   V3CMIT
101500110330     C*mm                MOVEL     ARBRSM        V3CMIT
101600110330     C*mm  ARBRMO        IFNE      *BLANKS
101700110330     C*mm                MOVEL     ARBRMO        V3CMIT
101800110330     C*mm                END
101900131203     C                   z-add     ARBpdc        Vh2pdr
102000941121     C                   MOVEL     ARBRSD        V02DES
102100110908     C                   MOVEL     ARBRSD        Vh2rsd
102200110908     C                   MOVEL     ARBRSm        Vh2rsm
102300941116     C                   Z-ADD     ARBNTC        V2CNTC
102400941116     C                   Z-ADD     ARBAAS        V2CAAS
102500941115     C                   Z-ADD     ARBLNP        V2CLNP
102600030305     C                   Z-ADD     arbLNA        V2Clna
102700941115     C                   Z-ADD     ARBNRS        V2CNRS
102800941115     C                   Z-ADD     ARBNSP        V2CNSP
102900941115     C                   MOVEL     ARBCBO        V2CCBO
103000941115     C                   MOVEL     ARBGMA        V2CGMA
103100141202ab    * per testare in seguito se la riga è stata forzata a seguito di un errore
103200141202ab    * forzabile  (V2hFORZA)
103300141202ab   c                   clear                   v2hForza
103400071003     c                   clear                   v2harp
103500941214     C     V2CGMA        COMP      *BLANKS                                08
103600000503      *TESTO SE FILIALE POSTE NON ESEGUO FORZATURA C7
103700000503     C     ARBLNP        LOOKUP    FIP                                    23
103800000503     C     *IN23         IFEQ      '0'
103900000504     C                   MOVE      *BLANK        V2POST
104000990419     C     ARBGMA        IFEQ      *BLANKS
104100990419     C                   MOVEL     'C7'          V2CGMA
104200000427     C                   MOVEL     'C7'          ARBGMA
104300990423     C                   MOVEL     '1'           V2IN09
104400990422     C                   SETON                                        09
104500990422     C                   ELSE
104600990423     C                   MOVEL     '0'           V2IN09
104700990422     C                   SETOFF                                       09
104800990419     C                   END
104900000504      *INSERISCO FLAG PER SEGNALARE BOLLA POSTE CAMPO NASCOSTO
105000000504     C                   ELSE
105100000504     C                   MOVE      'P'           V2POST
105200000503     C                   END
105300000504      *
105400941115     C                   MOVEL     ARBMGS        MM                2 0
105500941115     C                   MOVE      ARBMGS        GG                2 0
105600941115     C                   MOVEL     GG            V2CGMS
105700941115     C                   MOVE      MM            V2CGMS
105800941115     C                   Z-ADD     ARBSTP        V02STP
105900010402     C* CONTROLLO ESISTENZA EVENTO DI COMUNICAZIONE CONSEGNA
106000010402     C* SE ESISTE IMPOSTO GIORNO MESE ORA MINUTO DI CONSEGNA DA EVENTO
106100010402     C                   EXSR      EVECON
106200010402     C*
106300941115     C* Controllo se esiste bolla mamma
106400941115     C                   Z-ADD     ARBAAS        KAAS
106500941115     C                   Z-ADD     ARBLNP        KLNP
106600941115     C                   Z-ADD     ARBNRS        KNRS
106700941115     C                   Z-ADD     ARBNSP        KNSP
106800941115     C     KSPE          CHAIN     FNLBL01L                           76
106900941115     C     *IN76         IFEQ      '0'
107000941115     C     LBLLAN        ANDEQ     LBLLAP
107100980429     C                   Z-ADD     LBLAAP        KAAS
107200980429     C                   Z-ADD     LBLLPP        KLNP
107300980429     C                   Z-ADD     LBLNRP        KNRS
107400980429     C                   Z-ADD     LBLNSP        KNSP
107500980429     C     KSPE          CHAIN     FNARB01L                           76
107600980429     C* Memorizzo il legame esistente per i successivi accessi su FNARB
107700980429     C     *IN76         IFEQ      '0'
107800980429     C     ARBCCA        ANDNE     '2'
107900980429     C     ARBCCA        ANDNE     '6'
108000980429     C                   Z-ADD     LBLAAP        VH2AAP
108100980429     C                   Z-ADD     LBLLPP        VH2LPP
108200980429     C                   Z-ADD     LBLNRP        VH2NRP
108300980429     C                   Z-ADD     LBLNSP        VH2NSP
108400941115     C                   END
108500980429     C                   Z-ADD     LBLAAN        KAAS
108600980429     C                   Z-ADD     LBLLPN        KLNP
108700980429     C                   Z-ADD     LBLNRN        KNRS
108800980429     C                   Z-ADD     LBLNSN        KNSP
108900980429     C     KSPE          CHAIN     FNARB01L                           76
109000090218     C                   END
109100941115     C*  controllo se c'è il contrassegno
109200941115     C                   Z-ADD     1             X
109300941115     C                   CLEAR                   DS3A
109400941115     C     ARBCBO        LOOKUP    C3A(X)                                 95
109500941115     C     *IN95         IFEQ      '1'
109600950116     C                   MOVEL     D3A(X)        DS3A
109700941115     C                   END
109800941115     C*  Controllo se c'è assegnato
109900941115     C     ARBICA        IFEQ      ' '
110000941115     C     ARBICA        OREQ      'R'
110100941115     C     ARBFIP        OREQ      'I'
110200951206     C                   MOVEL     §3ATB1        WPORTO            1
110300941115     C     WPORTO        IFEQ      'A'
110400990922     C                   MOVEL     '1'           KTRC
110500990922     C                   ELSE
110600990922     C                   MOVEL     '2'           KTRC
110700990922     C                   END
110800990922     C     KAR6          CHAIN     FIAR601L                           95
110900990922     C     AR6DFT        IFNE      *ZEROS
111000990922     C     *IN95         ANDEQ     '0'
111100990922     C                   Z-ADD     AR6IFT        WIMA
111200990922     C                   MOVEL     AR6DIV        WDIV
111300990922     C* Loop su file incassi parziali x determinare importo da inc.
111400990922     C                   MOVEL     'A'           KTIP
111500990922     C     KARI          CHAIN     FIARI01L                           95
111600990922     C     *IN95         DOWEQ     '0'
111700990927     C     AR6DIV        IFEQ      ARIDIV
111800990922     C                   SUB       ARIIPP        WIMA
111900991012     C                   ELSE
112000990927     C* Se la divisa di incasso è diversa converto importo già
112100990927     C* incassato nella divisa della bolla
112200990927     C                   CLEAR                   YEURDS
112300990927     C                   MOVEL     ARIDIV        YECDVI
112400990927     C                   Z-ADD     ARIIPP        YECIMI
112500990927     C                   MOVEL     AR6DIV        YECDVO
112600990927     C                   MOVEL     'H'           YECTAR
112700990927     C                   CALL      'YEURCO'
112800990927     C                   PARM                    YEURDS
112900990927     C                   SUB       YECIMO        WIMA
113000990927     C                   END
113100990922     C     KARI          READE     FIARI01L                               95
113200990922     C                   END
113300950620     C                   Z-ADD     AR6DFT        V3CDFT
113400951206     C                   MOVEL     §3ATB1        V2CPOR
113500951206     C                   END
113600941115     C*
113700941123     C                   ELSE
113800941219     C                   MOVE      'S'           V2CICA
113900941115     C                   END
114000941115     C* Metto la 'V' in v02sel se per la bolla è prevista la gestione
114100941115     C* della firma del destinatario
114200941115     C     ARBGMA        IFNE      *BLANKS
114300941115     C                   Z-ADD     1             X
114400941115     C                   CLEAR                   DS7R
114500960119     C                   SETON                                        08
114600941115     C     ARBGMA        LOOKUP    C7R(X)                                 95
114700951206     C     *IN95         IFEQ      '1'
114800960119     C                   SETOFF                                       08
114900951206     C                   MOVEL     D7R(X)        DS7R
115000951206     C                   MOVEL     §7R4TM        V2C4TM
115100951206     C                   Z-ADD     §7R4NT        V2C4NT
115200960116     C                   MOVEL     §7R1BF        V2C1BF
115300120518     C                   MOVEL     §7Rvpcd       V2Cvpcd
115400941115     C                   END
115500941115     C     §7R2FI        IFEQ      'S'
115600941115     C                   MOVEL     'V'           V02SEL
115700941115     C                   END
115800941115     C                   END
115900941115     C*  Controllo se c'è firmatario
116000941115     C                   MOVEL     '1'           KTRC
116100060214     C     KAR4          CHAIN     FiAR401L                           95
116200941115     C  N95              MOVEL     AR4NOT        V2CFIR
116300941115     C*  Controllo se ci sono delle note relative alla consegna
116400941115     C                   MOVEL     '0'           KTRC
116500060214     C     KAR4          CHAIN     FiAR401L                           95
116600941115     C  N95              MOVEL     AR4NOT        V2CNT1
116700941115     C* Memorizzo se c'è precedente consegna parziale
116800941115     C     ARBDCP        IFGT      0
116900941115     C                   MOVEL     'S'           V02PRZ
117000941115     C                   END
117100000512     C*  Controllo se LE SPEDIZIONI SONO AUTOGENERATE
117200000503     C                   SETOFF                                       24
117300000503     C                   MOVEL     'A'           KTRC
117400060214     C     KAR4          CHAIN     FiAR401L                           95
117500000503     C  N95              MOVEL     AR4NOT        DSBL4A
117600000503     C  N95§B4ASO        COMP      '&'                                    24
117700000522     C   24              MOVE      '1'           V2IN24
117800000522     C  N24              MOVE      '0'           V2IN24
117900941115     C*
118000941115     C                   MOVEL     ARBGC1        V021GC
118100941115     C                   MOVEL     ARBGC2        V022GC
118200941115     C* muovo eventuale tipo consegna particolare
118300941115     C                   MOVEL     ARBTC1        V2CCP1
118400941115     C                   MOVEL     ARBTC1        V2HCP1
118500941115     C                   MOVEL     ARBTC2        V2CCP2
118600941115     C                   MOVEL     ARBTC2        V2HCP2
118700950206     C*
118800941118     C                   MOVE      ARBTCR        V2CTCR
118900941118     C                   MOVE      ARBDCR        V2CDCR
119000941118     C                   MOVE      ARBHCR        V2CHCR
119100091214     c                   move      arbtsp        v02tsp
119200091026     c                   clear                   v02fhob
119300091216     c                   if        arbtsp = 'H' or arbtsp ='E'
119400091026     c                   move      arbtsp        v02fhob
119500091026     c                   endif
119600051215      * richiamo pgm per informazione relativamente alla data cons. rich.
119700051215      * se TASSATIVA
119800051215     c                   clear                   v2ctat
119900051215     c                   clear                   fnlv80ds
120000051215     c                   z-add     arbAAS        ILV80AAS
120100051215     c                   z-add     arbLNP        ILV80LNP
120200051215     c                   z-add     arbNRS        ILV80NRS
120300051215     c                   z-add     arbNSP        ILV80NSP
120400051215     c                   z-add     arbMGS        ILV80MGS
120500051215     c                   move      arbTCR        ILV80TCR
120600051215     c                   z-add     arbDCR        ILV80DCR
120700051215     c                   z-add     arbHCR        ILV80HCR
120800051215     c                   move      arbTC1        ILV80TC1
120900051215     c                   move      arbTC2        ILV80TC2
121000051215     c                   call      'FNLV80R'
121100051215     c                   parm                    kpjba
121200051215     c                   parm                    fnlv80ds
121300051215     c                   move      olv80tat      v2ctat
121400051215      *
121500000615     C*  SE DISTINTA SDA :
121600000615     C*  ANNULLO CONTRASSEGNO DALLA SPEDIZIONE
121700000615      * FORZO 0 NEL CAMPO §3AFCA (FLAG ESISTENZA C/ASS) SE RIUSCITO
121800000615      * ANNULLAMENTO.   NON  CI SI PREOCCUPA SE IL C/ASS È SULLA MAMMA O
121900000615      * SULLA FIGLIA IN QUANTO PER LE POSTE NON CI SONO CAMBI PORTO ETC..
122000000531     C     DISSDA        IFEQ      'S'
122100000531     C     §3AFCA        ANDGT     0                                            --- 01 ---
122200000531     C                   CLEAR                   DS48
122300000531     C                   CLEAR                   DSARBK
122400000601     C                   MOVE      KAAS          D48AAS
122500000531     C                   MOVE      KLNP          D48LNP
122600000531     C                   MOVE      KNRS          D48NRS
122700000531     C                   MOVE      KNSP          D48NSP
122800000531     C                   MOVE      'KB'          D48CVB
122900000531     C                   MOVE      'E'           D48FFR
123000000531     C                   MOVE      'K'           D48TRC
123100000531     C                   MOVE      'A'           D48TBO
123200000531     C                   MOVE      *BLANK        §BKCBN
123300000531     C                   MOVE      *BLANK        §BKTIC
123400000531     C                   Z-ADD     0             §BKCAS
123500000531     C                   MOVEL     DS48          KPJBU
123600000531     C                   CALL      'FNLR48R'
123700000531     C                   PARM                    KPJBA
123800000531     C                   PARM                    DSARBD
123900000531     C                   PARM                    DSARBK
124000000531     C                   PARM                    DARBT
124100000531     C                   PARM                    DSARBG
124200030718     C                   PARM                    trul90ds
124300000531     C                   MOVEL     KPJBU         DS48
124400000615      * SE NON C'È ERRORE FORZO CHE NON C'È CONTRASSEGNO
124500000531     C     D48ERR        IFEQ      *BLANK
124600000531     C                   Z-ADD     0             §3AFCA
124700000601     C                   ELSE
124800000601     C                   MOVE      'X'           ERRCS             1
124900000531     C                   END
125000000531     C                   ENDIF
125100000615      *  SOLO PER DISTINTA SDA SI DEVE FORZARE IL COD. CONS. ANOMALA
125200000615     C     DISSDA        IFEQ      'S'
125300000615     C     ARBFFD        IFEQ      'S'
125400000615     C                   MOVE      'H'           V02CCA
125500000615     C                   END
125600000615      * CONTROLLO SE DETERMINO IL COD. CONS. ANOMALA DAGLI EVENTI
125700000615     C                   EXSR      EVENTI
125800000615     C                   END
125900000615      *
126000941115     C*
126100941123     C     §3AFCA        IFGT      0                                            --- 01 ---
126200941206     C     ARBICC        IFEQ      ' '
126300941206     C     ARBICC        OREQ      'R'
126400941115     C* Controllo se C/Assegno sulla figlia
126500051115     C     KSPE          CHAIN     FIAR901L                           95
126600941123     C     *IN95         IFEQ      '1'                                          --- 02 ---
126700941115     C* Controllo se C/Assegno sulla mamma
126800941206     C     VH2AAP        IFNE      0
126900941115     C                   Z-ADD     LBLAAP        KAAP
127000941115     C                   Z-ADD     LBLLPP        KLPP
127100941115     C                   Z-ADD     LBLNRP        KNRP
127200000615     C                   Z-ADD     LBLNSP        KNSPP
127300941115     C     KARB1         CHAIN     FNARB01L                           95
127400950222      * CONTROLLO CHE IL C/ASS. SULLA MAMMA NON SIA GIA' INCASSATO
127500950222     C  N95ARBICC        IFNE      ' '
127600950222     C     ARBICC        ANDNE     'R'
127700950222     C                   MOVEL     'S'           V2CICC
127800950222     C                   SETON                                        95
127900950222     C                   ENDIF
128000950222      *
128100140911      *? attenzione ad utilizzare i campi ARB da qui in poi perchè hanno il valore della mamma
128200140911      *?se presente per i riferimenti della spedizione in consegna usare i campi del video
128300051115     C  N95KARB1         CHAIN     FIAR901L                           95
128400941206     C                   END
128500941206     C                   END                                                    --- 02 ---
128600941123     C     *IN95         IFEQ      '0'                                          --- 02 ---
128700941115     C                   Z-ADD     AR9CAS        WCAS
128800941115     C                   MOVEL     AR9VCA        WVCA
128900941115     C                   MOVEL     AR9TIC        WTIC
129000941123     C                   END                                                    --- 02 ---
129100061108     C* Carico cliente x controllo tabella "TM"
129200061108     C     ARBCCM        IFGT      *ZEROS
129300061108     C                   MOVE      ARBCCM        §CLITM
129400941115     C                   ELSE
129500941115     C                   MOVE      ARBKSC        §CLITM
129600941115     C                   END
129700941123     C                   ELSE
129800941219     C                   MOVE      'S'           V2CICC
129900941123     C                   END                                                    --- 01 ---
130000941206     C                   END                                                    --- 01 ---
130100061127     C* salvo il codice cliente perchè se 9999 e no cod. fisc. richiamo
130200061127     c* la manutenzione bolle per inserirlo
130300061127     C                   eval      savksc = ARBksc
130400061127     C                   clear                   savcf
130500021107      *
130600941115     C* Conto i colli ancora da consegnare
130700941115     C*  Colli su figlia
130800941115     C     KSPE          CHAIN     FNART01L                           95
130900941115     C     *IN95         IFEQ      '0'
131000941115     C     *IN95         DOWEQ     '0'
131100941115     C     ARTDCM        IFEQ      0
131200941124     C                   ADD       1             V02NCL
131300021107      *
131400000427      *ROUTINE PER GESTIONE SPUNTE SU FOGLIO 8 MANCATA CONSEGNA
131500101119      *  richiama se distinta non PDA
131600101119     c                   if        wpda = 'N'
131700000427     C                   EXSR      MANCON
131800080708     c                   endif
131900941115     C                   END
132000941124     C     KSPE          READE     FNART01L                               95
132100941115     C                   END
132200941123     C                   ELSE
132300941115     C*  Colli su mamma
132400941206     C     VH2AAP        IFNE      0
132500941206     C                   Z-ADD     LBLAAP        KAAP
132600941206     C                   Z-ADD     LBLLPP        KLPP
132700941206     C                   Z-ADD     LBLNRP        KNRP
132800000615     C                   Z-ADD     LBLNSP        KNSPP
132900941115     C     KARB1         CHAIN     FNART01L                           95
133000941115     C     *IN95         DOWEQ     '0'
133100941115     C     ARTDCM        IFEQ      0
133200941124     C                   ADD       1             V02NCL
133300021107      *
133400000427      *ROUTINE PER GESTIONE SPUNTE SU FOGLIO 8 MANCATA CONSEGNA
133500101119      * richiama se distinta non PDA
133600101119     c                   if        wpda  = 'N'
133700000427     C                   EXSR      MANCON
133800080708     c                   endif
133900941115     C                   END
134000941124     C     KARB1         READE     FNART01L                               95
134100941115     C                   END
134200941206     C                   END
134300941206     C*
134400941124     C                   END                                                    Colli su figlia
134500031013      * se chisura con spunta forzo § per evidenziare le bolle non spuntate
134600031013     c                   if        kcdaz ='XR53' and arbtpr = 'A'and
134700031014     c                             arbprg = 0 and v2ccon = 'NO'
134800031013     c                   move      '§'           v02ccn
134900031013     c                   endif
135000031013     c
135100941115     C*
135200941206     C     V02NCL        IFNE      0
135300941115     C                   Z-ADD     ARTAAS        HARAAS
135400941115     C                   Z-ADD     ARTLNP        HARLNP
135500941115     C                   Z-ADD     ARTNRS        HARNRS
135600941115     C                   Z-ADD     ARTNSP        HARNSP
135700941117     C                   Z-ADD     ARTFLS        V2CFLS
135800941206     C                   END
135900941206     C                   ADD       1             NRR2
136000021107     C*
136100941130     C*  Routine per determinazione volume da utilizzare
136200941130     C                   EXSR      DETVOL
136300021107      *
136400941130     C* Determino peso e volume ancora da consegnare
136500021107      * ----------------------------------------------
136600021107      *   (Prima prendeva solo il peso da Peso Fatturazione)
136700021107      *   adesso in base al calcolo della routine (detPES) prende il peso
136800021107      *    da fatturazione o da VDL in base al maggiore dei 2 x poter
136900021107      *    calcolare il peso Medio da attribuire ai colli che non sono passati
137000021107      *    sul VDL.
137100021107      *
137200021107      *  Routine per determinazione peso da utilizzare
137300021107     C                   EXSR      DETpes
137400021107      *
137500021107      * nuovo calcolo del peso:
137600021107     C**** ARBPKF        MULT      V02NCL        APPOG            18 3
137700021107     C     Wpes          MULT      V02NCL        APPOG            18 3
137800941202     C     APPOG         DIV(H)    ARBNCL        V02PKG
137900021107      *
138000021107     C     WVOL          MULT      V02NCL        APPOG            18 3
138100021107     C     APPOG         DIV(H)    ARBNCL        V02VOL
138200080714      *solo per distinte PDA
138300080714     c* imposto tipo incasso contrassegno e assegnato da fipctCET
138400080714     c* verifico se esiste riga in FIPCT00F relativa alla bolla
138500131203     c                   setoff                                       52
138600101119     c                   if        wpda = 'S'
138700080714     c                   clear                   fipctcetds
138800140911     C                   Z-ADD     vnmdoc        PCTNDC
138900131203      * verifico se per la chiave bolla distinta esiste un evento cep abbinato
139000131203      *nel caso carico i campi hidden per effettuare i controlli
139100131203     c     kcetcep       chain     fipct02l
139200131203     c                   if        %found(fipct02l)
139300131203     c                   movel     pctdati       fipctcetds
139400131203     c                   seton                                        52
139500131203     c                   endif
139600131203     c                   movel     §PCTORA       w04ora            4 0
139700131205     c                   movel     w04ora        vh2hco
139800131205     c                   move      w04ora        vh2mco
139900131203     c                   move      §pctcmc       vh2evcep
140000131203     c                   clear                   fipctcetds
140100080714     c     kcet          setgt     fipct02l
140200080714     c     kcet          readpe    fipct02l
140300080714     c                   if        not %eof(fipct02l)
140400080714     c                   movel     pctdati       fipctcetds
140500080714     c                   endif
140600080718     c* imposta campi data e ora consegna reali se consegnata o parziale
140700101123     c                   move      §PCTcmc       V02CCN
140800131205     c                   move      §PCTcmc       Vh2CCN
140900131205     c                   move      §PCTflgspc    Vh2spc
141000101123     c                   z-add     1             xy                3 0
141100101123     c     §pctcmc       lookup    c2a(xy)                                92
141200101123     c                   if        *in92
141300101123     c                   movel     d2a(xy)       ds2a
141400101123     c                   if        §2aflg = '§' and §2acpt = 'S'
141500101123     c                   move      'T  '         v02ccn
141600131205     c                   move      'T  '         vh2ccn
141700101123     c                   endif
141800101123     c                   endif
141900101014     c*                  z-add     §PCTDCR       V2CDCR
142000101014     c*                  z-add     §PCTHCR       V2CHCR
142100080723     c                   if        §pctdata > 0
142200080714     c                   move      §PCTDATA      dataisoc
142300080714     c                   move      dataisoc      dataeurc
142400080714     c                   move      dataeurc      wv02dtc           8 0
142500080714     c                   movel     wv02dtc       v02dtc
142600131203     c                   movel     §PCTORA       w04ora            4 0
142700131203     c                   move      w04ora        v02mco
142800131203     c                   movel     w04ora        v02hco
142900080723     c                   endif
143000080714     c                   move      §PCTRISERV    v02ris
143100110215      * se inserita da autista mette in reverse campo tipo consegna particolare x ora solo PIANI
143200110215     c                   setoff                                       2125
143300110215     c                   if        §pcttc1 <> *blank
143400101203     c                   if        v2ccp1 = *blank
143500080714     c                   move      §PCTTC1       V2CCP1
143600110215     c                   seton                                        21
143700101203     c                   else
143800101203     c                   if        v2ccp2 = *blank
143900101203     c                   move      §PCTTC1       V2CCP2
144000110215     c                   seton                                        25
144100101203     c                   endif
144200101203     c                   endif
144300110215     c                   endif
144400101215     c                   if        v2ccp1 = v2ccp2
144500101215     c                   clear                   v2ccp2
144600101215     c                   endif
144700110214      * prepara flag per scrittura file spia in fnlr53 utilizza v2sda vecchio flag per
144800110214      * riconoscere distinte SDA non più utilizzato
144900110214      * verifica che non ci sia spunta in predistinta e dichiarazione di raccolta firma su PDA
145000110214     c                   clear                   v2sda
145100110214     c                   if        arbscn = ' '
145200110214     c                   movel     'L'           v2sda
145300110214     c                   endif
145400101111      * se non ho già un motivo di turno verifico che non sia stato effettuato un doppio tentativo
145500101111      * testando il campo del "tentativo precedente"
145600101123     c                   if        §pctcmcp <> *blank and v02ccn <> 'T  '
145700131209     c                   z-add     1             xy
145800101123     c     §pctcmcp      lookup    c2a(xy)                                92
145900101123     c                   if        *in92
146000101123     c                   movel     d2a(xy)       ds2a
146100101123     c                   if        §2acpt = 'S'
146200101123     c                   movel     'T  '         v2ctur
146300101123     c                   endif
146400101111     c                   endif
146500101123     c                   endif
146600080714      * se presente nome firmatario rilevato sul PDA lo preimposto
146700080714     c                   if        §PCTNOMFIR <> *blank
146800080714     c                   movel     §PCTNOMFIR    v2cfir
146900080714     c                   endif
147000080714      * se presente nota alla consegna la preimposto
147100080717     c                   if        §PCTNOTAUT <> *blank
147200080717     c                   movel     §PCTNOTAUT    v2cnt1
147300090216     c                   clear                   v02sel
147400090220     c                   clear                   v02selb
147500090216     c                   endif
147600080714     c                   endif
147700021107      *
147800941115     C*  Scrivo record importi
147900941115     C     WIMA          IFNE      0
148000941115     C     WCAS          ORNE      0
148100120312     c                   movel     v2clnp        comlnp
148200120313     c                   if        v2cnrs <> 0
148300120312     c                   movel     v2cnrs        comnrs
148400120313     c                   else
148500120313     c                   clear                   comnrs
148600120313     c                   end
148700120312     c                   movel     v2cnsp        comnsp
148800120312     c                   eval      v3calfa1 = comlnp + ' ' + comnrs + ' ' +
148900120312     c                             comnsp
149000120312     c                   clear                   v3calfa2
149100110214     C                   MOVE      V2POST        V3POST
149200941123     C                   Z-ADD     V2CAAS        V3CAAS
149300941115     C                   Z-ADD     V2CLNP        V3CLNP
149400030305     C                   Z-ADD     v2cLNA        v3cLNA
149500941115     C                   Z-ADD     V2CNRS        V3CNRS
149600941115     C                   Z-ADD     V2CNSP        V3CNSP
149700941115     C                   MOVEL     V2CCBO        V3CCBO
149800941115     C                   MOVEL     V2CCON        V3CCON
149900941115     C                   MOVEL     V02DES        V3CDES
150000941115     C     NRR3          ADD       1             V2CNRD
150100941118     C                   MOVEL     *BLANKS       V3CTOT
150200941121     C                   Z-ADD     NRR2          V3CNRI
150300941207     C                   SETON                                        22
150400941118     C*  Controllo se esiste C/Assegno
150500941118     C     WCAS          IFGT      0
150600941118     C                   ADD       1             NRR3
150700941118     C                   MOVEL     'C/A'         V3DTPI
150800941118     C                   Z-ADD     WCAS          V3CIMP
150900941118     C                   MOVEL     WVCA          V3CVCA
151000990924     C                   MOVEL     WVCA          VH3DIV
151100120312     c* ragione sociale intestatario assegno.
151200110330     c     kar5          chain     fiar501l
151300110330     c                   if        %found(fiar501l)
151400110330     c                   movel     ar5uni        dar5gen
151500110330     c                   reset                   trulintds
151600110330     c                   eval      iINTtic  = wtic
151700110330     c                   eval      iINTrscM = arbrsm
151800110330     c                   eval      iINTrscO = arbrmo
151900110330     c                   eval      iINTcdi  = §AR5cdi
152000110330     c                   call      'TRULINTR'
152100110330     c                   parm                    TRULINTDS
152200110330     c                   if        oINTerr = *blank
152300110330     c                   eval      v3cmit  = oINTrsci
152400120312     c                   eval      v3calfa1 = oINTrsci
152500120312     c                   eval      v3calfa2 = comlnp + ' ' + comnrs + ' ' +
152600120312     c                             comnsp
152700110330     c                   end
152800120312     c                   end
152900991006     C* Gestisco riga di totale solo se esiste anche assegnato
153000941118     C     WIMA          IFGT      0
153100991006     C* .. & la divisa di incasso assegnato è quella corrente...
153200011009     C     WDIV          IFEQ      §GEDCR
153300991006     C* .. o quella alternativa ..
153400991006     C     WDIV          OREQ      DIVEUR
153500991006     C* .. & la divisa di incasso C/Assegno è quella corrente...
153600011009     C     WVCA          IFEQ      §GEDCR
153700991006     C* .. o quella alternativa ..
153800991006     C     WVCA          OREQ      DIVEUR
153900941118     C                   MOVEL     'SI'          V3CTOT
154000941118     C                   END
154100991006     C                   END
154200991006     C                   END
154300941118     C                   MOVEL     WTIC          V3CTIC
154400101119     c                   if        wpda = 'S'
154500101119     c                   move      §PCTINCCAS    vh3ctic
154600080708     c*spedizione consegnata ma non incassata  contrassegno
154700090126     c                   if        §pctinccas = *blank or
154800090126     c                             §pctinccas = 'N'
154900110208     c                   move      'R'           v02sn1
155000090126     c                   endif
155100080708     c                   endif
155200990923     C* Se tipo incasso non prevede divisa proteggo campo relativo
155300990923     C                   Z-ADD     1             X
155400990923     C     WTIC          LOOKUP    C1A(X)                                 95
155500990923     C                   MOVEL     D1A(X)        DS1A
155600991115     C     §1AFMB        COMP      'M'                                    06
155700011009     C  N06WVCA          IFNE      §GEDCR
155800991116     C     WVCA          ANDNE     DIVEUR
155900991116     C                   SETON                                        06
156000991116     C                   END
156100941118     C                   WRITE     LR53S03
156200941207     C                   SETON                                        03
156300941118     C                   END
156400941115     C*  Controllo se esiste Assegnato
156500941115     C     WIMA          IFGT      0
156600941115     C                   ADD       1             NRR3
156700941115     C                   MOVEL     'Ass'         V3DTPI
156800941115     C                   Z-ADD     WIMA          V3CIMP
156900990923     C                   MOVEL     WDIV          V3CVCA
157000990924     C                   MOVEL     WDIV          VH3DIV
157100941123     C                   MOVEL     ARBMIF        V3CTIC
157200080708     c                   move      §PCTINCFAT    vh3ctic
157300101119     c                   if        wpda = 'S'
157400101119     c                   move      §PCTINCFAT    vh3ctic
157500080708     c*spedizione consegnata ma non incassata  assegnato
157600080708     c                   if        §PCTCMC = *blank and
157700080708     c                             (§PCTINCFAT = *blank or
157800080708     c                             §PCTINCFAT = 'N')
157900110208     c                   move      'R'           v02sn1
158000080708     c                   endif
158100080708     c                   endif
158200990927     C                   SETOFF                                       06
158300941115     C                   WRITE     LR53S03
158400941115     C                   MOVEL     *BLANKS       V3CDES
158500941115     C                   END
158600991006     C*  Se esitono entrambi verifico se scrivere riga totale:
158700941115     C     WCAS          IFGT      0
158800941115     C     WIMA          ANDGT     0
158900991006     C* .. Divisa di incasso assegnato è quella corrente...
159000011009     C     WDIV          IFEQ      §GEDCR
159100991006     C* .. o quella alternativa ..
159200991006     C     WDIV          OREQ      DIVEUR
159300991006     C* .. & la divisa di incasso C/Assegno è quella corrente...
159400011009     C     WVCA          IFEQ      §GEDCR
159500991006     C* .. o quella alternativa ..
159600991006     C     WVCA          OREQ      DIVEUR
159700990928     C                   SETON                                        05
159800941115     C                   ADD       1             NRR3
159900011009     C                   MOVEL     §GEDCR        V3DTPI
160000991006     C* Se l'assegnato non è in moneta di conto converto e sommo
160100991006     C* il risultato nel totale
160200011009     C     WDIV          IFNE      §GEDCR
160300991006     C                   CLEAR                   YEURDS
160400991006     C                   MOVEL     WDIV          YECDVI
160500991006     C                   Z-ADD     WIMA          YECIMI
160600011009     C                   MOVEL     §GEDCR        YECDVO
160700991006     C                   MOVEL     'H'           YECTAR
160800991006     C                   CALL      'YEURCO'
160900991006     C                   PARM                    YEURDS
161000991006     C                   Z-ADD     YECIMO        V3CIMP
161100991006     C                   ELSE
161200991006     C                   Z-ADD     WIMA          V3CIMP
161300991006     C                   END
161400991006     C* Se il C/Assegno non è in moneta di conto converto e sommo
161500991006     C* il risultato nel totale
161600011009     C     WVCA          IFNE      §GEDCR
161700991006     C                   CLEAR                   YEURDS
161800991006     C                   MOVEL     WVCA          YECDVI
161900991006     C                   Z-ADD     WCAS          YECIMI
162000011009     C                   MOVEL     §GEDCR        YECDVO
162100991006     C                   MOVEL     'H'           YECTAR
162200991006     C                   CALL      'YEURCO'
162300991006     C                   PARM                    YEURDS
162400991007     C                   ADD       YECIMO        V3CIMP
162500991006     C                   ELSE
162600991007     C                   ADD       WCAS          V3CIMP
162700991006     C                   END
162800011009     C                   MOVEL     §GEDCR        V3CVCA
162900011009     C                   MOVEL     §GEDCR        VH3DIV
163000941115     C                   MOVEL     *BLANKS       V3CTIC
163100120313     C                   MOVEL     *BLANKS       V3Calfa1
163200120313     C                   MOVEL     *BLANKS       V3Calfa2
163300941115     C                   WRITE     LR53S03
163400941115     C                   END
163500991006     C                   END
163600991006     C                   END
163700941115     C*
163800941115     C                   Z-ADD     NRR3          V2CNRA
163900941115     C                   END
164000101015     c* se la distinta è con PDA accendo il sflnexchg perchè un esito devo
164100101015     c* averlo ricevuto
164200101119     c                   if        wpda = 'S'
164300101119     c*mm                if        (§DSTTSTPDA = *blank
164400101119     c*mm                          or §DSTTSTPDA = 'O') and
164500101119     c*mm                          (dstpda = 'E' or dstpda = 'C')
164600101015     C                   SETOn                                        22
164700101015     c                   else
164800101015     C                   SETOFF                                       22
164900101015     c                   end
165000941115     C                   WRITE     LR53S02
165100031017     c                   if        v2ccon = 'NO' and d53ita <> 'E'
165200031016     c                   exsr      eventimic
165300031016     c                   endif
165400941115     C*
165500030828      * controllo l'azione per sapere se leggere wf o arb
165600030828     c                   if        kcdaz <> 'XR53'
165700020822     C     KARB          READE     FNARB70L                               96
165800030828     c                   else
165900120529     C                   READ      fnarbj0f                               96
166000030828     c                   endif
166100941115     C                   END
166200941115     C*
166300000601     C                   ENDSR
166400000602      *---------------------------------------------------------*
166500000615      *  Richiamo EVENTI PER ATTRIBUZIONE COD.CONS.ANOMALA      *
166600000602      *---------------------------------------------------------*
166700000602     C     EVENTI        BEGSR
166800000615      * CERCO GLI EVENTI CON NR SPEDIZ. CHE SI È IN DISTINTA
166900000615      * IN QUANTO NON SI PREVEDONO LEGAMI
167000000615      *
167100000615     C                   Z-ADD     KAAS          KAASE
167200000615     C                   Z-ADD     KLNP          KLNPE
167300000615     C                   Z-ADD     KNRS          KNRSE
167400000615     C                   Z-ADD     KNSP          KNSPE
167500000615     C     KEVB          SETGT     FNEVB05L
167600000602     C                   DO        *HIVAL
167700000615     C     KEVB          READPE    FNEVB05L                               99
167800000602     C   99              LEAVE
167900000602     C                   Z-ADD     1             X
168000000602     C     EVBCEV        LOOKUP    C2Z(X)                                 98
168100000614     C     *IN98         IFEQ      '1'
168200000602     C                   MOVEL     D2Z(X)        DS2Z
168300000602     C     §2ZCCA        IFNE      *BLANK
168400000602     C                   MOVE      §2ZCCA        V02CCA
168500000602     C                   LEAVE
168600000602     C                   ELSE
168700000602     C                   ITER
168800000602     C                   END
168900000602     C                   END
169000000602     C                   ENDDO
169100000602     C                   ENDSR
169200010402      *---------------------------------------------------------*
169300010402      *  Richiamo EVECON PER ATTRIBUZIONE COMUNICAZIONE CONSEGNA*
169400010402      *---------------------------------------------------------*
169500010402     C     EVECON        BEGSR
169600010402      * CERCO GLI EVENTI CON NR SPEDIZ. E TIPO EVENTO COC
169700010402      *
169800010402     C                   SETOFF                                       97
169900131106     C                   CLEAR                   Devb01
170000010402     C     KEVB4         SETLL     FNEVB04L
170100010402     C                   DO        *HIVAL
170200010402     C     KEVB4         READE     FNEVB04L                               97
170300010402     C   97              LEAVE
170400131106     C                   MOVEL     EVBNOT        Devb01
170500131106     C                   IF        §notNDC = %editc(ARBNDC:'X')
170600010402     C                   MOVEL     EVBHEV        V02HCO
170700010402     C                   MOVE      EVBHEV        V02MCO
170800010402     C                   MOVE      EVBDEV        DTCCO2            2 0
170900010402     C                   MOVEL     EVBDEV        DTCCO6            6 0
171000010402     C                   MOVE      DTCCO6        V02DTC
171100010402     C                   MOVEL     DTCCO2        V02DTC
171200010402     C                   LEAVE
171300010402     C                   ELSE
171400010402     C                   ITER
171500010402     C                   END
171600010402     C                   ENDDO
171700010402     C                   ENDSR
171800031016      *---------------------------------------------------------*
171900031016      *  Richiamo eventimic  ATTRIBUZIONE MIC se non esiste     *
172000031016      *---------------------------------------------------------*
172100031016     C     eventimic     BEGSR
172200031016      * CERCO GLI EVENTI CON NR SPEDIZ. E TIPO EVENTO COC
172300031016      *
172400031016     C                   SETOFF                                       97
172500131106     C                   CLEAR                   Devb01
172600031016     C                   CLEAR                   esiste            1
172700031016     c                   if        vh2aap = 0
172800031016     c                   z-add     v2caas        Kanno
172900031016     c                   z-add     v2clnp        Kpart
173000031016     c                   z-add     v2cnrs        Kser
173100031016     c                   z-add     v2cnsp        Ksped
173200031016     c                   else
173300031016     c                   z-add     vh2aap        Kanno
173400031016     c                   z-add     vh2lpp        Kpart
173500031016     c                   z-add     vh2nrp        Kser
173600031016     c                   z-add     vh2nsp        Ksped
173700031016     c                   endif
173800031016     C     KEVbmic       SETLL     FNEVB04L
173900031016     C                   DO        *HIVAL
174000031016     C     KEVbmic       READE     FNEVB04L                               97
174100031016     C   97              LEAVE
174200031104     c                   if        evbnot <> *blank
174300131106     C                   MOVEL     EVBNOT        Devb01
174400131106     C                   IF        §notNDC = %editc(vnmDOC:'X')
174500031016     C                   MOVE      'X'           esiste
174600031016     C                   LEAVE
174700031016     C                   ELSE
174800031016     C                   ITER
174900031016     C                   END
175000031104     c                   endif
175100031016     C                   ENDDO
175200031016     c                   if        esiste = *blank
175300031016     c                   clear                   fnevb0
175400031016     C                   MOVEL     'MIC'         xEVBCEV
175500031016     C                   Z-ADD     OGGI          xEVBDTV
175600031016     C                   TIME                    xEVBORV
175700031016     C                   Z-ADD     V2CLNA        xEVBFLE
175800031016     C                   Z-ADD     DUTCOU        xEVBCDU
175900031016     C                   Z-ADD     Kanno         xEVBAAS
176000031016     C                   Z-ADD     Kpart         xEVBLNP
176100031016     C                   Z-ADD     Kser          xEVBNRS
176200031016     C                   Z-ADD     Ksped         xEVBNSP
176300031016     C                   Z-ADD     vh2ddt        xEVBDEV
176400031016     c
176500031016     C     fpp(i)        IFEQ      'M'
176600031016     C                   MOVEL     0800          xEVBHEV
176700031016     C                   ELSE
176800031016     C                   MOVEL     1200          xEVBHEV
176900031016     C                   END
177000131106     c                   move      vnmdoc        §notNDC
177100131106     c                   move      apdpdr        §notPDC
177200140110     c                   move      'C'           §notagg
177300131106     C                   MOVEL     Devb01        xEVBNOT
177400031016     c                   write     fnevb0
177500031016     c                   endif
177600031016     C                   ENDSR
177700941115      *---------------------------------------------------------*
177800941115      *  Richiamo pgm. FNLR53R                                  *
177900941115      *---------------------------------------------------------*
178000941115     C     CAL53         BEGSR
178100941115     C*
178200960326     C                   MOVEL     'S'           WCAL53            1
178300960116     C                   Z-ADD     V1CLNA        D53FGS
178400050711     c                   if        d53ita = *blank
178500960116     C                   MOVEL     'I'           D53ITA
178600050711     c                   endif
178700941124     C                   MOVEA     NDC           NDC1
178800941124     C                   MOVEL     APDRSC        D53DSF
178900080715     C                   MOVE      wpda          D53PDA
179000050516     C                   move      APDpdr        D53dsf
179100950530     C                   MOVEL     APDFCM        D53CMG
179200941115     C                   MOVEL     ' '           D53F03
179300941115     C                   MOVEL     CNTPDR        D53WRT
179400941115     C                   MOVEL     FNLR53        KPJBU
179500140519     C                   CALL      'FNLR53CM'
179600941115     C                   PARM                    KPJBA
179700941115     C                   MOVEL     KPJBU         FNLR53
179800941124     C                   MOVEL     ' '           D53TRI
179900950223     C*
180000941115     C*
180100941115     C                   ENDSR
180200941115     C*-----------------------------------------------------*
180300941115     C     DETVOL        BEGSR
180400941115     C*-----------------------------------------------------*
180500941115     C* VOLUME : - SE ARBFVR   ("T"), VOLUME ARBVOL
180600941115     C*            ALTRIMENTI
180700941115     C*               - SE TOTALE   ("T"), VOLUME ARBVLS
180800941115     C*               - SE PARZIALE ("Z"), VOLUME ARBVLS (SE MAGGIORE)
180900941115     C*                               ALTRIMENTI SOMMO ARBVOL
181000941115     C     ARBFVF        IFEQ      'T'
181100941115     C                   Z-ADD     ARBVLF        WVOL
181200941115     C                   ELSE
181300050208     C     ARBNCR        ifeq      ARBNCL
181400941115     C                   Z-ADD     ARBVLC        WVOL
181500941115     C*
181600941115     C                   ELSE
181700941115     C     ARBVLC        IFGE      ARBVLF
181800941115     C                   Z-ADD     ARBVLC        WVOL
181900941115     C                   ELSE
182000941115     C                   Z-ADD     ARBVLF        WVOL
182100941115     C                   ENDIF
182200941115     C                   ENDIF
182300941115     C                   ENDIF
182400941115     C*
182500941115     C                   ENDSR
182600021107     C*-----------------------------------------------------*
182700021107     C     detpes        begsr
182800021107     C*-----------------------------------------------------*
182900021107      * se è considerata un'unica spedizione
183000050208     C     arbncp        ifeq      arbncl
183100021107     C                   Z-ADD     arbPKC        wpes
183200021107     C*
183300021107     C                   ELSE
183400021107     C*
183500021107     C     arbPKC        ifge      arbPKF
183600021107     C                   Z-ADD     arbPKC        wpes
183700021107     C                   else
183800021107     C                   Z-ADD     arbPKF        wpes
183900021107     C                   end
184000021107     C*
184100021107     C                   ENDIF
184200021107     C*
184300021107     C                   ENDSR
184400000427     C*-----------------------------------------------------*
184500000427     C     MANCON        BEGSR
184600000427     C*-----------------------------------------------------*
184700000427     C*ROUTINE GESTIONE SPUNTE SU FOGLIO DI CATEGORIA 8 MANCATE CONSEGN
184800000427     C*LEGGO TUTTE LE SPUNTE PRESENTI SUL FOGLIO E MEMORIZZO ALL'INTERNO
184900000427     C*DELLA SPEDIZIONE LA MANCATA CONSEGNA CON PRIORITÀ PIÙ BASSA E
185000000427     C*IMPOSTO IL SFL PROPONENDO L'EVENTO RELATIVO DA TABELLA 3E
185100000427      *
185200000427      *CONTROLLO CAMBIO SPEDIZIONE PER INIZIALIZZARE CAMPI DI COMODO
185300000427     C     EXNSP         IFNE      ARTNSP
185400000427     C                   MOVE      '99'          COMPRI
185500000427     C                   MOVE      ARTNSP        EXNSP
185600000427     C                   END
185700000503     C                   Z-ADD     ARBNDC        ARBND6
185800070122     C     KFIB          CHAIN     FNBRV01L                           93
185900000427     C     *IN93         IFEQ      '0'
186000000526     C     BRVCAN        IFNE      *BLANK
186100000427     C                   Z-ADD     1             X
186200000427     C     BRVCAN        LOOKUP    COA(X)                                 93
186300000427     C     *IN93         IFEQ      '1'
186400000427     C     PMC(X)        ANDLT     COMPRI
186500001227     C     V2POST        IFEQ      'P'
186600000427     C                   MOVEL     CMC(X)        V02CCN
186700001227     C                   ELSE
186800001227     C                   MOVEL     CMB(X)        V02CCN
186900001227     C                   END
187000000427     C                   MOVEL     PMC(X)        COMPRI
187100000526     C                   ENDIF
187200000526     C                   ELSE
187300000526     C                   MOVEL     'N'           V02CCN
187400000427     C                   ENDIF
187500000427     C                   ENDIF
187600030829      * se chisura con spunta forzo § per evidenziare le bolle non spuntate
187700031013     c*                  if        kcdaz ='XR53' and arbtpr = 'A'and
187800031013     c*                            arbprg = 0
187900031013     c*                  move      '§'           v02ccn
188000031013     c*                  endif
188100030829     c
188200000427     C*
188300000427     C                   ENDSR
188400070319     C*----------------------------------------------------*
188500070319     C     geslck        begsr
188600070319     C*----------------------------------------------------*
188700070319     c                   Eval      ul82§rrn = dstnrr
188800070319     c                   Eval      UL82§FIL = 'FIDST00F'
188900070319     c                   Eval      UL82§WIN = 'S'
189000070319     c                   Eval      UL82§F7  = 'S'
189100070319     c                   Eval      UL82§num = 2
189200070319     c                   Eval      UL82§att = 1
189300070319     c                   Eval      UL82§mss = 'Si sta bloccando la Distinta -
189400070319     c                             numero :  '
189500070319     c                             + %editc(knfv6:'Z') +  ' terminare il lav-
189600070319     c                             oro '
189700070319     c                   Eval      UL82§msw = 'La distinta numero : '
189800070319     c                             + %editc(knfv6:'Z') +  ' non è disponibile '
189900070319      * Effettuo la chiamata al *PGM d utilità
190000070319     c                   Call(e)   'TRUL82R'
190100070319     c                   Parm                    Trul82ds
190200070319     c                   if        ul82§sts = 'A'
190300070319     c                   goto      fine
190400070319     c                   endif
190500070319
190600070319     C                   ENDSR
190700941115     C*-----------------------------------------------------*
190800941115     C*  Operazioni iniziali
190900941115     C*-----------------------------------------------------*
191000941115     C     *INZSR        BEGSR
191100941115     C*
191200941115     C     *ENTRY        PLIST
191300941115     C                   PARM                    KPJBA
191400030718     C                   PARM                    trul90ds
191500960326     C                   MOVEL     'N'           WCAL53            1
191600941115     C*
191700941115     C* Apertura controllata dei files
191800020822     C                   OPEN      FNFVV01L                             16
191900070410     C  N16              OPEN      fidst09L                             16
192000941124     C  N16              OPEN      FNARB01L                             16
192100060214     C  N16              OPEN      FiAR401L                             16
192200051115     C  N16              OPEN      FIAR901L                             16
192300941124     C  N16              OPEN      FNART01L                             16
192400941115     C   16              GOTO      FINE                                         OPEN vado a fine
192500941115     C* Decodifica Elaboratore
192600020822     c     *dtaara       define    §azute        azuteds
192700020822     c     *dtaara       define    §datiute      ddatiute
192800020822     C                   in(E)     *dtaara
192900020822     C                   IF        %Error  or  RSUT = *blanks
193000020822     C                   call      'TIBS34R'
193100020822     C                   parm                    Tibs34Ds
193200020822     C                   in        *dtaara
193300020822     c                   ENDIF
193400020822     C                   Z-ADD     Dutpou        V1CLNA            3 0
193500941115     C*
193600941115     C*  Definizione chiavi
193700080708     C     Kcet          KLIST
193800101007     C                   KFLD                    V1CLNA
193900101007     C                   KFLD                    PCTNDC
194000140911     C                   KFLD                    vh2pdr
194100080708     C                   KFLD                    TTRD
194200140911     C                   KFLD                    v2cAAS
194300140911     C                   KFLD                    v2cLNP
194400140911     C                   KFLD                    v2cNRS
194500140911     C                   KFLD                    v2cNSP
194600080708     C                   move      'CET'         TTRD              3
194700131203     C     Kcetcep       KLIST
194800131203     C                   KFLD                    V1CLNA
194900131203     C                   KFLD                    PCTNDC
195000140911     C                   KFLD                    vh2pdr
195100131203     C                   KFLD                    TTRDcep
195200140911     C                   KFLD                    v2cAAS
195300140911     C                   KFLD                    v2cLNP
195400140911     C                   KFLD                    v2cNRS
195500140911     C                   KFLD                    v2cNSP
195600131203     C                   move      'CEP'         TTRDcep           3
195700080708     c
195800110330     C     Kar5          KLIST
195900110330     C                   KFLD                    v2cAAS
196000110330     C                   KFLD                    v2cLNP
196100110330     C                   KFLD                    v2cNRS
196200110330     C                   KFLD                    v2cNSP
196300110330     c                   kfld                    t5trd
196400110330     C                   move      'GEN'         T5trd             3
196500110330     c
196600941115     C     KSPE          KLIST
196700941115     C                   KFLD                    KAAS
196800941115     C                   KFLD                    KLNP
196900941115     C                   KFLD                    KNRS
197000941115     C                   KFLD                    KNSP
197100000615     C     KEVB          KLIST
197200000615     C                   KFLD                    KAASE
197300000615     C                   KFLD                    KLNPE
197400000615     C                   KFLD                    KNRSE
197500000615     C                   KFLD                    KNSPE
197600990922     C     KAR6          KLIST
197700990922     C                   KFLD                    KAAS
197800990922     C                   KFLD                    KLNP
197900990922     C                   KFLD                    KNRS
198000990922     C                   KFLD                    KNSP
198100990922     C                   KFLD                    KTRC
198200990922     C     KARI          KLIST
198300990922     C                   KFLD                    KAAS
198400990922     C                   KFLD                    KLNP
198500990922     C                   KFLD                    KNRS
198600990922     C                   KFLD                    KNSP
198700990922     C                   KFLD                    KTIP
198800941115     C*
198900941115     C     KAR4          KLIST
199000941115     C                   KFLD                    KAAS
199100941115     C                   KFLD                    KLNP
199200941115     C                   KFLD                    KNRS
199300941115     C                   KFLD                    KNSP
199400941115     C                   KFLD                    KTRC
199500941115     C*
199600941115     C     KARB1         KLIST
199700941115     C                   KFLD                    KAAP
199800941115     C                   KFLD                    KLPP
199900941115     C                   KFLD                    KNRP
200000000615     C                   KFLD                    KNSPP
200100941115     C*
200200941115     C     KFVV          KLIST
200300941115     C                   KFLD                    KNPG
200400941115     C                   KFLD                    KNFV
200500020822     C                   KFLD                    v1clna
200600070316      *
200700070316     C     Kdst          klist
200800070410     C                   kfld                    v1clna
200900070316     C                   kfld                    knpg
201000070316     C                   kfld                    knfv6             6 0
201100070316      *
201200080707     C                   Z-ADD     4             KNPG
201300080707      *
201400080707     C     Kdsf          klist
201500080707     C                   kfld                    knpg
201600080707     C                   kfld                    knfv6             6 0
201700080707     C                   kfld                    v1clna
201800080707     C                   kfld                    fase              3
201900080707     C                   move      'CRI'         fase
202000941115     C*
202100941115     C     KTAB1         KLIST
202200941115     C                   KFLD                    KKUT
202300941115     C                   KFLD                    KCOD
202400941115     C*
202500941115     C     KTAB2         KLIST
202600941115     C                   KFLD                    KKUT
202700941115     C                   KFLD                    KCOD
202800941115     C                   KFLD                    KKEY
202900991006     C     KTBE          KLIST
203000991006     C                   KFLD                    KCODE
203100991006     C                   KFLD                    KKEYE
203200941115     C*
203300941115     C     KFTT          KLIST
203400941115     C                   KFLD                    KPDR
203500941115     C                   KFLD                    KFVL
203600941115     C                   KFLD                    KDDC
203700941115     C                   KFLD                    KTSR
203800960702     C     KARB          KLIST
203900020923     C                   KFLD                    KIFP
204000020822     C                   KFLD                    KNDC
204100960702     C                   KFLD                    KDDC
204200000427     C     KFIB          KLIST
204300000427     C                   KFLD                    FOGLIO
204400000503     C                   KFLD                    ARBND6            6 0
204500000427     C                   KFLD                    FVVFGS
204600000427     C                   KFLD                    ARTFLS
204700000427     C                   KFLD                    ARTLNA
204800000427     C                   KFLD                    ARTNRS
204900000427     C                   KFLD                    ARTNSC
205000000427     C                   Z-ADD     8             FOGLIO
205100010402     C     KEVB4         KLIST
205200010402     C                   KFLD                    ARBAAS
205300010402     C                   KFLD                    ARBLNP
205400010402     C                   KFLD                    ARBNRS
205500010402     C                   KFLD                    ARBNSP
205600010402     C                   KFLD                    EVECOC
205700010402     C                   MOVE      'COC'         EVECOC
205800021204     C     Kapd          KLIST
205900021204     C                   KFLD                    apdtip
206000021204     C                   KFLD                    arbpdc
206100021204     c                   movel     'A'           apdtip
206200031016     C     KEVBmic       KLIST
206300031016     C                   KFLD                    Kanno
206400031016     C                   KFLD                    Kpart
206500031016     C                   KFLD                    Kser
206600031016     C                   KFLD                    Ksped
206700031016     C                   KFLD                    kevmic
206800941115     C*----------------------------------------------------*
206900941115     C*  Definizione delle variabili
207000000608     C     *LIKE         DEFINE    KPJBU         SAVJBU
207100010402     C     *LIKE         DEFINE    EVBCEV        EVECOC
207200000427     C     *LIKE         DEFINE    ARTNSP        EXNSP
207300000427     C     *LIKE         DEFINE    §3EPMC        COMPRI
207400000427     C     *LIKE         DEFINE    BRVNPG        FOGLIO
207500080714     C     *LIKE         DEFINE    dstpda        wpda
207600080714     C     *LIKE         DEFINE    dstpda        savpda
207700941115     C     *LIKE         DEFINE    ARBAAS        KAAS
207800941115     C     *LIKE         DEFINE    ARBLNP        KLNP
207900941115     C     *LIKE         DEFINE    ARBNRS        KNRS
208000941115     C     *LIKE         DEFINE    ARBNSP        KNSP
208100000615     C     *LIKE         DEFINE    ARBNSP        KNSPP
208200000615     C     *LIKE         DEFINE    ARBAAS        KAASE
208300000615     C     *LIKE         DEFINE    ARBLNP        KLNPE
208400000615     C     *LIKE         DEFINE    ARBNRS        KNRSE
208500000615     C     *LIKE         DEFINE    ARBNSP        KNSPE
208600941115     C     *LIKE         DEFINE    AR4TRC        KTRC
208700941115     C     *LIKE         DEFINE    ARBAAS        KAAP
208800941115     C     *LIKE         DEFINE    ARBLNP        KLPP
208900941115     C     *LIKE         DEFINE    ARBNRS        KNRP
209000941115     C     *LIKE         DEFINE    ARBVLF        WVOL
209100021107     C     *like         define    arbpkF        wpes
209200941115     C     *LIKE         DEFINE    FVVNPG        KNPG
209300941115     C     *LIKE         DEFINE    FVVNFV        KNFV
209400941115     C     *LIKE         DEFINE    TBLKUT        KKUT
209500941115     C     *LIKE         DEFINE    TBLCOD        KCOD
209600941115     C     *LIKE         DEFINE    TBLKEY        KKEY
209700941115     C     *LIKE         DEFINE    AR6IFT        WIMA
209800941115     C     *LIKE         DEFINE    AR9CAS        WCAS
209900941115     C     *LIKE         DEFINE    AR9VCA        WVCA
210000941115     C     *LIKE         DEFINE    AR9TIC        WTIC
210100990922     C     *LIKE         DEFINE    AR6DIV        WDIV
210200941115     C     *LIKE         DEFINE    FTTPDR        KPDR
210300941115     C     *LIKE         DEFINE    FTTFVL        KFVL
210400941115     C     *LIKE         DEFINE    FTTDDC        KDDC
210500960702     C     *LIKE         DEFINE    ARBNDC        KNDC
210600020923     C     *LIKE         DEFINE    ARBIFP        KIFP
210700941115     C     *LIKE         DEFINE    FTTTSR        KTSR
210800990922     C     *LIKE         DEFINE    ARITIP        KTIP
210900991006     C     *LIKE         DEFINE    TBECOD        KCODE
211000991006     C     *LIKE         DEFINE    TBEKE1        KKEYE
211100031016     C     *LIKE         DEFINE    ARBAAS        Kanno
211200031016     C     *LIKE         DEFINE    ARBLNP        Kpart
211300031016     C     *LIKE         DEFINE    ARBNRS        Kser
211400031016     C     *LIKE         DEFINE    ARBNSP        Ksped
211500031016     C     *LIKE         DEFINE    evbcev        kevmic
211600031016     C     *LIKE         DEFINE    evbdev        kdata
211700031016     c                   move      'MIC'         kevmic
211800941115     C*----------------------------------------------------*
211900941115     C* Giro data del giorno
212000941115     C                   TIME                    WHHDAT           14 0
212100941115     C                   MOVE      WHHDAT        G02DAT
212200941115     C                   MOVEL     *BLANK        G02ERR
212300941115     C                   CALL      'XSRDA8'
212400941115     C                   PARM                    WLBDA8
212500941115     C                   Z-ADD     G02INV        OGGI              8 0
212600941214     C                   MOVE      V1CLNA        D53LNA
212700941115     C*----------------------------------------------------*
212800941115     C                   Z-ADD     1             KKUT
212900941115     C                   MOVEL     ' '           WFINE             1
213000941115     C                   MOVEL     '1'           WTPVID            1
213100020826     C                   Z-ADD     dutcou        V1COPR
213200941124     C                   MOVEL     'P'           D53TRI
213300941115     C* Se profilo uguale a REM prendo le filiali a loro associate
213400020822     C     Dutlpo        IFEQ      '1'
213500941115     C                   SETON                                        81
213600941115     C                   END
213700020822     C     Dutlpo        IFEQ      '2'
213800941115     C                   SETOFF                                       81
213900941115     C                   END
214000941115     C*----------------------------------------------------*
214100991006     C*  Aggancio tabella 'GED' x reperire la divisa corrente
214200991006     C                   MOVEL     'GED'         KCODE
214300991006     C                   MOVEL     *BLANKS       KKEYE
214400991006     C                   MOVEL     '1'           KKEYE
214500991006     C     KTBE          CHAIN     TNTBE01L                           13
214600991006     C     *IN13         IFEQ      '0'
214700991006     C                   MOVEL     TBEUNI        DGED
214800991006     C                   END
214900991006     C*
215000020418     C*  tabelle dgd x reperimento limiti chiusura spedizioni
215100020418     C                   MOVEL     'DGD'         KCODe
215200020418     C                   MOVEL     *BLANKS       KKEye
215300020418     C                   MOVEL     '1'           KKEye
215400020418     C     KTBE          CHAIN     TNTBE01L                           13
215500020418     C     *IN13         IFEQ      '0'
215600020418     C                   MOVEL     TBEUNI        DDGD
215700020418     C                   END
215800020418     C*
215900991006     C*  CARICO DATI TABELLA CV
216000991006     C                   MOVEL     *BLANKS       DIVEUR
216100991006     C                   MOVEL     'CV'          KCOD
216200991006     C     KTAB1         CHAIN     TABEL00F                           13
216300991006     C     *IN13         DOWEQ     '0'
216400991006     C                   MOVEL     TBLKEY        DIVCV             3
216500991006     C                   MOVEL     TBLUNI        DSCV
216600991006     C* Se divisa diversa dalla moneta di conto reperisco data
216700991006     C* validità x trovare altra divisa da utilizzare x incassi
216800011009     C     §GEDCR        IFNE      DIVCV
216900991006     C     DIVEUR        ANDEQ     *BLANKS
217000991006     C     §CVDVA        ANDGE     'S'
217100991006     C                   MOVEL     DIVCV         DIVEUR            3
217200991006     C                   END
217300991006     C     KTAB1         READE     TABEL00F                               13
217400991006     C                   END
217500991116     C*  Se non ho impostato divisa alternativa ci sbatto
217600991116     C*  la divisa corrente
217700991116     C     DIVEUR        IFEQ      *BLANKS
217800011009     C                   MOVEL     §GEDCR        DIVEUR
217900991116     C                   END
218000991006     C*----------------------------------------------------*
218100941115     C*  Caricamento £1
218200020509     c                   clear                   trul06ds
218300020509     c                   eval      d06cod = '£1'
218400020822     c                   movel     dutpou        d06key
218500020509     c                   movel     trul06ds      kpjbu
218600020509     c                   call      'TRUL06R'
218700020509     c                   parm                    kpjba
218800020509     c                   movel     kpjbu         trul06ds
218900020509     c                   movea     lin           £1
219000941115     C*  Caricamento Tabella 7R
219100941115     C                   Z-ADD     0             X                 3 0
219200941115     C                   MOVEL     '7R'          KCOD
219300941115     C     KTAB1         CHAIN     TABEL00F                           96
219400941115     C     *IN96         DOWEQ     '0'
219500080903     C     X             ANDLT     200
219600941115     C     TBLFLG        IFEQ      ' '
219700941115     C                   ADD       1             X
219800941115     C                   MOVEL     TBLKEY        C7R(X)
219900941115     C                   MOVEL     TBLUNI        D7R(X)
220000941115     C                   END
220100941115     C     KTAB1         READE     TABEL00F                               96
220200941115     C                   END
220300951019     C     *IN96         IFEQ      '0'
220400000211     C                   MOVEL     MSG(1)        DSMSG
220500000211     C                   MOVEL     KCOD          KCODM
220600000211     C                   MOVEL     DSMSG         MSG(1)
220700100825     c                   exsr      sndmail
220800100825     C*                  CALL      'TNSYM4C'
220900100825     C*                  PARM                    MSGT
221000100825     C*                  PARM                    MSG
221100100825     C*                  PARM      046           FIL               3 0
221200100825     C*                  PARM      'EDP*'        UTEN             10
221300951019     C                   END
221400941115     C*  Caricamento Tabella 3A
221500941115     C                   Z-ADD     0             X                 3 0
221600941115     C                   MOVEL     '3A'          KCOD
221700941115     C     KTAB1         CHAIN     TABEL00F                           96
221800941115     C     *IN96         DOWEQ     '0'
221900080903     C     X             ANDLT     200
222000941115     C     TBLFLG        IFEQ      ' '
222100941115     C                   ADD       1             X
222200941115     C                   MOVEL     TBLKEY        C3A(X)
222300941115     C                   MOVEL     TBLUNI        D3A(X)
222400941115     C                   END
222500941115     C     KTAB1         READE     TABEL00F                               96
222600941115     C                   END
222700951019     C     *IN96         IFEQ      '0'
222800000211     C                   MOVEL     MSG(1)        DSMSG
222900000211     C                   MOVEL     KCOD          KCODM
223000000211     C                   MOVEL     DSMSG         MSG(1)
223100100825     c                   exsr      sndmail
223200100825     C*                  CALL      'TNSYM4C'
223300100825     C*                  PARM                    MSGT
223400100825     C*                  PARM                    MSG
223500100825     C*                  PARM      046           FIL               3 0
223600100825     C*                  PARM      'EDP*'        UTEN             10
223700951019     C                   END
223800990923     C*
223900990923     C                   Z-ADD     0             X                 3 0
224000990923     C                   MOVEL     '1A'          KCOD
224100990923     C     KTAB1         CHAIN     TABEL00F                           96
224200990923     C     *IN96         DOWEQ     '0'
224300080903     C     X             ANDLT     200
224400990923     C     TBLFLG        IFEQ      ' '
224500990923     C                   ADD       1             X
224600990923     C                   MOVEL     TBLKEY        C1A(X)
224700990923     C                   MOVEL     TBLUNI        D1A(X)
224800990923     C                   END
224900990923     C     KTAB1         READE     TABEL00F                               96
225000990923     C                   END
225100990923     C     *IN96         IFEQ      '0'
225200000211     C                   MOVEL     MSG(1)        DSMSG
225300000211     C                   MOVEL     KCOD          KCODM
225400000211     C                   MOVEL     DSMSG         MSG(1)
225500100825     c                   exsr      sndmail
225600100825     C*                  CALL      'TNSYM4C'
225700100825     C*                  PARM                    MSGT
225800100825     C*                  PARM                    MSG
225900100825     C*                  PARM      046           FIL               3 0
226000100825     C*                  PARM      'EDP*'        UTEN             10
226100990923     C                   END
226200941115     C*
226300000427     C*  Caricamento Tabella 3E
226400000427     C                   Z-ADD     0             X                 3 0
226500000427     C                   MOVEL     '3E'          KCOD
226600000427     C     KTAB1         CHAIN     TABEL00F                           96
226700000427     C     *IN96         DOWEQ     '0'
226800080903     C     X             ANDLT     200
226900000427     C     TBLFLG        IFEQ      ' '
227000000427     C                   ADD       1             X
227100000427     C                   MOVEL     TBLKEY        COA(X)
227200000427     C                   MOVEL     TBLUNI        DS3E
227300000427     C                   MOVEL     §3ECMC        CMC(X)
227400001227     C                   MOVEL     §3ECMB        CMB(X)
227500000427     C                   MOVEL     §3EPMC        PMC(X)
227600000427     C                   END
227700000427     C     KTAB1         READE     TABEL00F                               96
227800000427     C                   END
227900000427     C     *IN96         IFEQ      '0'
228000000427     C                   MOVEL     MSG(1)        DSMSG
228100000427     C                   MOVEL     KCOD          KCODM
228200000427     C                   MOVEL     DSMSG         MSG(1)
228300100825     c                   exsr      sndmail
228400100825     C*                  CALL      'TNSYM4C'
228500100825     C*                  PARM                    MSGT
228600100825     C*                  PARM                    MSG
228700100825     C*                  PARM      046           FIL               3 0
228800100825     C*                  PARM      'EDP*'        UTEN             10
228900000427     C                   END
229000000503     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
229100000503     C                   CLEAR                   FIP
229200000503     C     *LOVAL        SETLL     AZORG01L
229300000503     C                   DO        *HIVAL
229400000503     C                   READ      AZORG01L                               95
229500000503     C   95              LEAVE
229600000503     C                   MOVEL     ORGDE3        OG143
229700020729     C     §OgNTW        IFEQ      'PPT'
229800000503     C                   Z-ADD     1             X
229900000503     C     *ZEROS        LOOKUP    FIP(X)                                 23
230000000503     C   23              Z-ADD     ORGFIL        FIP(X)
230100000503     C                   END
230200000503     C                   END
230300000602      *
230400000602     C*  Caricamento Tabella 2Z
230500000602     C                   Z-ADD     0             X                 3 0
230600000602     C                   MOVEL     '2Z'          KCOD
230700000602     C     KTAB1         CHAIN     TABEL00F                           96
230800000602     C     *IN96         DOWEQ     '0'
230900080903     C     X             ANDLT     200
231000000602     C     TBLFLG        IFEQ      ' '
231100000602     C                   ADD       1             X
231200000602     C                   MOVEL     TBLKEY        C2Z(X)
231300000602     C                   MOVEL     TBLUNI        D2Z(X)
231400000602     C                   END
231500000602     C     KTAB1         READE     TABEL00F                               96
231600000602     C                   END
231700000602     C     *IN96         IFEQ      '0'
231800000602     C                   MOVEL     MSG(1)        DSMSG
231900000602     C                   MOVEL     KCOD          KCODM
232000000602     C                   MOVEL     DSMSG         MSG(1)
232100100825     c                   exsr      sndmail
232200100825     C*                  CALL      'TNSYM4C'
232300100825     C*                  PARM                    MSGT
232400100825     C*                  PARM                    MSG
232500100825     C*                  PARM      046           FIL               3 0
232600100825     C*                  PARM      'EDP*'        UTEN             10
232700000602     C                   END
232800101123     C*  Caricamento Tabella 2a
232900101123     C                   Z-ADD     0             X                 3 0
233000101123     C                   MOVEL     '2A'          KCOD
233100101123     C     KTAB1         CHAIN     TABEL00F                           96
233200101123     C     *IN96         DOWEQ     '0'
233300101123     C     X             ANDLT     200
233400101123     C     TBLFLG        IFEQ      ' '
233500101123     C                   ADD       1             X
233600101123     C                   MOVEL     TBLKEY        C2A(X)
233700101123     C                   MOVEL     TBLUNI        D2A(X)
233800101123     C                   END
233900101123     C     KTAB1         READE     TABEL00F                               96
234000101123     C                   END
234100101123     C     *IN96         IFEQ      '0'
234200101123     C                   MOVEL     MSG(1)        DSMSG
234300101123     C                   MOVEL     KCOD          KCODM
234400101123     C                   MOVEL     DSMSG         MSG(1)
234500101123     c                   exsr      sndmail
234600101123     C*                  CALL      'TNSYM4C'
234700101123     C*                  PARM                    MSGT
234800101123     C*                  PARM                    MSG
234900101123     C*                  PARM      046           FIL               3 0
235000101123     C*                  PARM      'EDP*'        UTEN             10
235100101123     C                   END
235200941115     C                   ENDSR
235300100825      *----------------------------------------------------------------
235400100825     c     sndmail       begsr
235500100825      *----------------------------------------------------------------
235600120305     c                   eval      emlto = 'CedAlert@brt.it'
235700100825     c                   eval      emlcc = *blank
235800100825     c                   eval      emogg = 'ATTENZIONE Chiusura distinta '
235900100825     C                   eval      emmsg = msg(1)  + ' ' +
236000100825     c                             msg(2) + ' ' + msg(3)
236100100825     C                   CALL      'TIS701C1'
236200100825     C                   PARM                    emlto           253
236300100825     C                   PARM                    emlcc           253
236400100825     C                   PARM                    emogg            44
236500100825     C                   PARM                    emmsg          5000
236600100825     C                   PARM                    emesito           1
236700100825      *
236800100825     C                   ENDSR
236900941115      *----------------------------------------------------------------
237000941115     OFNFVV000  E            AGGFVV
237100941115     O                       FVVFCF
237200000211**  ERRORI
237300961115Punto operativo non in gestione su questo AS                          01
237400941123Numero distinta errato                                                02
237500941123Immettere tipo uscita M=Mattino/P=Pomeriggio !                        03
237600941123Immettere flag calcolo Picking !                                      04
237700941123Attenzione !! Flag calcolo picking diverso da anagrafica              05
237800941123Distinta in uso ad altro operatore o già chiusa !                     06
237900941123Data errata !                                                         07
238000941123Ora errata !                                                          08
238100941123Data Distinta errata: già confermati i conteggi !                     09
238200020418Data Consegna anteriore al limite inferiore ammesso  !!               10
238300100211Distinte con diverso codice autista !                                 11
238400961115Distinta in gestione ad altra punto operativo                         12
238500941124Immettere almeno un numero di distinta da chiudere !!!                13
238600941128Attenzione !! Data di consegna anteriore alla data distinta           14
238700950221Non chiudere dist. di questo padr.x conflitti! Riprovare fra 2 min.   15
238800010302Ora errata ! PER UNA DISTINTA POMERIDIANA L'ORA MINIMA SONO LE 13     16
238900030204Immettere flag calcolo Carico !                                       17
239000030204Attenzione !! Flag calcolo carico diverso da anagrafica               18
239100070319Distinta già chiusa o non disponibile               FIDST00F          19
239200080707Distinte incongruenti Alcune gestite con PDA e altre NO               20
239300080707Manca Fase controllo rientro (CRI) distinta non disponibile           21
239400080707Distinta con gestione PDA obbligatorio chiudere da lettore ottico     22
239500000211**
239600000211ERRORE IN CHIUSURA DISTINTA
239700000211**
239800000211ATTENZIONE ! La tabella XX utilizzata nella chiusura distinta ha r
239900000211aggiunto la capienza massima ! Correggere il programma FNLR53R inc
240000000211rementando la relativa schiera.
