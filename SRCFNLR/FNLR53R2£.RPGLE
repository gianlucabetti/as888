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
103100071003     c                   clear                   v2harp
103200941214     C     V2CGMA        COMP      *BLANKS                                08
103300000503      *TESTO SE FILIALE POSTE NON ESEGUO FORZATURA C7
103400000503     C     ARBLNP        LOOKUP    FIP                                    23
103500000503     C     *IN23         IFEQ      '0'
103600000504     C                   MOVE      *BLANK        V2POST
103700990419     C     ARBGMA        IFEQ      *BLANKS
103800990419     C                   MOVEL     'C7'          V2CGMA
103900000427     C                   MOVEL     'C7'          ARBGMA
104000990423     C                   MOVEL     '1'           V2IN09
104100990422     C                   SETON                                        09
104200990422     C                   ELSE
104300990423     C                   MOVEL     '0'           V2IN09
104400990422     C                   SETOFF                                       09
104500990419     C                   END
104600000504      *INSERISCO FLAG PER SEGNALARE BOLLA POSTE CAMPO NASCOSTO
104700000504     C                   ELSE
104800000504     C                   MOVE      'P'           V2POST
104900000503     C                   END
105000000504      *
105100941115     C                   MOVEL     ARBMGS        MM                2 0
105200941115     C                   MOVE      ARBMGS        GG                2 0
105300941115     C                   MOVEL     GG            V2CGMS
105400941115     C                   MOVE      MM            V2CGMS
105500941115     C                   Z-ADD     ARBSTP        V02STP
105600010402     C* CONTROLLO ESISTENZA EVENTO DI COMUNICAZIONE CONSEGNA
105700010402     C* SE ESISTE IMPOSTO GIORNO MESE ORA MINUTO DI CONSEGNA DA EVENTO
105800010402     C                   EXSR      EVECON
105900010402     C*
106000941115     C* Controllo se esiste bolla mamma
106100941115     C                   Z-ADD     ARBAAS        KAAS
106200941115     C                   Z-ADD     ARBLNP        KLNP
106300941115     C                   Z-ADD     ARBNRS        KNRS
106400941115     C                   Z-ADD     ARBNSP        KNSP
106500941115     C     KSPE          CHAIN     FNLBL01L                           76
106600941115     C     *IN76         IFEQ      '0'
106700941115     C     LBLLAN        ANDEQ     LBLLAP
106800980429     C                   Z-ADD     LBLAAP        KAAS
106900980429     C                   Z-ADD     LBLLPP        KLNP
107000980429     C                   Z-ADD     LBLNRP        KNRS
107100980429     C                   Z-ADD     LBLNSP        KNSP
107200980429     C     KSPE          CHAIN     FNARB01L                           76
107300980429     C* Memorizzo il legame esistente per i successivi accessi su FNARB
107400980429     C     *IN76         IFEQ      '0'
107500980429     C     ARBCCA        ANDNE     '2'
107600980429     C     ARBCCA        ANDNE     '6'
107700980429     C                   Z-ADD     LBLAAP        VH2AAP
107800980429     C                   Z-ADD     LBLLPP        VH2LPP
107900980429     C                   Z-ADD     LBLNRP        VH2NRP
108000980429     C                   Z-ADD     LBLNSP        VH2NSP
108100941115     C                   END
108200980429     C                   Z-ADD     LBLAAN        KAAS
108300980429     C                   Z-ADD     LBLLPN        KLNP
108400980429     C                   Z-ADD     LBLNRN        KNRS
108500980429     C                   Z-ADD     LBLNSN        KNSP
108600980429     C     KSPE          CHAIN     FNARB01L                           76
108700090218     C                   END
108800941115     C*  controllo se c'è il contrassegno
108900941115     C                   Z-ADD     1             X
109000941115     C                   CLEAR                   DS3A
109100941115     C     ARBCBO        LOOKUP    C3A(X)                                 95
109200941115     C     *IN95         IFEQ      '1'
109300950116     C                   MOVEL     D3A(X)        DS3A
109400941115     C                   END
109500941115     C*  Controllo se c'è assegnato
109600941115     C     ARBICA        IFEQ      ' '
109700941115     C     ARBICA        OREQ      'R'
109800941115     C     ARBFIP        OREQ      'I'
109900951206     C                   MOVEL     §3ATB1        WPORTO            1
110000941115     C     WPORTO        IFEQ      'A'
110100990922     C                   MOVEL     '1'           KTRC
110200990922     C                   ELSE
110300990922     C                   MOVEL     '2'           KTRC
110400990922     C                   END
110500990922     C     KAR6          CHAIN     FIAR601L                           95
110600990922     C     AR6DFT        IFNE      *ZEROS
110700990922     C     *IN95         ANDEQ     '0'
110800990922     C                   Z-ADD     AR6IFT        WIMA
110900990922     C                   MOVEL     AR6DIV        WDIV
111000990922     C* Loop su file incassi parziali x determinare importo da inc.
111100990922     C                   MOVEL     'A'           KTIP
111200990922     C     KARI          CHAIN     FIARI01L                           95
111300990922     C     *IN95         DOWEQ     '0'
111400990927     C     AR6DIV        IFEQ      ARIDIV
111500990922     C                   SUB       ARIIPP        WIMA
111600991012     C                   ELSE
111700990927     C* Se la divisa di incasso è diversa converto importo già
111800990927     C* incassato nella divisa della bolla
111900990927     C                   CLEAR                   YEURDS
112000990927     C                   MOVEL     ARIDIV        YECDVI
112100990927     C                   Z-ADD     ARIIPP        YECIMI
112200990927     C                   MOVEL     AR6DIV        YECDVO
112300990927     C                   MOVEL     'H'           YECTAR
112400990927     C                   CALL      'YEURCO'
112500990927     C                   PARM                    YEURDS
112600990927     C                   SUB       YECIMO        WIMA
112700990927     C                   END
112800990922     C     KARI          READE     FIARI01L                               95
112900990922     C                   END
113000950620     C                   Z-ADD     AR6DFT        V3CDFT
113100951206     C                   MOVEL     §3ATB1        V2CPOR
113200951206     C                   END
113300941115     C*
113400941123     C                   ELSE
113500941219     C                   MOVE      'S'           V2CICA
113600941115     C                   END
113700941115     C* Metto la 'V' in v02sel se per la bolla è prevista la gestione
113800941115     C* della firma del destinatario
113900941115     C     ARBGMA        IFNE      *BLANKS
114000941115     C                   Z-ADD     1             X
114100941115     C                   CLEAR                   DS7R
114200960119     C                   SETON                                        08
114300941115     C     ARBGMA        LOOKUP    C7R(X)                                 95
114400951206     C     *IN95         IFEQ      '1'
114500960119     C                   SETOFF                                       08
114600951206     C                   MOVEL     D7R(X)        DS7R
114700951206     C                   MOVEL     §7R4TM        V2C4TM
114800951206     C                   Z-ADD     §7R4NT        V2C4NT
114900960116     C                   MOVEL     §7R1BF        V2C1BF
115000120518     C                   MOVEL     §7Rvpcd       V2Cvpcd
115100941115     C                   END
115200941115     C     §7R2FI        IFEQ      'S'
115300941115     C                   MOVEL     'V'           V02SEL
115400941115     C                   END
115500941115     C                   END
115600941115     C*  Controllo se c'è firmatario
115700941115     C                   MOVEL     '1'           KTRC
115800060214     C     KAR4          CHAIN     FiAR401L                           95
115900941115     C  N95              MOVEL     AR4NOT        V2CFIR
116000941115     C*  Controllo se ci sono delle note relative alla consegna
116100941115     C                   MOVEL     '0'           KTRC
116200060214     C     KAR4          CHAIN     FiAR401L                           95
116300941115     C  N95              MOVEL     AR4NOT        V2CNT1
116400941115     C* Memorizzo se c'è precedente consegna parziale
116500941115     C     ARBDCP        IFGT      0
116600941115     C                   MOVEL     'S'           V02PRZ
116700941115     C                   END
116800000512     C*  Controllo se LE SPEDIZIONI SONO AUTOGENERATE
116900000503     C                   SETOFF                                       24
117000000503     C                   MOVEL     'A'           KTRC
117100060214     C     KAR4          CHAIN     FiAR401L                           95
117200000503     C  N95              MOVEL     AR4NOT        DSBL4A
117300000503     C  N95§B4ASO        COMP      '&'                                    24
117400000522     C   24              MOVE      '1'           V2IN24
117500000522     C  N24              MOVE      '0'           V2IN24
117600941115     C*
117700941115     C                   MOVEL     ARBGC1        V021GC
117800941115     C                   MOVEL     ARBGC2        V022GC
117900941115     C* muovo eventuale tipo consegna particolare
118000941115     C                   MOVEL     ARBTC1        V2CCP1
118100941115     C                   MOVEL     ARBTC1        V2HCP1
118200941115     C                   MOVEL     ARBTC2        V2CCP2
118300941115     C                   MOVEL     ARBTC2        V2HCP2
118400950206     C*
118500941118     C                   MOVE      ARBTCR        V2CTCR
118600941118     C                   MOVE      ARBDCR        V2CDCR
118700941118     C                   MOVE      ARBHCR        V2CHCR
118800091214     c                   move      arbtsp        v02tsp
118900091026     c                   clear                   v02fhob
119000091216     c                   if        arbtsp = 'H' or arbtsp ='E'
119100091026     c                   move      arbtsp        v02fhob
119200091026     c                   endif
119300051215      * richiamo pgm per informazione relativamente alla data cons. rich.
119400051215      * se TASSATIVA
119500051215     c                   clear                   v2ctat
119600051215     c                   clear                   fnlv80ds
119700051215     c                   z-add     arbAAS        ILV80AAS
119800051215     c                   z-add     arbLNP        ILV80LNP
119900051215     c                   z-add     arbNRS        ILV80NRS
120000051215     c                   z-add     arbNSP        ILV80NSP
120100051215     c                   z-add     arbMGS        ILV80MGS
120200051215     c                   move      arbTCR        ILV80TCR
120300051215     c                   z-add     arbDCR        ILV80DCR
120400051215     c                   z-add     arbHCR        ILV80HCR
120500051215     c                   move      arbTC1        ILV80TC1
120600051215     c                   move      arbTC2        ILV80TC2
120700051215     c                   call      'FNLV80R'
120800051215     c                   parm                    kpjba
120900051215     c                   parm                    fnlv80ds
121000051215     c                   move      olv80tat      v2ctat
121100051215      *
121200000615     C*  SE DISTINTA SDA :
121300000615     C*  ANNULLO CONTRASSEGNO DALLA SPEDIZIONE
121400000615      * FORZO 0 NEL CAMPO §3AFCA (FLAG ESISTENZA C/ASS) SE RIUSCITO
121500000615      * ANNULLAMENTO.   NON  CI SI PREOCCUPA SE IL C/ASS È SULLA MAMMA O
121600000615      * SULLA FIGLIA IN QUANTO PER LE POSTE NON CI SONO CAMBI PORTO ETC..
121700000531     C     DISSDA        IFEQ      'S'
121800000531     C     §3AFCA        ANDGT     0                                            --- 01 ---
121900000531     C                   CLEAR                   DS48
122000000531     C                   CLEAR                   DSARBK
122100000601     C                   MOVE      KAAS          D48AAS
122200000531     C                   MOVE      KLNP          D48LNP
122300000531     C                   MOVE      KNRS          D48NRS
122400000531     C                   MOVE      KNSP          D48NSP
122500000531     C                   MOVE      'KB'          D48CVB
122600000531     C                   MOVE      'E'           D48FFR
122700000531     C                   MOVE      'K'           D48TRC
122800000531     C                   MOVE      'A'           D48TBO
122900000531     C                   MOVE      *BLANK        §BKCBN
123000000531     C                   MOVE      *BLANK        §BKTIC
123100000531     C                   Z-ADD     0             §BKCAS
123200000531     C                   MOVEL     DS48          KPJBU
123300000531     C                   CALL      'FNLR48R'
123400000531     C                   PARM                    KPJBA
123500000531     C                   PARM                    DSARBD
123600000531     C                   PARM                    DSARBK
123700000531     C                   PARM                    DARBT
123800000531     C                   PARM                    DSARBG
123900030718     C                   PARM                    trul90ds
124000000531     C                   MOVEL     KPJBU         DS48
124100000615      * SE NON C'È ERRORE FORZO CHE NON C'È CONTRASSEGNO
124200000531     C     D48ERR        IFEQ      *BLANK
124300000531     C                   Z-ADD     0             §3AFCA
124400000601     C                   ELSE
124500000601     C                   MOVE      'X'           ERRCS             1
124600000531     C                   END
124700000531     C                   ENDIF
124800000615      *  SOLO PER DISTINTA SDA SI DEVE FORZARE IL COD. CONS. ANOMALA
124900000615     C     DISSDA        IFEQ      'S'
125000000615     C     ARBFFD        IFEQ      'S'
125100000615     C                   MOVE      'H'           V02CCA
125200000615     C                   END
125300000615      * CONTROLLO SE DETERMINO IL COD. CONS. ANOMALA DAGLI EVENTI
125400000615     C                   EXSR      EVENTI
125500000615     C                   END
125600000615      *
125700941115     C*
125800941123     C     §3AFCA        IFGT      0                                            --- 01 ---
125900941206     C     ARBICC        IFEQ      ' '
126000941206     C     ARBICC        OREQ      'R'
126100941115     C* Controllo se C/Assegno sulla figlia
126200051115     C     KSPE          CHAIN     FIAR901L                           95
126300941123     C     *IN95         IFEQ      '1'                                          --- 02 ---
126400941115     C* Controllo se C/Assegno sulla mamma
126500941206     C     VH2AAP        IFNE      0
126600941115     C                   Z-ADD     LBLAAP        KAAP
126700941115     C                   Z-ADD     LBLLPP        KLPP
126800941115     C                   Z-ADD     LBLNRP        KNRP
126900000615     C                   Z-ADD     LBLNSP        KNSPP
127000941115     C     KARB1         CHAIN     FNARB01L                           95
127100950222      * CONTROLLO CHE IL C/ASS. SULLA MAMMA NON SIA GIA' INCASSATO
127200950222     C  N95ARBICC        IFNE      ' '
127300950222     C     ARBICC        ANDNE     'R'
127400950222     C                   MOVEL     'S'           V2CICC
127500950222     C                   SETON                                        95
127600950222     C                   ENDIF
127700950222      *
127800140911      *? attenzione ad utilizzare i campi ARB da qui in poi perchè hanno il valore della mamma
127900140911      *?se presente per i riferimenti della spedizione in consegna usare i campi del video
128000051115     C  N95KARB1         CHAIN     FIAR901L                           95
128100941206     C                   END
128200941206     C                   END                                                    --- 02 ---
128300941123     C     *IN95         IFEQ      '0'                                          --- 02 ---
128400941115     C                   Z-ADD     AR9CAS        WCAS
128500941115     C                   MOVEL     AR9VCA        WVCA
128600941115     C                   MOVEL     AR9TIC        WTIC
128700941123     C                   END                                                    --- 02 ---
128800061108     C* Carico cliente x controllo tabella "TM"
128900061108     C     ARBCCM        IFGT      *ZEROS
129000061108     C                   MOVE      ARBCCM        §CLITM
129100941115     C                   ELSE
129200941115     C                   MOVE      ARBKSC        §CLITM
129300941115     C                   END
129400941123     C                   ELSE
129500941219     C                   MOVE      'S'           V2CICC
129600941123     C                   END                                                    --- 01 ---
129700941206     C                   END                                                    --- 01 ---
129800061127     C* salvo il codice cliente perchè se 9999 e no cod. fisc. richiamo
129900061127     c* la manutenzione bolle per inserirlo
130000061127     C                   eval      savksc = ARBksc
130100061127     C                   clear                   savcf
130200021107      *
130300941115     C* Conto i colli ancora da consegnare
130400941115     C*  Colli su figlia
130500941115     C     KSPE          CHAIN     FNART01L                           95
130600941115     C     *IN95         IFEQ      '0'
130700941115     C     *IN95         DOWEQ     '0'
130800941115     C     ARTDCM        IFEQ      0
130900941124     C                   ADD       1             V02NCL
131000021107      *
131100000427      *ROUTINE PER GESTIONE SPUNTE SU FOGLIO 8 MANCATA CONSEGNA
131200101119      *  richiama se distinta non PDA
131300101119     c                   if        wpda = 'N'
131400000427     C                   EXSR      MANCON
131500080708     c                   endif
131600941115     C                   END
131700941124     C     KSPE          READE     FNART01L                               95
131800941115     C                   END
131900941123     C                   ELSE
132000941115     C*  Colli su mamma
132100941206     C     VH2AAP        IFNE      0
132200941206     C                   Z-ADD     LBLAAP        KAAP
132300941206     C                   Z-ADD     LBLLPP        KLPP
132400941206     C                   Z-ADD     LBLNRP        KNRP
132500000615     C                   Z-ADD     LBLNSP        KNSPP
132600941115     C     KARB1         CHAIN     FNART01L                           95
132700941115     C     *IN95         DOWEQ     '0'
132800941115     C     ARTDCM        IFEQ      0
132900941124     C                   ADD       1             V02NCL
133000021107      *
133100000427      *ROUTINE PER GESTIONE SPUNTE SU FOGLIO 8 MANCATA CONSEGNA
133200101119      * richiama se distinta non PDA
133300101119     c                   if        wpda  = 'N'
133400000427     C                   EXSR      MANCON
133500080708     c                   endif
133600941115     C                   END
133700941124     C     KARB1         READE     FNART01L                               95
133800941115     C                   END
133900941206     C                   END
134000941206     C*
134100941124     C                   END                                                    Colli su figlia
134200031013      * se chisura con spunta forzo § per evidenziare le bolle non spuntate
134300031013     c                   if        kcdaz ='XR53' and arbtpr = 'A'and
134400031014     c                             arbprg = 0 and v2ccon = 'NO'
134500031013     c                   move      '§'           v02ccn
134600031013     c                   endif
134700031013     c
134800941115     C*
134900941206     C     V02NCL        IFNE      0
135000941115     C                   Z-ADD     ARTAAS        HARAAS
135100941115     C                   Z-ADD     ARTLNP        HARLNP
135200941115     C                   Z-ADD     ARTNRS        HARNRS
135300941115     C                   Z-ADD     ARTNSP        HARNSP
135400941117     C                   Z-ADD     ARTFLS        V2CFLS
135500941206     C                   END
135600941206     C                   ADD       1             NRR2
135700021107     C*
135800941130     C*  Routine per determinazione volume da utilizzare
135900941130     C                   EXSR      DETVOL
136000021107      *
136100941130     C* Determino peso e volume ancora da consegnare
136200021107      * ----------------------------------------------
136300021107      *   (Prima prendeva solo il peso da Peso Fatturazione)
136400021107      *   adesso in base al calcolo della routine (detPES) prende il peso
136500021107      *    da fatturazione o da VDL in base al maggiore dei 2 x poter
136600021107      *    calcolare il peso Medio da attribuire ai colli che non sono passati
136700021107      *    sul VDL.
136800021107      *
136900021107      *  Routine per determinazione peso da utilizzare
137000021107     C                   EXSR      DETpes
137100021107      *
137200021107      * nuovo calcolo del peso:
137300021107     C**** ARBPKF        MULT      V02NCL        APPOG            18 3
137400021107     C     Wpes          MULT      V02NCL        APPOG            18 3
137500941202     C     APPOG         DIV(H)    ARBNCL        V02PKG
137600021107      *
137700021107     C     WVOL          MULT      V02NCL        APPOG            18 3
137800021107     C     APPOG         DIV(H)    ARBNCL        V02VOL
137900080714      *solo per distinte PDA
138000080714     c* imposto tipo incasso contrassegno e assegnato da fipctCET
138100080714     c* verifico se esiste riga in FIPCT00F relativa alla bolla
138200131203     c                   setoff                                       52
138300101119     c                   if        wpda = 'S'
138400080714     c                   clear                   fipctcetds
138500140911     C                   Z-ADD     vnmdoc        PCTNDC
138600131203      * verifico se per la chiave bolla distinta esiste un evento cep abbinato
138700131203      *nel caso carico i campi hidden per effettuare i controlli
138800131203     c     kcetcep       chain     fipct02l
138900131203     c                   if        %found(fipct02l)
139000131203     c                   movel     pctdati       fipctcetds
139100131203     c                   seton                                        52
139200131203     c                   endif
139300131203     c                   movel     §PCTORA       w04ora            4 0
139400131205     c                   movel     w04ora        vh2hco
139500131205     c                   move      w04ora        vh2mco
139600131203     c                   move      §pctcmc       vh2evcep
139700131203     c                   clear                   fipctcetds
139800080714     c     kcet          setgt     fipct02l
139900080714     c     kcet          readpe    fipct02l
140000080714     c                   if        not %eof(fipct02l)
140100080714     c                   movel     pctdati       fipctcetds
140200080714     c                   endif
140300080718     c* imposta campi data e ora consegna reali se consegnata o parziale
140400101123     c                   move      §PCTcmc       V02CCN
140500131205     c                   move      §PCTcmc       Vh2CCN
140600131205     c                   move      §PCTflgspc    Vh2spc
140700101123     c                   z-add     1             xy                3 0
140800101123     c     §pctcmc       lookup    c2a(xy)                                92
140900101123     c                   if        *in92
141000101123     c                   movel     d2a(xy)       ds2a
141100101123     c                   if        §2aflg = '§' and §2acpt = 'S'
141200101123     c                   move      'T  '         v02ccn
141300131205     c                   move      'T  '         vh2ccn
141400101123     c                   endif
141500101123     c                   endif
141600101014     c*                  z-add     §PCTDCR       V2CDCR
141700101014     c*                  z-add     §PCTHCR       V2CHCR
141800080723     c                   if        §pctdata > 0
141900080714     c                   move      §PCTDATA      dataisoc
142000080714     c                   move      dataisoc      dataeurc
142100080714     c                   move      dataeurc      wv02dtc           8 0
142200080714     c                   movel     wv02dtc       v02dtc
142300131203     c                   movel     §PCTORA       w04ora            4 0
142400131203     c                   move      w04ora        v02mco
142500131203     c                   movel     w04ora        v02hco
142600080723     c                   endif
142700080714     c                   move      §PCTRISERV    v02ris
142800110215      * se inserita da autista mette in reverse campo tipo consegna particolare x ora solo PIANI
142900110215     c                   setoff                                       2125
143000110215     c                   if        §pcttc1 <> *blank
143100101203     c                   if        v2ccp1 = *blank
143200080714     c                   move      §PCTTC1       V2CCP1
143300110215     c                   seton                                        21
143400101203     c                   else
143500101203     c                   if        v2ccp2 = *blank
143600101203     c                   move      §PCTTC1       V2CCP2
143700110215     c                   seton                                        25
143800101203     c                   endif
143900101203     c                   endif
144000110215     c                   endif
144100101215     c                   if        v2ccp1 = v2ccp2
144200101215     c                   clear                   v2ccp2
144300101215     c                   endif
144400110214      * prepara flag per scrittura file spia in fnlr53 utilizza v2sda vecchio flag per
144500110214      * riconoscere distinte SDA non più utilizzato
144600110214      * verifica che non ci sia spunta in predistinta e dichiarazione di raccolta firma su PDA
144700110214     c                   clear                   v2sda
144800110214     c                   if        arbscn = ' '
144900110214     c                   movel     'L'           v2sda
145000110214     c                   endif
145100101111      * se non ho già un motivo di turno verifico che non sia stato effettuato un doppio tentativo
145200101111      * testando il campo del "tentativo precedente"
145300101123     c                   if        §pctcmcp <> *blank and v02ccn <> 'T  '
145400131209     c                   z-add     1             xy
145500101123     c     §pctcmcp      lookup    c2a(xy)                                92
145600101123     c                   if        *in92
145700101123     c                   movel     d2a(xy)       ds2a
145800101123     c                   if        §2acpt = 'S'
145900101123     c                   movel     'T  '         v2ctur
146000101123     c                   endif
146100101111     c                   endif
146200101123     c                   endif
146300080714      * se presente nome firmatario rilevato sul PDA lo preimposto
146400080714     c                   if        §PCTNOMFIR <> *blank
146500080714     c                   movel     §PCTNOMFIR    v2cfir
146600080714     c                   endif
146700080714      * se presente nota alla consegna la preimposto
146800080717     c                   if        §PCTNOTAUT <> *blank
146900080717     c                   movel     §PCTNOTAUT    v2cnt1
147000090216     c                   clear                   v02sel
147100090220     c                   clear                   v02selb
147200090216     c                   endif
147300080714     c                   endif
147400021107      *
147500941115     C*  Scrivo record importi
147600941115     C     WIMA          IFNE      0
147700941115     C     WCAS          ORNE      0
147800120312     c                   movel     v2clnp        comlnp
147900120313     c                   if        v2cnrs <> 0
148000120312     c                   movel     v2cnrs        comnrs
148100120313     c                   else
148200120313     c                   clear                   comnrs
148300120313     c                   end
148400120312     c                   movel     v2cnsp        comnsp
148500120312     c                   eval      v3calfa1 = comlnp + ' ' + comnrs + ' ' +
148600120312     c                             comnsp
148700120312     c                   clear                   v3calfa2
148800110214     C                   MOVE      V2POST        V3POST
148900941123     C                   Z-ADD     V2CAAS        V3CAAS
149000941115     C                   Z-ADD     V2CLNP        V3CLNP
149100030305     C                   Z-ADD     v2cLNA        v3cLNA
149200941115     C                   Z-ADD     V2CNRS        V3CNRS
149300941115     C                   Z-ADD     V2CNSP        V3CNSP
149400941115     C                   MOVEL     V2CCBO        V3CCBO
149500941115     C                   MOVEL     V2CCON        V3CCON
149600941115     C                   MOVEL     V02DES        V3CDES
149700941115     C     NRR3          ADD       1             V2CNRD
149800941118     C                   MOVEL     *BLANKS       V3CTOT
149900941121     C                   Z-ADD     NRR2          V3CNRI
150000941207     C                   SETON                                        22
150100941118     C*  Controllo se esiste C/Assegno
150200941118     C     WCAS          IFGT      0
150300941118     C                   ADD       1             NRR3
150400941118     C                   MOVEL     'C/A'         V3DTPI
150500941118     C                   Z-ADD     WCAS          V3CIMP
150600941118     C                   MOVEL     WVCA          V3CVCA
150700990924     C                   MOVEL     WVCA          VH3DIV
150800120312     c* ragione sociale intestatario assegno.
150900110330     c     kar5          chain     fiar501l
151000110330     c                   if        %found(fiar501l)
151100110330     c                   movel     ar5uni        dar5gen
151200110330     c                   reset                   trulintds
151300110330     c                   eval      iINTtic  = wtic
151400110330     c                   eval      iINTrscM = arbrsm
151500110330     c                   eval      iINTrscO = arbrmo
151600110330     c                   eval      iINTcdi  = §AR5cdi
151700110330     c                   call      'TRULINTR'
151800110330     c                   parm                    TRULINTDS
151900110330     c                   if        oINTerr = *blank
152000110330     c                   eval      v3cmit  = oINTrsci
152100120312     c                   eval      v3calfa1 = oINTrsci
152200120312     c                   eval      v3calfa2 = comlnp + ' ' + comnrs + ' ' +
152300120312     c                             comnsp
152400110330     c                   end
152500120312     c                   end
152600991006     C* Gestisco riga di totale solo se esiste anche assegnato
152700941118     C     WIMA          IFGT      0
152800991006     C* .. & la divisa di incasso assegnato è quella corrente...
152900011009     C     WDIV          IFEQ      §GEDCR
153000991006     C* .. o quella alternativa ..
153100991006     C     WDIV          OREQ      DIVEUR
153200991006     C* .. & la divisa di incasso C/Assegno è quella corrente...
153300011009     C     WVCA          IFEQ      §GEDCR
153400991006     C* .. o quella alternativa ..
153500991006     C     WVCA          OREQ      DIVEUR
153600941118     C                   MOVEL     'SI'          V3CTOT
153700941118     C                   END
153800991006     C                   END
153900991006     C                   END
154000941118     C                   MOVEL     WTIC          V3CTIC
154100101119     c                   if        wpda = 'S'
154200101119     c                   move      §PCTINCCAS    vh3ctic
154300080708     c*spedizione consegnata ma non incassata  contrassegno
154400090126     c                   if        §pctinccas = *blank or
154500090126     c                             §pctinccas = 'N'
154600110208     c                   move      'R'           v02sn1
154700090126     c                   endif
154800080708     c                   endif
154900990923     C* Se tipo incasso non prevede divisa proteggo campo relativo
155000990923     C                   Z-ADD     1             X
155100990923     C     WTIC          LOOKUP    C1A(X)                                 95
155200990923     C                   MOVEL     D1A(X)        DS1A
155300991115     C     §1AFMB        COMP      'M'                                    06
155400011009     C  N06WVCA          IFNE      §GEDCR
155500991116     C     WVCA          ANDNE     DIVEUR
155600991116     C                   SETON                                        06
155700991116     C                   END
155800941118     C                   WRITE     LR53S03
155900941207     C                   SETON                                        03
156000941118     C                   END
156100941115     C*  Controllo se esiste Assegnato
156200941115     C     WIMA          IFGT      0
156300941115     C                   ADD       1             NRR3
156400941115     C                   MOVEL     'Ass'         V3DTPI
156500941115     C                   Z-ADD     WIMA          V3CIMP
156600990923     C                   MOVEL     WDIV          V3CVCA
156700990924     C                   MOVEL     WDIV          VH3DIV
156800941123     C                   MOVEL     ARBMIF        V3CTIC
156900080708     c                   move      §PCTINCFAT    vh3ctic
157000101119     c                   if        wpda = 'S'
157100101119     c                   move      §PCTINCFAT    vh3ctic
157200080708     c*spedizione consegnata ma non incassata  assegnato
157300080708     c                   if        §PCTCMC = *blank and
157400080708     c                             (§PCTINCFAT = *blank or
157500080708     c                             §PCTINCFAT = 'N')
157600110208     c                   move      'R'           v02sn1
157700080708     c                   endif
157800080708     c                   endif
157900990927     C                   SETOFF                                       06
158000941115     C                   WRITE     LR53S03
158100941115     C                   MOVEL     *BLANKS       V3CDES
158200941115     C                   END
158300991006     C*  Se esitono entrambi verifico se scrivere riga totale:
158400941115     C     WCAS          IFGT      0
158500941115     C     WIMA          ANDGT     0
158600991006     C* .. Divisa di incasso assegnato è quella corrente...
158700011009     C     WDIV          IFEQ      §GEDCR
158800991006     C* .. o quella alternativa ..
158900991006     C     WDIV          OREQ      DIVEUR
159000991006     C* .. & la divisa di incasso C/Assegno è quella corrente...
159100011009     C     WVCA          IFEQ      §GEDCR
159200991006     C* .. o quella alternativa ..
159300991006     C     WVCA          OREQ      DIVEUR
159400990928     C                   SETON                                        05
159500941115     C                   ADD       1             NRR3
159600011009     C                   MOVEL     §GEDCR        V3DTPI
159700991006     C* Se l'assegnato non è in moneta di conto converto e sommo
159800991006     C* il risultato nel totale
159900011009     C     WDIV          IFNE      §GEDCR
160000991006     C                   CLEAR                   YEURDS
160100991006     C                   MOVEL     WDIV          YECDVI
160200991006     C                   Z-ADD     WIMA          YECIMI
160300011009     C                   MOVEL     §GEDCR        YECDVO
160400991006     C                   MOVEL     'H'           YECTAR
160500991006     C                   CALL      'YEURCO'
160600991006     C                   PARM                    YEURDS
160700991006     C                   Z-ADD     YECIMO        V3CIMP
160800991006     C                   ELSE
160900991006     C                   Z-ADD     WIMA          V3CIMP
161000991006     C                   END
161100991006     C* Se il C/Assegno non è in moneta di conto converto e sommo
161200991006     C* il risultato nel totale
161300011009     C     WVCA          IFNE      §GEDCR
161400991006     C                   CLEAR                   YEURDS
161500991006     C                   MOVEL     WVCA          YECDVI
161600991006     C                   Z-ADD     WCAS          YECIMI
161700011009     C                   MOVEL     §GEDCR        YECDVO
161800991006     C                   MOVEL     'H'           YECTAR
161900991006     C                   CALL      'YEURCO'
162000991006     C                   PARM                    YEURDS
162100991007     C                   ADD       YECIMO        V3CIMP
162200991006     C                   ELSE
162300991007     C                   ADD       WCAS          V3CIMP
162400991006     C                   END
162500011009     C                   MOVEL     §GEDCR        V3CVCA
162600011009     C                   MOVEL     §GEDCR        VH3DIV
162700941115     C                   MOVEL     *BLANKS       V3CTIC
162800120313     C                   MOVEL     *BLANKS       V3Calfa1
162900120313     C                   MOVEL     *BLANKS       V3Calfa2
163000941115     C                   WRITE     LR53S03
163100941115     C                   END
163200991006     C                   END
163300991006     C                   END
163400941115     C*
163500941115     C                   Z-ADD     NRR3          V2CNRA
163600941115     C                   END
163700101015     c* se la distinta è con PDA accendo il sflnexchg perchè un esito devo
163800101015     c* averlo ricevuto
163900101119     c                   if        wpda = 'S'
164000101119     c*mm                if        (§DSTTSTPDA = *blank
164100101119     c*mm                          or §DSTTSTPDA = 'O') and
164200101119     c*mm                          (dstpda = 'E' or dstpda = 'C')
164300101015     C                   SETOn                                        22
164400101015     c                   else
164500101015     C                   SETOFF                                       22
164600101015     c                   end
164700941115     C                   WRITE     LR53S02
164800031017     c                   if        v2ccon = 'NO' and d53ita <> 'E'
164900031016     c                   exsr      eventimic
165000031016     c                   endif
165100941115     C*
165200030828      * controllo l'azione per sapere se leggere wf o arb
165300030828     c                   if        kcdaz <> 'XR53'
165400020822     C     KARB          READE     FNARB70L                               96
165500030828     c                   else
165600120529     C                   READ      fnarbj0f                               96
165700030828     c                   endif
165800941115     C                   END
165900941115     C*
166000000601     C                   ENDSR
166100000602      *---------------------------------------------------------*
166200000615      *  Richiamo EVENTI PER ATTRIBUZIONE COD.CONS.ANOMALA      *
166300000602      *---------------------------------------------------------*
166400000602     C     EVENTI        BEGSR
166500000615      * CERCO GLI EVENTI CON NR SPEDIZ. CHE SI È IN DISTINTA
166600000615      * IN QUANTO NON SI PREVEDONO LEGAMI
166700000615      *
166800000615     C                   Z-ADD     KAAS          KAASE
166900000615     C                   Z-ADD     KLNP          KLNPE
167000000615     C                   Z-ADD     KNRS          KNRSE
167100000615     C                   Z-ADD     KNSP          KNSPE
167200000615     C     KEVB          SETGT     FNEVB05L
167300000602     C                   DO        *HIVAL
167400000615     C     KEVB          READPE    FNEVB05L                               99
167500000602     C   99              LEAVE
167600000602     C                   Z-ADD     1             X
167700000602     C     EVBCEV        LOOKUP    C2Z(X)                                 98
167800000614     C     *IN98         IFEQ      '1'
167900000602     C                   MOVEL     D2Z(X)        DS2Z
168000000602     C     §2ZCCA        IFNE      *BLANK
168100000602     C                   MOVE      §2ZCCA        V02CCA
168200000602     C                   LEAVE
168300000602     C                   ELSE
168400000602     C                   ITER
168500000602     C                   END
168600000602     C                   END
168700000602     C                   ENDDO
168800000602     C                   ENDSR
168900010402      *---------------------------------------------------------*
169000010402      *  Richiamo EVECON PER ATTRIBUZIONE COMUNICAZIONE CONSEGNA*
169100010402      *---------------------------------------------------------*
169200010402     C     EVECON        BEGSR
169300010402      * CERCO GLI EVENTI CON NR SPEDIZ. E TIPO EVENTO COC
169400010402      *
169500010402     C                   SETOFF                                       97
169600131106     C                   CLEAR                   Devb01
169700010402     C     KEVB4         SETLL     FNEVB04L
169800010402     C                   DO        *HIVAL
169900010402     C     KEVB4         READE     FNEVB04L                               97
170000010402     C   97              LEAVE
170100131106     C                   MOVEL     EVBNOT        Devb01
170200131106     C                   IF        §notNDC = %editc(ARBNDC:'X')
170300010402     C                   MOVEL     EVBHEV        V02HCO
170400010402     C                   MOVE      EVBHEV        V02MCO
170500010402     C                   MOVE      EVBDEV        DTCCO2            2 0
170600010402     C                   MOVEL     EVBDEV        DTCCO6            6 0
170700010402     C                   MOVE      DTCCO6        V02DTC
170800010402     C                   MOVEL     DTCCO2        V02DTC
170900010402     C                   LEAVE
171000010402     C                   ELSE
171100010402     C                   ITER
171200010402     C                   END
171300010402     C                   ENDDO
171400010402     C                   ENDSR
171500031016      *---------------------------------------------------------*
171600031016      *  Richiamo eventimic  ATTRIBUZIONE MIC se non esiste     *
171700031016      *---------------------------------------------------------*
171800031016     C     eventimic     BEGSR
171900031016      * CERCO GLI EVENTI CON NR SPEDIZ. E TIPO EVENTO COC
172000031016      *
172100031016     C                   SETOFF                                       97
172200131106     C                   CLEAR                   Devb01
172300031016     C                   CLEAR                   esiste            1
172400031016     c                   if        vh2aap = 0
172500031016     c                   z-add     v2caas        Kanno
172600031016     c                   z-add     v2clnp        Kpart
172700031016     c                   z-add     v2cnrs        Kser
172800031016     c                   z-add     v2cnsp        Ksped
172900031016     c                   else
173000031016     c                   z-add     vh2aap        Kanno
173100031016     c                   z-add     vh2lpp        Kpart
173200031016     c                   z-add     vh2nrp        Kser
173300031016     c                   z-add     vh2nsp        Ksped
173400031016     c                   endif
173500031016     C     KEVbmic       SETLL     FNEVB04L
173600031016     C                   DO        *HIVAL
173700031016     C     KEVbmic       READE     FNEVB04L                               97
173800031016     C   97              LEAVE
173900031104     c                   if        evbnot <> *blank
174000131106     C                   MOVEL     EVBNOT        Devb01
174100131106     C                   IF        §notNDC = %editc(vnmDOC:'X')
174200031016     C                   MOVE      'X'           esiste
174300031016     C                   LEAVE
174400031016     C                   ELSE
174500031016     C                   ITER
174600031016     C                   END
174700031104     c                   endif
174800031016     C                   ENDDO
174900031016     c                   if        esiste = *blank
175000031016     c                   clear                   fnevb0
175100031016     C                   MOVEL     'MIC'         xEVBCEV
175200031016     C                   Z-ADD     OGGI          xEVBDTV
175300031016     C                   TIME                    xEVBORV
175400031016     C                   Z-ADD     V2CLNA        xEVBFLE
175500031016     C                   Z-ADD     DUTCOU        xEVBCDU
175600031016     C                   Z-ADD     Kanno         xEVBAAS
175700031016     C                   Z-ADD     Kpart         xEVBLNP
175800031016     C                   Z-ADD     Kser          xEVBNRS
175900031016     C                   Z-ADD     Ksped         xEVBNSP
176000031016     C                   Z-ADD     vh2ddt        xEVBDEV
176100031016     c
176200031016     C     fpp(i)        IFEQ      'M'
176300031016     C                   MOVEL     0800          xEVBHEV
176400031016     C                   ELSE
176500031016     C                   MOVEL     1200          xEVBHEV
176600031016     C                   END
176700131106     c                   move      vnmdoc        §notNDC
176800131106     c                   move      apdpdr        §notPDC
176900140110     c                   move      'C'           §notagg
177000131106     C                   MOVEL     Devb01        xEVBNOT
177100031016     c                   write     fnevb0
177200031016     c                   endif
177300031016     C                   ENDSR
177400941115      *---------------------------------------------------------*
177500941115      *  Richiamo pgm. FNLR53R                                  *
177600941115      *---------------------------------------------------------*
177700941115     C     CAL53         BEGSR
177800941115     C*
177900960326     C                   MOVEL     'S'           WCAL53            1
178000960116     C                   Z-ADD     V1CLNA        D53FGS
178100050711     c                   if        d53ita = *blank
178200960116     C                   MOVEL     'I'           D53ITA
178300050711     c                   endif
178400941124     C                   MOVEA     NDC           NDC1
178500941124     C                   MOVEL     APDRSC        D53DSF
178600080715     C                   MOVE      wpda          D53PDA
178700050516     C                   move      APDpdr        D53dsf
178800950530     C                   MOVEL     APDFCM        D53CMG
178900941115     C                   MOVEL     ' '           D53F03
179000941115     C                   MOVEL     CNTPDR        D53WRT
179100941115     C                   MOVEL     FNLR53        KPJBU
179200140519     C                   CALL      'FNLR53CM'
179300941115     C                   PARM                    KPJBA
179400941115     C                   MOVEL     KPJBU         FNLR53
179500941124     C                   MOVEL     ' '           D53TRI
179600950223     C*
179700941115     C*
179800941115     C                   ENDSR
179900941115     C*-----------------------------------------------------*
180000941115     C     DETVOL        BEGSR
180100941115     C*-----------------------------------------------------*
180200941115     C* VOLUME : - SE ARBFVR   ("T"), VOLUME ARBVOL
180300941115     C*            ALTRIMENTI
180400941115     C*               - SE TOTALE   ("T"), VOLUME ARBVLS
180500941115     C*               - SE PARZIALE ("Z"), VOLUME ARBVLS (SE MAGGIORE)
180600941115     C*                               ALTRIMENTI SOMMO ARBVOL
180700941115     C     ARBFVF        IFEQ      'T'
180800941115     C                   Z-ADD     ARBVLF        WVOL
180900941115     C                   ELSE
181000050208     C     ARBNCR        ifeq      ARBNCL
181100941115     C                   Z-ADD     ARBVLC        WVOL
181200941115     C*
181300941115     C                   ELSE
181400941115     C     ARBVLC        IFGE      ARBVLF
181500941115     C                   Z-ADD     ARBVLC        WVOL
181600941115     C                   ELSE
181700941115     C                   Z-ADD     ARBVLF        WVOL
181800941115     C                   ENDIF
181900941115     C                   ENDIF
182000941115     C                   ENDIF
182100941115     C*
182200941115     C                   ENDSR
182300021107     C*-----------------------------------------------------*
182400021107     C     detpes        begsr
182500021107     C*-----------------------------------------------------*
182600021107      * se è considerata un'unica spedizione
182700050208     C     arbncp        ifeq      arbncl
182800021107     C                   Z-ADD     arbPKC        wpes
182900021107     C*
183000021107     C                   ELSE
183100021107     C*
183200021107     C     arbPKC        ifge      arbPKF
183300021107     C                   Z-ADD     arbPKC        wpes
183400021107     C                   else
183500021107     C                   Z-ADD     arbPKF        wpes
183600021107     C                   end
183700021107     C*
183800021107     C                   ENDIF
183900021107     C*
184000021107     C                   ENDSR
184100000427     C*-----------------------------------------------------*
184200000427     C     MANCON        BEGSR
184300000427     C*-----------------------------------------------------*
184400000427     C*ROUTINE GESTIONE SPUNTE SU FOGLIO DI CATEGORIA 8 MANCATE CONSEGN
184500000427     C*LEGGO TUTTE LE SPUNTE PRESENTI SUL FOGLIO E MEMORIZZO ALL'INTERNO
184600000427     C*DELLA SPEDIZIONE LA MANCATA CONSEGNA CON PRIORITÀ PIÙ BASSA E
184700000427     C*IMPOSTO IL SFL PROPONENDO L'EVENTO RELATIVO DA TABELLA 3E
184800000427      *
184900000427      *CONTROLLO CAMBIO SPEDIZIONE PER INIZIALIZZARE CAMPI DI COMODO
185000000427     C     EXNSP         IFNE      ARTNSP
185100000427     C                   MOVE      '99'          COMPRI
185200000427     C                   MOVE      ARTNSP        EXNSP
185300000427     C                   END
185400000503     C                   Z-ADD     ARBNDC        ARBND6
185500070122     C     KFIB          CHAIN     FNBRV01L                           93
185600000427     C     *IN93         IFEQ      '0'
185700000526     C     BRVCAN        IFNE      *BLANK
185800000427     C                   Z-ADD     1             X
185900000427     C     BRVCAN        LOOKUP    COA(X)                                 93
186000000427     C     *IN93         IFEQ      '1'
186100000427     C     PMC(X)        ANDLT     COMPRI
186200001227     C     V2POST        IFEQ      'P'
186300000427     C                   MOVEL     CMC(X)        V02CCN
186400001227     C                   ELSE
186500001227     C                   MOVEL     CMB(X)        V02CCN
186600001227     C                   END
186700000427     C                   MOVEL     PMC(X)        COMPRI
186800000526     C                   ENDIF
186900000526     C                   ELSE
187000000526     C                   MOVEL     'N'           V02CCN
187100000427     C                   ENDIF
187200000427     C                   ENDIF
187300030829      * se chisura con spunta forzo § per evidenziare le bolle non spuntate
187400031013     c*                  if        kcdaz ='XR53' and arbtpr = 'A'and
187500031013     c*                            arbprg = 0
187600031013     c*                  move      '§'           v02ccn
187700031013     c*                  endif
187800030829     c
187900000427     C*
188000000427     C                   ENDSR
188100070319     C*----------------------------------------------------*
188200070319     C     geslck        begsr
188300070319     C*----------------------------------------------------*
188400070319     c                   Eval      ul82§rrn = dstnrr
188500070319     c                   Eval      UL82§FIL = 'FIDST00F'
188600070319     c                   Eval      UL82§WIN = 'S'
188700070319     c                   Eval      UL82§F7  = 'S'
188800070319     c                   Eval      UL82§num = 2
188900070319     c                   Eval      UL82§att = 1
189000070319     c                   Eval      UL82§mss = 'Si sta bloccando la Distinta -
189100070319     c                             numero :  '
189200070319     c                             + %editc(knfv6:'Z') +  ' terminare il lav-
189300070319     c                             oro '
189400070319     c                   Eval      UL82§msw = 'La distinta numero : '
189500070319     c                             + %editc(knfv6:'Z') +  ' non è disponibile '
189600070319      * Effettuo la chiamata al *PGM d utilità
189700070319     c                   Call(e)   'TRUL82R'
189800070319     c                   Parm                    Trul82ds
189900070319     c                   if        ul82§sts = 'A'
190000070319     c                   goto      fine
190100070319     c                   endif
190200070319
190300070319     C                   ENDSR
190400941115     C*-----------------------------------------------------*
190500941115     C*  Operazioni iniziali
190600941115     C*-----------------------------------------------------*
190700941115     C     *INZSR        BEGSR
190800941115     C*
190900941115     C     *ENTRY        PLIST
191000941115     C                   PARM                    KPJBA
191100030718     C                   PARM                    trul90ds
191200960326     C                   MOVEL     'N'           WCAL53            1
191300941115     C*
191400941115     C* Apertura controllata dei files
191500020822     C                   OPEN      FNFVV01L                             16
191600070410     C  N16              OPEN      fidst09L                             16
191700941124     C  N16              OPEN      FNARB01L                             16
191800060214     C  N16              OPEN      FiAR401L                             16
191900051115     C  N16              OPEN      FIAR901L                             16
192000941124     C  N16              OPEN      FNART01L                             16
192100941115     C   16              GOTO      FINE                                         OPEN vado a fine
192200941115     C* Decodifica Elaboratore
192300020822     c     *dtaara       define    §azute        azuteds
192400020822     c     *dtaara       define    §datiute      ddatiute
192500020822     C                   in(E)     *dtaara
192600020822     C                   IF        %Error  or  RSUT = *blanks
192700020822     C                   call      'TIBS34R'
192800020822     C                   parm                    Tibs34Ds
192900020822     C                   in        *dtaara
193000020822     c                   ENDIF
193100020822     C                   Z-ADD     Dutpou        V1CLNA            3 0
193200941115     C*
193300941115     C*  Definizione chiavi
193400080708     C     Kcet          KLIST
193500101007     C                   KFLD                    V1CLNA
193600101007     C                   KFLD                    PCTNDC
193700140911     C                   KFLD                    vh2pdr
193800080708     C                   KFLD                    TTRD
193900140911     C                   KFLD                    v2cAAS
194000140911     C                   KFLD                    v2cLNP
194100140911     C                   KFLD                    v2cNRS
194200140911     C                   KFLD                    v2cNSP
194300080708     C                   move      'CET'         TTRD              3
194400131203     C     Kcetcep       KLIST
194500131203     C                   KFLD                    V1CLNA
194600131203     C                   KFLD                    PCTNDC
194700140911     C                   KFLD                    vh2pdr
194800131203     C                   KFLD                    TTRDcep
194900140911     C                   KFLD                    v2cAAS
195000140911     C                   KFLD                    v2cLNP
195100140911     C                   KFLD                    v2cNRS
195200140911     C                   KFLD                    v2cNSP
195300131203     C                   move      'CEP'         TTRDcep           3
195400080708     c
195500110330     C     Kar5          KLIST
195600110330     C                   KFLD                    v2cAAS
195700110330     C                   KFLD                    v2cLNP
195800110330     C                   KFLD                    v2cNRS
195900110330     C                   KFLD                    v2cNSP
196000110330     c                   kfld                    t5trd
196100110330     C                   move      'GEN'         T5trd             3
196200110330     c
196300941115     C     KSPE          KLIST
196400941115     C                   KFLD                    KAAS
196500941115     C                   KFLD                    KLNP
196600941115     C                   KFLD                    KNRS
196700941115     C                   KFLD                    KNSP
196800000615     C     KEVB          KLIST
196900000615     C                   KFLD                    KAASE
197000000615     C                   KFLD                    KLNPE
197100000615     C                   KFLD                    KNRSE
197200000615     C                   KFLD                    KNSPE
197300990922     C     KAR6          KLIST
197400990922     C                   KFLD                    KAAS
197500990922     C                   KFLD                    KLNP
197600990922     C                   KFLD                    KNRS
197700990922     C                   KFLD                    KNSP
197800990922     C                   KFLD                    KTRC
197900990922     C     KARI          KLIST
198000990922     C                   KFLD                    KAAS
198100990922     C                   KFLD                    KLNP
198200990922     C                   KFLD                    KNRS
198300990922     C                   KFLD                    KNSP
198400990922     C                   KFLD                    KTIP
198500941115     C*
198600941115     C     KAR4          KLIST
198700941115     C                   KFLD                    KAAS
198800941115     C                   KFLD                    KLNP
198900941115     C                   KFLD                    KNRS
199000941115     C                   KFLD                    KNSP
199100941115     C                   KFLD                    KTRC
199200941115     C*
199300941115     C     KARB1         KLIST
199400941115     C                   KFLD                    KAAP
199500941115     C                   KFLD                    KLPP
199600941115     C                   KFLD                    KNRP
199700000615     C                   KFLD                    KNSPP
199800941115     C*
199900941115     C     KFVV          KLIST
200000941115     C                   KFLD                    KNPG
200100941115     C                   KFLD                    KNFV
200200020822     C                   KFLD                    v1clna
200300070316      *
200400070316     C     Kdst          klist
200500070410     C                   kfld                    v1clna
200600070316     C                   kfld                    knpg
200700070316     C                   kfld                    knfv6             6 0
200800070316      *
200900080707     C                   Z-ADD     4             KNPG
201000080707      *
201100080707     C     Kdsf          klist
201200080707     C                   kfld                    knpg
201300080707     C                   kfld                    knfv6             6 0
201400080707     C                   kfld                    v1clna
201500080707     C                   kfld                    fase              3
201600080707     C                   move      'CRI'         fase
201700941115     C*
201800941115     C     KTAB1         KLIST
201900941115     C                   KFLD                    KKUT
202000941115     C                   KFLD                    KCOD
202100941115     C*
202200941115     C     KTAB2         KLIST
202300941115     C                   KFLD                    KKUT
202400941115     C                   KFLD                    KCOD
202500941115     C                   KFLD                    KKEY
202600991006     C     KTBE          KLIST
202700991006     C                   KFLD                    KCODE
202800991006     C                   KFLD                    KKEYE
202900941115     C*
203000941115     C     KFTT          KLIST
203100941115     C                   KFLD                    KPDR
203200941115     C                   KFLD                    KFVL
203300941115     C                   KFLD                    KDDC
203400941115     C                   KFLD                    KTSR
203500960702     C     KARB          KLIST
203600020923     C                   KFLD                    KIFP
203700020822     C                   KFLD                    KNDC
203800960702     C                   KFLD                    KDDC
203900000427     C     KFIB          KLIST
204000000427     C                   KFLD                    FOGLIO
204100000503     C                   KFLD                    ARBND6            6 0
204200000427     C                   KFLD                    FVVFGS
204300000427     C                   KFLD                    ARTFLS
204400000427     C                   KFLD                    ARTLNA
204500000427     C                   KFLD                    ARTNRS
204600000427     C                   KFLD                    ARTNSC
204700000427     C                   Z-ADD     8             FOGLIO
204800010402     C     KEVB4         KLIST
204900010402     C                   KFLD                    ARBAAS
205000010402     C                   KFLD                    ARBLNP
205100010402     C                   KFLD                    ARBNRS
205200010402     C                   KFLD                    ARBNSP
205300010402     C                   KFLD                    EVECOC
205400010402     C                   MOVE      'COC'         EVECOC
205500021204     C     Kapd          KLIST
205600021204     C                   KFLD                    apdtip
205700021204     C                   KFLD                    arbpdc
205800021204     c                   movel     'A'           apdtip
205900031016     C     KEVBmic       KLIST
206000031016     C                   KFLD                    Kanno
206100031016     C                   KFLD                    Kpart
206200031016     C                   KFLD                    Kser
206300031016     C                   KFLD                    Ksped
206400031016     C                   KFLD                    kevmic
206500941115     C*----------------------------------------------------*
206600941115     C*  Definizione delle variabili
206700000608     C     *LIKE         DEFINE    KPJBU         SAVJBU
206800010402     C     *LIKE         DEFINE    EVBCEV        EVECOC
206900000427     C     *LIKE         DEFINE    ARTNSP        EXNSP
207000000427     C     *LIKE         DEFINE    §3EPMC        COMPRI
207100000427     C     *LIKE         DEFINE    BRVNPG        FOGLIO
207200080714     C     *LIKE         DEFINE    dstpda        wpda
207300080714     C     *LIKE         DEFINE    dstpda        savpda
207400941115     C     *LIKE         DEFINE    ARBAAS        KAAS
207500941115     C     *LIKE         DEFINE    ARBLNP        KLNP
207600941115     C     *LIKE         DEFINE    ARBNRS        KNRS
207700941115     C     *LIKE         DEFINE    ARBNSP        KNSP
207800000615     C     *LIKE         DEFINE    ARBNSP        KNSPP
207900000615     C     *LIKE         DEFINE    ARBAAS        KAASE
208000000615     C     *LIKE         DEFINE    ARBLNP        KLNPE
208100000615     C     *LIKE         DEFINE    ARBNRS        KNRSE
208200000615     C     *LIKE         DEFINE    ARBNSP        KNSPE
208300941115     C     *LIKE         DEFINE    AR4TRC        KTRC
208400941115     C     *LIKE         DEFINE    ARBAAS        KAAP
208500941115     C     *LIKE         DEFINE    ARBLNP        KLPP
208600941115     C     *LIKE         DEFINE    ARBNRS        KNRP
208700941115     C     *LIKE         DEFINE    ARBVLF        WVOL
208800021107     C     *like         define    arbpkF        wpes
208900941115     C     *LIKE         DEFINE    FVVNPG        KNPG
209000941115     C     *LIKE         DEFINE    FVVNFV        KNFV
209100941115     C     *LIKE         DEFINE    TBLKUT        KKUT
209200941115     C     *LIKE         DEFINE    TBLCOD        KCOD
209300941115     C     *LIKE         DEFINE    TBLKEY        KKEY
209400941115     C     *LIKE         DEFINE    AR6IFT        WIMA
209500941115     C     *LIKE         DEFINE    AR9CAS        WCAS
209600941115     C     *LIKE         DEFINE    AR9VCA        WVCA
209700941115     C     *LIKE         DEFINE    AR9TIC        WTIC
209800990922     C     *LIKE         DEFINE    AR6DIV        WDIV
209900941115     C     *LIKE         DEFINE    FTTPDR        KPDR
210000941115     C     *LIKE         DEFINE    FTTFVL        KFVL
210100941115     C     *LIKE         DEFINE    FTTDDC        KDDC
210200960702     C     *LIKE         DEFINE    ARBNDC        KNDC
210300020923     C     *LIKE         DEFINE    ARBIFP        KIFP
210400941115     C     *LIKE         DEFINE    FTTTSR        KTSR
210500990922     C     *LIKE         DEFINE    ARITIP        KTIP
210600991006     C     *LIKE         DEFINE    TBECOD        KCODE
210700991006     C     *LIKE         DEFINE    TBEKE1        KKEYE
210800031016     C     *LIKE         DEFINE    ARBAAS        Kanno
210900031016     C     *LIKE         DEFINE    ARBLNP        Kpart
211000031016     C     *LIKE         DEFINE    ARBNRS        Kser
211100031016     C     *LIKE         DEFINE    ARBNSP        Ksped
211200031016     C     *LIKE         DEFINE    evbcev        kevmic
211300031016     C     *LIKE         DEFINE    evbdev        kdata
211400031016     c                   move      'MIC'         kevmic
211500941115     C*----------------------------------------------------*
211600941115     C* Giro data del giorno
211700941115     C                   TIME                    WHHDAT           14 0
211800941115     C                   MOVE      WHHDAT        G02DAT
211900941115     C                   MOVEL     *BLANK        G02ERR
212000941115     C                   CALL      'XSRDA8'
212100941115     C                   PARM                    WLBDA8
212200941115     C                   Z-ADD     G02INV        OGGI              8 0
212300941214     C                   MOVE      V1CLNA        D53LNA
212400941115     C*----------------------------------------------------*
212500941115     C                   Z-ADD     1             KKUT
212600941115     C                   MOVEL     ' '           WFINE             1
212700941115     C                   MOVEL     '1'           WTPVID            1
212800020826     C                   Z-ADD     dutcou        V1COPR
212900941124     C                   MOVEL     'P'           D53TRI
213000941115     C* Se profilo uguale a REM prendo le filiali a loro associate
213100020822     C     Dutlpo        IFEQ      '1'
213200941115     C                   SETON                                        81
213300941115     C                   END
213400020822     C     Dutlpo        IFEQ      '2'
213500941115     C                   SETOFF                                       81
213600941115     C                   END
213700941115     C*----------------------------------------------------*
213800991006     C*  Aggancio tabella 'GED' x reperire la divisa corrente
213900991006     C                   MOVEL     'GED'         KCODE
214000991006     C                   MOVEL     *BLANKS       KKEYE
214100991006     C                   MOVEL     '1'           KKEYE
214200991006     C     KTBE          CHAIN     TNTBE01L                           13
214300991006     C     *IN13         IFEQ      '0'
214400991006     C                   MOVEL     TBEUNI        DGED
214500991006     C                   END
214600991006     C*
214700020418     C*  tabelle dgd x reperimento limiti chiusura spedizioni
214800020418     C                   MOVEL     'DGD'         KCODe
214900020418     C                   MOVEL     *BLANKS       KKEye
215000020418     C                   MOVEL     '1'           KKEye
215100020418     C     KTBE          CHAIN     TNTBE01L                           13
215200020418     C     *IN13         IFEQ      '0'
215300020418     C                   MOVEL     TBEUNI        DDGD
215400020418     C                   END
215500020418     C*
215600991006     C*  CARICO DATI TABELLA CV
215700991006     C                   MOVEL     *BLANKS       DIVEUR
215800991006     C                   MOVEL     'CV'          KCOD
215900991006     C     KTAB1         CHAIN     TABEL00F                           13
216000991006     C     *IN13         DOWEQ     '0'
216100991006     C                   MOVEL     TBLKEY        DIVCV             3
216200991006     C                   MOVEL     TBLUNI        DSCV
216300991006     C* Se divisa diversa dalla moneta di conto reperisco data
216400991006     C* validità x trovare altra divisa da utilizzare x incassi
216500011009     C     §GEDCR        IFNE      DIVCV
216600991006     C     DIVEUR        ANDEQ     *BLANKS
216700991006     C     §CVDVA        ANDGE     'S'
216800991006     C                   MOVEL     DIVCV         DIVEUR            3
216900991006     C                   END
217000991006     C     KTAB1         READE     TABEL00F                               13
217100991006     C                   END
217200991116     C*  Se non ho impostato divisa alternativa ci sbatto
217300991116     C*  la divisa corrente
217400991116     C     DIVEUR        IFEQ      *BLANKS
217500011009     C                   MOVEL     §GEDCR        DIVEUR
217600991116     C                   END
217700991006     C*----------------------------------------------------*
217800941115     C*  Caricamento £1
217900020509     c                   clear                   trul06ds
218000020509     c                   eval      d06cod = '£1'
218100020822     c                   movel     dutpou        d06key
218200020509     c                   movel     trul06ds      kpjbu
218300020509     c                   call      'TRUL06R'
218400020509     c                   parm                    kpjba
218500020509     c                   movel     kpjbu         trul06ds
218600020509     c                   movea     lin           £1
218700941115     C*  Caricamento Tabella 7R
218800941115     C                   Z-ADD     0             X                 3 0
218900941115     C                   MOVEL     '7R'          KCOD
219000941115     C     KTAB1         CHAIN     TABEL00F                           96
219100941115     C     *IN96         DOWEQ     '0'
219200080903     C     X             ANDLT     200
219300941115     C     TBLFLG        IFEQ      ' '
219400941115     C                   ADD       1             X
219500941115     C                   MOVEL     TBLKEY        C7R(X)
219600941115     C                   MOVEL     TBLUNI        D7R(X)
219700941115     C                   END
219800941115     C     KTAB1         READE     TABEL00F                               96
219900941115     C                   END
220000951019     C     *IN96         IFEQ      '0'
220100000211     C                   MOVEL     MSG(1)        DSMSG
220200000211     C                   MOVEL     KCOD          KCODM
220300000211     C                   MOVEL     DSMSG         MSG(1)
220400100825     c                   exsr      sndmail
220500100825     C*                  CALL      'TNSYM4C'
220600100825     C*                  PARM                    MSGT
220700100825     C*                  PARM                    MSG
220800100825     C*                  PARM      046           FIL               3 0
220900100825     C*                  PARM      'EDP*'        UTEN             10
221000951019     C                   END
221100941115     C*  Caricamento Tabella 3A
221200941115     C                   Z-ADD     0             X                 3 0
221300941115     C                   MOVEL     '3A'          KCOD
221400941115     C     KTAB1         CHAIN     TABEL00F                           96
221500941115     C     *IN96         DOWEQ     '0'
221600080903     C     X             ANDLT     200
221700941115     C     TBLFLG        IFEQ      ' '
221800941115     C                   ADD       1             X
221900941115     C                   MOVEL     TBLKEY        C3A(X)
222000941115     C                   MOVEL     TBLUNI        D3A(X)
222100941115     C                   END
222200941115     C     KTAB1         READE     TABEL00F                               96
222300941115     C                   END
222400951019     C     *IN96         IFEQ      '0'
222500000211     C                   MOVEL     MSG(1)        DSMSG
222600000211     C                   MOVEL     KCOD          KCODM
222700000211     C                   MOVEL     DSMSG         MSG(1)
222800100825     c                   exsr      sndmail
222900100825     C*                  CALL      'TNSYM4C'
223000100825     C*                  PARM                    MSGT
223100100825     C*                  PARM                    MSG
223200100825     C*                  PARM      046           FIL               3 0
223300100825     C*                  PARM      'EDP*'        UTEN             10
223400951019     C                   END
223500990923     C*
223600990923     C                   Z-ADD     0             X                 3 0
223700990923     C                   MOVEL     '1A'          KCOD
223800990923     C     KTAB1         CHAIN     TABEL00F                           96
223900990923     C     *IN96         DOWEQ     '0'
224000080903     C     X             ANDLT     200
224100990923     C     TBLFLG        IFEQ      ' '
224200990923     C                   ADD       1             X
224300990923     C                   MOVEL     TBLKEY        C1A(X)
224400990923     C                   MOVEL     TBLUNI        D1A(X)
224500990923     C                   END
224600990923     C     KTAB1         READE     TABEL00F                               96
224700990923     C                   END
224800990923     C     *IN96         IFEQ      '0'
224900000211     C                   MOVEL     MSG(1)        DSMSG
225000000211     C                   MOVEL     KCOD          KCODM
225100000211     C                   MOVEL     DSMSG         MSG(1)
225200100825     c                   exsr      sndmail
225300100825     C*                  CALL      'TNSYM4C'
225400100825     C*                  PARM                    MSGT
225500100825     C*                  PARM                    MSG
225600100825     C*                  PARM      046           FIL               3 0
225700100825     C*                  PARM      'EDP*'        UTEN             10
225800990923     C                   END
225900941115     C*
226000000427     C*  Caricamento Tabella 3E
226100000427     C                   Z-ADD     0             X                 3 0
226200000427     C                   MOVEL     '3E'          KCOD
226300000427     C     KTAB1         CHAIN     TABEL00F                           96
226400000427     C     *IN96         DOWEQ     '0'
226500080903     C     X             ANDLT     200
226600000427     C     TBLFLG        IFEQ      ' '
226700000427     C                   ADD       1             X
226800000427     C                   MOVEL     TBLKEY        COA(X)
226900000427     C                   MOVEL     TBLUNI        DS3E
227000000427     C                   MOVEL     §3ECMC        CMC(X)
227100001227     C                   MOVEL     §3ECMB        CMB(X)
227200000427     C                   MOVEL     §3EPMC        PMC(X)
227300000427     C                   END
227400000427     C     KTAB1         READE     TABEL00F                               96
227500000427     C                   END
227600000427     C     *IN96         IFEQ      '0'
227700000427     C                   MOVEL     MSG(1)        DSMSG
227800000427     C                   MOVEL     KCOD          KCODM
227900000427     C                   MOVEL     DSMSG         MSG(1)
228000100825     c                   exsr      sndmail
228100100825     C*                  CALL      'TNSYM4C'
228200100825     C*                  PARM                    MSGT
228300100825     C*                  PARM                    MSG
228400100825     C*                  PARM      046           FIL               3 0
228500100825     C*                  PARM      'EDP*'        UTEN             10
228600000427     C                   END
228700000503     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
228800000503     C                   CLEAR                   FIP
228900000503     C     *LOVAL        SETLL     AZORG01L
229000000503     C                   DO        *HIVAL
229100000503     C                   READ      AZORG01L                               95
229200000503     C   95              LEAVE
229300000503     C                   MOVEL     ORGDE3        OG143
229400020729     C     §OgNTW        IFEQ      'PPT'
229500000503     C                   Z-ADD     1             X
229600000503     C     *ZEROS        LOOKUP    FIP(X)                                 23
229700000503     C   23              Z-ADD     ORGFIL        FIP(X)
229800000503     C                   END
229900000503     C                   END
230000000602      *
230100000602     C*  Caricamento Tabella 2Z
230200000602     C                   Z-ADD     0             X                 3 0
230300000602     C                   MOVEL     '2Z'          KCOD
230400000602     C     KTAB1         CHAIN     TABEL00F                           96
230500000602     C     *IN96         DOWEQ     '0'
230600080903     C     X             ANDLT     200
230700000602     C     TBLFLG        IFEQ      ' '
230800000602     C                   ADD       1             X
230900000602     C                   MOVEL     TBLKEY        C2Z(X)
231000000602     C                   MOVEL     TBLUNI        D2Z(X)
231100000602     C                   END
231200000602     C     KTAB1         READE     TABEL00F                               96
231300000602     C                   END
231400000602     C     *IN96         IFEQ      '0'
231500000602     C                   MOVEL     MSG(1)        DSMSG
231600000602     C                   MOVEL     KCOD          KCODM
231700000602     C                   MOVEL     DSMSG         MSG(1)
231800100825     c                   exsr      sndmail
231900100825     C*                  CALL      'TNSYM4C'
232000100825     C*                  PARM                    MSGT
232100100825     C*                  PARM                    MSG
232200100825     C*                  PARM      046           FIL               3 0
232300100825     C*                  PARM      'EDP*'        UTEN             10
232400000602     C                   END
232500101123     C*  Caricamento Tabella 2a
232600101123     C                   Z-ADD     0             X                 3 0
232700101123     C                   MOVEL     '2A'          KCOD
232800101123     C     KTAB1         CHAIN     TABEL00F                           96
232900101123     C     *IN96         DOWEQ     '0'
233000101123     C     X             ANDLT     200
233100101123     C     TBLFLG        IFEQ      ' '
233200101123     C                   ADD       1             X
233300101123     C                   MOVEL     TBLKEY        C2A(X)
233400101123     C                   MOVEL     TBLUNI        D2A(X)
233500101123     C                   END
233600101123     C     KTAB1         READE     TABEL00F                               96
233700101123     C                   END
233800101123     C     *IN96         IFEQ      '0'
233900101123     C                   MOVEL     MSG(1)        DSMSG
234000101123     C                   MOVEL     KCOD          KCODM
234100101123     C                   MOVEL     DSMSG         MSG(1)
234200101123     c                   exsr      sndmail
234300101123     C*                  CALL      'TNSYM4C'
234400101123     C*                  PARM                    MSGT
234500101123     C*                  PARM                    MSG
234600101123     C*                  PARM      046           FIL               3 0
234700101123     C*                  PARM      'EDP*'        UTEN             10
234800101123     C                   END
234900941115     C                   ENDSR
235000100825      *----------------------------------------------------------------
235100100825     c     sndmail       begsr
235200100825      *----------------------------------------------------------------
235300120305     c                   eval      emlto = 'CedAlert@brt.it'
235400100825     c                   eval      emlcc = *blank
235500100825     c                   eval      emogg = 'ATTENZIONE Chiusura distinta '
235600100825     C                   eval      emmsg = msg(1)  + ' ' +
235700100825     c                             msg(2) + ' ' + msg(3)
235800100825     C                   CALL      'TIS701C1'
235900100825     C                   PARM                    emlto           253
236000100825     C                   PARM                    emlcc           253
236100100825     C                   PARM                    emogg            44
236200100825     C                   PARM                    emmsg          5000
236300100825     C                   PARM                    emesito           1
236400100825      *
236500100825     C                   ENDSR
236600941115      *----------------------------------------------------------------
236700941115     OFNFVV000  E            AGGFVV
236800941115     O                       FVVFCF
236900000211**  ERRORI
237000961115Punto operativo non in gestione su questo AS                          01
237100941123Numero distinta errato                                                02
237200941123Immettere tipo uscita M=Mattino/P=Pomeriggio !                        03
237300941123Immettere flag calcolo Picking !                                      04
237400941123Attenzione !! Flag calcolo picking diverso da anagrafica              05
237500941123Distinta in uso ad altro operatore o già chiusa !                     06
237600941123Data errata !                                                         07
237700941123Ora errata !                                                          08
237800941123Data Distinta errata: già confermati i conteggi !                     09
237900020418Data Consegna anteriore al limite inferiore ammesso  !!               10
238000100211Distinte con diverso codice autista !                                 11
238100961115Distinta in gestione ad altra punto operativo                         12
238200941124Immettere almeno un numero di distinta da chiudere !!!                13
238300941128Attenzione !! Data di consegna anteriore alla data distinta           14
238400950221Non chiudere dist. di questo padr.x conflitti! Riprovare fra 2 min.   15
238500010302Ora errata ! PER UNA DISTINTA POMERIDIANA L'ORA MINIMA SONO LE 13     16
238600030204Immettere flag calcolo Carico !                                       17
238700030204Attenzione !! Flag calcolo carico diverso da anagrafica               18
238800070319Distinta già chiusa o non disponibile               FIDST00F          19
238900080707Distinte incongruenti Alcune gestite con PDA e altre NO               20
239000080707Manca Fase controllo rientro (CRI) distinta non disponibile           21
239100080707Distinta con gestione PDA obbligatorio chiudere da lettore ottico     22
239200000211**
239300000211ERRORE IN CHIUSURA DISTINTA
239400000211**
239500000211ATTENZIONE ! La tabella XX utilizzata nella chiusura distinta ha r
239600000211aggiunto la capienza massima ! Correggere il programma FNLR53R inc
239700000211rementando la relativa schiera.
