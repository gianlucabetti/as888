000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICHE FASCE DI RICAVO/COLLI E BANCALI - STAMPA                   *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000401171220
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTABEL00F  IF   E           K DISK
001000080717     FWfsba00f  o    e             disk    usropn
001200000000     F* PRINTER FILE
001400010510     FTISE05P   O    E             PRINTER OFLIND(*IN01)
001500000000     F                                     USROPN
001700990824     F* ESTENSIONE BOLLE
001900990805     FTITA730C  IF   E           K DISK
002100990806     F* ANAGRAFICHE BOLLE
002400171220     FTITAA30C  IF   E           K DISK    IGNORE(TITAAP00)
002600000000     F* COMPETENZE FISSE E VARIABILI
002800000000     FECCET30C  IF   E           K DISK
002900000000     FECCED30C  IF   E           K DISK
003303171220
003400000000     D*------------------------------------------------------------------------*
003500000000     D* SCHIERE
003600000000     D*------------------------------------------------------------------------*
003800000000     D* TIPI BOLLA
004000000000     D TBL             S              2    DIM(20)
004300000000     D* VARIE
004500990806     D SVN             S              1    DIM(50)
004600010510     D VAN             S             17  5 DIM(50)
005101171220
005200000000     D*------------------------------------------------------------------------*
005300171220     D* Campi comodo
005400000000     D*------------------------------------------------------------------------*
005500041227     d dataiso         s               d   datfmt(*iso)
005800080717     d comman          s            110
005900080717     d lenght          s             15  5 inz(110)
006000080717     d wlib            s              9
006001171220     d sav_TASksc      s                   like(TASksc) inz
006002171220     d sav_TASctr      s                   like(TASctr) inz
006003171220     d sav_TASmes      s              2s 0 inz
006004171220     d mgsi            s              4s 0 inz
006006171220     d mgsf            s              4s 0 inz
006007171220     d TASmes          s              2s 0 inz
006008171220
006009171220     D*------------------------------------------------------------------------*
006010171220     D* Strutture dati
006011171220     D*------------------------------------------------------------------------*
007000000000     D* ARCHITETTURA
007200000000     D KPJBA         E DS
007400000000     D* DS PARAMETRI LANCIO
007600000000     D PARAM           DS
007601171220     D  paraai                 1      4  0
007700000000     D  PARDAI                 1      6  0
007701171220     D  parmmi                 5      6  0
007702171220     D  paraaf                 7     10  0
007800000000     D  PARDAF                 7     12  0
007801171220     D  parmmf                11     12  0
007900171219     D  PARBAN                13     13
008000171219     D  PARFIL                14     14
008200000000     D* DS TIPI BOLLA
008400000000     D DSTB          E DS
009000171220     D* DS REPERIMENTO DATI cliente
009200980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
009300980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
009400980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
009500980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
009600980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
009700171220      //File bolle
009800171220     dTitasds        e ds                  extname(TITAS00F)
012300041227      * DS "TISE70DS" - REPERIMENTO SALDI CLIENTI
012500041227     d Tise70ds      e ds
013000041227      * DS 'wdataf' x aaaamm da passare al tise70ds
013200041227     d                 ds
013300041227     d  wdataf                 1      8  0
013400041227     d  waaf                   1      4  0
013500041227     d  wmmf                   5      6  0
013600041227     d  waammf                 1      6  0
013700041227     d  wggf                   7      8  0
013900041227      * DS 'waammi' x aaaamm da passare al tise70ds
014100041227     d                 ds
014200041227     d  waammi                 1      6  0
014300041227     d  waai                   1      4  0
014400041227     d  wmmi                   5      6  0
014401171220
014500000000     C*------------------------------------------------------------------------*
014600020117     C* MAIN LINE
014700020117     C*------------------------------------------------------------------------*
014800171220
014900171220        exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015000171220
015200000000     C* APRE I FILE PRINTER DESIDERATI
015300000000     C                   EXSR      OPNPRT
015400171219
015500171220       // Leggo il file delle bolle per Cliente/Tariffa e data spedizione
015501171220       // solo le ficcate, solo bolle con tariffa a collo e senza la varia 'O'
015502171220       // solo bolle con tariffa a collo
015503171220       // solo bolle senza la varia 'O' bancali
015504171220       // solo bolle con peso > 1 Kg
015505171220       // solo bolle con colli > 0
015600171219         exec sql
015700171219         declare C1 cursor for
015800171219         select * from titas10f where
015901171220         TASaas >= :paraai and TASaas <= :paraaf and
015902171220         TASmgs >= :mgsi   and TASmgs <= :mgsf   and
015903171220         TASfic <> ' '     and
015904171220         substr(digits(TASctr), 1, 1) = '1'      and
015905171220         substr(TASgva, 1, 1) <> 'O'             and
015906171220         TASpkf > 1        and
015907171220         TASncl > 0
016000171219         union
016100171219         select * from titasp0f where
016101171220         TASaas >= :paraai and TASaas <= :paraaf and
016102171220         TASmgs >= :mgsi   and TASmgs <= :mgsf   and
016104171220         TASfic <> ' '     and
016105171220         substr(digits(TASctr), 1, 1) = '1'      and
016106171220         substr(TASgva, 1, 1) <> 'O'             and
016107171220         TASpkf > 1        and
016108171220         TASncl > 0
016300171220         order by TASksc, TASctr, TASaas, TASmgs
016400171219         for read only;
016500171219
016600171219         exec sql open C1;
016700171219
016800171219         DOW  sqlcod = *zeros;
017200171220           exec sql fetch C1 into :TITASDS;
017201171220
017202171220           TASmes = %dec(%subst(%editc(TASmgs:'X'):1:2):2:0);
017203171220           clear wrecok;
017300171219
017400171219         // A rottura di Cliente/Tariffa
017500171219           IF  sav_TASksc <> TASksc or sav_TASctr <> TASctr;
017600171219             exsr NEWKSC;
017601171220             sav_TASksc = TASksc;
017602171220             sav_TASctr = TASctr;
017603171220             clear sav_TASmes;
017604171220           ENDIF;
017605171220
017606171220         // A rottura di Mese
017607171220           IF  sav_TASmes <> TASmes;
017700171219         // calcolo la media peso/colli
017800171220             exsr CALMED;
017801171220             sav_TASmes = TASmes;
018100171219           ENDIF;
018101171220
018300171219         // VERIFICA SE RCD OK
018400171219           EXSR CHKREC;
018600171219         // MEMORIZZA I DATI DELLE BOLLE
018700171219           IF  WRECOK = 'S';
018800171219             EXSR MEMSAL;
019100171220           //STAMPA LA STATISTICA BANCALI (SE RICHIESTA)
019200171220             IF  PARBAN = '1';
019300171220               EXSR STABAN;
019400171220             ENDIF;
019401171220           ENDIF;
019500171219
019600171219         ENDDO;
019700171219
019800171219         exec sql close C1;
019900000000     C*
020000980116     C* OPERAZIONI FINALI
020100980116     C                   EXSR      FINSR
020200000000     C*
020300000000     C                   SETON                                        LR
020400000000     C*------------------------------------------------------------------------*
020500000621     C* CHKREC - CONTROLLA VALIDITA' RECORD (CONTROLLI SU TITAS)
020600000000     C*------------------------------------------------------------------------*
020700000000     C     CHKREC        BEGSR
020800000000     C*
020900171219     C                   Z-ADD     *ZEROS        BOLMED
021000171219     C                   Z-ADD     *ZEROS        WMED
022500171219
022600171219      * TIPO BOLLA DIVERSO DA "NORMALE", ESCLUDE
022700171219       IF  %lookup(TAStbl:tbl) > 0;
022800171219         leavesr;
022900171219       ENDIF;
024400171219
024900171219     C* SE LA MEDIA PESO/COLLO DELLA BOLLA E' SUPERIORE DI PIU' DI 5 VOLTE LA
025000171219     C* MEDIA STANDARD DEL CLIENTE LA BOLLA E' DA SEGNALARE, ALTRIMENTI ESCLUDE
025100171219     C     TASPKF        DIV       TASNCL        BOLMED           11 3
025200171219     C     BOLMED        DIV       5             WMED             11 3
025300171219IF  2C     WMED          IFLE      DEPMED
025400171219     C                   leavesr
025500171219E   2C                   ENDIF
025700000000     C*
025800171220     C                   MOVEL     'S'           WRECOK            1
025900000000     C                   ENDSR
026000000000     C*------------------------------------------------------------------------*
026100000000     C* MEMSAL - MEMORIZZA I DATI
026200000000     C*------------------------------------------------------------------------*
026300000000     C     MEMSAL        BEGSR
026400000000     C*
026500000000     C* PULISCE LE VARIABILI DI LAVORO
026600000000     C                   CLEAR                   SVN
026700000000     C                   CLEAR                   VAN
027100000000     C****
027200000000     C* REPERISCE I RICAVI E LE COMPETENZE
027300000000     C****
027400000000     C* CARICA IN SCHIERA LE VARIE DEL RECORD BOLLA
027500000000     C                   Z-ADD     *ZEROS        J                 4 0
027600000000IF  1C     TASSV1        IFNE      *BLANKS
027700000000     C                   ADD       1             J
027800010510     C                   MOVEL     TASSV1        SVN(J)
027900000111     C                   Z-ADD     TASVA1        VAN(J)
028000000000E   1C                   ENDIF
028100000000IF  1C     TASSV2        IFNE      *BLANKS
028200000000     C                   ADD       1             J
028300000000     C                   MOVEL     TASSV2        SVN(J)
028400000111     C                   Z-ADD     TASVA2        VAN(J)
028500000000E   1C                   ENDIF
028600000000IF  1C     TASSV3        IFNE      *BLANK
028700000000     C                   ADD       1             J
028800000000     C                   MOVEL     TASSV3        SVN(J)
028900000111     C                   Z-ADD     TASVA3        VAN(J)
029000000000E   1C                   ENDIF
029100000000     C*
029200000000     C* CARICA IN SCHIERA LE VARIE DEI RECORD ESTENSIONE BOLLA
029300000000IF  1C     TASSV3        IFNE      *BLANKS
029400000000     C                   Z-ADD     TASAAS        KT7AAS
029500000000     C                   Z-ADD     TASLNP        KT7LNP
029600000000     C                   Z-ADD     TASNRS        KT7NRS
029700000000     C                   Z-ADD     TASNSP        KT7NSP
029800000000     C                   MOVEL     TASTBL        KT7TBL
029900990805     C     KEYTA7        SETLL     TITA730C                           98
030000990805     C     KEYTA7        READE     TITA730C                               98
030100000000DO  2C     *IN98         DOWEQ     *OFF
030200000000     C                   ADD       1             J
030300000000     C                   MOVEL     TA7SVN        SVN(J)
030400000111     C                   Z-ADD     TA7VAN        VAN(J)
030500990805     C     KEYTA7        READE     TITA730C                               98
030600000000E   2C                   ENDDO
030700000000E   1C                   ENDIF
030800171219
030900171219     C                   Z-ADD     TASimv        CNVIMV           15 3
031000171219     C                   Z-ADD     TASpor        CNVPOR           15 3
031100990806     C*
031200000000     C* CALCOLA LE COMPETENZE
031300010510     C                   Z-ADD     *ZEROS        WCOM             17 5
031400000000     C                   Z-ADD     TASAAS        KCTAAS
031500000000     C                   Z-ADD     TASLNP        KCTLNP
031600000000     C                   Z-ADD     TASNRS        KCTNRS
031700000000     C                   Z-ADD     TASNSP        KCTNSP
031800000000     C                   MOVEL     TASTBL        KCTTBL
031900000000     C     KEYCET        CHAIN     ECCET30C                           98
032000000000IF  1C     *IN98         IFEQ      *OFF
032100000713     C                   ADD       CETPAR        WCOM
032200000713     C                   ADD       CETPAM        WCOM
032300000713     C                   ADD       CETPAU        WCOM
032400000713     C                   ADD       CETPAV        WCOM
032500000713     C                   ADD       CETARD        WCOM
032600000713     C                   ADD       CETARM        WCOM
032700000713     C                   ADD       CETARU        WCOM
032800000713     C                   ADD       CETARV        WCOM
032900000713     C                   ADD       CETTRL        WCOM
033000000713     C                   ADD       CETPAG        WCOM
033100000713     C                   ADD       CETSEA        WCOM
033200000713     C                   ADD       CETSEC        WCOM
033300000713     C                   ADD       CETRCV        WCOM
033400000713     C                   ADD       CETMDE        WCOM
033500000713     C                   ADD       CETTDE        WCOM
033600000713     C                   ADD       CETMAF        WCOM
033700000713     C                   ADD       CETTAF        WCOM
033800010829     C                   ADD       CETSES        WCOM
033900000000IF  2C     CETDET        IFEQ      'S'
034000000000     C     KEYCET        SETLL     ECCED30C                           98
034100000000     C     KEYCET        READE     ECCED30C                               98
034200000000DO  3C     *IN98         DOWEQ     *OFF
034300000713     C                   ADD       CEDIMP        WCOM
034400000000     C     KEYCET        READE     ECCED30C                               98
034500000000E   3C                   ENDDO
034600000000E   2C                   ENDIF
034700000000E   1C                   ENDIF
037900000000     C*
038000000000     C                   ENDSR
038100000000     C*------------------------------------------------------------------------*
038200000000     C* STABAN - STAMPA LA STATISTICA BANCALI
038300000000     C*------------------------------------------------------------------------*
038400000000     C     STABAN        BEGSR
038500000000     C*
038600000000     C* STAMPA PARAMETRI DI LANCIO (SOLO LA PRIMA VOLTA)
038700091016IF  1C     $VOLTB        IFEQ      '0'
038800091016     C                   EXSR      STAPAB
038900091016     C                   MOVEL     '1'           $VOLTB
039000091016E   1C                   ENDIF
039300091015
039400091015      * se richiesta scrittura del file scrivo un record su wfsba00f
039500091015     c                   if        parfil = 'S'
039600091015     c                   clear                   wfsba000
039700091015
039800091015     c                   eval      bandip = pardai
039900091015     c                   eval      bandfp = pardaf
040000091015
040100171219     C                   Z-ADD     DEPKSC        banksc
040200091015     C                   MOVEL     DESKSC        banrag
040300091015     C                   Z-ADD     DEPMED        banmed
040400091015     C                   Z-ADD     TASAAS        banaas
040500091015     C                   Z-ADD     TASLNP        banlnp
040600091015     C                   Z-ADD     TASNRS        bannrs
040700091015     C                   Z-ADD     TASNSP        bannsp
040800091015     C                   MOVEL     TASTBL        bantbl
040900091015     C                   Z-ADD     TASMGS        banmgs
041000091015     C                   Z-ADD     TASNCL        banncl
041100091015     C                   Z-ADD     TASPKF        banpkb
041200091015     C                   Z-ADD     TASVLB        banvlb
041300091015     C                   Z-ADD     CNVimv        banpor
041400091015     c                   Z-add(h)  wcom          bancom
041500091015     C                   Z-ADD     BOLMED        banmeb
041600080717
041700080717     c                   write     wfsba000
041800080717
041900080717     c                   endif
042000080717
042100000000     C                   ENDSR
042200000000     C*------------------------------------------------------------------------*
042300000000     C* STAPAB - STAMPA PAGINA PARAMETRI STATISTICA BANCALI
042400000000     C*------------------------------------------------------------------------*
042500091016     C     STAPAB        BEGSR
042600000000     C*
042700091016     C                   Z-ADD     PARDAI        PACDAI
042800091016     C                   Z-ADD     PARDAF        PACDAF
042900000000     C*
043000171219     C                   WRITE     SE05PT
043100171219     C                   WRITE     SE05PA
043200000000     C*
043300091016     C                   ENDSR
043400000000     C*------------------------------------------------------------------------*
043500000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
043600000000     C*------------------------------------------------------------------------*
043700000000     C     NEWKSC        BEGSR
043800000000     C*
043900000000     C* DEPOSITA IL NUOVO CODICE
044000000000     C                   Z-ADD     TASKSC        DEPKSC
044100990806     C*
044200990806     C* RECUPERA LA RAGIONE SOCIALE
044300990806     C                   MOVEL     *BLANKS       DESKSC           30
044400990806     C                   MOVE      DEPKSC        N4                4 0
044500990806     C                   MOVEL     TASTBL        A1                1
044600990806IF  1C     N4            IFNE      8888
044700980116     C                   CLEAR                   BS69DS
044800980116     C                   CLEAR                   ACODS
044900980116     C                   MOVEL     KNSIF         I69SIF
045000980116     C                   MOVEL     DEPKSC        I69KAC
045100990806     C                   CALL      'TIBS69R'
045200980116     C                   PARM                    BS69DS
045300980116     C                   PARM                    ACODS
045400980116     C                   PARM                    INDDS
045500980116     C                   PARM                    CLPDS
045600980116     C                   PARM                    CLSDS
045700990806IF  2C     O69ERR        IFNE      '1'
045800980116     C                   MOVEL     ACORAG        DESKSC
045900990806E   2C                   ENDIF
046000990806X   1C                   ELSE
046100990806IF  2C     A1            IFEQ      'F'
046200990806     C                   Z-ADD     TASAAS        KAAAAS
046300990806     C                   Z-ADD     TASLNP        KAALNP
046400990806     C                   Z-ADD     TASNRS        KAANRS
046500990806     C                   Z-ADD     TASNSP        KAANSP
046600990806     C                   MOVEL     'M'           KAATRC
046700990806     C     KEYTAA        CHAIN     TITAA30C                           98
046800990806IF  3C     *IN98         IFEQ      *OFF
046900990806     C                   MOVEL     TAARSC        DESKSC
047000990806E   3C                   ENDIF
047100990806X   2C                   ELSE
047200990806     C                   MOVEL     TASRSD        DESKSC
047300990806E   2C                   ENDIF
047400990806E   1C                   ENDIF
047500000000     C*
047600000000     C                   ENDSR
047700000000     C*------------------------------------------------------------------------*
047800000000     C* CALMED - CALCOLA LA MEDIA PESO/COLLO
047900000000     C*------------------------------------------------------------------------*
048000000000     C     CALMED        BEGSR
048100000000     C*
048200000000     C                   Z-ADD     *ZEROS        DEPMED
048300041227
048400041228     c                   move      tasaas        waaf
048500041228     c                   movel     tasmgs        wmmf
048600041227     c                   move      01            wggf
048700041227
048800041227     c     *iso          move      wdataf        dataiso
048900041228     c                   subdur    2:*m          dataiso
049000041227     c                   extrct    dataiso:*m    wmmi
049100041227     c                   extrct    dataiso:*y    waai
049200041227
049300041227     C                   clear                   TISE70DS
049400041227     C                   eval      I70TLA = 'S'
049500041227     C                   eval      I70TLE = 'ST'
049600041227     C                   eval      I70KSC = depksc
049700041227     C                   eval      I70DAF = waammf
049800041227     C                   eval      I70DAI = waammi
049900171220     C                   move      TASctr        I70ctr
050000041227
050100041227     C                   call      'TISE70R'
050200041227     c                   parm                    TISE70DS
050300041227
050400041227     c                   If        o70nco > *Zeros
050500041227     c     o70pkg        div       o70nco        depmed
050600041227     c                   EndIf
050700000000     C*
050800000000     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000000000     C* CARTAB - CARICA LE TABELLE
051100000000     C*------------------------------------------------------------------------*
051200000000     C     CARTAB        BEGSR
051300000000     C*--------------------
051400000000     C* TIPI BOLLA
051500000000     C*--------------------
051600000000     C                   Z-ADD     *ZEROS        I                 4 0
051700020906     C                   Z-ADD     1             KTBKUT
051800000000     C                   MOVEL     'TB'          KTBCOD
051900171219      * caricamento schiera dei tipi bolla da scartare
052000171219     c     ke2tab        setll     tabel00f
052100171219     c                   do        *hival
052200171219     c     ke2tab        reade     tabel00f
052300171219     c                   if        %eof(tabel00f)
052400171219     c                   leave
052500171219     c                   endif
052600171219     c                   movel     tbluni        dstb
052700171219     c                   if        §tbrbl <> 'N'
052800171219     c                   add       1             I
052900171219     c                   movel     tblkey        tbl(I)
053000171219     c                   endif
053100171219     c                   enddo
057300971204     C*
057400000000     C                   ENDSR
057500000000     C*------------------------------------------------------------------------*
057600000000     C* OPNPRT - APRE I FILE PRINTER DESIDERATI
057700000000     C*------------------------------------------------------------------------*
057800000000     C     OPNPRT        BEGSR
057900000000     C*
058000091016IF  1C     PARBAN        IFEQ      '1'
058100171219     C                   OPEN      TISE05P
058200091016E   1C                   ENDIF
058300171219
058400000000     C                   ENDSR
058500000000     C*------------------------------------------------------------------------*
058600980116     C* FINSR - OPERAZIONI FINALI
058700000000     C*------------------------------------------------------------------------*
058800980116     C     FINSR         BEGSR
058900000000     C*
059000980116     C* CHIUDE I FILE PRINTER APERTI
059100091016IF  1C     PARBAN        IFEQ      '1'
059200171219     C                   WRITE     SE05FI
059300171219     C                   CLOSE     TISE05P
059400091016E   1C                   ENDIF
059500980116     C*
059600980116     C* CHIUDE I FILE DEI PGM CHIAMATI
059700980116     C                   CLEAR                   BS69DS
059800980116     C                   CLEAR                   ACODS
059900980116     C                   CLEAR                   INDDS
060000980116     C                   CLEAR                   CLPDS
060100980116     C                   CLEAR                   CLSDS
060200980116     C                   MOVEL     'C'           I69TLA
060300980116     C                   MOVEL     KNSIF         I69SIF
060400980116     C                   CALL      'TIBS69R'
060500980116     C                   PARM                    BS69DS
060600980116     C                   PARM                    ACODS
060700980116     C                   PARM                    INDDS
060800980116     C                   PARM                    CLPDS
060900980116     C                   PARM                    CLSDS
061000000000     C*
061100000000     C                   ENDSR
066200000000     C*------------------------------------------------------------------------*
066300000000     C* *INZSR - OPERAZIONI INIZIALI
066400000000     C*------------------------------------------------------------------------*
066500000000     C     *INZSR        BEGSR
066600000000     C*--------------------
066700000000     C* RICEVIMENTO PARAMETRI
066800000000     C*--------------------
066900000000     C     *ENTRY        PLIST
067000000000     C                   PARM                    KPJBA
067100000000     C                   MOVEL     KPJBU         PARAM
067101171220
067102171220       mgsi = parmmi * 100 + 01;
067103171220       mgsf = parmmf * 100 + 31;
067104171220
067200000000     C*--------------------
067300000000     C* VARIABILI RIFERITE AL DATA BASE
067400000000     C*--------------------
067500000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
067600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
067700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
067800990805     C     *LIKE         DEFINE    TA7AAS        KT7AAS
067900000000     C     *LIKE         DEFINE    TA7LNP        KT7LNP
068000000000     C     *LIKE         DEFINE    TA7NRS        KT7NRS
068100000000     C     *LIKE         DEFINE    TA7NSP        KT7NSP
068200000000     C     *LIKE         DEFINE    TA7TBL        KT7TBL
068300990806     C     *LIKE         DEFINE    TAAAAS        KAAAAS
068400990806     C     *LIKE         DEFINE    TAALNP        KAALNP
068500990806     C     *LIKE         DEFINE    TAANRS        KAANRS
068600990806     C     *LIKE         DEFINE    TAANSP        KAANSP
068700990806     C     *LIKE         DEFINE    TAATRC        KAATRC
068800000000     C     *LIKE         DEFINE    CETAAS        KCTAAS
068900000000     C     *LIKE         DEFINE    CETLNP        KCTLNP
069000000000     C     *LIKE         DEFINE    CETNRS        KCTNRS
069100000000     C     *LIKE         DEFINE    CETNSP        KCTNSP
069200000000     C     *LIKE         DEFINE    CETTBL        KCTTBL
069300000000     C     *LIKE         DEFINE    TASKSC        DEPKSC
069800000000     C                   Z-ADD     *ZEROS        DEPMED           11 3
069900000000     C*--------------------
070000000000     C* CHIAVI DI LETTURA
070100000000     C*--------------------
070200000000     C* LETTURA TABEL00F - PARZIALE
070300000000     C     KE2TAB        KLIST
070400000000     C                   KFLD                    KTBKUT
070500000000     C                   KFLD                    KTBCOD
070600000000     C* LETTURA TABEL00F - COMPLETA
070700000000     C     KEYTAB        KLIST
070800000000     C                   KFLD                    KTBKUT
070900000000     C                   KFLD                    KTBCOD
071000000000     C                   KFLD                    KTBKEY
071100990805     C* LETTURA TITA730C - PARZIALE
071200000000     C     KEYTA7        KLIST
071300000000     C                   KFLD                    KT7AAS
071400000000     C                   KFLD                    KT7LNP
071500000000     C                   KFLD                    KT7NRS
071600000000     C                   KFLD                    KT7NSP
071700000000     C                   KFLD                    KT7TBL
071800990806     C* LETTURA TITAA00L - COMPLETA
071900990806     C     KEYTAA        KLIST
072000990806     C                   KFLD                    KAAAAS
072100990806     C                   KFLD                    KAALNP
072200990806     C                   KFLD                    KAANRS
072300990806     C                   KFLD                    KAANSP
072400990806     C                   KFLD                    KAATRC
072500000000     C* LETTURA ECCET30C - COMPLETA
072600000000     C     KEYCET        KLIST
072700000000     C                   KFLD                    KCTAAS
072800000000     C                   KFLD                    KCTLNP
072900000000     C                   KFLD                    KCTNRS
073000000000     C                   KFLD                    KCTNSP
073100000000     C                   KFLD                    KCTTBL
073600090326
073700000000     C*--------------------
073800000000     C* VARIABILI DI CONTROLLO
073900000000     C*--------------------
074000000000     C                   MOVEL     '0'           PASGEN            1
074100000000     C                   MOVEL     '0'           $VOLTB            1
074200000000     C                   MOVEL     '1'           *IN01
074300020117     C*
075100000000     C* CARICA LE TABELLE
075200000000     C                   EXSR      CARTAB
075300080717
075400080717      * se richiesta la scrittura del file lo pulisco prima di scriverci dei dati
075500080717
075600080717     c                   if        parfil = 'S'
075700080717      * se s.i. di prova imposto la libreria di prova
075800080717     c                   if        %subst(knsif:7:1) = 'P'
075900080717     c                   eval      wlib = 'GAITRAAZP '
076000080717     c                   else
076100080717     c                   eval      wlib = 'GAITRAAZM '
076200080717     c                   endif
076300080717      * pulisco il file
076400080717     c                   clear                   comman
076500080717     c                   EVAL      comman = 'CLRPFM FILE('
076600080717     c                             + %TRIMR(WLIB)
076700080717     c                             + '/WFSBA00F)'
076800080717     c                   call      'QCMDEXC'
076900080717     c                   parm                    comman
077000080717     c                   parm                    lenght
077100080717
077200080717     c                   open      wfsba00f
077300080717     c                   endif
077400000000     C                   ENDSR
