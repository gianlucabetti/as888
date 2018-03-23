000100110104     h dftactgrp(*no) actgrp(*caller)
000200000810     H DECEDIT('0,') DATEDIT(*DMY.)
000300101222      *--------------------------------------------------------------*
000400101222      *     - Controlli distinta consegna                            *
000500101221      *--------------------------------------------------------------*
000501160510     Ffiprm01l  IF   E           K DISK
000600110104     FFNLRQ4D   CF   E             WORKSTN
000700110104     F*--------------------------------------------------------------*
000800110107     FTabel00F  iF   E           K DISK
000900101223     Fazorg01L  iF   E           K DISK
001000101223     Fazcln01L  iF   E           K DISK
001100101223     Ffiar501L  iF   E           K DISK
001200101223     Ffiar901L  iF   E           K DISK
001300110107     Ffnlbl01L  iF   E           K DISK
001400101223     Ffnlbl02L  iF   E           K DISK
001500110104     F                                     RENAME(FNLBL000:FNLBL02)
001600110105     FFNARB01L  iF   E           K DISK    infds(arbds)   prefix(£)
001700110107     FFIARG01L  iF   E           K DISK    infds(argds)   prefix(£)
001800940321      *---------------------------------------------------------------
001900030730     d* schiere
002000030730      *---------------------------------------------------------------
002100160509     D ERR             S             66    DIM(50) CTDATA PERRCD(1)             MSG ERRORE
002200101213      *---------------------------------------------------------------
002300110104     D MES             S              1    DIM(31)
002400101223      *
002500101223     d arbds           ds
002600101223     d  arb_nrr              397    400b 0
002700110107      *
002800110107     d argds           ds
002900110107     d  arg_nrr              397    400b 0
003000101223      *
003100101223      * DS per richiamo pgm FNLR39R
003200101223     D DSLR02        E DS
003300110104     D Dar5GEN       E DS
003400110104     D DS3a          E DS
003401160509     D DS03          E DS
003500110107      *
003600110105     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
003700110105     d fnarbds£      e ds                  extname(fnarb00F) prefix(£)
003800110105     d fnarbds       e ds                  extname(fnarb00F)
003900110107      *
004000110107     d fiargds$      e ds                  extname(fiarg00F) prefix($)
004100110107     d fiargds£      e ds                  extname(fiarg00F) prefix(£)
004200110107     d fiargds       e ds                  extname(fiarg00F)
004201160509     D og150         E DS
004202160509     d fidstds       e ds                  extname(fidst00F)
004203160509     d ddstflr       e ds
004204160510     D fidg17ds      E DS
004300101223      *---------------------------------------------------------------
004400030730     d* ds esterne
004500030730      *---------------------------------------------------------------
004600030730     D KPJBA         E DS
004700110103     D   kpjbuS        S                   like(Kpjbu)
004800101223     D ddatiute      e ds
004900101223     D azuteds       e ds                  extname(AZUTE00F)
005000101223     D tibs34ds      E DS                  inz
005100110103      *
005200110103     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
005300110103     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
005400110103     D s7r             S              2    DIM(200)                             CODICE TABELLA 3A
005500110103     d s7rc            S              2    DIM(200)                             CODICE TABELLA 3A
005600110103     D DS7r          E DS
005700110103      *
005800101223     c*po gestione
005900101223     d trul06ds      e ds
006000110112     d  lint                   1     90  0 dim(30)
006100101220      * msg allocazione  inviato all'utente
006200101216     D trul82ds      E DS
006300070316     D og146         E DS                  inz
006400080416     D og148         E DS                  inz
006500130424     D tnsd99ds      E DS                  inz
006600031125      * DS per richiamo da fnlr50r x togliere bolla da distinta
006700101223     D fnlrq4ds      E DS
006800101223     D Fidg16ds      E DS
006900101223      *
007000101214     D WLBDA8          DS
007100101214     D  G02DAT                 1      8  0
007200101214     D  G02INV                 9     16  0
007300101214     D  G02ERR                17     17
007400101214     D  G02TGI                18     22  0
007500101214      *
007600101214     D DATPAR          DS
007700101214     D  GIODAT                 1      8  0
007800101214     D  GIOINV                 9     16  0
007900101214     D  GIOTGI                17     21  0
008000030730     d*---------------------------------------------------------------*
008100030730     d* campi di comodo                                               *
008200030730     d*---------------------------------------------------------------*
008300030730     d dtaeur          s               d   datfmt(*eur)
008400030730     d dtaiso          s               d
008500101214     d dtaiso_oggi     s               d   datfmt(*iso)
008600101214     d dtaiso_dst      s               d   datfmt(*iso)
008700101213      *-----
008800101223     D Found_Bolla     S              1    INZ('N')
008900101223     D Error_Bolla     S              1    INZ('N')
009000101213     D Rec_allocato    S              1    INZ('N')
009100110104     D SI              c                   'S'
009200110104     D NO              c                   'N'
009300101223      *-----------------------------------------------------*
009400101223     C     *ENTRY        PLIST
009500101223     C                   PARM                    KPJBA
009600110105     C                   PARM                    fnarbds$
009700110107     C                   PARM                    fiargds$
009701160509     C                   PARM                    fidstds
009800110114      *
009900110124     c                   movel     kpjbu         fnlrq4ds
010000110114     c                   clear                   LRQ4errO
010100110114     c                   clear                   LRQ4msgO
010101160509     c                   movel     dstflr        ddstflr
010200110114      *
010300101223     c                   exsr      inizializza
010400110105      *
010500110113     c                   if        LRQ4ERRO <> *blank
010600110124     c                   exsr      FINE
010700110113     c                   endif
010800110114      *
010900110105      *  Controlla assegnabilità
011000110105     C                   exsr      CHK_ASSEGNA
011100110112      *
011200110112      *  se non ci sono errori prima
011300110113     c                   if        LRQ4ERRO <> *blank
011400110124     c                   exsr      FINE
011500110113     c                   endif
011600110104      *
011700110104      *  Controlla Consegnabilità
011800150114     c                   if        argcgi <>'DIR'
011900110104     C                   exsr      CHK_CONSEGNA
012000150114     c                   end
012100110104      *
012200110124     c                   exsr      fine
012300110124      *-----------------------------------------------------*
012400110124     C*  FINE PROGRAMMA
012500110124      *-----------------------------------------------------*
012600110124     c     fine          begsr
012700110124      *  tipo chiusura del programma
012800110124     c                   if         LRQ4chiu = 'R'
012900110124     c                   seton                                        RT
013000110124     c                   elseIf     LRQ4chiu = 'L'
013100110124     c                   seton                                        LR
013200110124     c                   end
013300110124     c                   movel(p)  FNLRq4ds      kpjbu
013400110114     C                   eval      fnarbds$ = fnarbds
013500110114     C                   eval      fiargds$ = fiargds
013600110124     c                   return
013700110124     c                   endsr
013800110112      *-----------------------------------------------------*
013900110112     C* Inizializzazioni
014000110112      *-----------------------------------------------------*
014100110112     c     Inizializza   begsr
014200110113     c*
014300110113     C* Aggancio la £6 e recupero le linee a loro associate
014400110113     c                   clear                   trul06ds
014500110113     c                   eval      d06cod = '£6'
014600110113     c                   movel     dutpou        d06key
014700110113     c                   movel     trul06ds      kpjbu
014800110113     c                   call      'TRUL06R'
014900110113     c                   parm                    kpjba
015000110113     c                   movel     kpjbu         trul06ds
015100110113     c                   movel     kpjbus        kpjbu
015200110112      *
015300101223     C*  Controllo sul calendario il tipo di giorno che è la
015400110112     C*  data della distinta
015500101223     C     LRQ4DDCI      IFNE      0
015600101223     C                   Z-ADD     LRQ4DDCI      WMMGG             4 0
015700101223     C                   Z-ADD     LRQ4DDCI      WG                2 0
015800101223     C                   MOVEL     LRQ4DDCI      KANN
015900101223     C                   Z-ADD     0             KTFP
016000101223     C                   Z-ADD     0             KTFA
016100101223     C                   MOVEL     WMMGG         KMES
016200101223     C     KCLN          CHAIN     AZCLN01L                           31
016300101223     C                   MOVEA     CLNMAT        MES
016400101223     C                   MOVE      MES(WG)       WGG               1
016500101223     C                   END
016600110107      **
016700110107     c* aggancia la Bolla su ARB
016800110107     c                   clear                   fnarbds
016900110107     c                   IF        (%parms > 1 and $arbnsp >0)
017000110107     c                   eval      fnarbds = fnarbds$
017100110107      **
017200110107     c                   elseIF    %parms <2 or $arbnsp = 0
017300110107     C* AGGANCIO FNARB
017400110107     C     Bolla_Passata CHAIN(e)  FNARB01L
017500110107     c                   if        not %found(fnarb01l)
017600110113     c                   eval      LRQ4MSGO  = ERR(03)
017700110112     c                   eval      LRQ4ERRO  = 'E'
017800110112     c                   leaveSR
017900110107     c                   else
018000110107     c                   eval      fnarbds = fnarbds£
018100110107     c                   end
018200110107      **
018300110107     c                   endIF
018400110112     C*-------------
018500110112      ** TROVATA LA BOLLA, Controllo se linea di arrivo abilitata
018600110112     C     ARBLNA        LOOKUP    LINT                                   70
018700110112     c                   if        not *in70
018800110113     c                   eval      LRQ4MSGO  = ERR(02)
018900110112     c                   eval      LRQ4ERRO  = 'E'
019000110112     c                   leaveSR
019100110112     c                   end
019200110107      **
019300110107     c* aggancia la Bolla su ARG
019400110107     c                   clear                   fiargds
019500110107     c                   IF        (%parms > 2 and $argnsp >0)
019600110107     c                   eval      fiargds = fiargds$
019700110107      **
019800110107     c                   elseIF    %parms <3 or $argnsp = 0
019900110112     C* AGGANCIO FIARG
020000110107     C     Bolla_Passata CHAIN(e)  FIARG01L
020100110107     c                   if        not %found(fiarg01l)
020200110107     c                   else
020300110107     c                   eval      fiargds = fiargds£
020400110107     c                   end
020500110107      **
020600110107     c                   endIF
020700110107      **
020800110112     c* se c'è un lasciato avviso sblocco in automatico la spedizione
020900110112     c                   IF        arbfbc = 'A'
021000110112     c                   clear                   dslr02
021100110112     c                   eval      dr2vid = 'N'
021200110112     C                   Z-ADD     LRQ4AASI      DR2AAS
021300110112     C                   Z-ADD     LRQ4LNPI      DR2LNP
021400110112     C                   Z-ADD     LRQ4NRSI      DR2NRS
021500110112     C                   Z-ADD     LRQ4NSPI      DR2NSP
021600110112     C                   Z-ADD     arbLNA        DR2FGS
021700110112     C                   MOVEL     '1'           DR2RIC
021800110112     C                   MOVEL     DSLR02        KPJBU
021900110112     C                   CALL      'FNLR39R'
022000110112     C                   PARM                    KPJBA
022100110112     c                   movel     kpjbu         dslr02
022200110112      *
022300110112     c                   if        dr2err = *blanks
022400110112     c                   clear                   ARBcmc
022500110112     c                   clear                   ARBfbc
022600110112     c                   end
022700110112     c                   end
022800101223     c                   ENDSR
022900110111     C*-----------------------------------------------------*
023000110111     C* Controlla Assegnabilità
023100110111     C*-----------------------------------------------------*
023200110111     C     CHK_ASSEGNA   BEGSR
023300110111      *
023400110111      * controlla che il record sia giusto da prendere
023500110111      * chiamo pgm per vedere se spedizione da mettere in distinta
023600110111     c                   clear                   fidg16ds
023700110111     c                   eval      d16aasi    = LRQ4AASI
023800110111     c                   eval      d16lnpi    = LRQ4LNPI
023900110111     c                   eval      d16nrsi    = LRQ4NRSI
024000110111     c                   eval      d16nspi    = LRQ4NSPI
024100110111      **
024200110111      * deve essere impostata la data
024300110112     c                   eval      d16dtaI = 0                                  DATA LIMITE ABBINAME
024400110111     c                   eval      d16oraI = 0                                  ORA LIMITE ABBINAMEN
024500110111     c                   eval      d16LDCRI= 0                                  Data limite cons.ric
024600110112     c                   eval      d16dfvi = 0                                  DATA DISTINTA/0=OGG
024700110112     c                   if        LRQ4DDCI > 0
024800110112     c                   eval      d16dfvi = LRQ4DDCI                           DATA DISTINTA/0=OGG
024900110112     c                   end
025000110111      **
025100110111     c                   if        test_Parallelo = 'S'
025200110125     c                   eval      d16ndci = 'E'                                G=ARG/B=ARB/E=ENTRAM
025300110112     c                   else
025400110125     c                   eval      d16ndci = 'B'                                G=ARG/B=ARB/E=ENTRAM
025500110112     c                   end
025600110111      **
025700110112      **  controllo assegnabilità
025800110111     c                   eval      d16consi   = *blank                          BLK=SOLO CTR RICHI
025900110111      **
026000150114     c                   eval      D16LBLMI   = 'S'                             SE SPED.MAMMA/BLK=NO
026100150114     c                   eval      d16fbci    = 'S'                             ABIL.C/ASS./BLK=NO
026200150114     c                   eval      D16CMCI    = 'S'                             SPED.GIAC.DA APRIRE/
026300150114      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
026400150114      ***  quindi si deve sostituire il test per identificare il Perfect Order
026500150114     c**********         if        arbxco <> ' '
026600150114     c                   if        arbxco > '0'
026700150114     c                   eval      d16prfoI= 'S'
026800150114     c                   end
026900150114     c* se sto assegnando una spedizione presente nel giro DIR
027000150114     c* inibisco alcuni controlli di
027100150114     c* assegnabilità in quanto inutili visto che sto dirottando
027200150114     c* es. la sospensione alla consegna, la data consegna richiesta ecc.)
027300150114     c                   if        argcgi = 'DIR'
027400150114     c                   eval      d16dcmi = 'S'
027500150114     c                   eval      d16colci= 'S'
027600150114     c                   eval      d16parasgi='S'
027700150114     c                   else
027800110125     c                   eval      D16ACCI    = 'S'                             ABIL.C/ASS./BLK=NO
027900110111     c                   eval      D16ACAI    = 'S'                             ABIL.ASSEGN./BLK=NO
028000110111     c                   eval      D16DCRI    = 'S'                             CONSEGNA RICHIESTA
028100110111     c                   eval      D16BGEI    = 'S'                             SPED. SOSPESA ALLA C
028200150114     c                   end
028300150114     C*
028400110124     c                   movel(p)  fidg16ds      kpjbu
028500110111     c                   call      'FIDG16R'
028600110111     c                   parm                    kpjba
028700110111     c                   parm                    fnarbds
028800110111     c                   parm                    fiargds
028900110114      *
029000110124     c                   movel(p)  kpjbu         fidg16ds
029100110111      *
029200110125     c                   if        D16CONSO = 'N'
029300110111     c                   eval      LRQ4MSGO  = D16MSGO
029400110125     c* per alcuni errori non arriva il msg di ritorno perchè non si
029500110125     c* voleva riempire la stampa di anomalie in assegnaz. automatica
029600110125     c* quindi si deve intercettare l'errore e impostare il msg
029700110125     c                   if        d16msgo = ' '
029800110125     c                   select
029900110125     c                   when      d16ndco = 'S'
030000110125     c                   eval      LRQ4MSGO  = 'Spedizione già in distinta'
030100110125     c                   when      d16cmco = 'S'
030200110125     c                   eval      LRQ4MSGO  = 'Spedizione con giacenza da apri-
030300110125     c                             re'
030400110125     c                   when      d16lblmo = 'S'
030500110125     c                   eval      LRQ4MSGO  = 'Spedizione mamma'
030600110125     c                   when      d16fbco = 'S'
030700110125     c                   eval      LRQ4MSGO  = 'Spedizione bloccata/giacente'
030800110125     c                   endsl
030900110125     c                   end
031000110111     c                   eval      LRQ4ERRO  = 'E'
031100110111     c                   leaveSR
031200110111     c                   end
031300110111      *
031400110111     c                   endsr
031500101223      *-------------------------------------------------------*
031600101223     C*  Controllo la Bolla
031700101223      *-------------------------------------------------------*
031800110104     C     CHK_CONSEGNA  BEGSR
031900110107      **
032000110103     C*  Reperisco Tipo Bolla
032100110103     C                   Z-ADD     1             X                 2 0
032200110103     C                   CLEAR                   DS3A
032300110103     C     ARBCBO        LOOKUP    C3A(X)                                 13
032400110103     C     *IN13         IFEQ      '1'
032500110103     C                   MOVEL     D3A(X)        DS3A
032600110103     C                   END
032700101223     C*
032800101223     C* Controlli x bolle da consegnare
032900110111     C*-----------------*****
033000101223     C     ARBDCM        IFEQ      0
033100110111     C*-----------------*****
033200101223     C* Controllo il livello di abilitazione merce
033300101223     C     ARBAMA        IFEQ      *ZEROS
033400101223     c     arbdam        andeq     0
033500110113     c                   eval      LRQ4MSGO  = ERR(04)
033600110103     c                   eval      LRQ4ERRO  = 'E'
033700101223     c                   leaveSR
033800101223     C                   END
033900101223      *
034000101223     C     ARBAMA        IFEQ      *ZEROS
034100101223     c     arbdam        andne     0
034200110103      ** controllo Forzabile   --> veniva bypassato se c'era la forzatura
034300110113     c                   eval      LRQ4MSGO  = ERR(04)
034400110103     c                   eval      LRQ4ERRO  = 'F'
034500110104     c                   exsr      Forzatura
034600110104     c                   if        LRQ4ERRO  <> *blank
034700101223     c                   leaveSR
034800110104     C                   end
034900101223     C                   END
034901160509     c* turno
034902160509     c                   if        dstnfv <> 0 and dsttpm = ' ' and
034903160509     c                             §ogturno = 'S' and §dstturno = ' '
034904160510     c                             and d17tur1 <> 'U' and d17tur2 <> 'U'
034905160509     c                   clear                   ds03
034906160509     C                   eval      tblcod = '03'
034907160509     C                   movel     dstfgs        com05             5
034908160509     C                   move      arbznc        com05             5
034909160509     C                   movel(p)  com05         tblkey
034910160509     C     ktab1         chain     Tabel00f
034911160509     c                   if        %found(tabel00f)
034912160509     C                   MOVEL     TBLUNI        DS03
034913160509     c                   end
034914160509     c                   if        §03turno = '2'
034915160509     c                   eval      LRQ4MSGO  = ERR(13)
034916160509     c                   eval      LRQ4ERRO  = 'F'
034917160509     c                   exsr      Forzatura
034918160509     c                   if        LRQ4ERRO  <> *blank
034919160509     c                   leaveSR
034920160509     C                   end
034921160509     c                   end
034922160509     c                   end
035000110803      *
035100101223     C*----------------------------------------------------*
035200110112     C* se gestito l'appuntamento, controlla se inserita una data
035300110112     C* consegna richiesta e, se non c'è, è possibile solo se non
035400110112     C* esiste il telefono.
035500101223     C*----------------------------------------------------*
035600101223     C     ARBTC1        IFEQ      'A'
035700101223     C     ARBTC2        OREQ      'A'
035800101223      *
035900101223     C     ARBDCR        IFEQ      0
036000110103     C     ARBHCR        ANDEQ     0
036100101223     C     ARBTCR        ANDEQ     ' '
036200101223      *
036300101223      *    record GEN riferimenti destinatario
036400101223      * controllo numero di telefono se appuntamento
036500101223     C                   clear                   dar5gen
036600101223     C                   move      'GEN'         trcar5
036700110107     C     Kfiar5_GEN    CHAIN     FiAR501L                           43
036800110104     C                   IF        %Found(FiAR501L)
036900110104     C                   MOVEL     ar5uni        dar5gen
037000110104     c                   end
037100101223      *  errori
037200110103      * controllo numero di telefono se appuntamento
037300110104     c                   if        §ar5teld = *blank
037400110104      *  FORZABILE:
037500101223     c                   eval      LRQ4MSGO  = ERR(08)
037600110104     c                   eval      LRQ4ERRO  = 'F'
037700110104     c                   exsr      Forzatura
037800110104     c                   if        LRQ4ERRO  <> *blank
037900110104     c                   leaveSR
038000110104     C                   end
038100101223     c                   else
038200101223     c                   eval      LRQ4MSGO  = ERR(09)
038300110103     c                   eval      LRQ4ERRO  = 'E'
038400101223     c                   leaveSR
038500101223     c                   endif
038600101223      *
038700110104     C                   END
038800101223     C                   END
038900110111      *
039000110111     C*----------------------------------------------------*
039100110111     C* Controllo se la spedizione è in fermo deposito e lo segnalo
039200110111     C     ARBFFD        IFEQ      'S'
039300110111      *  FORZABILE:
039400110111     c                   eval      LRQ4MSGO  = ERR(10)
039500110111     c                   eval      LRQ4ERRO  = 'F'
039600110111     c                   exsr      Forzatura
039700110111     c                   if        LRQ4ERRO  <> *blank
039800110111     c                   leaveSR
039900110111     C                   end
040000110111     C                   END
040100110111      *
040200110103      *
040300101223     C*----------------------------------------------------*
040400110103      * Giorni Chiusura del destinatario
040500101223     C     ARBGC1        IFNE      *BLANKS
040600101223     C                   MOVEL     ARBGC1        WGIOR             1
040700101223     C     WGIOR         IFEQ      WGG
040800110112      ** controllo Forzabile
040900110104      *  FORZABILE:
041000101223     c                   eval      LRQ4MSGO  = ERR(11)
041100101223     c                   eval      LRQ4ERRO  = 'F'
041200110104     c                   exsr      Forzatura
041300110104     c                   if        LRQ4ERRO  <> *blank
041400110104     c                   leaveSR
041500110104     C                   end
041600101223     C                   END
041700101223     C                   END
041800110103      *
041900110103      * Giorni Chiusura del destinatario
042000101223     C     ARBGC2        IFNE      *BLANKS
042100101223     C                   MOVEL     ARBGC2        WGIOR             1
042200101223     C     WGIOR         IFEQ      WGG
042300110112      ** controllo Forzabile
042400110104      *  FORZABILE:
042500101223     c                   eval      LRQ4MSGO  = ERR(11)
042600101223     c                   eval      LRQ4ERRO  = 'F'
042700110104     c                   exsr      Forzatura
042800110104     c                   if        LRQ4ERRO  <> *blank
042900110104     c                   leaveSR
043000110104     C                   end
043100101223     C                   END
043200101223     C                   END
043300101223      *
043400101223     C*----------------------------------------------------*
043500101223     C* controllo se spedizione ha particolarità cons. di attesa
043600101223     C     ARBGma        IFNE      *BLANKS
043700101223     C     ARBTCR        andeq     'P'
043800101223      *
043900101223     C     ARBdcr        IFGT      LRQ4DDCI
044000130429     c                   clear                   tnsd99ds
044100130424     C                   Z-ADD     LRQ4DDCI      D98DFC
044200130424     C                   Z-ADD     2359          D98OFC
044300130424     C                   MOVEL     ' '           D98TLA
044400130424     C                   MOVEL     'A'           D98TBO
044500130424     C                   Z-ADD     ARBAAS        D98AAS
044600130424     C                   Z-ADD     ARBLNP        D98LNP
044700130424     C                   Z-ADD     ARBNRS        D98NRS
044800130424     C                   Z-ADD     ARBNSP        D98NSP
044900130424     C                   CALL      'TNSD99R'
045000130424     C                   PARM                    TNSD99DS
045100130424     c                   if        d98nci <=-24
045200101223     C     arbgma        lookup    s7r                                    48
045300101223     C                   IF        *in48
045400110104      *  FORZABILE:
045500101223     c                   eval      LRQ4MSGO  = ERR(12)
045600101223     c                   eval      LRQ4ERRO  = 'F'
045700110104     c                   exsr      Forzatura
045800110104     c                   if        LRQ4ERRO  <> *blank
045900110104     c                   leaveSR
046000110104     C                   end
046100101223     C                   END
046200101223     C                   End
046300101223      *
046400101223     C                   END
046500101223     C                   END
046600101223      *
046700101223     C*----------------------------------------------------*
046800101223     C     ARBFDC        IFNE      ' '
046900101223     C     ARBDCP        ANDEQ     0
047000110113     c                   eval      LRQ4MSGO  = ERR(05)
047100110103     c                   eval      LRQ4ERRO  = 'E'
047200101223     c                   leaveSR
047300101223     C                   END
047400101223     C*
047500110111     C*-----------------*****
047600110111     C* Altrimenti: Bolla già consegnata
047700110111     C                   ELSE
047800110111     C*-----------------*****
047900110114     C*  Resetta il campo
048000110114     C                   Z-ADD     0             AR9CAS
048100101223      *
048200101223     C* CONTROLLO SE I FLAG DI INCASSO ICA / ICC SONO DA PRENDERE IN
048300101223     C* CONSIDERAZIONE (NEL CASO DI CONSEGNA PARZIALE O CONSEGNA CON
048400101223     C* RITORNO ALL'INCASSO)
048500110112     C     ARBFIP        IFNE      'I'
048600110114     C     ARBICA        ANDNE     'R'
048700110114     C*
048800110114     C* Ritorno all'incasso
048900110114     C     ARBICC        IFNE      'R'
049000110114     c                   eval      LRQ4MSGO  = ERR(01)
049100110114     c                   eval      LRQ4ERRO  = 'E'
049200110114     c                   leaveSR
049300110114     C                   END
049400101223     C                   END
049500110103      *
049600101223     C                   END
049700110124     C*  Controllo se c'è contrassegno
049800110124     C     ARBICC        IFEQ      ' '
049900110124     C     §3AFCA        andNE     0
050000110124     C     Bolla_Passata CHAIN     FIAR901L
050100110124     C                   END
050200110124     c*
050300110114      *  verifico se la "Bolla Passata"  ha una mamma
050400110114      *  e se non è stata chiusa con CCA 2 o 6  ovvero come RESO
050500110114      *    e ha un C/Assegno.
050600110124     C                   If        §3AFCA <> 0 and ar9CAS = 0
050700110103     C     Bolla_Passata CHAIN     FNLBL01L
050800110103      *
050900110104      *  IN CASO di un CAMBIO DI PORTO ma non per un RESO ho tutte le informazioni
051000110104      *  necessarie da controllare sulla bolla Mamma quindi eseguo i seguenti
051100110104      *  controlli a parità di LNA.
051200110104      *
051300110104      *  Verifiche per il cambio di porto:
051400110114      *       quindi fra mamma e figlia ci deve essere la stessa LNA
051500110114     C                   IF        not %Found(FNLBL01L)  or
051600110114     C                             LBLLAP <> ARBLNA
051700110114      *  se non trovata bolla legata oppure se la LNA è differente
051800110114      *    deve segnalare l'errore x l'Incasso.
051900110114     c                   eval      LRQ4MSGO  = ERR(01)
052000110114     c                   eval      LRQ4ERRO  = 'E'
052100110114     c                   leaveSR
052200110114     c                   else
052300110103     C* (Precedente) Bolla Mamma  controlla se ha l'indicazione di RESO
052400110104     C     Bolla_Mamma   CHAIN     FNARB01L
052500110103     C                   IF        %Found(FNARB01L)
052600110104      * Se è un reso,
052700110104     C* si Riposiziona sulla Bolla Passata (in questo caso quindi sulla Figlia)
052800110124      * e FINE direttamente.
052900110103     C                   if        ARBCCA = '2' or ARBCCA = '6'
053000110103     C     Bolla_Passata CHAIN     FNARB01L
053100110114     c                   leaveSR
053200101223     C                   END
053300110112     C*  solo se è un CAMBIO DI PORTO.
053400110124     C*  Reperisco Tipo Bolla della mamma
053500101223     C                   Z-ADD     1             X                 2 0
053600101223     C                   CLEAR                   DS3A
053700101223     C     ARBCBO        LOOKUP    C3A(X)                                 13
053800101223     C     *IN13         IFEQ      '1'
053900101223     C                   MOVEL     D3A(X)        DS3A
054000101223     C                   END
054100101223     C*  Controllo se c'è contrassegno
054200101223     C                   Z-ADD     0             AR9CAS
054300101223     C     §3AFCA        IFNE      0
054400110112     C     Bolla_Mamma   CHAIN     FIAR901L
054500101223     C                   END
054600110114      *
054700110114      *    deve segnalare come errore l'Incasso.
054800110114     c                   if        ar9CAS = 0
054900110114     c                   eval      LRQ4MSGO  = ERR(01)
055000110114     c                   eval      LRQ4ERRO  = 'E'
055100110114     c                   leaveSR
055200110114     C                   END
055300110112      *
055400110112     C                   END
055500110103      *
055600101223     C*  Controllo se la mamma è già stata consegnata ma non ancora
055700101223     C*  contabilizzata
055800101223     C     ARBICC        IFEQ      'S'
055900101223     C     ARBNMI        ANDEQ     0
056000110103     C     AR9CAS        ANDNE     0
056100101223     C     ARBICC        OREQ      'I'
056200101223     C     ARBNMI        ANDEQ     0
056300101223     C     AR9CAS        ANDNE     *ZEROS
056400110113     c                   eval      LRQ4MSGO  = ERR(06)
056500110103     c                   eval      LRQ4ERRO  = 'E'
056600110103     c                   leaveSR
056700101223     C                   END
056800101223     C*
056900110103     C     ARBICC        IFEQ      *BLANKS
057000101223     C     AR9CAS        ANDNE     0
057100101223     C     ARBICC        OREQ      'R'
057200101223     C     AR9CAS        ANDNE     0
057300110103     C*
057400101223     C*  Controllo se la mamma della spedizione inserita ha un'altra
057500101223     C*  figlia inserita in un'altra distinta
057600110103     C*
057700110111     C     Bolla_Trovata setll     FNLBL02L
057800110111     C     Bolla_Trovata reade     FNLBL02L
057900110103     C*
058000110103     c                   DoW       not %Eof(FNLBL02L)
058100110103     C*
058200101223     C*   Se ho trovato una figlia diversa dall' originale mi posiziono
058300101223     C*   su di essa e controllo se ha impostato il flag di ditinta chiusa
058400110103     C     LRQ4AASI      IFNE      LBLAAN
058500110103     C     LRQ4LNPI      ORNE      LBLLPN
058600110103     C     LRQ4NRSI      ORNE      LBLNRN
058700110103     C     LRQ4NSPI      ORNE      LBLNSN
058800110103      *
058900110104     C     Bolla_Figlia  CHAIN     FNARB01L
059000110103     C                   IF        %Found(FNARB01L) and
059100110103     C                             ARBNDC <> 0 and ARBFDC = ' '
059200110113     c                   eval      LRQ4MSGO  = ERR(07)
059300110103     c                   eval      LRQ4ERRO  = 'E'
059400110103     c                   leaveSR
059500101223     C                   END
059600110103     C*
059700101223     C                   END
059800110103     C*
059900110111     C*   Leggo la figlia successiva impostando la mamma
060000110111     C                   Z-ADD     LBLAAP        ARBAAS
060100110111     C                   Z-ADD     LBLLPP        ARBLNP
060200110111     C                   Z-ADD     LBLNRP        ARBNRS
060300110111     C                   Z-ADD     LBLNSP        ARBNSP
060400110111     C     Bolla_Trovata READE     FNLBL02L
060500110111     C                   ENDdo
060600110103     C*
060700101223     C                   END
060800110114      *>>>>>>>
060900110103     C                   ENDif
061000110114      *>>>>>>>
061100110112     C* Riaggancio la Bolla passata
061200110112     C     Bolla_Passata CHAIN     FNARB01L
061300101223     C                   END
061400101223     c                   endsr
061500101223     C*-----------------------------------------------------*
061600110104      *  Controllo x forzatura
061700110104     C*-----------------------------------------------------*
061800110104     C     Forzatura     BegSR
061900110104      *
062000110113     c                   clear                   LRQ4FRZ
062100110104      * msg di errore
062200110104     c                   eval      ERR001 = LRQ4MSGO
062300110113     c                   eval      des001 = 'Sped.: ' +
062400110113     c                                 %Editc(arbAAS:'X') +  ' ' +
062500110113     c                                 %Editc(arbLNP:'X') +  ' ' +
062600110113     c                                 %Editc(arbNRS:'X') +  ' ' +
062700110113     c                                 %Editc(arbNSP:'X') +  '  '
062800110114     c                   eval      des002 = '       ' + ARBRSD
062900110114     c                   eval      des003 = '       ' + ARBIND + ' '
063000110114     c                   eval      des004 = '       ' + ARBCAD + ' '
063100110113     c                                                + ARBLOD + ' '
063200110113     c                                          + '(' + ARBPRD + ')'
063300110113     c
063400110104      *
063500110113     c     ancora        tag
063600110104     c                   exfmt     LRQ4FRZ
063700110104      *   F6 = Forza
063800110104     c                   IF        *inKF
063900110104     c                   clear                   LRQ4MSGO
064000110104     c                   clear                   LRQ4ERRO
064100110113     c                   leavesr
064200110104      *   F12= Lascia in errore  trsforma la "F" in "E"
064300110104     c                   elseIF    *inKL
064400110104     c                   eval      LRQ4ERRO  = 'E'
064500110113     c                   leavesr
064600110104     C                   end
064700110113     c                   goto      ancora
064800110104      *
064900110104     C                   ENDSR
065000110112      *-----------------------------------------------------*
065100110112     C* Da fare solo la prima volta
065200110112      *-----------------------------------------------------*
065300110112     c     *INZSR        begsr
065400110112      *
065500110113     c                   movel     kpjbu         kpjbus
065600110113      *
065700110112     C     Bolla_Passata KLIST
065800110112     C                   KFLD                    LRQ4AASI
065900110112     C                   KFLD                    LRQ4LNPI
066000110112     C                   KFLD                    LRQ4NRSI
066100110112     C                   KFLD                    LRQ4NSPI
066101160510     c*
066102160510     c     kprm          klist
066103160510     c                   kfld                    dutpou
066104160510     c                   kfld                    pgm              10
066200110112      *
066300110112     C     Bolla_Mamma   KLIST
066400110112     C                   KFLD                    LBLAAP
066500110112     C                   KFLD                    LBLLPP
066600110112     C                   KFLD                    LBLNRP
066700110112     C                   KFLD                    LBLNSP
066800110112     C*
066900110112     C     Bolla_Figlia  KLIST
067000110112     C                   KFLD                    LBLAAN
067100110112     C                   KFLD                    LBLLPN
067200110112     C                   KFLD                    LBLNRN
067300110112     C                   KFLD                    LBLNSN
067400110112      *
067500110112     C     Bolla_Trovata KLIST
067600110112     C                   KFLD                    arbAAS
067700110112     C                   KFLD                    arbLNP
067800110112     C                   KFLD                    arbNRS
067900110112     C                   KFLD                    arbNSP
068000110112     C*
068100110112     C     KCLN          KLIST
068200110112     C                   KFLD                    KTFP
068300110112     C                   KFLD                    KTFA
068400110112     C                   KFLD                    KANN
068500110112     C                   KFLD                    KMES
068600110112     C     *LIKE         DEFINE    CLNTFP        KTFP
068700110112     C     *LIKE         DEFINE    CLNTFA        KTFA
068800110112     C     *LIKE         DEFINE    CLNANN        KANN
068900110112     C     *LIKE         DEFINE    CLNMES        KMES
069000110112      *
069100110112     C     KTAB          KLIST
069200110112     C                   KFLD                    tblkut
069300110112     C                   KFLD                    TBLCOD
069400110112     C                   z-add     1             tblkut
069401160509     C     KTAB1         KLIST
069402160509     C                   KFLD                    tblkut
069403160509     C                   KFLD                    TBLCOD
069404160509     C                   KFLD                    TBLkey
069500110112      *
069600110112     C     Kfiar5_GEN    KLIST
069700110112     C                   KFLD                    arbAAS
069800110112     C                   KFLD                    arbLNP
069900110112     C                   KFLD                    arbNRS
070000110112     C                   KFLD                    arbNSP
070100110112     C                   KFLD                    trcar5            3
070200110112      *
070300110112     C* Inverto la data del giorno
070400110112     C                   TIME                    WHHDAT           14 0
070500110112     C                   MOVEL     WHHDAT        HHMMss            6 0          *ORA/MINUTI/sec
070600110112     C                   MOVE      WHHDAT        G02DAT
070700110112     C                   MOVE      *BLANKS       G02ERR
070800110112     C                   CALL      'XSRDA8'
070900110112     C                   PARM                    WLBDA8
071000110112     C                   Z-ADD     G02INV        DATEU             8 0
071100110112     c     *dtaara       define    §azute        azuteds
071200110112     c     *dtaara       define    §datiute      ddatiute
071300110112     C                   in(E)     *dtaara
071400110112     c*
071500110112     C                   IF        %Error  or  RSUT = *blanks
071600110112     C                   call      'TIBS34R'
071700110112     C                   parm                    Tibs34Ds
071800110112     C                   in        *dtaara
071900110112     c                   ENDIF
072000110112     C                   move      *date         dtaeur
072100110112     C                   move      dtaeur        dtaiso
072101160510     C                   move      dtaeur        dtaiso_oggi
072200110112     c                   move      dtaiso        oggin             8 0
072300110112     c                   move      oggin         oggi              8
072400110112     c*
072500110112     C     dutpou        CHAIN     AZORG01l
072600110112    1C                   IF        %found(azorg01l)
072700110112     C                   MOVEL     ORGde6        og146
072800110112     c                   movel     orgde8        og148
072801160509     c                   movel     orgdf0        og150
072900110112     c                   end
072901160510     c*
072902160510     c                   exsr      srprm
073000110112     c*
073100110112      * se c'è una data in organigramma
073200110112     c                   move      §ogdda        data_org          8 0
073300110112     C                   MOVE      *BLANK        test_Parallelo    1
073400110112     c                   if        §ogdda <> ' '
073500110112      * se siamo in fase di Parallelo prima di essere partiti
073600110112     c                   if        data_org = 20391231 or dateu < data_org
073700110112     C                   eval      test_Parallelo = 'S'
073800110112     c                   end
073900110112     c                   end
074000110112      *
074100110112     C*  CARICO DATI TABELLA 7r part. consegne attesa
074200110112     C                   MOVEL     '7R'          TBLCOD
074300110112     C                   Z-ADD     0             X
074400110112     C                   Z-ADD     0             XX                3 0
074500110112     C     KTAB          setll     TABEL00F
074600110112     C     KTAB          READE     TABEL00F                               13
074700110112     C                   DOW       not %EoF(TABEL00F)
074800110112     c                   movel     tbluni        ds7r
074900110112     C                   if        x < 200 and §7ratt = 'S'
075000110112     C                   ADD       1             X
075100110112     C                   MOVEL     TBLKEY        s7r(X)
075200110112     c                   end
075300110112     C                   if        xx < 200 and
075400110112     c                             §7RRC  = 'S'
075500110112     C                   ADD       1             Xx
075600110112     C                   MOVEL     TBLKEY        s7rc(Xx)
075700110112     c                   end
075800110112     C     KTAB          READE     TABEL00F                               13
075900110112     C                   END
076000110112      *----
076100110112     C*  CARICO DATI TABELLA 3A
076200110112     C                   MOVEL     '3A'          TBLCOD
076300110112     C                   Z-ADD     0             X
076400110112     C     KTAB          CHAIN     TABEL00F                           13
076500110112     C     *IN13         DOWEQ     '0'
076600110112     C     X             ANDLT     100
076700110112     C                   ADD       1             X
076800110112     C                   MOVEL     TBLKEY        C3A(X)
076900110112     C                   MOVEL     TBLUNI        D3A(X)
077000110112     C     KTAB          READE     TABEL00F                               13
077100110112     C                   END
077200110112      *
077300110112     c                   endsr
077301160510     C*--------------------------------------------------------------*
077302160510     C*  parametri ultimo lancio assegnazione automatica
077303160510     C*--------------------------------------------------------------*
077304160510     C     srprm         BEGSR
077305160510     c                   movel     'FIDG17R'     pgm
077306160510     c     kprm          chain     fiprm01l
077307160510     c                   if        %found(fiprm01l) and prmdta = dtaiso_oggi
077308160510     c                   movel     prmpar        fidg17ds
077309160510     c                   else
077310160510     c                   clear                   fidg17ds
077311160510     c                   eval      d17tur1 = 'U'
077312160510     c                   end
077313160510     C                   ENDSR
077400110104     C*-----------------------------------------------------*
077500101213**  err
077600110112Spedizione già consegnata ma senza incassi.                        01
077700101223Linea arrivo spedizione non in gestione.                           02
077800110113La Spedizione non è stata trovata. Spedizione Inesistente.         03
077900110113La Spedizione non è abilitata alla consegna.                       04
078000110113Spedizione consegnata/in consegna altra distinta                   05
078100110113Sped.legata a bolla mamma già consegnata con C/A da contabilizare  06
078200110113Sped.legata a bolla mamma già presente in un'altra distinta.       07
078300110112APPUNTAMENTO senza N°Tel..Manca data cons.Richiesta.               08
078400110113APPUNTAMENTO   con N°Tel..Manca data cons.Richiesta.               09
078500110112La Spedizione è in fermo Deposito...                               10
078600110112In consegna nel GIORNO DI TURNO DI CHIUSURA.                       11
078700110112Spediz.con PARTICOLARITA' consegna DI ATTESA.                      12
078800160509La spedizione è di una zona del secondo turno                      13  a del se-
