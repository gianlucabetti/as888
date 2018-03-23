000100110104     h dftactgrp(*no) actgrp(*caller)
000200000810     H DECEDIT('0,') DATEDIT(*DMY.)
000300101222      *--------------------------------------------------------------*
000400101222      *     - Controlli distinta consegna                            *
000500101221      *--------------------------------------------------------------*
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
002100101221     D ERR             S             66    DIM(50) CTDATA PERRCD(1)             MSG ERRORE
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
003500110107      *
003600110105     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
003700110105     d fnarbds£      e ds                  extname(fnarb00F) prefix(£)
003800110105     d fnarbds       e ds                  extname(fnarb00F)
003900110107      *
004000110107     d fiargds$      e ds                  extname(fiarg00F) prefix($)
004100110107     d fiargds£      e ds                  extname(fiarg00F) prefix(£)
004200110107     d fiargds       e ds                  extname(fiarg00F)
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
006401130424     D tnsd99ds      E DS                  inz
006500031125      * DS per richiamo da fnlr50r x togliere bolla da distinta
006600101223     D fnlrq4ds      E DS
006700101223     D Fidg16ds      E DS
006800101223      *
006900101214     D WLBDA8          DS
007000101214     D  G02DAT                 1      8  0
007100101214     D  G02INV                 9     16  0
007200101214     D  G02ERR                17     17
007300101214     D  G02TGI                18     22  0
007400101214      *
008000101214     D DATPAR          DS
008100101214     D  GIODAT                 1      8  0
008200101214     D  GIOINV                 9     16  0
008300101214     D  GIOTGI                17     21  0
008400030730     d*---------------------------------------------------------------*
008500030730     d* campi di comodo                                               *
008600030730     d*---------------------------------------------------------------*
008700030730     d dtaeur          s               d   datfmt(*eur)
008800030730     d dtaiso          s               d
008900101214     d dtaiso_oggi     s               d   datfmt(*iso)
009000101214     d dtaiso_dst      s               d   datfmt(*iso)
009100101213      *-----
009200101223     D Found_Bolla     S              1    INZ('N')
009300101223     D Error_Bolla     S              1    INZ('N')
009400101213     D Rec_allocato    S              1    INZ('N')
009500110104     D SI              c                   'S'
009600110104     D NO              c                   'N'
009700101223      *-----------------------------------------------------*
009800101223     C     *ENTRY        PLIST
009900101223     C                   PARM                    KPJBA
010000110105     C                   PARM                    fnarbds$
010100110107     C                   PARM                    fiargds$
010200110114      *
010300110124     c                   movel     kpjbu         fnlrq4ds
010400110114     c                   clear                   LRQ4errO
010500110114     c                   clear                   LRQ4msgO
010600110114      *
010700101223     c                   exsr      inizializza
010800110105      *
010900110113     c                   if        LRQ4ERRO <> *blank
011000110124     c                   exsr      FINE
011100110113     c                   endif
011200110114      *
011300110105      *  Controlla assegnabilità
011400110105     C                   exsr      CHK_ASSEGNA
011500110112      *
011600110112      *  se non ci sono errori prima
011700110113     c                   if        LRQ4ERRO <> *blank
011800110124     c                   exsr      FINE
011900110113     c                   endif
012000110104      *
012100110104      *  Controlla Consegnabilità
012200110104     C                   exsr      CHK_CONSEGNA
012300110104      *
012400110124     c                   exsr      fine
012500110124      *-----------------------------------------------------*
012600110124     C*  FINE PROGRAMMA
012700110124      *-----------------------------------------------------*
012800110124     c     fine          begsr
012900110124      *  tipo chiusura del programma
013000110124     c                   if         LRQ4chiu = 'R'
013100110124     c                   seton                                        RT
013200110124     c                   elseIf     LRQ4chiu = 'L'
013300110124     c                   seton                                        LR
013400110124     c                   end
013500110124     c                   movel(p)  FNLRq4ds      kpjbu
013600110114     C                   eval      fnarbds$ = fnarbds
013700110114     C                   eval      fiargds$ = fiargds
013800110124     c                   return
013900110124     c                   endsr
014000110112      *-----------------------------------------------------*
014100110112     C* Inizializzazioni
014200110112      *-----------------------------------------------------*
014300110112     c     Inizializza   begsr
014400110113     c*
014500110113     C* Aggancio la £6 e recupero le linee a loro associate
014600110113     c                   clear                   trul06ds
014700110113     c                   eval      d06cod = '£6'
014800110113     c                   movel     dutpou        d06key
014900110113     c                   movel     trul06ds      kpjbu
015000110113     c                   call      'TRUL06R'
015100110113     c                   parm                    kpjba
015200110113     c                   movel     kpjbu         trul06ds
015300110113     c                   movel     kpjbus        kpjbu
015400110112      *
015500101223     C*  Controllo sul calendario il tipo di giorno che è la
015600110112     C*  data della distinta
015700101223     C     LRQ4DDCI      IFNE      0
015800101223     C                   Z-ADD     LRQ4DDCI      WMMGG             4 0
015900101223     C                   Z-ADD     LRQ4DDCI      WG                2 0
016000101223     C                   MOVEL     LRQ4DDCI      KANN
016100101223     C                   Z-ADD     0             KTFP
016200101223     C                   Z-ADD     0             KTFA
016300101223     C                   MOVEL     WMMGG         KMES
016400101223     C     KCLN          CHAIN     AZCLN01L                           31
016500101223     C                   MOVEA     CLNMAT        MES
016600101223     C                   MOVE      MES(WG)       WGG               1
016700101223     C                   END
016800110107      **
016900110107     c* aggancia la Bolla su ARB
017000110107     c                   clear                   fnarbds
017100110107     c                   IF        (%parms > 1 and $arbnsp >0)
017200110107     c                   eval      fnarbds = fnarbds$
017300110107      **
017400110107     c                   elseIF    %parms <2 or $arbnsp = 0
017500110107     C* AGGANCIO FNARB
017600110107     C     Bolla_Passata CHAIN(e)  FNARB01L
017700110107     c                   if        not %found(fnarb01l)
017800110113     c                   eval      LRQ4MSGO  = ERR(03)
017900110112     c                   eval      LRQ4ERRO  = 'E'
018000110112     c                   leaveSR
018100110107     c                   else
018200110107     c                   eval      fnarbds = fnarbds£
018300110107     c                   end
018400110107      **
018500110107     c                   endIF
018600110112     C*-------------
018700110112      ** TROVATA LA BOLLA, Controllo se linea di arrivo abilitata
018800110112     C     ARBLNA        LOOKUP    LINT                                   70
018900110112     c                   if        not *in70
019000110113     c                   eval      LRQ4MSGO  = ERR(02)
019100110112     c                   eval      LRQ4ERRO  = 'E'
019200110112     c                   leaveSR
019300110112     c                   end
019400110107      **
019500110107     c* aggancia la Bolla su ARG
019600110107     c                   clear                   fiargds
019700110107     c                   IF        (%parms > 2 and $argnsp >0)
019800110107     c                   eval      fiargds = fiargds$
019900110107      **
020000110107     c                   elseIF    %parms <3 or $argnsp = 0
020100110112     C* AGGANCIO FIARG
020200110107     C     Bolla_Passata CHAIN(e)  FIARG01L
020300110107     c                   if        not %found(fiarg01l)
020400110107     c                   else
020500110107     c                   eval      fiargds = fiargds£
020600110107     c                   end
020700110107      **
020800110107     c                   endIF
020900110107      **
021000110112     c* se c'è un lasciato avviso sblocco in automatico la spedizione
021100110112     c                   IF        arbfbc = 'A'
021200110112     c                   clear                   dslr02
021300110112     c                   eval      dr2vid = 'N'
021400110112     C                   Z-ADD     LRQ4AASI      DR2AAS
021500110112     C                   Z-ADD     LRQ4LNPI      DR2LNP
021600110112     C                   Z-ADD     LRQ4NRSI      DR2NRS
021700110112     C                   Z-ADD     LRQ4NSPI      DR2NSP
021800110112     C                   Z-ADD     arbLNA        DR2FGS
021900110112     C                   MOVEL     '1'           DR2RIC
022000110112     C                   MOVEL     DSLR02        KPJBU
022100110112     C                   CALL      'FNLR39R'
022200110112     C                   PARM                    KPJBA
022300110112     c                   movel     kpjbu         dslr02
022400110112      *
022500110112     c                   if        dr2err = *blanks
022600110112     c                   clear                   ARBcmc
022700110112     c                   clear                   ARBfbc
022800110112     c                   end
022900110112     c                   end
023000101223     c                   ENDSR
023100110111     C*-----------------------------------------------------*
023200110111     C* Controlla Assegnabilità
023300110111     C*-----------------------------------------------------*
023400110111     C     CHK_ASSEGNA   BEGSR
023500110111      *
023600110111      * controlla che il record sia giusto da prendere
023700110111      * chiamo pgm per vedere se spedizione da mettere in distinta
023800110111     c                   clear                   fidg16ds
023900110111     c                   eval      d16aasi    = LRQ4AASI
024000110111     c                   eval      d16lnpi    = LRQ4LNPI
024100110111     c                   eval      d16nrsi    = LRQ4NRSI
024200110111     c                   eval      d16nspi    = LRQ4NSPI
024300110111      **
024400110111      * deve essere impostata la data
024500110112     c                   eval      d16dtaI = 0                                  DATA LIMITE ABBINAME
024600110111     c                   eval      d16oraI = 0                                  ORA LIMITE ABBINAMEN
024700110111     c                   eval      d16LDCRI= 0                                  Data limite cons.ric
024800110112     c                   eval      d16dfvi = 0                                  DATA DISTINTA/0=OGG
024900110112     c                   if        LRQ4DDCI > 0
025000110112     c                   eval      d16dfvi = LRQ4DDCI                           DATA DISTINTA/0=OGG
025100110112     c                   end
025200110111      **
025300110111     c                   if        test_Parallelo = 'S'
025400110125     c                   eval      d16ndci = 'E'                                G=ARG/B=ARB/E=ENTRAM
025500110112     c                   else
025600110125     c                   eval      d16ndci = 'B'                                G=ARG/B=ARB/E=ENTRAM
025700110112     c                   end
025800110111      **
025900110112      **  controllo assegnabilità
026000110111     c                   eval      d16consi   = *blank                          BLK=SOLO CTR RICHI
026100110111      **
026200110125     c                   eval      d16fbci    = 'S'                             ABIL.C/ASS./BLK=NO
026300110125     c                   eval      D16ACCI    = 'S'                             ABIL.C/ASS./BLK=NO
026400110111     c                   eval      D16ACAI    = 'S'                             ABIL.ASSEGN./BLK=NO
026500110111     c                   eval      D16CMCI    = 'S'                             SPED.GIAC.DA APRIRE/
026600110111     c                   eval      D16LBLMI   = 'S'                             SE SPED.MAMMA/BLK=NO
026700110111     c                   eval      D16DCRI    = 'S'                             CONSEGNA RICHIESTA
026800110111     c                   eval      D16BGEI    = 'S'                             SPED. SOSPESA ALLA C
026900130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
027000130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
027100130221     c**********         if        arbxco <> ' '
027200130221     c                   if        arbxco > '0'
027300110728     c                   eval      d16prfoI= 'S'
027400110728     c                   end
027500110124     c                   movel(p)  fidg16ds      kpjbu
027600110111     c                   call      'FIDG16R'
027700110111     c                   parm                    kpjba
027800110111     c                   parm                    fnarbds
027900110111     c                   parm                    fiargds
028000110114      *
028100110124     c                   movel(p)  kpjbu         fidg16ds
028200110111      *
028300110125     c                   if        D16CONSO = 'N'
028400110111     c                   eval      LRQ4MSGO  = D16MSGO
028500110125     c* per alcuni errori non arriva il msg di ritorno perchè non si
028600110125     c* voleva riempire la stampa di anomalie in assegnaz. automatica
028700110125     c* quindi si deve intercettare l'errore e impostare il msg
028800110125     c                   if        d16msgo = ' '
028900110125     c                   select
029000110125     c                   when      d16ndco = 'S'
029100110125     c                   eval      LRQ4MSGO  = 'Spedizione già in distinta'
029200110125     c                   when      d16cmco = 'S'
029300110125     c                   eval      LRQ4MSGO  = 'Spedizione con giacenza da apri-
029400110125     c                             re'
029500110125     c                   when      d16lblmo = 'S'
029600110125     c                   eval      LRQ4MSGO  = 'Spedizione mamma'
029700110125     c                   when      d16fbco = 'S'
029800110125     c                   eval      LRQ4MSGO  = 'Spedizione bloccata/giacente'
029900110125     c                   endsl
030000110125     c                   end
030100110111     c                   eval      LRQ4ERRO  = 'E'
030200110111     c                   leaveSR
030300110111     c                   end
030400110111      *
030500110111     c                   endsr
030600101223      *-------------------------------------------------------*
030700101223     C*  Controllo la Bolla
030800101223      *-------------------------------------------------------*
030900110104     C     CHK_CONSEGNA  BEGSR
031000110107      **
031100110103     C*  Reperisco Tipo Bolla
031200110103     C                   Z-ADD     1             X                 2 0
031300110103     C                   CLEAR                   DS3A
031400110103     C     ARBCBO        LOOKUP    C3A(X)                                 13
031500110103     C     *IN13         IFEQ      '1'
031600110103     C                   MOVEL     D3A(X)        DS3A
031700110103     C                   END
031800101223     C*
031900101223     C* Controlli x bolle da consegnare
032000110111     C*-----------------*****
032100101223     C     ARBDCM        IFEQ      0
032200110111     C*-----------------*****
032300101223     C* Controllo il livello di abilitazione merce
032400101223     C     ARBAMA        IFEQ      *ZEROS
032500101223     c     arbdam        andeq     0
032600110113     c                   eval      LRQ4MSGO  = ERR(04)
032700110103     c                   eval      LRQ4ERRO  = 'E'
032800101223     c                   leaveSR
032900101223     C                   END
033000101223      *
033100101223     C     ARBAMA        IFEQ      *ZEROS
033200101223     c     arbdam        andne     0
033300110103      ** controllo Forzabile   --> veniva bypassato se c'era la forzatura
033400110113     c                   eval      LRQ4MSGO  = ERR(04)
033500110103     c                   eval      LRQ4ERRO  = 'F'
033600110104     c                   exsr      Forzatura
033700110104     c                   if        LRQ4ERRO  <> *blank
033800101223     c                   leaveSR
033900110104     C                   end
034000101223     C                   END
034100110803      *
034200101223     C*----------------------------------------------------*
034300110112     C* se gestito l'appuntamento, controlla se inserita una data
034400110112     C* consegna richiesta e, se non c'è, è possibile solo se non
034500110112     C* esiste il telefono.
034600101223     C*----------------------------------------------------*
034700101223     C     ARBTC1        IFEQ      'A'
034800101223     C     ARBTC2        OREQ      'A'
034900101223      *
035000101223     C     ARBDCR        IFEQ      0
035100110103     C     ARBHCR        ANDEQ     0
035200101223     C     ARBTCR        ANDEQ     ' '
035300101223      *
035400101223      *    record GEN riferimenti destinatario
035500101223      * controllo numero di telefono se appuntamento
035600101223     C                   clear                   dar5gen
035700101223     C                   move      'GEN'         trcar5
035800110107     C     Kfiar5_GEN    CHAIN     FiAR501L                           43
035900110104     C                   IF        %Found(FiAR501L)
036000110104     C                   MOVEL     ar5uni        dar5gen
036100110104     c                   end
036200101223      *  errori
036300110103      * controllo numero di telefono se appuntamento
036400110104     c                   if        §ar5teld = *blank
036500110104      *  FORZABILE:
036600101223     c                   eval      LRQ4MSGO  = ERR(08)
036700110104     c                   eval      LRQ4ERRO  = 'F'
036800110104     c                   exsr      Forzatura
036900110104     c                   if        LRQ4ERRO  <> *blank
037000110104     c                   leaveSR
037100110104     C                   end
037200101223     c                   else
037300101223     c                   eval      LRQ4MSGO  = ERR(09)
037400110103     c                   eval      LRQ4ERRO  = 'E'
037500101223     c                   leaveSR
037600101223     c                   endif
037700101223      *
037800110104     C                   END
037900101223     C                   END
038000110111      *
038100110111     C*----------------------------------------------------*
038200110111     C* Controllo se la spedizione è in fermo deposito e lo segnalo
038300110111     C     ARBFFD        IFEQ      'S'
038400110111      *  FORZABILE:
038500110111     c                   eval      LRQ4MSGO  = ERR(10)
038600110111     c                   eval      LRQ4ERRO  = 'F'
038700110111     c                   exsr      Forzatura
038800110111     c                   if        LRQ4ERRO  <> *blank
038900110111     c                   leaveSR
039000110111     C                   end
039100110111     C                   END
039200110111      *
039300110103      *
039400101223     C*----------------------------------------------------*
039500110103      * Giorni Chiusura del destinatario
039600101223     C     ARBGC1        IFNE      *BLANKS
039700101223     C                   MOVEL     ARBGC1        WGIOR             1
039800101223     C     WGIOR         IFEQ      WGG
039900110112      ** controllo Forzabile
040000110104      *  FORZABILE:
040100101223     c                   eval      LRQ4MSGO  = ERR(11)
040200101223     c                   eval      LRQ4ERRO  = 'F'
040300110104     c                   exsr      Forzatura
040400110104     c                   if        LRQ4ERRO  <> *blank
040500110104     c                   leaveSR
040600110104     C                   end
040700101223     C                   END
040800101223     C                   END
040900110103      *
041000110103      * Giorni Chiusura del destinatario
041100101223     C     ARBGC2        IFNE      *BLANKS
041200101223     C                   MOVEL     ARBGC2        WGIOR             1
041300101223     C     WGIOR         IFEQ      WGG
041400110112      ** controllo Forzabile
041500110104      *  FORZABILE:
041600101223     c                   eval      LRQ4MSGO  = ERR(11)
041700101223     c                   eval      LRQ4ERRO  = 'F'
041800110104     c                   exsr      Forzatura
041900110104     c                   if        LRQ4ERRO  <> *blank
042000110104     c                   leaveSR
042100110104     C                   end
042200101223     C                   END
042300101223     C                   END
042400101223      *
042500101223     C*----------------------------------------------------*
042600101223     C* controllo se spedizione ha particolarità cons. di attesa
042700101223     C     ARBGma        IFNE      *BLANKS
042800101223     C     ARBTCR        andeq     'P'
042900101223      *
043000101223     C     ARBdcr        IFGT      LRQ4DDCI
043001130429     c                   clear                   tnsd99ds
043002130424     C                   Z-ADD     LRQ4DDCI      D98DFC
043003130424     C                   Z-ADD     2359          D98OFC
043004130424     C                   MOVEL     ' '           D98TLA
043005130424     C                   MOVEL     'A'           D98TBO
043006130424     C                   Z-ADD     ARBAAS        D98AAS
043007130424     C                   Z-ADD     ARBLNP        D98LNP
043008130424     C                   Z-ADD     ARBNRS        D98NRS
043009130424     C                   Z-ADD     ARBNSP        D98NSP
043010130424     C                   CALL      'TNSD99R'
043011130424     C                   PARM                    TNSD99DS
043012130424     c                   if        d98nci <=-24
043700101223     C     arbgma        lookup    s7r                                    48
043800101223     C                   IF        *in48
043900110104      *  FORZABILE:
044000101223     c                   eval      LRQ4MSGO  = ERR(12)
044100101223     c                   eval      LRQ4ERRO  = 'F'
044200110104     c                   exsr      Forzatura
044300110104     c                   if        LRQ4ERRO  <> *blank
044400110104     c                   leaveSR
044500110104     C                   end
044600101223     C                   END
044700101223     C                   End
044800101223      *
044900101223     C                   END
045000101223     C                   END
045100101223      *
045200101223     C*----------------------------------------------------*
045300101223     C     ARBFDC        IFNE      ' '
045400101223     C     ARBDCP        ANDEQ     0
045500110113     c                   eval      LRQ4MSGO  = ERR(05)
045600110103     c                   eval      LRQ4ERRO  = 'E'
045700101223     c                   leaveSR
045800101223     C                   END
045900101223     C*
046000110111     C*-----------------*****
046100110111     C* Altrimenti: Bolla già consegnata
046200110111     C                   ELSE
046300110111     C*-----------------*****
046400110114     C*  Resetta il campo
046500110114     C                   Z-ADD     0             AR9CAS
046600101223      *
046700101223     C* CONTROLLO SE I FLAG DI INCASSO ICA / ICC SONO DA PRENDERE IN
046800101223     C* CONSIDERAZIONE (NEL CASO DI CONSEGNA PARZIALE O CONSEGNA CON
046900101223     C* RITORNO ALL'INCASSO)
047000110112     C     ARBFIP        IFNE      'I'
047100110114     C     ARBICA        ANDNE     'R'
047200110114     C*
047300110114     C* Ritorno all'incasso
047400110114     C     ARBICC        IFNE      'R'
047500110114     c                   eval      LRQ4MSGO  = ERR(01)
047600110114     c                   eval      LRQ4ERRO  = 'E'
047700110114     c                   leaveSR
047800110114     C                   END
047900101223     C                   END
048000110103      *
048100101223     C                   END
048200110124     C*  Controllo se c'è contrassegno
048300110124     C     ARBICC        IFEQ      ' '
048400110124     C     §3AFCA        andNE     0
048500110124     C     Bolla_Passata CHAIN     FIAR901L
048600110124     C                   END
048700110124     c*
048800110114      *  verifico se la "Bolla Passata"  ha una mamma
048900110114      *  e se non è stata chiusa con CCA 2 o 6  ovvero come RESO
049000110114      *    e ha un C/Assegno.
049100110124     C                   If        §3AFCA <> 0 and ar9CAS = 0
049200110103     C     Bolla_Passata CHAIN     FNLBL01L
049300110103      *
049400110104      *  IN CASO di un CAMBIO DI PORTO ma non per un RESO ho tutte le informazioni
049500110104      *  necessarie da controllare sulla bolla Mamma quindi eseguo i seguenti
049600110104      *  controlli a parità di LNA.
049700110104      *
049800110104      *  Verifiche per il cambio di porto:
049900110114      *       quindi fra mamma e figlia ci deve essere la stessa LNA
050000110114     C                   IF        not %Found(FNLBL01L)  or
050100110114     C                             LBLLAP <> ARBLNA
050200110114      *  se non trovata bolla legata oppure se la LNA è differente
050300110114      *    deve segnalare l'errore x l'Incasso.
050400110114     c                   eval      LRQ4MSGO  = ERR(01)
050500110114     c                   eval      LRQ4ERRO  = 'E'
050600110114     c                   leaveSR
050700110114     c                   else
050800110103     C* (Precedente) Bolla Mamma  controlla se ha l'indicazione di RESO
050900110104     C     Bolla_Mamma   CHAIN     FNARB01L
051000110103     C                   IF        %Found(FNARB01L)
051100110104      * Se è un reso,
051200110104     C* si Riposiziona sulla Bolla Passata (in questo caso quindi sulla Figlia)
051300110124      * e FINE direttamente.
051400110103     C                   if        ARBCCA = '2' or ARBCCA = '6'
051500110103     C     Bolla_Passata CHAIN     FNARB01L
051600110114     c                   leaveSR
051700101223     C                   END
051800110112     C*  solo se è un CAMBIO DI PORTO.
051900110124     C*  Reperisco Tipo Bolla della mamma
052000101223     C                   Z-ADD     1             X                 2 0
052100101223     C                   CLEAR                   DS3A
052200101223     C     ARBCBO        LOOKUP    C3A(X)                                 13
052300101223     C     *IN13         IFEQ      '1'
052400101223     C                   MOVEL     D3A(X)        DS3A
052500101223     C                   END
052600101223     C*  Controllo se c'è contrassegno
052700101223     C                   Z-ADD     0             AR9CAS
052800101223     C     §3AFCA        IFNE      0
052900110112     C     Bolla_Mamma   CHAIN     FIAR901L
053000101223     C                   END
053100110114      *
053200110114      *    deve segnalare come errore l'Incasso.
053300110114     c                   if        ar9CAS = 0
053400110114     c                   eval      LRQ4MSGO  = ERR(01)
053500110114     c                   eval      LRQ4ERRO  = 'E'
053600110114     c                   leaveSR
053700110114     C                   END
053800110112      *
053900110112     C                   END
054000110103      *
054100101223     C*  Controllo se la mamma è già stata consegnata ma non ancora
054200101223     C*  contabilizzata
054300101223     C     ARBICC        IFEQ      'S'
054400101223     C     ARBNMI        ANDEQ     0
054500110103     C     AR9CAS        ANDNE     0
054600101223     C     ARBICC        OREQ      'I'
054700101223     C     ARBNMI        ANDEQ     0
054800101223     C     AR9CAS        ANDNE     *ZEROS
054900110113     c                   eval      LRQ4MSGO  = ERR(06)
055000110103     c                   eval      LRQ4ERRO  = 'E'
055100110103     c                   leaveSR
055200101223     C                   END
055300101223     C*
055400110103     C     ARBICC        IFEQ      *BLANKS
055500101223     C     AR9CAS        ANDNE     0
055600101223     C     ARBICC        OREQ      'R'
055700101223     C     AR9CAS        ANDNE     0
055800110103     C*
055900101223     C*  Controllo se la mamma della spedizione inserita ha un'altra
056000101223     C*  figlia inserita in un'altra distinta
056100110103     C*
056200110111     C     Bolla_Trovata setll     FNLBL02L
056300110111     C     Bolla_Trovata reade     FNLBL02L
056400110103     C*
056500110103     c                   DoW       not %Eof(FNLBL02L)
056600110103     C*
056700101223     C*   Se ho trovato una figlia diversa dall' originale mi posiziono
056800101223     C*   su di essa e controllo se ha impostato il flag di ditinta chiusa
056900110103     C     LRQ4AASI      IFNE      LBLAAN
057000110103     C     LRQ4LNPI      ORNE      LBLLPN
057100110103     C     LRQ4NRSI      ORNE      LBLNRN
057200110103     C     LRQ4NSPI      ORNE      LBLNSN
057300110103      *
057400110104     C     Bolla_Figlia  CHAIN     FNARB01L
057500110103     C                   IF        %Found(FNARB01L) and
057600110103     C                             ARBNDC <> 0 and ARBFDC = ' '
057700110113     c                   eval      LRQ4MSGO  = ERR(07)
057800110103     c                   eval      LRQ4ERRO  = 'E'
057900110103     c                   leaveSR
058000101223     C                   END
058100110103     C*
058200101223     C                   END
058300110103     C*
058400110111     C*   Leggo la figlia successiva impostando la mamma
058500110111     C                   Z-ADD     LBLAAP        ARBAAS
058600110111     C                   Z-ADD     LBLLPP        ARBLNP
058700110111     C                   Z-ADD     LBLNRP        ARBNRS
058800110111     C                   Z-ADD     LBLNSP        ARBNSP
058900110111     C     Bolla_Trovata READE     FNLBL02L
059000110111     C                   ENDdo
059100110103     C*
059200101223     C                   END
059300110114      *>>>>>>>
059400110103     C                   ENDif
059500110114      *>>>>>>>
059600110112     C* Riaggancio la Bolla passata
059700110112     C     Bolla_Passata CHAIN     FNARB01L
059800101223     C                   END
059900101223     c                   endsr
060000101223     C*-----------------------------------------------------*
060100110104      *  Controllo x forzatura
060200110104     C*-----------------------------------------------------*
060300110104     C     Forzatura     BegSR
060400110104      *
060500110113     c                   clear                   LRQ4FRZ
060600110104      * msg di errore
060700110104     c                   eval      ERR001 = LRQ4MSGO
060800110113     c                   eval      des001 = 'Sped.: ' +
060900110113     c                                 %Editc(arbAAS:'X') +  ' ' +
061000110113     c                                 %Editc(arbLNP:'X') +  ' ' +
061100110113     c                                 %Editc(arbNRS:'X') +  ' ' +
061200110113     c                                 %Editc(arbNSP:'X') +  '  '
061300110114     c                   eval      des002 = '       ' + ARBRSD
061400110114     c                   eval      des003 = '       ' + ARBIND + ' '
061500110114     c                   eval      des004 = '       ' + ARBCAD + ' '
061600110113     c                                                + ARBLOD + ' '
061700110113     c                                          + '(' + ARBPRD + ')'
061800110113     c
061900110104      *
062000110113     c     ancora        tag
062100110104     c                   exfmt     LRQ4FRZ
062200110104      *   F6 = Forza
062300110104     c                   IF        *inKF
062400110104     c                   clear                   LRQ4MSGO
062500110104     c                   clear                   LRQ4ERRO
062600110113     c                   leavesr
062700110104      *   F12= Lascia in errore  trsforma la "F" in "E"
062800110104     c                   elseIF    *inKL
062900110104     c                   eval      LRQ4ERRO  = 'E'
063000110113     c                   leavesr
063100110104     C                   end
063200110113     c                   goto      ancora
063300110104      *
063400110104     C                   ENDSR
063500110112      *-----------------------------------------------------*
063600110112     C* Da fare solo la prima volta
063700110112      *-----------------------------------------------------*
063800110112     c     *INZSR        begsr
063900110112      *
064000110113     c                   movel     kpjbu         kpjbus
064100110113      *
064200110112     C     Bolla_Passata KLIST
064300110112     C                   KFLD                    LRQ4AASI
064400110112     C                   KFLD                    LRQ4LNPI
064500110112     C                   KFLD                    LRQ4NRSI
064600110112     C                   KFLD                    LRQ4NSPI
064700110112      *
064800110112     C     Bolla_Mamma   KLIST
064900110112     C                   KFLD                    LBLAAP
065000110112     C                   KFLD                    LBLLPP
065100110112     C                   KFLD                    LBLNRP
065200110112     C                   KFLD                    LBLNSP
065300110112     C*
065400110112     C     Bolla_Figlia  KLIST
065500110112     C                   KFLD                    LBLAAN
065600110112     C                   KFLD                    LBLLPN
065700110112     C                   KFLD                    LBLNRN
065800110112     C                   KFLD                    LBLNSN
065900110112      *
066000110112     C     Bolla_Trovata KLIST
066100110112     C                   KFLD                    arbAAS
066200110112     C                   KFLD                    arbLNP
066300110112     C                   KFLD                    arbNRS
066400110112     C                   KFLD                    arbNSP
066500110112     C*
066600110112     C     KCLN          KLIST
066700110112     C                   KFLD                    KTFP
066800110112     C                   KFLD                    KTFA
066900110112     C                   KFLD                    KANN
067000110112     C                   KFLD                    KMES
067100110112     C     *LIKE         DEFINE    CLNTFP        KTFP
067200110112     C     *LIKE         DEFINE    CLNTFA        KTFA
067300110112     C     *LIKE         DEFINE    CLNANN        KANN
067400110112     C     *LIKE         DEFINE    CLNMES        KMES
067500110112      *
067600110112     C     KTAB          KLIST
067700110112     C                   KFLD                    tblkut
067800110112     C                   KFLD                    TBLCOD
067900110112     C                   z-add     1             tblkut
068000110112      *
068100110112     C     Kfiar5_GEN    KLIST
068200110112     C                   KFLD                    arbAAS
068300110112     C                   KFLD                    arbLNP
068400110112     C                   KFLD                    arbNRS
068500110112     C                   KFLD                    arbNSP
068600110112     C                   KFLD                    trcar5            3
068700110112      *
068800110112     C* Inverto la data del giorno
068900110112     C                   TIME                    WHHDAT           14 0
069000110112     C                   MOVEL     WHHDAT        HHMMss            6 0          *ORA/MINUTI/sec
069100110112     C                   MOVE      WHHDAT        G02DAT
069200110112     C                   MOVE      *BLANKS       G02ERR
069300110112     C                   CALL      'XSRDA8'
069400110112     C                   PARM                    WLBDA8
069500110112     C                   Z-ADD     G02INV        DATEU             8 0
069600110112     c     *dtaara       define    §azute        azuteds
069700110112     c     *dtaara       define    §datiute      ddatiute
069800110112     C                   in(E)     *dtaara
069900110112     c*
070000110112     C                   IF        %Error  or  RSUT = *blanks
070100110112     C                   call      'TIBS34R'
070200110112     C                   parm                    Tibs34Ds
070300110112     C                   in        *dtaara
070400110112     c                   ENDIF
070500110112     C                   move      *date         dtaeur
070600110112     C                   move      dtaeur        dtaiso
070700110112     c                   move      dtaiso        oggin             8 0
070800110112     c                   move      oggin         oggi              8
070900110112     c*
071000110112     C     dutpou        CHAIN     AZORG01l
071100110112    1C                   IF        %found(azorg01l)
071200110112     C                   MOVEL     ORGde6        og146
071300110112     c                   movel     orgde8        og148
071400110112     c                   end
071500110112     c*
071600110112      * se c'è una data in organigramma
071700110112     c                   move      §ogdda        data_org          8 0
071800110112     C                   MOVE      *BLANK        test_Parallelo    1
071900110112     c                   if        §ogdda <> ' '
072000110112      * se siamo in fase di Parallelo prima di essere partiti
072100110112     c                   if        data_org = 20391231 or dateu < data_org
072200110112     C                   eval      test_Parallelo = 'S'
072300110112     c                   end
072400110112     c                   end
072500110112      *
072600110112     C*  CARICO DATI TABELLA 7r part. consegne attesa
072700110112     C                   MOVEL     '7R'          TBLCOD
072800110112     C                   Z-ADD     0             X
072900110112     C                   Z-ADD     0             XX                3 0
073000110112     C     KTAB          setll     TABEL00F
073100110112     C     KTAB          READE     TABEL00F                               13
073200110112     C                   DOW       not %EoF(TABEL00F)
073300110112     c                   movel     tbluni        ds7r
073400110112     C                   if        x < 200 and §7ratt = 'S'
073500110112     C                   ADD       1             X
073600110112     C                   MOVEL     TBLKEY        s7r(X)
073700110112     c                   end
073800110112     C                   if        xx < 200 and
073900110112     c                             §7RRC  = 'S'
074000110112     C                   ADD       1             Xx
074100110112     C                   MOVEL     TBLKEY        s7rc(Xx)
074200110112     c                   end
074300110112     C     KTAB          READE     TABEL00F                               13
074400110112     C                   END
074500110112      *----
074600110112     C*  CARICO DATI TABELLA 3A
074700110112     C                   MOVEL     '3A'          TBLCOD
074800110112     C                   Z-ADD     0             X
074900110112     C     KTAB          CHAIN     TABEL00F                           13
075000110112     C     *IN13         DOWEQ     '0'
075100110112     C     X             ANDLT     100
075200110112     C                   ADD       1             X
075300110112     C                   MOVEL     TBLKEY        C3A(X)
075400110112     C                   MOVEL     TBLUNI        D3A(X)
075500110112     C     KTAB          READE     TABEL00F                               13
075600110112     C                   END
075700110112      *
075800110112     c                   endsr
075900110104     C*-----------------------------------------------------*
076000101213**  err
076100110112Spedizione già consegnata ma senza incassi.                        01
076200101223Linea arrivo spedizione non in gestione.                           02
076300110113La Spedizione non è stata trovata. Spedizione Inesistente.         03
076400110113La Spedizione non è abilitata alla consegna.                       04
076500110113Spedizione consegnata/in consegna altra distinta                   05
076600110113Sped.legata a bolla mamma già consegnata con C/A da contabilizare  06
076700110113Sped.legata a bolla mamma già presente in un'altra distinta.       07
076800110112APPUNTAMENTO senza N°Tel..Manca data cons.Richiesta.               08
076900110113APPUNTAMENTO   con N°Tel..Manca data cons.Richiesta.               09
077000110112La Spedizione è in fermo Deposito...                               10
077100110112In consegna nel GIORNO DI TURNO DI CHIUSURA.                       11
077200110112Spediz.con PARTICOLARITA' consegna DI ATTESA.                      12
