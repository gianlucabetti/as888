000100150527     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('PJXBND')
000200150309     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000300000000     H*--------------------------------------------------------------*
000400151103     FaiATa01l  if   e           k disk
000401151103     faiats01l  if   e           k disk
000800151103     Fwftntl880fuf a e           k disk    usropn
001000150527     D*-------------
001100150527     D xatbds        e ds                  inz
001400150527     D*-------------
001500020304     dkpjba          e ds
001600030630     d Tibs36Ds      e ds
001700150309     D TIBS34DS      E DS                                                       *Profili utente
001800150309     D DDATIUTE      E DS                                                       *Dati utente
001900150309     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
002000151103     d TNTL88DS      e ds
002300150309     d dtaiso          s               d   datfmt(*iso)
002400150309     d dtaeur          s               d   datfmt(*eur)
002500150605     d oggi_iso        s               d   datfmt(*iso)
002700150309     d COM08           s              8  0
002800070605     D WLBDA8          DS
002900070605     D  G02DAT                 1      8  0
003000070605     D  G02INV                 9     16  0
003100070605     D  G02ERR                17     17
003200070605     D  G02TGI                18     22  0
003300090518     D tibsSocI0...
003400090518     D               E DS                  QUALIFIED
003500090518     D                                     INZ
003600090518     D tibsSocO0...
003700090518     D               E DS                  QUALIFIED
003800090518     D                                     INZ
003900090518
004000090518     D prmRqsOpCode...
004100090518     D                 S             10A
004200090518     D prmRpyOpCode...
004300090518     D                 S             10A
004400090518     D prmRpyIdMsg...
004500090518     D                 S             10I 0
004600090518     D prmRqsFormato...
004700090518     D                 S             10A
004800090518     D prmRqsDataSize...
004900090518     D                 S             10I 0
005000090518     D prmRpyFormato...
005100090518     D                 S             10A
005200090518     D prmRpyDataSize...
005300090518     D                 S             10I 0
005400080502      *
005500090519     d coman           s             99
005600090519     d luncmd          s             15  5
005700090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
006200000000     C*---------------------------------------------------------------*
006300020304      *
006400050623     c                   exsr      elabora
006500150527      *
007100150309     c* stampa
007200151103     c                   if        i88tipoel ='S'
007300150527     c***********        exsr      srstampa
007400150309     c                   end
008800150527      *
008900090519     c                   exsr      comandi
009000150527      *
009100020308     c                   seton                                        lr
009200050802     c*-------------------------------------------------
009300090518     c     elabora       begsr
009400050802     c*-------------------------------------------------
009500151103     c     *loval        setll     aiATS01l
009600150527      *
009700070530     c                   do        *hival
009800151103     c                   read      aiATS01l                               98
009900070530     c   98              leave
010000151103     c                   clear                   tNTL8800
010100150527      *
010200090518      * filtro se cerca società in forza alla data
010300151103     c                   if        ATSatb <> *blank
010400120413     c                   iter
010500120413     c                   endif
010501151103     C* SOLO TRAZIONISTI VERI
010502151103     c                   if        atsDEC = 0 or atsnrc = 0
010503151103     c                   iter
010504151103     c                   endif
010600150527      *
010700151103      *considera anche quelli disaccreditate nel mese
010800151103     c                   if        i88datal > 0
010900151103     c                   if        i88mese = 'S'
011000150527      *
011100151103     c                   if        ATSDTFIC > 0 and ATSDTFIC < inimese
011200070530     c                   iter
011300070530     c                   endif
011400150527      *
011500130123     c                   else
011600151103      *li esclude
011700151103     c                   if        ATSDTFIC > 0
011800130123     c                   iter
011900130123     c                   endif
012000150527      *
012100130123     c                   endif
012200090518     c                   endif
012300150527      *
012400090518      * filtro se cerca società in forza alla data
012500151103     c                   if        i88datdal > 0
012600151103     c                   if        ATSDTFIC > 0 or atsdtinc < i88datdal
012700090518     c                   iter
012800090518     c                   endif
012900090518     c                   endif
013100150309     c*
013200151103     c                   clear                   t88socgd
013201151103     c                   move      atsSOC        T88SOCG
013300090518     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
013400090518     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
013500090518     C                   RESET                   tibsSocI0
013600151103     C                   EVAL      tibsSocI0.idSocieta = t88socg
013700090518     C
013800090518     C                   CALL      'TIBSSOCR'
013900090518     C                   PARM      'GETANAGRAF'  prmRqsOpCode
014000090518     C                   PARM      *BLANK        prmRpyOpCode
014100090518     C                   PARM      *ZERO         prmRpyIdMsg
014200090518     C                   PARM      'TIBSSOCI0'   prmRqsFormato
014300090518     C                   PARM                    tibsSocI0
014400090518     C                   PARM                    prmRqsDataSize
014500090518     C                   PARM      'TIBSSOCO0'   prmRpyFormato
014600090518     C                   PARM                    tibsSocO0
014700090518     C                   PARM                    prmRpyDataSize
014800090518     c                   if         PRMRPYIDMSG >= 0 and
014900090518     c                              TIBSSOCO0.IDSOCIETA <> *blank
015000151103     c                   eval      t88socgd = tibsSocO0.RAGSOCIALE
015100090518     c                   else
015200151103     c                   eval      t88socgd = 'Società Errata'
015300090518     c                   end
015400150527      *
015500090518      * ragione sociale società
015600151103     c                   eval      t88note ='Non esistono autisti accredita ti'
015700151103     c     kata          setll     aiata01l
015800090924     c                   do        *hival
015900151103     c     kata          reade     aiata01l
016000151103     c                   if        %eof(aiata01l)
016100090924     c                   leave
016200090924     c                   endif
016201151103     c                   if        ataatb <> ' '
016202151103     c                   iter
016203151103     c                   endif
016500151103     c                   clear                   t88note
016600090924     c                   leave
016800090924     c                   enddo
016900150527      *
017000151103      * se la società risulta senza autisti non la scriviamo
017100151103     c                   if        t88note ='Non esistono autisti accreditati'
017200130304     c                   iter
017300130304     c                   endif
019000151103     c                   z-add     atsNRC        T88NRC
019300151103     c                   move      atspiva       T88IVA
019301151103     c                   movel     atsrags       T88rsoc
019400151103     c                   z-add     atsdec        T88DEC
019500151103     c                   z-add     atsdfc        T88DFC
019501151103     c                   z-add     atsdtinc      T88inc
019502151103     c                   z-add     atsdtfic      T88fic
019600151103     c                   move      atsDIA        T88DIA
019601151103     c                   move      atsnIA        T88nIA
019602151103     c                   move      atspIA        T88pIA
025200150527      *
025300151103     c                   write     tntl8800
025800090518     c                   enddo
025900070530      *
026000070530     c                   endsr
036100150612     c*-------------------------------------------------
036200150612     c     comandi       begsr
036300150612     c*-------------------------------------------------
036400090519     C                   z-add     99            luncmd
036500090519     C                   CALL      'QCMDEXC'                            34
036600090519     C                   parm                    coman
036700090519     C                   parm                    luncmd
036800090519     c                   endsr
036900150309     c*-------------------------------------------------
037000150309     c     *inzsr        begsr
037100150309     c*-------------------------------------------------
037200020304     c     *entry        plist
037300020304     c                   parm                    kpjba
037400150306     C*
037500150306     C                   time                    w0120            14 0
037600150306     C                   move      w0120         wdat              8 0
037700150309     c                   move      wdat          dtaeur
037800150309     c                   move      dtaeur        dtaiso
037900150309     c                   move      dtaiso        wdat
038000150605     c                   move      dtaiso        oggi_iso
038100150612     c                   move      wdat          oggi              8 0
038200150306     c*
038300150306     c                   if        kpjbu = ' '
038400151103     c                   clear                   tntl88ds
038500151103     c                   eval      i88tipoel = 'F'
038600151103     c                   eval      i88datal = wdat
038700151103     c                   eval      i88mese = 'N'
038800150306     c                   else
038900151103     c                   movel     kpjbu         tntl88ds
039000150309     c                   end
039100151103     c                   if        i88datal > 0
039200151103     c                   move      i88datal      inimese           8 0
039300150309     c                   move      01            inimese
039400150309     c                   else
039500150309     c                   clear                   inimese
039600150309     c                   end
039700050801
039800151103     c     kata          klist
040000151103     c                   kfld                    ATsSOC
040001151103     c                   kfld                    ATsKSC
040900050801
041000030630     C                   CLEAR                   Tibs36Ds
041100030630     C                   EVAL      I36ute = knmus
041200030630     C                   CALL      'TIBS36R'
041300030630     C                   PARM                    tibs36ds
041400150309     C* INIZIALIZZA VARIABILI DI WRK
041500150309     C                   CLEAR                   TIBS34DS
041600150309     C                   CLEAR                   AZUTEDS
041700150309     C                   CLEAR                   DDATIUTE
041800150309     C*
041900150309     C     *DTAARA       DEFINE    §azute        azuteds
042000150309     C     *DTAARA       DEFINE    §datiute      ddatiute
042100150309     C                   IN(E)     *DTAARA
042200150309     C                   IF        %Error
042300150309     C                   EVAL      I34Tla = 'L'
042400150309     C                   CALL      'TIBS34R'
042500150309     C                   PARM                    Tibs34Ds
042600150309     C                   IN        *DTAARA
042700150309     C                   ENDIF
042800150309     C*
042900150309     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
043000150309     C                   MOVEL(P)  RSUT          DSFIRS           20
043100090518     ** Inizializzo il programma. per dati societari
043200090518     C                   CALL      'TIBSSOCR'
043300090518     C                   PARM      'INIT'        prmRqsOpCode
043400090518     C                   PARM                    prmRpyOpCode
043500090518     C                   PARM                    prmRpyIdMsg
043600090518
043700090519      *clear file
043800090519     c                   movea     cmd1          coman
043900090519     c                   exsr      comandi
044200151103     c                   open      wftntl880f
044300090519
044400090518     c                   endsr
044500090519**  CMD1
044600151103CLRPFM    FILE(wftntl880f)
