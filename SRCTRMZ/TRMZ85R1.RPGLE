000100150309     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000200000000     H*--------------------------------------------------------------*
000400090518     Faitrs01l  if   e           k disk
000500090518     Faitra01l  if   e           k disk
000600090518     Fazorg01l  if   e           k disk
000700090519     Ftrmz850f  uf a e           k disk    usropn
000702150309     FTrmz85p   O    E             PRINTER OFLIND(*IN01)
000800050801     ftabel00f  if   e           k disk
000900020304     dkpjba          e ds
001000030630     d Tibs36Ds      e ds
001001150309     D TIBS34DS      E DS                                                       *Profili utente
001002150309     D DDATIUTE      E DS                                                       *Dati utente
001003150309     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001100150306     d TRMZ85DS      e ds
001200090518     d daip          e ds
001300150309     d dtrsflr1      e ds
001400150309     d dtaiso          s               d   datfmt(*iso)
001500150309     d dtaeur          s               d   datfmt(*eur)
001501150309     d COM08           s              8  0
001600070605     D WLBDA8          DS
001700070605     D  G02DAT                 1      8  0
001800070605     D  G02INV                 9     16  0
001900070605     D  G02ERR                17     17
002000070605     D  G02TGI                18     22  0
002100090518     D tibsSocI0...
002200090518     D               E DS                  QUALIFIED
002300090518     D                                     INZ
002400090518     D tibsSocO0...
002500090518     D               E DS                  QUALIFIED
002600090518     D                                     INZ
002700090518
002800090518     D prmRqsOpCode...
002900090518     D                 S             10A
003000090518     D prmRpyOpCode...
003100090518     D                 S             10A
003200090518     D prmRpyIdMsg...
003300090518     D                 S             10I 0
003400090518     D prmRqsFormato...
003500090518     D                 S             10A
003600090518     D prmRqsDataSize...
003700090518     D                 S             10I 0
003800090518     D prmRpyFormato...
003900090518     D                 S             10A
004000090518     D prmRpyDataSize...
004100090518     D                 S             10I 0
004200080502      *
004300090519     d coman           s             99
004400090519     d luncmd          s             15  5
004500090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
004600090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
004700090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
004800090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
004900090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
005000000000     C*---------------------------------------------------------------*
005100020304      *
006000050623     c                   exsr      elabora
006500090518     ** Chiudo il programma.x dati societari
006600090518     C                   CALL      'TIBSSOCR'
006700090518     C                   PARM      'FINALIZE'    prmRqsOpCode
006800090518     C                   PARM                    prmRpyOpCode
006900090518     C                   PARM                    prmRpyIdMsg
006901150309     c* stampa
006903150309     c                   if        i85tipoela ='S'
006904150309     c                   exsr      srstampa
006905150309     c                   end
007100090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
007101150309     c                   if        i85tipoela ='F'
007200090519     c                   movea     cmd4          coman
007300090519     c                   exsr      comandi
007400090519      *esegue crtdupobj da qtemp a wrkquery
007500090519     c                   movea     cmd5          coman
007600090519     c                   exsr      comandi
007601150309     c                   end
007700090519      *elimina file in qtemp x la filiale
007800090519     c                   close     trmz850f
007900090519     c                   movea     cmd3          coman
008000090519     c                   exsr      comandi
008100020308     c                   seton                                        lr
008200050802     c*-------------------------------------------------
008300090518     c     elabora       begsr
008400050802     c*-------------------------------------------------
008500090518     c     *loval        setll     aitrs01l
008600070530     c                   do        *hival
008700090518     c                   read      aitrs01l                               98
008800070530     c   98              leave
008900090518     c                   clear                   trmz8500
009000090518      * filtro se cerca società in forza alla data
009100120413     c                   if        trsann <> *blank
009200120413     c                   iter
009300120413     c                   endif
009301150309      *considera anche quelle disaccreditate nel mese
009400150309     c                   if        i85datal > 0
009600150309     c                   if        i85mese = 'S'
009700090518     c                   if        trsdfc > 0 and trsdfc < inimese
009800070530     c                   iter
009900070530     c                   endif
010000130123     c                   else
010100130123      *le esclude
010200130123     c                   if        trsdfc > 0
010300130123     c                   iter
010400130123     c                   endif
010500130123     c                   endif
010600090518     c                   endif
010700090518      * filtro se cerca società in forza alla data
010800150309     c                   if        i85datdal > 0
010900150309     c                   if        trsdfc > 0 or trsdec < i85datdal
011000090518     c                   iter
011100090518     c                   endif
011200090518     c                   endif
011201150309     C* filtra se durc e cip a 0
011202150309     c                   movel     Trsflr1       dtrsflr1
011203150309     c                   if        i85data0 = 'N'
011204150309     c                   if        (§trsddurc = ' ' or
011205150309     c                             §trsddurc = '00000000') and
011206150309     c                             (§trsdcip = ' ' or
011207150309     c                             §trsdcip = '00000000')
011208150309     c                   iter
011209150309     c                   endif
011210150309     c                   endif
011211150309     c* controllo i 30 gg del cip
011212150309     c                   if        (§trsddurc = ' ' or
011213150309     c                             §trsddurc = '00000000')
011214150309     c                   if        (§trsdcip <>' ' and
011215150309     c                             §trsdcip <>'00000000')
011216150309     c                   move      §trsdcip      com08
011217150309     c                   move      com08         dtaiso
011219150309     c                   adddur    30:*d         dtaiso
011220150309     c                   move      dtaiso        com08
011221150309     c                   if        com08 < wdat
011222150309     c                   iter
011223150309     c                   endif
011224150309     c                   endif
011225150309     c                   else
011226150309     c                   iter
011227150309     c                   end
011228150309     c*
011300090518     c                   clear                   t85socgd
011400090518     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
011500090518     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
011600090518     C                   RESET                   tibsSocI0
011700090518     C                   EVAL      tibsSocI0.idSocieta = trssocg
011800090518     C
011900090518     C                   CALL      'TIBSSOCR'
012000090518     C                   PARM      'GETANAGRAF'  prmRqsOpCode
012100090518     C                   PARM      *BLANK        prmRpyOpCode
012200090518     C                   PARM      *ZERO         prmRpyIdMsg
012300090518     C                   PARM      'TIBSSOCI0'   prmRqsFormato
012400090518     C                   PARM                    tibsSocI0
012500090518     C                   PARM                    prmRqsDataSize
012600090518     C                   PARM      'TIBSSOCO0'   prmRpyFormato
012700090518     C                   PARM                    tibsSocO0
012800090518     C                   PARM                    prmRpyDataSize
012900090518     c                   if         PRMRPYIDMSG >= 0 and
013000090518     c                              TIBSSOCO0.IDSOCIETA <> *blank
013100090518     c                   eval      t85socgd = tibsSocO0.RAGSOCIALE
013200090518     c                   else
013300090518     c                   eval      t85socgd = 'Società Errata'
013400090518     c                   end
013500090518      * ragione sociale società
013600090924     c                   eval      t85note = 'Non esistono autisti accreditati'
013700090924     c     trsnrc        setll     aitra01l
013800090924     c                   do        *hival
013900090924     c     trsnrc        reade     aitra01l
014000090924     c                   if        %eof(aitra01l)
014100090924     c                   leave
014200090924     c                   endif
014300090924     c                   movel     trasoc        t85rsoc
014400090924     c                   if        tradfi = 0
014500090924     c                   clear                   t85note
014600090924     c                   leave
014700090518     c                   endif
014800090924     c                   enddo
014900130304      * se la società risulta senza autisti non la scriviamo più chiesto Stefano Bartolini 04/03/13
015000130304     c                   if        t85note = 'Non esistono autisti accreditati'
015100130304     c                   iter
015200130304     c                   endif
015300090518      * descrizione filiale
015400090518     c     trsfil        chain     azorg01l
015500090518     c                   if        %found(azorg01l)
015600090518     c                   movel     orgdes        t85fild
015700090518     c                   move      orgfl3        T85DIST
015800090518     c                   move      orgcar        T85AREA
015900090518     c                   endif
016000070530      * descrizione area
016100090518     c                   movel     orgcar        kkey
016200070530     c                   move      '05'          kcod
016300070530     c     ktab          chain     tabel00f
016400070530     c                   if        %found(tabel00f)
016500090518     c                   movel     tbluni        t85aread
016600070530     c                   endif
018400090518     c                   z-add     TrsNRC        T85NRC
018500090518     c                   z-add     TrsFIL        T85FIL
018600090518     c                   move      TrsSOCG       T85SOCG
018700090518     c                   move      TrsIVA        T85IVA
018800090518     c                   z-add     TrsDEC        T85DEC
018900090518     c                   z-add     TrsDFC        T85DFC
019700090518     c                   move      TrsDIA        T85DIA
019800090518     c                   move      Trsiva        T85IPIVA
019802150309     c                   if        §trsddurc <> ' ' AND
019803150309     c                             §trsddurc <> '00000000'
019804150309     c                   move      §TRSDDURC     t85ddurc
019805150309     c                   else
019806150309     c                   clear                   t85ddurc
019807150309     c                   end
019808150309     c                   if        §trsdcip <> ' ' and
019809150309     c                             §trsdcip <> '00000000'
019810150309     c                   move      §TRSDCIP      t85dcip
019811150309     c                   else
019812150309     c                   clear                   t85dcip
019813150309     c                   end
019900090518     c                   write     trmz8500
020000090518     c                   enddo
020100070530      *
020200070530     c                   endsr
024100090519     c*-------------------------------------------------
024200090519     c     comandi       begsr
024300090519     c*-------------------------------------------------
024400090519      *
024500090519     C                   z-add     99            luncmd
024600090519     C                   CALL      'QCMDEXC'                            34
024700090519     C                   parm                    coman
024800090519     C                   parm                    luncmd
024900090519     c                   endsr
025000011026     c*-------------------------------------------------
025100150309     c     srstampa      begsr
025200011026     c*-------------------------------------------------
025202150309     C                   WRITE     TESTA
025203150309     C     *loval        SETLL     trmz850f
025204150309     c                   do        *hival
025205150309     C                   read      trmz850f
025206150309     c                   if        %eof(trmz850f)
025207150309     c                   leave
025208150309     c                   end
025209150309     C                   IF        *IN01
025210150309     C                   WRITE     TESTA
025211150309     C                   SETOFF                                       01
025212150309     C                   END
025214150309     c                   movel     t85fild       S85FILD
025215150309     c                   z-add     T85FIL        S85FIL
025216150309     c                   moveL     T85DIST       S85DIST
025217150309     c                   moveL     T85AREA       S85AREA
025223150309     c                   movel     T85aread      S85AREAD
025225150309     c                   z-add     T85NRC        S85NRC
025227150309     c                   moveL     T85SOCG       S85SOCG
025228150309     c                   moveL     T85IVA        S85IVA
025229150309     c                   if        t85dec <> 0
025230150309     c                   MOVE      T85DEC        DTAISO
025231150309     c                   MOVE      DTAISO        DTAEUR
025232150309     c                   MOVE      DTAEUR        S85DEC
025233150309     c                   else
025234150309     c                   clear                   s85dec
025235150309     c                   end
025236150309     c                   if        t85dfc <> 0
025237150309     c                   MOVE      T85DFC        DTAISO
025240150309     c                   MOVE      DTAISO        DTAEUR
025241150309     c                   MOVE      DTAEUR        S85DFC
025242150309     c                   else
025243150309     c                   clear                   s85dfc
025244150309     c                   end
025245150309     c                   if        t85dia <> 0
025246150309     c                   move      T85DIA        DTAISO
025247150309     c                   MOVE      DTAISO        DTAEUR
025248150309     c                   MOVE      DTAEUR        S85DIA
025249150309     c                   else
025250150309     c                   clear                   s85dia
025251150309     c                   end
025252150309     c                   if        t85dcip<> 0
025253150309     c                   move      t85DCIP       DTAISO
025254150309     c                   MOVE      DTAISO        DTAEUR
025255150309     c                   MOVE      DTAEUR        S85DCIP
025256150309     c                   else
025257150309     c                   clear                   s85dcip
025258150309     c                   end
025259150309     c                   MOVEL     t85socgd      S85SOCGD
025262150309     c                   movel     t85rsoc       S85RSOC
025263150309     c                   WRITE     DETTAGLIO
025264150309     c                   enddo
025265150309     C                   WRITE     FINE
025266150309     c                   endsr
025267150309     c*-------------------------------------------------
025268150309     c     *inzsr        begsr
025269150309     c*-------------------------------------------------
025300020304     c     *entry        plist
025400020304     c                   parm                    kpjba
025401150306     C*
025402150306     C                   time                    w0120            14 0
025403150306     C                   move      w0120         wdat              8 0
025404150309     c                   move      wdat          dtaeur
025405150309     c                   move      dtaeur        dtaiso
025406150309     c                   move      dtaiso        wdat
025407150306     c*
025408150306     c                   if        kpjbu = ' '
025409150306     c                   clear                   trmz85ds
025410150306     c                   eval      i85tipoela = 'S'
025411150306     c                   eval      i85datal = wdat
025412150306     c                   eval      i85mese = 'N'
025413150306     c                   eval      i85data0= 'N'
025414150306     c                   else
025415150306     c                   movel     kpjbu         trmz85ds
025416150309     c                   end
025417150309     c                   if        i85datal > 0
025418150309     c                   move      i85datal      inimese           8 0
025419150309     c                   move      01            inimese
025420150309     c                   else
025421150309     c                   clear                   inimese
025422150309     c                   end
025500050801
025600050801     c     ktab          klist
025700050801     c                   kfld                    kkut              1 0
025800050801     c                   kfld                    kcod              2
025900050801     c                   kfld                    kkey              8
026000050801     c                   z-add     1             kkut
026100050801     c                   move      '05'          kcod
026200050801     c
026300050801
026600030630     C                   CLEAR                   Tibs36Ds
026700030630     C                   EVAL      I36ute = knmus
026800030630     C                   CALL      'TIBS36R'
026900030630     C                   PARM                    tibs36ds
026904150309     C* INIZIALIZZA VARIABILI DI WRK
026905150309     C                   CLEAR                   TIBS34DS
026906150309     C                   CLEAR                   AZUTEDS
026907150309     C                   CLEAR                   DDATIUTE
026908150309     C*
026909150309     C     *DTAARA       DEFINE    §azute        azuteds
026910150309     C     *DTAARA       DEFINE    §datiute      ddatiute
026911150309     C                   IN(E)     *DTAARA
026912150309     C                   IF        %Error
026913150309     C                   EVAL      I34Tla = 'L'
026914150309     C                   CALL      'TIBS34R'
026915150309     C                   PARM                    Tibs34Ds
026916150309     C                   IN        *DTAARA
026917150309     C                   ENDIF
026918150309     C*
026919150309     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
026920150309     C                   MOVEL(P)  RSUT          DSFIRS           20
027000090518     ** Inizializzo il programma. per dati societari
027100090518     C                   CALL      'TIBSSOCR'
027200090518     C                   PARM      'INIT'        prmRqsOpCode
027300090518     C                   PARM                    prmRpyOpCode
027400090518     C                   PARM                    prmRpyIdMsg
027500090518
027600090519      *clear file
027700090519     c                   movea     cmd1          coman
027800090519     c                   exsr      comandi
027900090519     c                   movea     cmd2          coman
028000090519     c                   exsr      comandi
028100090519     c                   open      trmz850f
028200090519
028300090518     c                   endsr
028400090519**  CMD1
028500090519CLRPFM    FILE(trmz850f)
028600090519**  CMD2
028700090519CRTDUPOBJ OBJ(trmz850F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
028800090519**  CMD3
028900090519DLTF FILE(QTEMP/trmz850F)
029000090519**  CMD4
029100090519dltf file(WRKQUERY/trmz850f)
029200090519**  CMD5
029300090519CRTDUPOBJ OBJ(trmz850F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz850f) DATA(*YES)
