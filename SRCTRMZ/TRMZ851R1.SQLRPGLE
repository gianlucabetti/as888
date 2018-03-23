000100150527     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('PJXBND')
000200150309     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000300160223      *
000400160223      * il programma produce 2 file il primo TRMZ851F lavora in QTEMP e viene duplicato in WRKQUERY
000500160223      * È IL FILE ESTRATTO
000600160223      * il secondo file è il TRMZ851F0 lavora in QTEMP e viene duplicato in WRKQUERY è l'estensione
000700160223      * del primo e contiene le anagrafiche contabili dei fornitori in coda
000800000000     H*--------------------------------------------------------------*
000900090518     Faitrs01l  if   e           k disk
001000090518     Faitra01l  if   e           k disk
001100150611     Faitrc01l  if   e           k disk
001200090518     Fazorg01l  if   e           k disk
001300150527     Ftrmz851f  uf a e           k disk    usropn
001400050801     ftabel00f  if   e           k disk
001500150527     D*-------------
001600150527     D xatbds        e ds                  inz
001700150527     D angg16ds      e ds                  inz
001800150612     D dtrafiller    e ds
001900150527     D*-------------
002000020304     dkpjba          e ds
002100030630     d Tibs36Ds      e ds
002200150309     D TIBS34DS      E DS                                                       *Profili utente
002300150309     D DDATIUTE      E DS                                                       *Dati utente
002400150309     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
002500150527     d TRMZ851DS     e ds
002600090518     d daip          e ds
002700150309     d dtrsflr1      e ds
002800150309     d dtaiso          s               d   datfmt(*iso)
002900150309     d dtaeur          s               d   datfmt(*eur)
003000150605     d oggi_iso        s               d   datfmt(*iso)
003100150611     D WrkSqlCmd       S           1024
003200150309     d COM08           s              8  0
003300070605     D WLBDA8          DS
003400070605     D  G02DAT                 1      8  0
003500070605     D  G02INV                 9     16  0
003600070605     D  G02ERR                17     17
003700070605     D  G02TGI                18     22  0
003800090518     D tibsSocI0...
003900090518     D               E DS                  QUALIFIED
004000090518     D                                     INZ
004100090518     D tibsSocO0...
004200090518     D               E DS                  QUALIFIED
004300090518     D                                     INZ
004400090518
004500090518     D prmRqsOpCode...
004600090518     D                 S             10A
004700090518     D prmRpyOpCode...
004800090518     D                 S             10A
004900090518     D prmRpyIdMsg...
005000090518     D                 S             10I 0
005100090518     D prmRqsFormato...
005200090518     D                 S             10A
005300090518     D prmRqsDataSize...
005400090518     D                 S             10I 0
005500090518     D prmRpyFormato...
005600090518     D                 S             10A
005700090518     D prmRpyDataSize...
005800090518     D                 S             10I 0
005900080502      *
006000090519     d coman           s             99
006100090519     d luncmd          s             15  5
006200090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
006300090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
006400090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
006500090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
006600090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
006700000000     C*---------------------------------------------------------------*
006800020304      *
006900050623     c                   exsr      elabora
007000150527      *
007100090518     ** Chiudo il programma.x dati societari
007200090518     C                   CALL      'TIBSSOCR'
007300090518     C                   PARM      'FINALIZE'    prmRqsOpCode
007400090518     C                   PARM                    prmRpyOpCode
007500090518     C                   PARM                    prmRpyIdMsg
007600150309     c* stampa
007700150527     c                   if        i851tipoel ='S'
007800150527     c***********        exsr      srstampa
007900150309     c                   end
008000150527      *
008100090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
008200150527     c                   if        i851tipoel ='F'
008300090519     c                   movea     cmd4          coman
008400090519     c                   exsr      comandi
008500090519      *esegue crtdupobj da qtemp a wrkquery
008600090519     c                   movea     cmd5          coman
008700090519     c                   exsr      comandi
008800150309     c                   end
008900150527      *
009000090519      *elimina file in qtemp x la filiale
009100150527     c                   close     trmz851f
009200160222      *
009300160222     c                   call      'TRMZ851R0'
009400160222     c                   parm                    kpjba
009500150527      *
009600160222     c                   movea     cmd3          coman
009700090519     c                   exsr      comandi
009800151111      *
009900020308     c                   seton                                        lr
010000050802     c*-------------------------------------------------
010100090518     c     elabora       begsr
010200050802     c*-------------------------------------------------
010300090518     c     *loval        setll     aitrs01l
010400150527      *
010500070530     c                   do        *hival
010600090518     c                   read      aitrs01l                               98
010700070530     c   98              leave
010800150527     c                   clear                   trmz8510
010900150527      *
011000090518      * filtro se cerca società in forza alla data
011100120413     c                   if        trsann <> *blank
011200120413     c                   iter
011300120413     c                   endif
011400150527      *
011500150309      *considera anche quelle disaccreditate nel mese
011600150527     c                   if        i851datal > 0
011700150527     c                   if        i851mese = 'S'
011800150527      *
011900090518     c                   if        trsdfc > 0 and trsdfc < inimese
012000070530     c                   iter
012100070530     c                   endif
012200150527      *
012300130123     c                   else
012400130123      *le esclude
012500130123     c                   if        trsdfc > 0
012600130123     c                   iter
012700130123     c                   endif
012800150527      *
012900130123     c                   endif
013000090518     c                   endif
013100150527      *
013200090518      * filtro se cerca società in forza alla data
013300150527     c                   if        i851datdal > 0
013400150527     c                   if        trsdfc > 0 or trsdec < i851datdal
013500090518     c                   iter
013600090518     c                   endif
013700090518     c                   endif
013800150527      *
013900150309     c*
014000150527     c                   clear                   t851socgd
014100090518     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
014200090518     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
014300090518     C                   RESET                   tibsSocI0
014400090518     C                   EVAL      tibsSocI0.idSocieta = trssocg
014500090518     C
014600090518     C                   CALL      'TIBSSOCR'
014700090518     C                   PARM      'GETANAGRAF'  prmRqsOpCode
014800090518     C                   PARM      *BLANK        prmRpyOpCode
014900090518     C                   PARM      *ZERO         prmRpyIdMsg
015000090518     C                   PARM      'TIBSSOCI0'   prmRqsFormato
015100090518     C                   PARM                    tibsSocI0
015200090518     C                   PARM                    prmRqsDataSize
015300090518     C                   PARM      'TIBSSOCO0'   prmRpyFormato
015400090518     C                   PARM                    tibsSocO0
015500090518     C                   PARM                    prmRpyDataSize
015600090518     c                   if         PRMRPYIDMSG >= 0 and
015700090518     c                              TIBSSOCO0.IDSOCIETA <> *blank
015800150527     c                   eval      t851socgd = tibsSocO0.RAGSOCIALE
015900090518     c                   else
016000150527     c                   eval      t851socgd = 'Società Errata'
016100090518     c                   end
016200150527      *
016300090518      * ragione sociale società
016400150527     c                   eval      t851note ='Non esistono autisti accreditati'
016500090924     c     trsnrc        setll     aitra01l
016600090924     c                   do        *hival
016700090924     c     trsnrc        reade     aitra01l
016800090924     c                   if        %eof(aitra01l)
016900090924     c                   leave
017000090924     c                   endif
017100150527     c                   movel     trasoc        t851rsoc
017200090924     c                   if        tradfi = 0
017300150527     c                   clear                   t851note
017400090924     c                   leave
017500090518     c                   endif
017600090924     c                   enddo
017700150527      *
017800130304      * se la società risulta senza autisti non la scriviamo più chiesto Stefano Bartolini 04/03/13
017900150527     c                   if        t851note ='Non esistono autisti accreditati'
018000130304     c                   iter
018100130304     c                   endif
018200150527      *
018300090518      * descrizione filiale
018400090518     c     trsfil        chain     azorg01l
018500090518     c                   if        %found(azorg01l)
018600150527     c                   movel     orgdes        t851fild
018700150527     c                   move      orgfl3        T851DIST
018800150527     c                   move      orgcar        T851AREA
018900090518     c                   endif
019000150527      *
019100070530      * descrizione area
019200090518     c                   movel     orgcar        kkey
019300070530     c                   move      '05'          kcod
019400070530     c     ktab          chain     tabel00f
019500070530     c                   if        %found(tabel00f)
019600150527     c                   movel     tbluni        t851aread
019700070530     c                   endif
019800150527     c                   z-add     TrsNRC        T851NRC
019900150527     c                   z-add     TrsFIL        T851FIL
020000150527     c                   move      TrsSOCG       T851SOCG
020100150527     c                   move      TrsIVA        T851IVA
020200150527     c                   z-add     TrsDEC        T851DEC
020300150527     c                   z-add     TrsDFC        T851DFC
020400150527     c                   move      TrsDIA        T851DIA
020500160222     c                   move      Trscor        T851cor
020600150527     c                   move      Trsiva        T851IPIVA
020700150923     c                   movel     Trsflr1       dtrsflr1
020800150309     c                   if        §trsddurc <> ' ' AND
020900150309     c                             §trsddurc <> '00000000'
021000150527     c                   move      §TRSDDURC     t851ddurc
021100150309     c                   else
021200150527     c                   clear                   t851ddurc
021300150309     c                   end
021400150527      *
021500150309     c                   if        §trsdcip <> ' ' and
021600150309     c                             §trsdcip <> '00000000'
021700150527     c                   move      §TRSDCIP      t851dcip
021800150309     c                   else
021900150527     c                   clear                   t851dcip
022000150309     c                   end
022100150527      *
022200150527      *  la Natura Giuridica
022300150527     c                   clear                   naturas           2
022400150527     C/EXEC SQL
022500150527     c+ SELECT SOGNATGIUR into :naturas
022600150630     c+ FROM ansog00f join anrco00f ON RCOSOGG = SOGSOGG
022700150630     c+ WHERE RCOSNATURA = 'F'
022800150630     c+   and SOGPARTIVA = :TrsIVA and
022900150605     c+  (SOGDTFIVL = '0001-01-01' or SOGDTFIVL >= :oggi_iso)
023000150605     c+   and SOGDTINVL <= :oggi_iso
023100150605     c+  and sogsogg in
023200150605     c+ (SELECT frnsogg  FROM anfrn00f
023300150605     c+  WHERE frnsocieta = :TrsSOCG and
023400150605     c+   (FRNDTFIRPF = '0001-01-01' or FRNDTFIRPF >= :oggi_iso)
023500150605     c+    and FRNDTINRPF <= :oggi_iso )
023600150630     c+ ORDER BY RCODTIMM desc
023700150605     c+ FETCH FIRST 1 ROWS ONLY
023800150527     C/END-EXEC
023900150527     c                   if        sqlcod = 0
024000150527     c                   eval      T851NATG = naturas
024100150527      *-
024200150527     C                   reset                   xatbds
024300150527     C                   eval      xtbcod = 'G16'
024400150527     C                   eval      xtbkey = *zeros
024500150527     C                   move      naturas       xtbkey
024600150527     C*
024700150527     C                   eval      xtbazi = trssocg
024800150527     C                   eval      xtbric = '1'
024900150527     C                   callb     'XATB'
025000150527     C                   parm                    xatbds
025100150527     C*
025200150527     C                   if        xtberr = '1'
025300150527     C                   reset                   angg16ds
025400150527     C                   else
025500150527     C                   eval      angg16ds  = xtbuni
025600150527     C                   eval      T851NATGD = desg16
025700150527     C                   endif
025800150527     C*
025900150527     C* -----------------
026000150527     c                   end
026100150527      *
026200150527     c                   write     trmz8510
026300150611      *  se ci sono:
026400150611      *  scrive le consorziate    NATURA GIURIDICA =11
026500150611     c                   exsr      WRTconsorziate
026600150611      *
026700090518     c                   enddo
026800070530      *
026900070530     c                   endsr
027000150611      *-------------------------------------------------
027100150611     c     WRTconsorziatebegsr
027200150611      *-------------------------------------------------
027300150611      * imposta le Consorziate
027400150615     c                   clear                   almeno_uno        1
027500150611     C                   EVAL      WrkSqlCmd
027600150611     C                             =
027700150611     C                             'SELECT trafiller FROM aitra00f '
027800150612     c                              + ' where TRANRC = ' + %editc(Trsnrc:'Z')
027900150611     c                              + ' and tradfi=0 '
028000150615     c                              + ' and trasco<>''S'' '
028100150611     c                              + ' group by trafiller '
028200150611     c                              + ' order by trafiller '
028300150611     C/EXEC SQL
028400150611     C+ PREPARE S1 FROM :WrkSqlCmd
028500150611     C/END-EXEC
028600150611
028700150611     C/EXEC SQL
028800150611     C+ DECLARE A1 CURSOR FOR S1
028900150611     C/END-EXEC
029000150611
029100150611     C/EXEC SQL
029200150611     C+ OPEN A1
029300150611     C/END-EXEC
029400150611     C                   DOU       SqlCod <> 0
029500150611     c*
029600150611     c                   clear                   filler20         20
029700150611     C/EXEC SQL
029800150611     C+ FETCH NEXT FROM A1 INTO :FILLER20
029900150611     C/END-EXEC
030000150611     C                   SELECT
030100150611     c*
030200150611     c* a fine file Totali x rotture
030300150611     C                   WHEN      SqlCod = 100
030400150615     **
030500150611     c                   leave
030600150611     **
030700150611     C                   WHEN      SqlCod < 0
030800150611     C                   seton                                        H1
030900150612     c                   RETURN
031000150611     c*
031100150611     C                   OTHER
031200150611     c* DETTAGLIO
031300150615     c                   movel     'S'           almeno_uno        1
031400150612     c                   movel     filler20      dtrafiller
031500150612     c                   exsr      WRI_AITRC
031600150615      * x un'anomalia deve stampare una sola riga
031700150615     c                   if          uscita_ciclo = 'S'
031800150615     c                   leave
031900150615     c                   end
032000150611      *
032100150611     C                   ENDSL
032200150611
032300150611     C                   ENDDO
032400150611     C/EXEC SQL
032500150611     C+ CLOSE A1
032600150611     C/END-EXEC
032700150611      *
032800150611     c                   endsr
032900150611     c*-------------------------------------------------
033000150612     c     WRI_AITRC     begsr
033100150611     c*-------------------------------------------------
033200150612      *
033300150615     c                   clear                   uscita_ciclo      1
033400150612     c                   movel     TrsIVA        TRCIVA
033500150612     c                   movel     §TRAIVAC      TRCPICF
033600150612     c     ktrc          chain     aitrc01l
033700150612     c                   if        %Found(aitrc01l) and
033800150612     c                               TRCDFC >= oggi
033900150612      *
034000150612     c                   z-add     trcDDURC      t851dDURC
034100150615     c                   movel     §traIVAC      T851IVACOP
034200150615     c                   movel     trcRAGSOC     t851RSOCOP
034300150615     c                   movel     'C'           t851TIPO
034400160407     c                   if        naturas <> '11'
034500160407     c                   eval      t851NOTE = 'Nat.Giuridica diversa da Consor'
034600160407     c                             + 'zio.'
034700160407     c*m                 movel     'S'           uscita_ciclo
034800160407     c                   end
034900150612     c                   write     trmz8510
035000150615     **
035100150615     c                   else
035200150615     **
035300150615     ** se NATURA GIUR. Consorzio e non c'è nulla
035400160407     c                   if        naturas = '11'
035500160407     c                   eval      t851NOTE = 'Nat.Giuridica Consorzio senza'
035600150615     c                             + ' anagrafica.'
035700160407     c*m                 movel     'S'           uscita_ciclo
035800160407     c                   write     trmz8510
035900160407     c                   end
036000150612      *
036100150615     c                   end
036200150615      *
036300150612     c                   endsr
036400150612     c*-------------------------------------------------
036500150612     c     comandi       begsr
036600150612     c*-------------------------------------------------
036700090519     C                   z-add     99            luncmd
036800090519     C                   CALL      'QCMDEXC'                            34
036900090519     C                   parm                    coman
037000090519     C                   parm                    luncmd
037100090519     c                   endsr
037200150309     c*-------------------------------------------------
037300150309     c     *inzsr        begsr
037400150309     c*-------------------------------------------------
037500020304     c     *entry        plist
037600020304     c                   parm                    kpjba
037700150306     C*
037800150306     C                   time                    w0120            14 0
037900150306     C                   move      w0120         wdat              8 0
038000150309     c                   move      wdat          dtaeur
038100150309     c                   move      dtaeur        dtaiso
038200150309     c                   move      dtaiso        wdat
038300150605     c                   move      dtaiso        oggi_iso
038400150612     c                   move      wdat          oggi              8 0
038500150306     c*
038600150306     c                   if        kpjbu = ' '
038700150527     c                   clear                   trmz851ds
038800150527     c                   eval      i851tipoel = 'F'
038900150527     c                   eval      i851datal = wdat
039000150527     c                   eval      i851mese = 'N'
039100150306     c                   else
039200150527     c                   movel     kpjbu         trmz851ds
039300150309     c                   end
039400150527     c                   if        i851datal > 0
039500150527     c                   move      i851datal     inimese           8 0
039600150309     c                   move      01            inimese
039700150309     c                   else
039800150309     c                   clear                   inimese
039900150309     c                   end
040000050801
040100050801     c     ktab          klist
040200050801     c                   kfld                    kkut              1 0
040300050801     c                   kfld                    kcod              2
040400050801     c                   kfld                    kkey              8
040500050801     c                   z-add     1             kkut
040600050801     c                   move      '05'          kcod
040700050801     c
040800150612
040900150612     c     ktrc          klist
041000150612     c                   kfld                    TRCIVA
041100150612     c                   kfld                    TRCPICF
041200050801
041300030630     C                   CLEAR                   Tibs36Ds
041400030630     C                   EVAL      I36ute = knmus
041500030630     C                   CALL      'TIBS36R'
041600030630     C                   PARM                    tibs36ds
041700150309     C* INIZIALIZZA VARIABILI DI WRK
041800150309     C                   CLEAR                   TIBS34DS
041900150309     C                   CLEAR                   AZUTEDS
042000150309     C                   CLEAR                   DDATIUTE
042100150309     C*
042200150309     C     *DTAARA       DEFINE    §azute        azuteds
042300150309     C     *DTAARA       DEFINE    §datiute      ddatiute
042400150309     C                   IN(E)     *DTAARA
042500150309     C                   IF        %Error
042600150309     C                   EVAL      I34Tla = 'L'
042700150309     C                   CALL      'TIBS34R'
042800150309     C                   PARM                    Tibs34Ds
042900150309     C                   IN        *DTAARA
043000150309     C                   ENDIF
043100150309     C*
043200150309     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
043300150309     C                   MOVEL(P)  RSUT          DSFIRS           20
043400090518     ** Inizializzo il programma. per dati societari
043500090518     C                   CALL      'TIBSSOCR'
043600090518     C                   PARM      'INIT'        prmRqsOpCode
043700090518     C                   PARM                    prmRpyOpCode
043800090518     C                   PARM                    prmRpyIdMsg
043900090518
044000090519      *clear file
044100090519     c                   movea     cmd1          coman
044200090519     c                   exsr      comandi
044300090519     c                   movea     cmd2          coman
044400090519     c                   exsr      comandi
044500150527     c                   open      trmz851f
044600090519
044700090518     c                   endsr
044800090519**  CMD1
044900150527CLRPFM    FILE(trmz851f)
045000090519**  CMD2
045100150527CRTDUPOBJ OBJ(trmz851F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
045200090519**  CMD3
045300150527DLTF FILE(QTEMP/trmz851F)
045400090519**  CMD4
045500150527dltf file(WRKQUERY/trmz851f)
045600090519**  CMD5
045700150527CRTDUPOBJ OBJ(trmz851F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz851f) DATA(*YES)
