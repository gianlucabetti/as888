000100150527     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('PJXBND')
000200150309     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000201160223      *
000202160223      * il programma produce 2 file il primo TRMZ851F lavora in QTEMP e viene duplicato in WRKQUERY
000203160223      * È IL FILE ESTRATTO
000204160223      * il secondo file è il TRMZ851F0 lavora in QTEMP e viene duplicato in WRKQUERY è l'estensione
000205160223      * del primo e contiene le anagrafiche contabili dei fornitori in coda
000300000000     H*--------------------------------------------------------------*
000400090518     Faitrs01l  if   e           k disk
000500090518     Faitra01l  if   e           k disk
000600150611     Faitrc01l  if   e           k disk
000700090518     Fazorg01l  if   e           k disk
000800150527     Ftrmz851f  uf a e           k disk    usropn
000900050801     ftabel00f  if   e           k disk
001000150527     D*-------------
001100150527     D xatbds        e ds                  inz
001200150527     D angg16ds      e ds                  inz
001300150612     D dtrafiller    e ds
001400150527     D*-------------
001500020304     dkpjba          e ds
001600030630     d Tibs36Ds      e ds
001700150309     D TIBS34DS      E DS                                                       *Profili utente
001800150309     D DDATIUTE      E DS                                                       *Dati utente
001900150309     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
002000150527     d TRMZ851DS     e ds
002100090518     d daip          e ds
002200150309     d dtrsflr1      e ds
002300150309     d dtaiso          s               d   datfmt(*iso)
002400150309     d dtaeur          s               d   datfmt(*eur)
002500150605     d oggi_iso        s               d   datfmt(*iso)
002600150611     D WrkSqlCmd       S           1024
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
005800090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
005900090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
006000090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
006100090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
006200000000     C*---------------------------------------------------------------*
006300020304      *
006400050623     c                   exsr      elabora
006500150527      *
006600090518     ** Chiudo il programma.x dati societari
006700090518     C                   CALL      'TIBSSOCR'
006800090518     C                   PARM      'FINALIZE'    prmRqsOpCode
006900090518     C                   PARM                    prmRpyOpCode
007000090518     C                   PARM                    prmRpyIdMsg
007100150309     c* stampa
007200150527     c                   if        i851tipoel ='S'
007300150527     c***********        exsr      srstampa
007400150309     c                   end
007500150527      *
007600090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
007700150527     c                   if        i851tipoel ='F'
007800090519     c                   movea     cmd4          coman
007900090519     c                   exsr      comandi
008000090519      *esegue crtdupobj da qtemp a wrkquery
008100090519     c                   movea     cmd5          coman
008200090519     c                   exsr      comandi
008300150309     c                   end
008400150527      *
008500090519      *elimina file in qtemp x la filiale
008600150527     c                   close     trmz851f
008601160222      *
008602160222     c                   call      'TRMZ851R0'
008603160222     c                   parm                    kpjba
008800150527      *
008801160222     c                   movea     cmd3          coman
008900090519     c                   exsr      comandi
009300151111      *
009400020308     c                   seton                                        lr
009500050802     c*-------------------------------------------------
009600090518     c     elabora       begsr
009700050802     c*-------------------------------------------------
009800090518     c     *loval        setll     aitrs01l
009900150527      *
010000070530     c                   do        *hival
010100090518     c                   read      aitrs01l                               98
010200070530     c   98              leave
010300150527     c                   clear                   trmz8510
010400150527      *
010500090518      * filtro se cerca società in forza alla data
010600120413     c                   if        trsann <> *blank
010700120413     c                   iter
010800120413     c                   endif
010900150527      *
011000150309      *considera anche quelle disaccreditate nel mese
011100150527     c                   if        i851datal > 0
011200150527     c                   if        i851mese = 'S'
011300150527      *
011400090518     c                   if        trsdfc > 0 and trsdfc < inimese
011500070530     c                   iter
011600070530     c                   endif
011700150527      *
011800130123     c                   else
011900130123      *le esclude
012000130123     c                   if        trsdfc > 0
012100130123     c                   iter
012200130123     c                   endif
012300150527      *
012400130123     c                   endif
012500090518     c                   endif
012600150527      *
012700090518      * filtro se cerca società in forza alla data
012800150527     c                   if        i851datdal > 0
012900150527     c                   if        trsdfc > 0 or trsdec < i851datdal
013000090518     c                   iter
013100090518     c                   endif
013200090518     c                   endif
013300150527      *
013400150309     c*
013500150527     c                   clear                   t851socgd
013600090518     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
013700090518     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
013800090518     C                   RESET                   tibsSocI0
013900090518     C                   EVAL      tibsSocI0.idSocieta = trssocg
014000090518     C
014100090518     C                   CALL      'TIBSSOCR'
014200090518     C                   PARM      'GETANAGRAF'  prmRqsOpCode
014300090518     C                   PARM      *BLANK        prmRpyOpCode
014400090518     C                   PARM      *ZERO         prmRpyIdMsg
014500090518     C                   PARM      'TIBSSOCI0'   prmRqsFormato
014600090518     C                   PARM                    tibsSocI0
014700090518     C                   PARM                    prmRqsDataSize
014800090518     C                   PARM      'TIBSSOCO0'   prmRpyFormato
014900090518     C                   PARM                    tibsSocO0
015000090518     C                   PARM                    prmRpyDataSize
015100090518     c                   if         PRMRPYIDMSG >= 0 and
015200090518     c                              TIBSSOCO0.IDSOCIETA <> *blank
015300150527     c                   eval      t851socgd = tibsSocO0.RAGSOCIALE
015400090518     c                   else
015500150527     c                   eval      t851socgd = 'Società Errata'
015600090518     c                   end
015700150527      *
015800090518      * ragione sociale società
015900150527     c                   eval      t851note ='Non esistono autisti accreditati'
016000090924     c     trsnrc        setll     aitra01l
016100090924     c                   do        *hival
016200090924     c     trsnrc        reade     aitra01l
016300090924     c                   if        %eof(aitra01l)
016400090924     c                   leave
016500090924     c                   endif
016600150527     c                   movel     trasoc        t851rsoc
016700090924     c                   if        tradfi = 0
016800150527     c                   clear                   t851note
016900090924     c                   leave
017000090518     c                   endif
017100090924     c                   enddo
017200150527      *
017300130304      * se la società risulta senza autisti non la scriviamo più chiesto Stefano Bartolini 04/03/13
017400150527     c                   if        t851note ='Non esistono autisti accreditati'
017500130304     c                   iter
017600130304     c                   endif
017700150527      *
017800090518      * descrizione filiale
017900090518     c     trsfil        chain     azorg01l
018000090518     c                   if        %found(azorg01l)
018100150527     c                   movel     orgdes        t851fild
018200150527     c                   move      orgfl3        T851DIST
018300150527     c                   move      orgcar        T851AREA
018400090518     c                   endif
018500150527      *
018600070530      * descrizione area
018700090518     c                   movel     orgcar        kkey
018800070530     c                   move      '05'          kcod
018900070530     c     ktab          chain     tabel00f
019000070530     c                   if        %found(tabel00f)
019100150527     c                   movel     tbluni        t851aread
019200070530     c                   endif
019300150527     c                   z-add     TrsNRC        T851NRC
019400150527     c                   z-add     TrsFIL        T851FIL
019500150527     c                   move      TrsSOCG       T851SOCG
019600150527     c                   move      TrsIVA        T851IVA
019700150527     c                   z-add     TrsDEC        T851DEC
019800150527     c                   z-add     TrsDFC        T851DFC
019900150527     c                   move      TrsDIA        T851DIA
019901160222     c                   move      Trscor        T851cor
020000150527     c                   move      Trsiva        T851IPIVA
020100150923     c                   movel     Trsflr1       dtrsflr1
020200150309     c                   if        §trsddurc <> ' ' AND
020300150309     c                             §trsddurc <> '00000000'
020400150527     c                   move      §TRSDDURC     t851ddurc
020500150309     c                   else
020600150527     c                   clear                   t851ddurc
020700150309     c                   end
020800150527      *
020900150309     c                   if        §trsdcip <> ' ' and
021000150309     c                             §trsdcip <> '00000000'
021100150527     c                   move      §TRSDCIP      t851dcip
021200150309     c                   else
021300150527     c                   clear                   t851dcip
021400150309     c                   end
021500150527      *
021600150527      *  la Natura Giuridica
021700150527     c                   clear                   naturas           2
021800150527     C/EXEC SQL
021900150527     c+ SELECT SOGNATGIUR into :naturas
022000150630     c+ FROM ansog00f join anrco00f ON RCOSOGG = SOGSOGG
022100150630     c+ WHERE RCOSNATURA = 'F'
022200150630     c+   and SOGPARTIVA = :TrsIVA and
022300150605     c+  (SOGDTFIVL = '0001-01-01' or SOGDTFIVL >= :oggi_iso)
022400150605     c+   and SOGDTINVL <= :oggi_iso
022500150605     c+  and sogsogg in
022600150605     c+ (SELECT frnsogg  FROM anfrn00f
022700150605     c+  WHERE frnsocieta = :TrsSOCG and
022800150605     c+   (FRNDTFIRPF = '0001-01-01' or FRNDTFIRPF >= :oggi_iso)
022900150605     c+    and FRNDTINRPF <= :oggi_iso )
023000150630     c+ ORDER BY RCODTIMM desc
023100150605     c+ FETCH FIRST 1 ROWS ONLY
023200150527     C/END-EXEC
023300150527     c                   if        sqlcod = 0
023400150527     c                   eval      T851NATG = naturas
023500150527      *-
023600150527     C                   reset                   xatbds
023700150527     C                   eval      xtbcod = 'G16'
023800150527     C                   eval      xtbkey = *zeros
023900150527     C                   move      naturas       xtbkey
024000150527     C*
024100150527     C                   eval      xtbazi = trssocg
024200150527     C                   eval      xtbric = '1'
024300150527     C                   callb     'XATB'
024400150527     C                   parm                    xatbds
024500150527     C*
024600150527     C                   if        xtberr = '1'
024700150527     C                   reset                   angg16ds
024800150527     C                   else
024900150527     C                   eval      angg16ds  = xtbuni
025000150527     C                   eval      T851NATGD = desg16
025100150527     C                   endif
025200150527     C*
025300150527     C* -----------------
025400150527     c                   end
025500150527      *
025600150527     c                   write     trmz8510
025700150611      *  se ci sono:
025800150611      *  scrive le consorziate    NATURA GIURIDICA =11
025900150611     c                   exsr      WRTconsorziate
026000150611      *
026100090518     c                   enddo
026200070530      *
026300070530     c                   endsr
026400150611      *-------------------------------------------------
026500150611     c     WRTconsorziatebegsr
026600150611      *-------------------------------------------------
026700150611      * imposta le Consorziate
026800150615     c                   clear                   almeno_uno        1
026900150611     C                   EVAL      WrkSqlCmd
027000150611     C                             =
027100150611     C                             'SELECT trafiller FROM aitra00f '
027200150612     c                              + ' where TRANRC = ' + %editc(Trsnrc:'Z')
027300150611     c                              + ' and tradfi=0 '
027400150615     c                              + ' and trasco<>''S'' '
027500150611     c                              + ' group by trafiller '
027600150611     c                              + ' order by trafiller '
027700150611     C/EXEC SQL
027800150611     C+ PREPARE S1 FROM :WrkSqlCmd
027900150611     C/END-EXEC
028000150611
028100150611     C/EXEC SQL
028200150611     C+ DECLARE A1 CURSOR FOR S1
028300150611     C/END-EXEC
028400150611
028500150611     C/EXEC SQL
028600150611     C+ OPEN A1
028700150611     C/END-EXEC
028800150611     C                   DOU       SqlCod <> 0
028900150611     c*
029000150611     c                   clear                   filler20         20
029100150611     C/EXEC SQL
029200150611     C+ FETCH NEXT FROM A1 INTO :FILLER20
029300150611     C/END-EXEC
029400150611     C                   SELECT
029500150611     c*
029600150611     c* a fine file Totali x rotture
029700150611     C                   WHEN      SqlCod = 100
029800150615     **
029900150611     c                   leave
030000150611     **
030100150611     C                   WHEN      SqlCod < 0
030200150611     C                   seton                                        H1
030300150612     c                   RETURN
030400150611     c*
030500150611     C                   OTHER
030600150611     c* DETTAGLIO
030700150615     c                   movel     'S'           almeno_uno        1
030800150612     c                   movel     filler20      dtrafiller
030900150612     c                   exsr      WRI_AITRC
031000150615      * x un'anomalia deve stampare una sola riga
031100150615     c                   if          uscita_ciclo = 'S'
031200150615     c                   leave
031300150615     c                   end
031400150611      *
031500150611     C                   ENDSL
031600150611
031700150611     C                   ENDDO
031800150611     C/EXEC SQL
031900150611     C+ CLOSE A1
032000150611     C/END-EXEC
032100150611      *
032200150611     c                   endsr
032300150611     c*-------------------------------------------------
032400150612     c     WRI_AITRC     begsr
032500150611     c*-------------------------------------------------
032600150612      *
032700150615     c                   clear                   uscita_ciclo      1
032800150612     c                   movel     TrsIVA        TRCIVA
032900150612     c                   movel     §TRAIVAC      TRCPICF
033000150612     c     ktrc          chain     aitrc01l
033100150612     c                   if        %Found(aitrc01l) and
033200150612     c                               TRCDFC >= oggi
033300150612      *
033400150612     c                   z-add     trcDDURC      t851dDURC
033600150615     c                   movel     §traIVAC      T851IVACOP
033700150615     c                   movel     trcRAGSOC     t851RSOCOP
033800150615     c                   movel     'C'           t851TIPO
033900150615     c                   if        naturas <> '11'
034000150615     c                   eval      t851NOTE = 'Nat.Giuridica diversa da Consor'
034100150615     c                             + 'zio.'
034200150615     c                   movel     'S'           uscita_ciclo
034300150615     c                   end
034400150612     c                   write     trmz8510
034500150615     **
034600150615     c                   else
034700150615     **
034800150615     ** se NATURA GIUR. Consorzio e non c'è nulla
034900150615     c                   if        naturas = '11'
035000150615     c                   z-add     0             t851dDURC
035100150615     c                   z-add     0             t851dCIP
035200150615     c                   movel     *blank        t851RSOCOP
035300150615     c                   movel     *blank        t851IVACOP
035400150615     c                   movel     'C'           t851TIPO
035500150615     c                   eval      t851NOTE = 'Nat.Giuridica Consorzio senza'
035600150615     c                             + ' anagrafica.'
035700150615     c                   movel     'S'           uscita_ciclo
035800150615     c                   write     trmz8510
035900150615     c                   end
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
