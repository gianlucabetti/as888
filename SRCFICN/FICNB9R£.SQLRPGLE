000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030429     H* FICND9R *----------------------------------------------------*
000300030429     H*   In Base ai gg.in tabella CCG controlla se non sono stati
000400030429     H*   eseguiti dei conteggi AUT x Autotrasportatori da Fatturare
000500030429     H*   entro i termini dovuti.
000600000000     H*--------------------------------------------------------------*
000700121113     fazorg01l  if   e           k disk
000800121113     ftntbe01l  if   e           k disk
000900121120     ftabel00f  if   e           k disk
001000030429     ffiftt04l  if   e           k disk
001100030418      *--------------------------------------------------------------*
001200121128     D MSGT            S             44    DIM(1) CTDATA PERRCD(1)              MSG PER SEDE
001300030430     D MSG             S             66    DIM(3) CTDATA PERRCD(1)              MSG PER SEDE
001400030429      *
001500030429     d dccg          e ds
001600121120     d ds05          e ds
001700030418      *
001800030409     D* PASSAGGIO DATI
001900931129     D KPJBA         E DS
002000900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002100030409     D CNCR80        E DS
002200121204     D dAPDflr       E DS
002300030429     D*
002400030429     d dataiso         s               d   datfmt(*iso)
002500030429     d dataiso2        s               d   datfmt(*iso)
002600030429     d alladata        s               d   datfmt(*iso)
002700030429     d dataymd         s               d   datfmt(*ymd)
002800030429     d datadmy         s               d   datfmt(*dmy)
002900030429     d finoal          s              8  0
003000121204     d alla_data       s              8  0
003100030429      *
003200121113      *
003300030429     d fgs             s                   like(fttfgs)
003400030429     d pdr             s                   like(fttpdr)
003500030429     d ddc             s                   like(fttddc)
003600121204     d flr             s                   like(dapdflr)
003700030429      *
003800121204     d gg_in_piu       s              2  0
003900121204      *
004000030429     d Wdata8          DS
004100030429     d  dadata                 1      8  0
004200030429     d  adata                  9     16  0
004300030429     d  GioLav                17     21  0
004400030429     D*
004500030409     D WLBDAT          DS
004600030409     D  G02DAT                 1      8  0
004700030409     D  G02INV                 9     16  0
004800030409     D  G02ERR                17     17
004900030409     D  G02TGI                18     22  0
005000030429      *
005100030409     D WGIDAT          DS
005200030409     D  GIODAT                 1      8  0
005300030409     D  GIOINV                 9     16  0
005400030409     D  GIOTGI                17     21  0
005500030429      *
005600030429     D  DSMSG          DS
005700030429     D  MSFGS                  5      7
005800030429     D  MSPDR                 42     50
005900040607     D  MSDDC                 56     63
006000030430      *
006100030430     D  DSMST          DS
006200040608     D  MTPDR1                 5      7
006300040608     D  MTPDR2                 9     12
006400040608     D  MTDDC                 17     21
006500030429      *
006600030429     d Digits          c                   Const('0123456789')
006700900521     C****************************************************************
006800900521     C*  RIEPILOGO INDICATORI
006900900521     C*****************************************************************
007000030429     c     ktbe          klist
007100030429     c                   kfld                    tbecod
007200030429     c                   kfld                    tbeke1
007300121120     C     KTAB          KLIST
007400121120     C                   KFLD                    CODUT
007500121120     C                   KFLD                    COD               2
007600121120     C                   KFLD                    KEY               8
007700030429      *--------------------------------------------------------
007800000000     C     *ENTRY        PLIST
007900000000     C                   PARM                    KPJBA
008000030429     C***
008100030429     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
008200030429     C***
008300030429     C                   TIME                    W0140            14 0
008400030429     C                   MOVEl     W0140         Wora              6 0
008500030429     C                   MOVE      W0140         WDAT              8 0
008600030429     C*
008700030429     C                   Z-ADD     Wdat          G02DAT
008800030429     C                   MOVEL     *BLANK        G02ERR
008900030429     C                   CALL      'XSRDA8'
009000030429     C                   PARM                    WLBDAT
009100030429     C* UDATE A 8 IN AAAA/MM/GG
009200030429     C                   move      G02INV        dataiso
009300030429      *
009400030429      *controllo se udate è festivo non lancio l'elaborazione
009500030429     c                   move      dataiso       dadata
009600030429     c                   move      dataiso       adata
009700030429     c                   CALL      'XSRLAV8'
009800030429     c                   PARM                    Wdata8
009900030429     c                   if        giolav = 0
010000030429     c                   seton                                        lr
010100030429     c                   endif
010200121113      *
010300121113      *-----------
010400030429      *
010500030429      *reperisco limiti da tabella per calcolare il D-Day
010600030429     c                   movel(p)  '1'           tbeke1
010700030429     c                   movel     'CCG'         tbecod
010800030429     c     ktbe          chain     tntbe01l                           90
010900030429     c                   if        %Found(tntbe01l)
011000030429     c                   move      *blank        fld2              2
011100030429     c                   eval      fld2=%Subst(tbeuni:18:2)
011200030429     c     digits        check     fld2:1
011300030429     c                   if        %Found
011400030429     c                   z-add     5             §cggca
011500030429     c                   else
011600030429     c                   movel     tbeuni        dccg
011700030429     c                   endif
011800030429     c                   endif
011900030429      *
012000030429      * calcolo il D-Day partendo da udate cercando i giorni lavorativi
012100030429      * all'indietro a seconda dei giorni impostati in tabella
012200030429     c     dataiso       subdur    1:*d          dataiso2
012300030429      *
0124000304292    c                   do        *hival
012500030429     c                   move      dataiso2      dadata
012600030429     c                   move      dataiso       adata
012700030429     c                   CALL      'XSRLAV8'
012800030429     c                   PARM                    Wdata8
012900030429      * se corrisponde ho trovato il D-Day
0130000304293    c                   if        giolav = §cggca
013100030429     c                   leave
0132000304293x   c                   else
013300030429      * altrimenti continuo a sottrarre i giorni da udate
013400030429     c     dataiso2      subdur    1:*d          dataiso2
013500030429     c                   iter
0136000304293-   c                   endif
0137000304292-   c                   enddo
013800030429      *
013900030429      * ho stabilito il D-DAY limite oltre il quale non si può andare
014000030429      *  e se ci fossero dei conteggi ancora da Confermare verranno
014100030429      *   segnalati.
014200030429     c                   move      dataiso2      dataYMD
014300030429     c                   move      dataiso2      finoal            8 0
014400030429      *-----
014500000000     C                   Z-ADD     1             CODUT
014600900615     C                   CALL      'X§PARUT'
014700900615     C                   PARM                    UT§DSE
014800000000     C                   MOVEL     RAGUT         RSUT             20
014900900615     C                   MOVEL     REC80         CNCR80
015000030429      *
015100030429      * lettura fittizia per definire i campi
015200030429     c                   read      fiftt04l
015300900511     C*---------------------------------------------------------------*
015400030429     C*  Deve controllare tutte le testate Conteggi degli AUT da fatturare
015500030429     C*   che non sono state Confermate prima della data limite calcolata.
015600030429     C*  Per ogni AUT/Data invia un msg. al Controllo Qualità di sede con
015700030429     C* il seguente testo:
015800030429     C* " P.O. xxx: Esistono Valorizzazioni dell'AUT xxxxxxxx del
015900030429     C* xx/xx/xx non ancora confermate".
016000030429     C*---------------------------------------------------------------*
016100030429     C/EXEC SQL
016200030429     C+ Declare A1 Cursor for
016300121204     c+ SELECT fttfgs, fttpdr, fttddc, apdflr
016400121204     c+   FROM (
016500030429     c+ SELECT fttfgs, fttpdr, fttddc FROM fiftt00f
016600030429     c+ WHERE fttfvl<>'C' and ftttsr<>' ' and fttpdr in
016700030429     c+  (SELECT apdpdr FROM fiapd00f WHERE APDTIP ='A' and
016800030429     c+   APDPDD <>'S' and APDATB =' ')
016900030429     c+ and fttddc < :FINOAL
017000030429     c+ GROUP BY fttfgs, fttpdr, fttddc
017100121204     c+ ) as A JOIN FIAPD00f ON fttPDR = apdPDR
017200121204     c+ WHERE  APDTIP ='A' and APDPDD <>'S' and APDATB =' '
017300121204     c+ ORDER BY fttfgs, fttpdr, fttddc
017400030429     C/END-EXEC
017500030429      *          apertura cursore
017600030429     C/EXEC SQL
017700030429     C+ OPEN A1
017800030429     C/END-EXEC
017900030429     C                   DOU       SqlCod = 100 OR SqlCod < 0
018000030429      *          lettura cursore
018100030429     C/EXEC SQL
018200121204     C+ Fetch Next From A1 Into
018300121204     C+            :fgs, :pdr, :ddc, :flr
018400030429     C/END-EXEC
018500030429     C                   SELECT
018600030429     C                   WHEN      SqlCod = 100
018700030429      *
018800030429     C                   WHEN      SqlCod >= 0
018900030429      *
019000030429      * Invia msg al Controllo Qualità
019100030429     C                   Exsr      Send_Msg_CQ
019200030429      *
019300030429     C                   OTHER
019400030429      *
019500030429     C                   EXSR      SQLError
019600030429      *
019700030429     C                   ENDSL
019800030429      *
019900030429     C                   ENDDO
020000030429     C/EXEC SQL
020100030429     C+ Close A1
020200030429     C/END-EXEC
020300030409     C*
020400000000     C     FINE          TAG
020500030429      *
020600931027     C                   SETON                                        LR
020700030429      **********************************************************************
020800030429     c     SEND_MSG_CQ   begsr
020900030429      *
021000121204      *  Occorre verificare che non vi siano giorni in più da considerare in
021100121204      *  eccezione sull'autista (vedi Isole Eolie). Le Valorizzazioni vengono
021200121204      *  confermate non il giorno "C" bensì con più giorni segnalati in anagrafica.
021300121204      *  quindi la DATA rilevata dall'SQL deve essere riconfrontata per fare
021400121204      *  l'invio corretto della Mail.
021500121204     c                   clear                   gg_in_piu
021600121204     c                   clear                   alla_data
021700121204     c                   movel     flr           dAPDflr
021800121204      **
021900121204      **  solo in presenza di eccezioni sui giorni
022000121204     c                   if        §APDGCD <>'00'
022100121204     c                                and
022200121204     c                             §APDGCD <>'  '
022300121204      **
022400121204      *  solo in presenza di numeri
022500121204     c     digits        check     §APDGCD
022600121204     c                   if        not %Found
022700121204     c                   move      §APDGCD       gg_in_piu
022800121204     c                   endif
022900121204      **
023000121204     c                   endif
023100121204      *
023200121204      *   deve controllare in presenza di giorni in più - in eccezione
023300121204     c                   if        gg_in_piu  > 0
023400121204     c                   move      dataiso2      dataiso
023500121204      *
023600121204      * ciclo di enne giorni indietro
023700121204      *  sottraendo sempre 1 gg.
0238001212042    c                   do        *hival
023900121204     c     dataiso2      subdur    1:*d          dataiso2
024000121204     c                   move      dataiso2      dadata
024100121204     c                   move      dataiso       adata
024200121204     c                   CALL      'XSRLAV8'
024300121204     c                   PARM                    Wdata8
024400121204      * se corrisponde ho trovato il D-Day nuovo di eccezione
024500121204      *   da verificare con la data Valorizzazione NON confermata
0246001212043    c                   if        giolav = gg_in_piu
024700121204     c                   move      dataiso2      alla_data
024800121204     c                   leave
0249001212043x   c                   end
0250001212042-   c                   enddo
025100121204      *
025200121204      *   A questo punto confronta la data con quella letta da FTT
025300121204      *   se NON deve essere considerata nei termini previsti allora
025400121204      *   salta la MAIL.
025500121204     c                   if        ddc > alla_data
025600121204     c                   goto      SALTA_MAIL
025700121204     c                   end
025800121204      *  di conseguenza Manderà la Mail se passati gli enne giorni in più
025900121204     c                   endif
026000121204      *-----
026100030429     C                   MOVEL     MSG(1)        DSMSG
026200030429     C                   MOVE      pdr           MSpdr
026300040601     C                   MOVEL     pdr           fgs
026400040601     C                   MOVE      fgs           MSfgs
026500030429     C*
026600030429     C                   Z-ADD     0             G02dat
026700030429     C                   Z-ADD     ddc           G02INV
026800030429     C                   MOVEL     '3'           G02ERR
026900030429     C                   CALL      'XSRDA8'
027000030429     C                   PARM                    WLBDAT
027100030429     C     *eur          move      G02DAT        dataDMY
027200121128     C                   MOVE      dataDMY       datadi6           6 0
027300121128     C                   MOVE      dataDMY       MSddc
027400030429     C                   MOVEL     DSMSG         MSG(1)
027500030429      *
027600030430     C                   MOVEL     MSGT(1)       DSMST
027700040608     C                   MOVEL     pdr           MTpdr1
027800040608     C                   MOVE      pdr           MTpdr2
027900040603     C                   MOVEL     MSDDC         MTddc
028000030430     C                   MOVEL     DSMST         MSGT(1)
028100121113      *
028200121113      * Sostituzione della vecchia POSTA su AS con invio di E-mail
028300121113     c                   exsr      eMail_alert
028400121113      *
028500121204     c     SALTA_MAIL    endsr
028600121113      * ?------------------------------------------------------------------ */
028700121113      *?  Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
028800121113      * ?------------------------------------------------------------------ */
028900121113     C     eMail_alert   BEGSR
029000121113      *
029100121113     c                   eval      emlTO = *blank
029200121113      **
029300121113     C* con la filiale aggancia l'organigramma
029400121113     c     fgs           chain     azorg01l
029500121120      **
029600121113     c                   if        %Found(Azorg01L)
029700121120     c                   clear                   Ds05
029800121120     C                   movel     '05'          COD               2
029900121120     C                   movel(p)  orgcar        KEY               8
030000121120      **
030100121120     C     KTAB          chain     tabel00f
030200121120     c                   if        %Found(tabel00f)
030300121120     c                   movel     tblUNI        Ds05
030400121113      *
030500121113     C*  x comporre l'indirizzo del REAxxx
030600121120     c                   if        §05MAIL <> ' '
030700121129     C                   eval      EmlTO =  %trim(§05MAIL) + '@brt.it'
030800121113     C*
030900121113      * invia segnalazione via mail
031000121128     c*********          eval      emogg = MSGT(1)
031100121128     c                   eval      emogg =
031200121128     c                             'Valorizzazioni Autisti - NON CONFERMATE.'
031300121128     C*********          eval      emmsg = msg(1) + MSG(2) + MSG(3)
031400121128     C                   eval      emmsg ='Esistono Valorizzazioni dell''Auti-
031500121128     C                             sta: ' + %EDITC(pdr:'Z') + ' del ' +
031600121128     C                             %EDITW(datadi6:'  /  /  ') +' ancora da Conf-
031700121128     C                             ermare. Si prega di sollecitare la Filiale a-
031800121128     C                              Confermare i dati entro i termini previsti.'
031900121113     c
032000121128     C                   CALL      'TIS701C'
032100121113     C                   PARM                    emlto           253
032200121113     C                   PARM                    emogg            44
032300121113     C                   PARM                    emmsg          5000
032400121113      *
032500121121     c                   end
032600121121     c                   end
032700121121     c                   end
032800121121      *
032900121113     C                   ENDSR
033000030429      **********************************************************************
033100030429     C*  Routine di Controllo
033200121121     ***********************************************************************
033300030429     c     SQLError      begsr
033400030429     C*
033500030429     c                   endsr
033600030429     ***********************************************************************
033700030429** MSGT
033800121128Aut XXX XXXX il XX/XX  VAL.  NON  CONFERMATE<
033900030429** MSG
034000121121Fil.xxx: Esistono Valorizzazioni dell'AUT          del
034100121121ancora da confermare
034200121128Si prega di SOLLECITARE la FILIALE a Confermare i dati
