000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030429     H* FICND9R *----------------------------------------------------*
000300030429     H*   In Base ai gg.in tabella CCG controlla se non sono stati
000400030429     H*   eseguiti dei conteggi AUT x Autotrasportatori da Fatturare
000500030429     H*   entro i termini dovuti.
000600130104     H*  Il programma è stato modificato per inviare direttamente ai Capi AREA
000700130104     H*   la MAIL e non mandarla più al CDG.  Inoltre la mail sarà una unica
000800130104     H*   per singola filiale con l'elenco di tutti gli autisti anzichè una
000900130104     H*   mail per ogni autista.
001000000000     H*--------------------------------------------------------------*
001100121113     fazorg01l  if   e           k disk
001200121113     ftntbe01l  if   e           k disk
001300121120     ftabel00f  if   e           k disk
001400030429     ffiftt04l  if   e           k disk
001500030418      *--------------------------------------------------------------*
001600030429     d dccg          e ds
001700121120     d ds05          e ds
001800030418      *
001900030409     D* PASSAGGIO DATI
002000931129     D KPJBA         E DS
002100900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200030409     D CNCR80        E DS
002300121204     D dAPDflr       E DS
002400030429     D*
002500030429     d dataiso         s               d   datfmt(*iso)
002600030429     d dataiso2        s               d   datfmt(*iso)
002700030429     d alladata        s               d   datfmt(*iso)
002800030429     d dataymd         s               d   datfmt(*ymd)
002900030429     d datadmy         s               d   datfmt(*dmy)
003000030429     d finoal          s              8  0
003100121204     d alla_data       s              8  0
003200030429      *
003300121113      *
003400030429     d fgs             s                   like(fttfgs)
003500030429     d pdr             s                   like(fttpdr)
003600030429     d ddc             s                   like(fttddc)
003700121204     d flr             s                   like(dapdflr)
003800130104     d SAVfgs          s                   like(fttfgs)
003900130104     d SAVpdr          s                   like(fttpdr)
004000130104     d Prima_VOLTA     s              1
004100130104     d AlmenoUNO       s              1
004200030429      *
004300121204     d gg_in_piu       s              2  0
004400121204      *
004500030429     d Wdata8          DS
004600030429     d  dadata                 1      8  0
004700030429     d  adata                  9     16  0
004800030429     d  GioLav                17     21  0
004900030429     D*
005000030409     D WLBDAT          DS
005100030409     D  G02DAT                 1      8  0
005200030409     D  G02INV                 9     16  0
005300030409     D  G02ERR                17     17
005400030409     D  G02TGI                18     22  0
005500030429      *
005600030409     D WGIDAT          DS
005700030409     D  GIODAT                 1      8  0
005800030409     D  GIOINV                 9     16  0
005900030409     D  GIOTGI                17     21  0
006000030429      *
006100030429     d Digits          c                   Const('0123456789')
006200900521     C****************************************************************
006300900521     C*  RIEPILOGO INDICATORI
006400900521     C*****************************************************************
006500030429     c     ktbe          klist
006600030429     c                   kfld                    tbecod
006700030429     c                   kfld                    tbeke1
006800121120     C     KTAB          KLIST
006900121120     C                   KFLD                    CODUT
007000121120     C                   KFLD                    COD               2
007100121120     C                   KFLD                    KEY               8
007200030429      *--------------------------------------------------------
007300000000     C     *ENTRY        PLIST
007400000000     C                   PARM                    KPJBA
007500030429     C***
007600030429     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
007700030429     C***
007800030429     C                   TIME                    W0140            14 0
007900030429     C                   MOVEl     W0140         Wora              6 0
008000030429     C                   MOVE      W0140         WDAT              8 0
008100030429     C*
008200030429     C                   Z-ADD     Wdat          G02DAT
008300030429     C                   MOVEL     *BLANK        G02ERR
008400030429     C                   CALL      'XSRDA8'
008500030429     C                   PARM                    WLBDAT
008600030429     C* UDATE A 8 IN AAAA/MM/GG
008700030429     C                   move      G02INV        dataiso
008800030429      *
008900030429      *controllo se udate è festivo non lancio l'elaborazione
009000030429     c                   move      dataiso       dadata
009100030429     c                   move      dataiso       adata
009200030429     c                   CALL      'XSRLAV8'
009300030429     c                   PARM                    Wdata8
009400030429     c                   if        giolav = 0
009500030429     c                   seton                                        lr
009600030429     c                   endif
009700121113      *
009800121113      *-----------
009900030429      *
010000030429      *reperisco limiti da tabella per calcolare il D-Day
010100030429     c                   movel(p)  '1'           tbeke1
010200030429     c                   movel     'CCG'         tbecod
010300030429     c     ktbe          chain     tntbe01l                           90
010400030429     c                   if        %Found(tntbe01l)
010500030429     c                   move      *blank        fld2              2
010600030429     c                   eval      fld2=%Subst(tbeuni:18:2)
010700030429     c     digits        check     fld2:1
010800030429     c                   if        %Found
010900030429     c                   z-add     5             §cggca
011000030429     c                   else
011100030429     c                   movel     tbeuni        dccg
011200030429     c                   endif
011300030429     c                   endif
011400030429      *
011500030429      * calcolo il D-Day partendo da udate cercando i giorni lavorativi
011600030429      * all'indietro a seconda dei giorni impostati in tabella
011700030429     c     dataiso       subdur    1:*d          dataiso2
011800030429      *
0119000304292    c                   do        *hival
012000030429     c                   move      dataiso2      dadata
012100030429     c                   move      dataiso       adata
012200030429     c                   CALL      'XSRLAV8'
012300030429     c                   PARM                    Wdata8
012400030429      * se corrisponde ho trovato il D-Day
0125000304293    c                   if        giolav = §cggca
012600030429     c                   leave
0127000304293x   c                   else
012800030429      * altrimenti continuo a sottrarre i giorni da udate
012900030429     c     dataiso2      subdur    1:*d          dataiso2
013000030429     c                   iter
0131000304293-   c                   endif
0132000304292-   c                   enddo
013300030429      *
013400030429      * ho stabilito il D-DAY limite oltre il quale non si può andare
013500030429      *  e se ci fossero dei conteggi ancora da Confermare verranno
013600030429      *   segnalati.
013700030429     c                   move      dataiso2      dataYMD
013800030429     c                   move      dataiso2      finoal            8 0
013900030429      *-----
014000000000     C                   Z-ADD     1             CODUT
014100900615     C                   CALL      'X§PARUT'
014200900615     C                   PARM                    UT§DSE
014300000000     C                   MOVEL     RAGUT         RSUT             20
014400900615     C                   MOVEL     REC80         CNCR80
014500030429      *
014600030429      * lettura fittizia per definire i campi
014700030429     c                   read      fiftt04l
014800900511     C*---------------------------------------------------------------*
014900030429     C*  Deve controllare tutte le testate Conteggi degli AUT da fatturare
015000030429     C*   che non sono state Confermate prima della data limite calcolata.
015100030429     C*  Per ogni AUT/Data invia un msg. al Controllo Qualità di sede con
015200030429     C* il seguente testo:
015300030429     C* " P.O. xxx: Esistono Valorizzazioni dell'AUT xxxxxxxx del
015400030429     C* xx/xx/xx non ancora confermate".
015500030429     C*---------------------------------------------------------------*
015600030429     C/EXEC SQL
015700030429     C+ Declare A1 Cursor for
015800121204     c+ SELECT fttfgs, fttpdr, fttddc, apdflr
015900121204     c+   FROM (
016000030429     c+ SELECT fttfgs, fttpdr, fttddc FROM fiftt00f
016100030429     c+ WHERE fttfvl<>'C' and ftttsr<>' ' and fttpdr in
016200030429     c+  (SELECT apdpdr FROM fiapd00f WHERE APDTIP ='A' and
016300030429     c+   APDPDD <>'S' and APDATB =' ')
016400030429     c+ and fttddc < :FINOAL
016500030429     c+ GROUP BY fttfgs, fttpdr, fttddc
016600121204     c+ ) as A JOIN FIAPD00f ON fttPDR = apdPDR
016700121204     c+ WHERE  APDTIP ='A' and APDPDD <>'S' and APDATB =' '
016800121204     c+ ORDER BY fttfgs, fttpdr, fttddc
016900030429     C/END-EXEC
017000030429      *          apertura cursore
017100030429     C/EXEC SQL
017200030429     C+ OPEN A1
017300030429     C/END-EXEC
017400030429     C                   DOU       SqlCod = 100 OR SqlCod < 0
017500030429      *          lettura cursore
017600030429     C/EXEC SQL
017700121204     C+ Fetch Next From A1 Into
017800121204     C+            :fgs, :pdr, :ddc, :flr
017900030429     C/END-EXEC
018000030429     C                   SELECT
018100130104      * EoF
018200030429     C                   WHEN      SqlCod = 100
018300130104      * a EoF invia la mail se ce n'era almeno una da inviare
018400130110     c                   if            AlmenoUno = 'S'
018500130104      * ultimo invio a FINE FILE
018600130104     C                   Exsr      Invia_MSG
018700130104     c                   end
018800130104      *
018900130104      * Lettura dei records
019000030429     C                   WHEN      SqlCod >= 0
019100130104      *
019200130104      * a rottura di filiale invia la mail  quindi aveva composto
019300130104      * il messaggio al giro della precedente filiale.
019400130104     c                   if            SAVfgs <> fgs
019500130110      *
019600130110      *  solo se ha da comunicarne almeno uno INVIA la mail
019700130110     c                   if            AlmenoUno = 'S'
019800130104     C                   Exsr      Invia_MSG
019900130110      *
020000130110      *   resetta il flag per la prossima filiale
020100130110     c                   eval      AlmenoUNO = *blank
020200130104     c                   exsr      INIT_MAIL
020300130104      *
020400130104      * solo la prima volta che si entra
020500130104     c                   eLSe
020600130104     c                   exsr      INIT_MAIL
020700130110     c                   end
020800130110      *
020900130104     c                   end
021000130104      *
021100130104      *  Compone il messaggio
021200130104     c                   eval          SAVfgs = fgs
021300130104     C                   Exsr      Compone_MSG
021400030429      *
021500030429     C                   OTHER
021600130104      *   Errori
021700030429     C                   EXSR      SQLError
021800030429      *
021900030429     C                   ENDSL
022000030429      *
022100030429     C                   ENDDO
022200030429     C/EXEC SQL
022300030429     C+ Close A1
022400030429     C/END-EXEC
022500030409     C*
022600000000     C     FINE          TAG
022700931027     C                   SETON                                        LR
022800030429      **********************************************************************
022900130104     c     Compone_MSG   begsr
023000030429      *
023100121204      *  Occorre verificare che non vi siano giorni in più da considerare in
023200121204      *  eccezione sull'autista (vedi Isole Eolie). Le Valorizzazioni vengono
023300121204      *  confermate non il giorno "C" bensì con più giorni segnalati in anagrafica.
023400121204      *  quindi la DATA rilevata dall'SQL deve essere riconfrontata per fare
023500121204      *  l'invio corretto della Mail.
023600121204     c                   clear                   gg_in_piu
023700121204     c                   clear                   alla_data
023800121204     c                   movel     flr           dAPDflr
023900121204      **
024000121204      **  solo in presenza di eccezioni sui giorni
024100121204     c                   if        §APDGCD <>'00'
024200121204     c                                and
024300121204     c                             §APDGCD <>'  '
024400121204      **
024500121204      *  solo in presenza di numeri
024600121204     c     digits        check     §APDGCD
024700121204     c                   if        not %Found
024800121204     c                   move      §APDGCD       gg_in_piu
024900121204     c                   endif
025000121204      **
025100121204     c                   endif
025200121204      *
025300121204      *   deve controllare in presenza di giorni in più - in eccezione
025400121204     c                   if        gg_in_piu  > 0
025500121204     c                   move      dataiso2      dataiso
025600121204      *
025700121204      * ciclo di enne giorni indietro
025800121204      *  sottraendo sempre 1 gg.
0259001212042    c                   do        *hival
026000121204     c     dataiso2      subdur    1:*d          dataiso2
026100121204     c                   move      dataiso2      dadata
026200121204     c                   move      dataiso       adata
026300121204     c                   CALL      'XSRLAV8'
026400121204     c                   PARM                    Wdata8
026500121204      * se corrisponde ho trovato il D-Day nuovo di eccezione
026600121204      *   da verificare con la data Valorizzazione NON confermata
0267001212043    c                   if        giolav = gg_in_piu
026800121204     c                   move      dataiso2      alla_data
026900121204     c                   leave
0270001212043x   c                   end
0271001212042-   c                   enddo
027200121204      *
027300121204      *   A questo punto confronta la data con quella letta da FTT
027400121204      *   se NON deve essere considerata nei termini previsti allora
027500121204      *   salta la MAIL.
027600121204     c                   if        ddc > alla_data
027700121204     c                   goto      SALTA_MAIL
027800121204     c                   end
027900130104      **
028000121204      *  di conseguenza Manderà la Mail se passati gli enne giorni in più
028100121204     c                   endif
028200121204      *-----
028300130104      * Prepara la MAIL solo a rottura di Autista
028400130104     c                   if           SAVpdr <> pdr
028500130104     c                   eval      AlmenoUNO  = 'S'
028600130104     c                   exsr      PREPARA_MAIL
028700130104     c                   eval         SAVpdr =  pdr
028800130104     c                   end
028900121113      *
029000121204     c     SALTA_MAIL    endsr
029100121113      * ?------------------------------------------------------------------ */
029200121113      *?  Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
029300121113      * ?------------------------------------------------------------------ */
029400130104     C     PREPARA_MAIL  BEGSR
029500130104     C*
029600121113      *
029700130104     C*  x comporre l'indirizzo del REAxxx   quindi c'è una mail da INVIARE
029800130104     c                   if          §05MAIL <> ' '
029900121113     C*
030000130104     C*   Data da cui Valorizzare
030100130104     C                   Z-ADD     0             G02dat
030200130104     C                   Z-ADD     ddc           G02INV
030300130104     C                   MOVEL     '3'           G02ERR
030400130104     C                   CALL      'XSRDA8'
030500130104     C                   PARM                    WLBDAT
030600130104     C     *eur          move      G02DAT        dataDMY
030700130104     C                   MOVE      dataDMY       datadi6           6 0
030800130104      **
030900121113      * invia segnalazione via mail
031000130104     C                   eval      emmsg = %trim(emmsg) +
031100130104     C                             %EDITC(pdr:'Z') + ' dal ' +
031200130104     C                             %EDITW(datadi6:'  /  /  ') + ':/N'
031300121121     c                   end
031400130104      *
031500130104     C                   ENDSR
031600130104      **********************************************************************
031700130104     C*  Inizializza la MAIL
031800130104     ***********************************************************************
031900130104     c     Init_MAIL     begsr
032000130104      *
032100130104     c                   eval      emlTO = *blank
032200130104     c                   eval      emogg =
032300130104     c                             'Valorizzazioni NON CONFERMATE filiale '
032400130104     c                             + %EDITC(fgs:'X')
032500130104      *
032600130104      * preimposta il messaggio successivo
032700130104     C                   eval      emmsg ='Si prega di sollecitare la "Conferma-
032800130104     C                              Valorizzazione Giornaliera Autotrasportator-
032900130104     C                             i" per i seguenti Autisti: '
033000130104     C                             + ':/N'
033100130104      *
033200130104     C* con la filiale aggancia l'organigramma
033300130104     c                   clear                   Ds05
033400130104     c     fgs           chain     azorg01l
033500130104     c                   if        %Found(Azorg01L)
033600130104     C                   movel     '05'          COD               2
033700130104     C                   movel(p)  orgcar        KEY               8
033800130104     C     KTAB          chain     tabel00f
033900130104     c                   if        %Found(tabel00f)
034000130104     c                   movel     tblUNI        Ds05
034100130104     c                   if          §05MAIL <> ' '
034200130104     C                   eval      EmlTO =  %trim(§05MAIL) + '@brt.it'
034300130104     c                   end
034400130104     c                   end
034500130104     c                   end
034600121121      *
034700121113     C                   ENDSR
034800030429      **********************************************************************
034900130104     C*  Invio del messaggio
035000121121     ***********************************************************************
035100130104     c     Invia_MSG     begsr
035200130104      *
035300130104     c** invia fisicamente la Email
035400130104     C                   CALL      'TIS701C'
035500130104     C                   PARM                    emlto           253
035600130104     C                   PARM                    emogg            44
035700130104     C                   PARM                    emmsg          5000
035800130104      *
035900130104     C                   ENDSR
036000130104      **********************************************************************
036100130104     C*  Routine di Controllo
036200130104     ***********************************************************************
036300130104     c     SQLError      begsr
036400130104     C*
036500030429     c                   endsr
036600030429     ***********************************************************************
