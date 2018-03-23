000100080912     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200080912      *
000300120509     Ftntl86d   CF   E             WORKSTN
000400081030     Faiats01l  uF a E           k disk
000900080912      *
001000080912     D Psds           SDS
001100080912     D  PgmName          *PROC
001200080912     D Kpjba         E DS
001300081028     d param           ds
001400081030     d  parsoc                        3s 0
001500081030     d  paroldc                       7s 0
001800080912      *
001801120601     d paralleg        ds
001802120601     d  paroldnrc                     7s 0
001803120601     d  parolddat                     8s 0
001804120601     d  parnewnrc                     7s 0
001805120601     d  parnewdat                     8s 0
001807120601      *
001900080912     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002000080912     D CNCR80        E DS
002001120601     D trul33ds      E DS
002100080912     d Tibs36Ds      e ds
002200081030     d Tntl90Ds      e ds
002300080912     d data            ds            10
002400080912     d aa                      3      4
002500080912     d mm                      6      7
002600080912     d gg                      9     10
002700080912      *
002800080912     D modalita        S              1
002900080912     D x               S              4  0
003000080912     D $VIDEO          S             10
003100080912     D Savkpjbu        S                   like(kpjbu)
003200080912     d dataiso         s               d   datfmt(*iso)
003300080912     d dataeur         s               d   datfmt(*eur)
003400080912     D WLBDA8          DS
003500080912     D  G02DAT                 1      8  0
003600080912     D  G02INV                 9     16  0
003700080912     D  G02ERR                17     17
003800080912     D  G02TGI                18     22  0
003900081028      * se il programma parte da menu o se richiamato da altro
004000081030      *---------------- Gestione formato video01----------------------
004100081028      *
004200081210     C                   EVAL      $Video = 'VIDEO1'
004300080912     C                   EXSR      Inzvideo1
004400080912     C                   DOU       $Video <> 'VIDEO1'
004500080912      *----------- Visualizzo dati di output in caso di errori.-------------
004600080912     C                   IF        *IN99
004700080912     C                   EVAL      *IN99 = *OFF
004800080912     C                   WRITE     video1
004900080912     C                   EVAL      *IN99 = *ON
005000080912     C                   ENDIF
005100080912      *
005200080912     c     emetti        tag
005300080912     C                   EXFMT     video1
005301120611     c                   setoff                                       28
005302120611     c                   clear                   $msg
005400080912      *
005500080912     C                   SELECT
005600080912     C                   WHEN      *INKC
005700080912     C                             OR
005800080912     C                             *INKL
005900080912     C                   EVAL      $Video = *BLANKS
006000080912
006100080912     C                   WHEN      *INKE
006200080912     C                   EXSR      Inzvideo1
006300080912     C                   WHEN      *INKB
006400080912     C                   EXSR      Chkvideo1
006500080912     C                   OTHER
006600080912     C                   EXSR      Chkvideo1
006601120611     c     kats2         chain     aiats01l
006602120611     c                   if        %found(aiats01l)
006603120611     c                   eval      $msg = 'Esiste già un contratto con questa -
006604120611     c                             decorrenza '
006605120611     c                   seton                                        8428
006606120611     c                   endif
006700080912     c   84              goto      emetti
006800081030     c                   if        *inkf
006801120601      *scadenza vecchio contratto apertura nuovo
006900081030     C                   EXSR      conferma
006901120601      *scadenza vecchi allegati  apertura nuovi su nuovo contratto
006902120601     C                   EXSR      allegati
006903120606     c                   leave
007000081030     c                   endif
007100080912     C                   ENDSL
007200080912     C                   ENDDO
007300080912      *
007400080912     C                   EXSR      Uscita
007500080912     c**********************************************************************
007600080912     c* uscita
007700080912     c**********************************************************************
007800080912     C     Uscita        BEGSR
007900080912      *
008000080912     C                   EVAL      *INLR = *ON
008100080912     C                   RETURN
008200080912      *
008300080912     C                   ENDSR
008400080912     c**********************************************************************
008500080912     c* Controlli video R01.
008600080912     c**********************************************************************
008700080912     C     Chkvideo1     BEGSR
008800080912      *
008900080912     C                   EVAL      *IN99 = *OFF
009000080912     C                   EVAL      *IN84 = *OFF
009100081030     c                   movea     '00000000'    *in(40)
009200081030      * controllo esistenza codice vecchio
009300081030     c                   move      vsoc          codsoc
009400081030     c                   move      voldcod       codats
009500081030     c     kats          chain(n)  aiats01l
009600081030     c                   if        not %found(aiats01l)
009700081030     c                   seton                                        4084
009800081030     c                   else
009900081030     c                   movel     atsrags       voldrags
010000080912     c                   endif
012800120509      *data decorrenza nuovo contratto
012900081030     c                   if        vdata > 0
013000081030     C                   move      Vdata         G02DAT
013100081030     C                   movel     *BLaNK        G02ERR
013200081030     C                   call      'XSRDA8'
013300081030     C                   parm                    WLBDA8
013400081030     C     G02ERR        ifeq      '1'
013500081030     C                   seton                                        4384
013600081030     C                   end
013700081030     c                   move      g02dat        vdata
013800120509     c                   move      g02inv        newdat            8 0
013900120509     c                   move      newdat        dataiso
014000120509     c     dataiso       subdur    1:*d          dataiso
014100120509     c                   move      dataiso       olddat            8 0
014200081030     c                   else
014300081030     C                   seton                                        4384
014400081030     c                   endif
014500080912      *
014600080912     C                   ENDSR
014700080912     c**********************************************************************
014800080912     c* inizializza r01
014900080912     c**********************************************************************
015000080912     C     Inzvideo1     BEGSR
015100080912
015200081030     c                   if        paroldc <> *zeros
015300081030     c                   z-add     paroldc       voldcod
015400081030     c                   move      parsoc        vsoc
015500080912     c                   endif
015600081030     c                   z-add     wdat          vdata
015700080912     C                   EXSR      Chkvideo1
015800080912
015900080912     C                   ENDSR
016000081030     c**********************************************************************
016100081030     C     conferma      BEGSR
016200081030     c**********************************************************************
016300081030      *
016400081030      * aggancio rekord con codice vecchio per farlo scadere
016500081030     c                   move      voldcod       codats
016600081030     c     kats          chain     aiats01l
016700081030     c                   if        %found(aiats01l)
016701120601      *prendo numero contratto vecchio per chiudere allegait legati
016702120601     c                   z-add     atsnrc        oldnrc            7 0
017000081030      *update x scadenza rekord vecchio
017101120504     c                   z-add     olddat        atsdfc
017200081030     c                   update    aiats000
017300081030      *write del nuovo rekord con nuovo codice e data inizio +1g scadenza precedente
017500120509     c                   clear                   atsdfc
017501120509     c                   clear                   atsdrc
017900120509     c                   z-add     newdat        atsdec
017901120605     c                   z-add     20391231      atsdfc
017902120601     c                   exsr      repnum
018000081030     c                   write     aiats000
018200081030     c                   endif
018300081030     c
018400081030     C                   endsr
018401120601     c**********************************************************************
018402120601     C     repnum        BEGSR
018403120601     c                   clear                   trul33ds
018404120601     c                   move      'L'           i33tla
018405120601     c                   z-add     37            i33cnu
018406120601     c                   z-add     1             i33num
018407120601     c                   movel     trul33ds      kpjbu
018408120601     c                   call      'TRUL33R'
018409120601     c                   PARM                    kpjba
018410120601     c                   movel     kpjbu         trul33ds
018411120601     c                   z-add     o33nrf        atsnrc
018412120601     C                   ENDSR
018413120601     c**********************************************************************
018414120601     C     allegati      BEGSR
018415120601     c**********************************************************************
018416120601      *
018417120601      * aggancio rekord con codice vecchio per farlo scadere
018418120601     c                   move      oldnrc        paroldnrc
018422120601     c                   z-add     olddat        parolddat
018423120601     c                   move      atsnrc        parnewnrc
018424120601     c                   z-add     atsdec        parnewdat
018430120601     c                   call      'TNTL86R1'
018431120601     c                   parm                    paralleg
018434120601     c
018435120601     C                   endsr
023000080912     c**********************************************************************
023100080912     C     *INZSR        BEGSR
023200080912     c**********************************************************************
023300080912      *
023400080912     C     *ENTRY        PLIST
023500080912     C                   PARM                    Kpjba
023600081028      *richiamato da altri
023700081028     c                   clear                   param
023800081028     c                   if        kpjbu <> *blank
023900081028     c                   movel     kpjbu         param
024000081028     c                   endif
024200080912     C                   Z-ADD     1             CODUT
024300080912     C                   CALL      'X§PARUT'
024400080912     C                   PARM                    UTEDSE
024500080912     C                   MOVEL     REC80         CNCR80
024600080912     C                   MOVEL     Ragut         rsut
024700080912     C                   MOVEL     knsif         vknsif
024800080912     C                   MOVEL     knmus         vknmus
024900080912      *reperimento data
025000080912     C                   TIME                    W0120            14 0
025100080912     C                   MOVE      W0120         WDAT              8 0
025200080912     C*
025300080912     C                   Z-ADD     WDAT          G02DAT
025400080912     C                   MOVEL     *BLANK        G02ERR
025500080912     C                   CALL      'XSRDA8'
025600080912     C                   PARM                    WLBDA8
025700080912     C* UDATE A 8 IN AAAA/MM/GG
025800080912     C                   Z-ADD     G02INV        WUDA              8 0
025900080912      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
026000080912     C                   CLEAR                   Tibs36Ds
026100080912     C                   EVAL      I36ute = knmus
026200080912     C                   CALL      'TIBS36R'
026300080912     C                   PARM                    tibs36ds
026400080912
026900081030     C     Kats          KLIST
027000081030     C                   KFLD                    codsoc            3 0
027100081030     C                   KFLD                    codats            7 0
027101120611     C     Kats2         KLIST
027102120611     C                   KFLD                    codsoc            3 0
027103120611     C                   KFLD                    codats            7 0
027104120611     C                   KFLD                    newdat            8 0
027500081031      *
027600081031     C                   CALL      'TNTL90R'
027700081031     C                   PARM                    tntl90ds
027800081031     c                   if        %subst(d90nsi: 7: 3) = 'PSP'
028000120509     c                   move      219           vsoc
028100081031     c                   endif
028200080912      *---------------------------------------------------------------------
028300080912     C                   ENDSR
