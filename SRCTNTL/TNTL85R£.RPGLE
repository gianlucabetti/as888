000100080912     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200080912      *
000300081031     Ftntl85d   CF   E             WORKSTN
000400081030     Faiats01l  uF a E           k disk
000500081030     Faiatm01l  uF a E           k disk
000600081030     Faiata01l  uF a E           k disk
000700081016     Fanfrn01l  iF   E           k disk
000800081016     Fansog01l  iF   E           k disk
000900080912      *
001000080912     D Psds           SDS
001100080912     D  PgmName          *PROC
001200080912     D Kpjba         E DS
001300081028     d param           ds
001400081030     d  parsoc                        3s 0
001500081030     d  paroldc                       7s 0
001600081030     d  parnewc                       7s 0
001700081028     d  parmod                        1
001800080912      *
001900080912     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002000080912     D CNCR80        E DS
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
006700080912     c   84              goto      emetti
006800081030     c                   if        *inkf
006900081030     C                   EXSR      conferma
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
010100081030      * controllo esistenza nuovo codice nei traini se esiste errore
010200090528      * se cambio codice
010300090528     c                   if        parmod <> 'T'
010400081030     c                   move      vnewcod       codats
010500081030     c     kats          chain(n)  aiats01l
010600081030     c                   if        %found(aiats01l)
010700081030     c                   seton                                        4184
010800081030     c                   endif
010900090528     c                   else
011000090528      * se invece opzione di trasferimento il codice deve esistere in anagrafica
011100090528     c                   move      vnewcod       codats
011200090528     c     kats          chain(n)  aiats01l
011300090528     c                   if        not %found(aiats01l)
011400090528     c                   seton                                        4484
011500090528     c                   endif
011600090528     c                   endif
011700081030      * controllo esistenza nuovo codice in anagrafica fornitori
011800081030     c                   movel     vsoc          societa
011900081030     c                   movel     *zeros        codice
012000081030     c                   move      vnewcod       codice
012100081030     c     kfrn          chain     anfrn01l
012200081030     c                   if        not%found(anfrn01l)
012300081030     c                   seton                                        4284
012400081030     c                   else
012500081030     c     frnsogg       chain     ansog01l
012600081030     c                   movel     sogdes        vnewrags
012700081030     c                   endif
012800081030      *data scadenza vecchio codice
012900081030     c                   if        vdata > 0
013000081030     C                   move      Vdata         G02DAT
013100081030     C                   movel     *BLaNK        G02ERR
013200081030     C                   call      'XSRDA8'
013300081030     C                   parm                    WLBDA8
013400081030     C     G02ERR        ifeq      '1'
013500081030     C                   seton                                        4384
013600081030     C                   end
013700081030     c                   move      g02dat        vdata
013800081030     c                   move      g02inv        olddat            8 0
013900081030     c                   move      olddat        dataiso
014000081030     c     dataiso       adddur    1:*d          dataiso
014100081030     c                   move      dataiso       newdat            8 0
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
016800081030      *aggiorno dettagli prima della testata
016900081030     c                   exsr      dettagli
017000081030      *update x scadenza rekord vecchio
017100081030     c                   z-add     olddat        atsdtfic
017101120504     c                   z-add     olddat        atsdfc
017200081030     c                   update    aiats000
017300081030      *write del nuovo rekord con nuovo codice e data inizio +1g scadenza precedente
017400090528     c                   if        parmod <> 'T'
017500090108     c                   clear                   aiats000
017600090108     c                   move      vsoc          atssoc
017700081030     c                   move      vnewcod       atsksc
017800081030     c                   move      vnewrags      atsrags
017900081030     c                   z-add     newdat        atsdtinc
018000081030     c                   write     aiats000
018100090528     c                   endif
018200081030     c                   endif
018300081030     c
018400081030     C                   endsr
018500081030     c**********************************************************************
018600081030     C     dettagli      BEGSR
018700081030     c**********************************************************************
018800081030      *
018900081030      * leggo dettaglio con codice vecchio per farlo scadere
019000081030      *AUTOMEZZI
019100081030     c     kats          setll     aiatm01l
019200081030     c                   do        *hival
019300081030     c     kats          reade     aiatm01l
019400081030     c                   if        %eof(aiatm01l)
019500081030     c                   leave
019600081030     c                   endif
019700081215     c                   if        atmdtfir > 0
019800081215     c                   iter
019900081215     c                   endif
020000081030      *update x scadenza rekord vecchio
020100081030     c                   z-add     olddat        atmdtfir
020200081030     c                   update    aiatm000
020300081030      *write del nuovo rekord con nuovo codice e data inizio +1g scadenza precedente
020400090108     c                   clear                   atmdtfir
020500081030     c                   move      vnewcod       atmksc
020600081030     c                   z-add     newdat        atmdtinr
020700081030     c                   write     aiatm000
020800081030     c                   enddo
020900081030      *AUTISTI
021000081030     c     kats          setll     aiata01l
021100081030     c                   do        *hival
021200081030     c     kats          reade     aiata01l
021300081030     c                   if        %eof(aiata01l)
021400081030     c                   leave
021500081030     c                   endif
021600081215     c                   if        atadtfir > 0
021700081215     c                   iter
021800081215     c                   endif
021900081030      *update x scadenza rekord vecchio
022000081030     c                   z-add     olddat        atadtfir
022100081030     c                   update    aiata000
022200081030      *write del nuovo rekord con nuovo codice e data inizio +1g scadenza precedente
022300090108     c                   clear                   atadtfir
022400081030     c                   move      vnewcod       ataksc
022500081030     c                   z-add     newdat        atadtinr
022600081030     c                   write     aiata000
022700081030     c                   enddo
022800081030     c
022900081030     C                   endsr
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
024100090528     c     parmod        comp      'T'                                    66
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
026500081016     C     Kfrn          KLIST
026600081016     C                   KFLD                    societa           3
026700081016     C                   KFLD                    codice            8
026800080912      *
026900081030     C     Kats          KLIST
027000081030     C                   KFLD                    codsoc            3 0
027100081030     C                   KFLD                    codats            7 0
027200081030     C     Kdet          KLIST
027300081030     C                   KFLD                    atssoc
027400081030     C                   KFLD                    atsksc
027500081031      *
027600081031     C                   CALL      'TNTL90R'
027700081031     C                   PARM                    tntl90ds
027800081031     c                   if        %subst(d90nsi: 7: 3) = 'PSP'
027900120509     c                   move      '219'         societa
028000120509     c                   move      219           vsoc
028100081031     c                   endif
028200080912      *---------------------------------------------------------------------
028300080912     C                   ENDSR
