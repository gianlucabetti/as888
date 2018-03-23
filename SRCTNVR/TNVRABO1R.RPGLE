000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020205     h*
000300020206     ffnarb05l  Uf   e           k disk
000400150416     d dteur           s               d   datfmt(*eur)
000500150416     d dtiso           s               d   datfmt(*iso)
000600000607     D kpjba         e ds
000700020205     D cncr80        E DS
000800020205     D UT§DSE0F      E DS
000900020205     d gg              s              3  0
001000020206     d data6           s              6  0
001100020205     d dateu           s              8  0
001200020206     D Wdata           S               D   DATFMT(*eur)
001300020206     D oggi            S               d
001400020206     D dataiso         S               d
001500020205     D nrrec           S              7  0
001600020206     D w0140           S             14  0
001700020206     D w0080           S              8  0
001800021014     d   §membro       ds
001900021014     d  §emme                  1      1    inz('M')
002000021014     d  §fle                   2      4s 0
002100021014     d  §sed                   5      7s 0 inz(046)
002200021014     d  §mbr                   2      7
002300150408      * ?------------------------------------------------------------------ */
002400150408      *  Invio E-mail
002500150408      * ?------------------------------------------------------------------ */
002600150408     d wrkEml          s            253
002700150408     d wrkEmcc         s            253
002800150408     d wrkMsg          s           5000
002900150408     d wrkOgg          s             44
003000150416     d wrkEsit         s              1
003100150408      *
003200150416     D lista           S            100    DIM(30)
003300150408     D vuoto           S            100
003400150408      * ?------------------------------------------------------------------ */
003500020205     c**********************************************************************
003600020205     c                   z-add     0             nrrec
003700020206     c                   z-add     0             setll             3 0
003800020206     c     setll         setll     fnarb05l
003900000607     c                   do        *hival
004000020205     c                   read      fnarb05l
004100000607     c                   if        %eof
004200000607     c                   leave
004300000607     c                   end
004400020205      * solo bolle con data consegna > 0 e incasso contrassegno = 'R'
004500020205     c                   if        arbdcm = 0 or arbicc <> 'R'
004600020205     c                   iter
004700020205     c                   end
004800150416      ***
004900150416      ***  carica in schiera per invio mail finale
005000150416     c                   exsr      sped_saltata
005100150416     c                   if        not *in99
005200150416     c                   leave
005300150416     c                   end
005400150416      ***
005500020205     c                   enddo
005600150416      ** se ha caricato la schiera
005700150416      **   invia la mail
005800150416     C                   if        lista(1) <> *blank
005900150416     c                   exsr      invia_mail
006000150416     c                   end
006100150416      **
006200000607     c                   seton                                        lr
006300000607      *-----------------------------------------------
006400000607     c     *inzsr        begsr
006500000607      *-----------------------------------------------
006600000607     c
006700000607     c     *entry        plist
006800020205     c                   parm                    kpjba
006900020205     C                   Z-ADD     1             CODUT
007000020205     C                   CALL      'X§PARUT'
007100020205     C                   PARM                    UT§DSE0F
007200020205     C                   MOVEL     REC80         CNCR80
007300021014     C                   MOVE      simfel        §fle
007400000607
007500000607     c     karb          klist
007600020205     c                   kfld                    arbaas
007700020205     c                   kfld                    arblnp
007800020205     c                   kfld                    arbnrs
007900020205     c                   kfld                    arbnsp
008000000607
008100020206     c                   time                    w0140
008200020206     c                   move      w0140         w0080
008300020206     C     *eur          MOVEL     w0080         Wdata
008400020206     C     *iso          MOVEL     Wdata         dateu
008500020206     c                   move      dateu         oggi
008600150416
008700000607     c                   endsr
008800150408      * ?------------------------------------------------------------------ */
008900150408      * ?     Input            Verifica cosa deve segnalare
009000150408      * ?------------------------------------------------------------------ */
009100150416     C     sped_saltata  BEGSR                                                  *
009200150416      *
009300150416     c                   move      arbDDC        dtiso
009400150416     c                   move      dtiso         dteur
009500150416     c                   move      dteur         DDCgma            8 0
009600150416      *
009700150416     c                   z-add     1             xy                3 0
009800150408     c     vuoto         lookup    lista(xy)                              99
009900150416     c   99              eval      lista(xy) =    'Sped.: ' +
010000150416     c                             %editc(arbAAS:'X') + ' ' +
010100150416     c                             %editc(arbLNP:'X') + ' ' +
010200150416     c                             %editc(arbNRS:'X') + ' ' +
010300150416     c                             %editc(arbNSP:'X')       +
010400150416     c                             '    Nr.Dist.: '         +
010500150416     c                             %editc(arbNDC:'Z') + ' ' +
010600150416     c                             'del '                   +
010700150416     c                             %editc(DDCgma:'Y')
010800150416     c  n99              clear                   lista(30)
010900150416     c  n99              eval      lista(30) = 'Presenti altri C/assegni non ' +
011000150416     c                             'inviati'
011100150416     c                   if        xy =  6
011200150416     c                   setoff                                       99
011300150416     C                   end
011400150416      *
011500150408     C                   endsr
011600150408      *____________________________________________________________________
011700150408      *   INVIA MAIL
011800150408      *____________________________________________________________________
011900150408     c     INVIA_MAIL    BEGSR
012000150408      *
012100150408     C* Inizializzo variabili
012200150416     C                   eval      wrkEml= 'andrea.bertocchi@brt.it'
012300150416      **
012400150416     C                   eval      wrkOgg='C/Ass omessi non passati in gestio-
012500150416     C                             ne a sede'
012600150416      **
012700150416     C                   eval      wrkMsg='Attenzione i seguenti C/ASS OMES' +
012800150416     C                             'SI non sono stati passati in gestione a '+
012900150416     C                             'sede perché presenti in distinte Autotra'+
013000150416     C                             'sportatori NON ancora Contabilizzate.'
013100150416     c                                      +':/N'+
013200150416     C                             'Questa la lista delle spedizioni:'
013300150416     c                                      +':/N'+
013400150408     C                             lista(1) +':/N'+
013500150408     C                             lista(2) +':/N'+
013600150408     C                             lista(3) +':/N'+
013700150408     C                             lista(4) +':/N'+
013800150408     C                             lista(5) +':/N'+
013900150408     C                             lista(6) +':/N'+
014000150408     C                             lista(7) +':/N'+
014100150408     C                             lista(8) +':/N'+
014200150408     C                             lista(9) +':/N'+
014300150408     C                             lista(10)
014400150416      *
014500150416     c                   if        lista(11) <> *blank
014600150416     C                   eval      wrkMsg=   %trim(wrkMsg)
014700150416     c                                       +':/N'+
014800150416     C                             lista(11) +':/N'+
014900150416     C                             lista(12) +':/N'+
015000150416     C                             lista(13) +':/N'+
015100150416     C                             lista(14) +':/N'+
015200150416     C                             lista(15) +':/N'+
015300150416     C                             lista(16) +':/N'+
015400150416     C                             lista(17) +':/N'+
015500150416     C                             lista(18) +':/N'+
015600150416     C                             lista(19) +':/N'+
015700150416     C                             lista(20)
015800150416     c                   end
015900150416     c                   if        lista(21) <> *blank
016000150416     C                   eval      wrkMsg=   %trim(wrkMsg)
016100150416     c                                       +':/N'+
016200150416     C                             lista(21) +':/N'+
016300150416     C                             lista(22) +':/N'+
016400150416     C                             lista(23) +':/N'+
016500150416     C                             lista(24) +':/N'+
016600150416     C                             lista(25) +':/N'+
016700150416     C                             lista(26) +':/N'+
016800150416     C                             lista(27) +':/N'+
016900150416     C                             lista(28) +':/N'+
017000150416     C                             lista(29) +':/N'+
017100150416     C                             lista(30)
017200150416     c                   end
017300150408     C*
017400150408     C                   call(e)   'TIS701C1'
017500150408     C                   parm                    wrkEml
017600150408     C                   parm                    wrkEmcc
017700150408     C                   parm                    wrkOgg
017800150408     C                   parm                    wrkMsg
017900150408     C                   parm                    wrkesit
018000150408      *
018100150408     c                   ENDSR
018200150408      * ?------------------------------------------------------------------ */
