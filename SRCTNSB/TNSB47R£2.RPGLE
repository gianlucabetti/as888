000100060105     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200060105
000300060105      *------------------------------------------------------------------------*
000400060221      *       Lancio    manca Tariffa Giornaliero / settimanale / mensile      *
000500060105      *------------------------------------------------------------------------*
000600070629     FTABEL00F  IF   E           K DISK
000700060105
000800060105      *------------------------------------------------------------------------*
000900060105      *   N O T E
001000060105      *------------------------------------------------------------------------*
001100060209      *  questo programma viene schedulato tutti i giorni ma viene eseguito
001200060209      *  tutti i giorni tranne il lunedì  SABATO E DOMENICA li controllo ?
001300060105      *------------------------------------------------------------------------*
001400060105
001500060105      *------------------------------------------------------------------------*
001600060105      *   C A M P I   D I   L A V O R O
001700060105      *------------------------------------------------------------------------*
001800060105     d dateu           s              8  0
001900060105     d w0140           s             14  0
002000060222     d wora            s              6  0
002100060213     d wggmmaa         s              6
002200060213     d wgioset         s              1
002300060221     d festa           s              2
002400060221     d mensile         s              2
002500060228     d com8a           s              8
002600060213
002700070629     d Cod             S                   like(TblCod)
002800070629     d Key             S                   like(TblKey)
002900070629
003000060213     d Datadmy         s               d   datfmt(*dmy)
003100060213     d dataiso         s               d   datfmt(*iso)
003200060301     d dataiso1        s               d   datfmt(*iso)
003300060213     d dataiso2        s               d   datfmt(*iso)
003400060105      *------------------------------------------------------------------------*
003500060105      *   S C H I E R E
003600060105      *------------------------------------------------------------------------*
003700060105
003800060105      *------------------------------------------------------------------------*
003900060105      *   D S   I N T E R N E / E S T E R N E
004000060105      *------------------------------------------------------------------------*
004100060105
004200070629     d Param           DS
004300070629     d  Libr                   1     10
004400070629     d  File                  11     20
004500070629     d  Data                  21     26  0
004600070629
004700060105     d wlbdat          ds                  Inz
004800060105     d  g02dat                 1      8  0
004900060105     d  g02inv                 9     16  0
005000060105     d  g02err                17     17
005100060105     d  g02tgi                18     22  0
005200070629
005300070629     d Wgidat          DS                  inz
005400070629     d  giodat                 1      8  0
005500070629     d  gioinv                 9     16  0
005600070629     d  giotgi                17     21  0
005700060213
005800060213     d                 ds
005900060213     d Wdata                   1      8  0
006000060221     d dtagg                   1      2  0
006100060213     d dtaggmm                 1      4  0
006200060213     d dtaaa                   7      8  0
006300060207
006400060206
006500060213     d Wdata8          DS
006600060213     d  dadata                 1      8  0
006700060213     d  adata                  9     16  0
006800060213     d  GioLav                17     21  0
006900060213
007000060222      * Data e ora shedulazione per Kpjba
007100060222     d Scheds          DS
007200060222     d  D1Dats                 1      6
007300060222     d  D1Oras                 7     10
007400060222      *
007500060206     d Tnsb44ds      e ds
007600070629
007700070629     d Ds3s          e dS
007800060206
007900060105     d Kpjba         e ds
008000060105
008100060105      *------------------------------------------------------------------------*
008200060105      *   I N D I C A T O R I
008300060105      *------------------------------------------------------------------------*
008400060105
008500060105      *------------------------------------------------------------------------*
008600060105      * CICLO PRINCIPALE
008700060105      *------------------------------------------------------------------------*
008800070704      *------------------------------------------------------------------------*
008900070704      * KLIST
009000070704      *------------------------------------------------------------------------*
009100070704      *---- File Tabel00f
009200070704
009300070704     c     KeyTab        Klist
009400070704     c                   Kfld                    Codut
009500070704     c                   Kfld                    cod
009600070704     c                   Kfld                    key
009700060213
009800060109     c     *entry        plist
009900060109     c                   parm                    Kpjba
010000060307     c                   clear                   kbuff
010100070629     c                   z-add     1             codut             1 0
010200070629      *
010300070704     c                   clear                   tnsb44ds
010400070704      * Reperisco data corrente
010500060109     c                   time                    w0140
010600060109     c                   move      w0140         wData
010700060222      * ora
010800060222     c                   movel     w0140         wora
010900060206
011000060213     c                   clear                   WLBdat
011100060213     c                   eval      G02dat = wData
011200060213     c                   call      'XSRDA8'
011300060213     c                   parm                    WLBdat
011400060213     c                   move      G02inv        Dataiso
011500060307     c                   z-add     G02inv        DateU
011600070629     c                   z-add     G02tgi        giorni            5 0
011700060213
011800060213      * controllo se udate è festivo non lancio l'elaborazione
011900060213     c                   move      dataiso       dadata
012000060213     c                   move      dataiso       adata
012100060213     c                   CALL      'XSRLAV8'
012200060213     c                   PARM                    Wdata8
012300060213     c                   if        giolav = 0
012400060221     c                   eval      festa = 'SI'
012500060213     c                   endif
012600060213      * verifico che giorno è della settimana
012700060213     c                   Clear                   wgioset
012800060213     c                   Movel     dtaggmm       wggmmaa
012900060213     c                   Move      dtaaa         wggmmaa
013000060213     c                   Call      'XGIOSE1'
013100060213     c                   Parm                    wggmmaa
013200060213     c                   Parm                    wgioset
013300060202
013400060921      * se è lunedì  non si lancia il giornaliero
013500060213
013600060213     c                   If        wgioset = '1'
013700060221     c                   eval      mensile = 'SI'
013800060213     c                   endif
013900060221
014000060221      * se primo giorno  del mese
014100060221     c                   If        dtagg = 01
014200060221     c                   eval      mensile = 'SI'
014300070629      * faccio la pulizia dei membri del file di lavoro manca tariffa
014400070629     c                   exsr      SR_pulmbr
014500070629
014600060221     c                   endif
014700060221
014800060221      * se giorno di festa e non devo fare l'elaborazione mensile vado a fine
014900060221     c                   If        festa = 'SI' and mensile = '  '
015000060221     c                   goto      fine
015100060221     c                   endif
015200060213
015300060221      * preparo le date di elaborazione se giornaliera
015400060213      * calcolo partendo da udate cercando 4 giorni lavorativi
015500060213      * all'indietro
015600060221     c                   If        mensile = '  '
015700060213     c     dataiso       subdur    1:*d          dataiso2
015800060213     c                   do        *hival
015900060213     c                   move      dataiso2      dadata
016000060213     c                   move      dataiso       adata
016100060213     c                   CALL      'XSRLAV8'
016200060213     c                   PARM                    Wdata8
016300060213      * se corrisponde ho trovato la data al
016400060213     c                   if        giolav = 4
016500060213     c                   leave
016600060213     c                   else
016700060213      * altrimenti continuo a sottrarre i giorni da udate
016800060213     c     dataiso2      subdur    1:*d          dataiso2
016900060213     c                   iter
017000060213     c                   endif
017100060213     c                   enddo
017200060213      * ho stabilito la data spedizione dal
017300060213     c                   move      dataiso2      D44dsd
017400060301      * ho stabilito la data spedizione al
017500060301     c     dataiso       subdur    1:*d          dataiso1
017600060301     c                   move      dataiso1      D44dsa
017700060921      *
017800060222      * valorizza la data e l'ora di schedulazione  per il giornaliero se ora minore delle 6
017900060308     c                   if        wora < 60000
018000060222     c                   clear                   kbuff
018100060222     c                   move      udate         d1dats
018200060308     c                   move      '0600'        d1oras
018300060222     c                   movel     Scheds        kbuff
018400060222     c                   endif
018500060228      * stampa imponibile
018600060228     c                   eval      d44fsi = 'N'
018700060228      * stampa separata per P.O.
018800060228     c                   eval      d44spp = 'S'
018900060228      * stampa unica
019000060228     c                   eval      d44sun = 'N'
019100060228      * coda di stampa
019200060228     c                   eval      d44out = 'O'
019300060228      * creazione file
019400060228     c                   eval      d44crf = 'N'
019500060228      * totali per area
019600060228     c                   eval      d44tot = 'N'
019700060228      * stampante totali per area
019800060228     c                   eval      d44Tou = 'T'
019900060921      * valorizzo il flag della giornaliera
020000060921     c                   eval      d44gio = 'S'
020100060228
020200060221
020300060221     c                   else
020400060221      * MENSILE
020500060301     c                   move      19900101      D44dsd
020600060301      * ho stabilito la data spedizione al
020700060301     c     dataiso       subdur    1:*d          dataiso1
020800060301     c                   move      dataiso1      D44dsa
020900060213      * stampa imponibile
021000060213     c                   eval      d44fsi = 'N'
021100060213      * stampa separata per P.O.
021200060213     c                   eval      d44spp = 'S'
021300060213      * stampa unica
021400060213     c                   eval      d44sun = 'N'
021500060213      * coda di stampa
021600060213     c                   eval      d44out = 'O'
021700060221      * creazione file
021800060221     c                   eval      d44crf = 'S'
021900060307     c                   movel     dateu         com8a
022000060228     c                   eval      D44mbr = 'M'+ com8a
022100060221      * totali per area
022200060221     c                   eval      d44tot = 'S'
022300060222      * stampante totali per area
022400060222     c                   eval      d44Tou = 'T'
022500060921      * pulisco il flag della giornaliera
022600060921     c                   eval      d44gio = ' '
022700060222
022800060222     c                   endif
022900060213
023000060213      * LANCIO
023100060213     c                   movel     tnsb44ds      Kpjbu
023200060213     c                   movel     'SB45'        Kcoaz
023300060213     c                   call      'BCH10'
023400060213     c                   parm                    Kpjba
023500120402      *
023600120402      *
023700120402      * - Elenco spedizioni "Manca Tariffa" a SAS
023800120402      *   (solo mensile)
023900120402      *
024000120402if  1c                   If             mensile = 'SI'
024100120402     c                             and  dtagg   = 01
024200120402     c                   clear                   Kpjbu
024300120402     c                   movel     'SB66'        Kcoaz
024400120402     c                   call      'BCH10'
024500120402     c                   parm                    Kpjba
024600120402e   1c                   EndIf
024700060213
024800060213     c     fine          tag
024900060213     c                   seton                                        lr
025000070629      *------------------------------------------------------------------------*
025100070629      *      Routine di richiamo pulizia membri WFMTC00F vecchi                *
025200070629      *------------------------------------------------------------------------*
025300070629     c     Sr_pulmbr     Begsr
025400070629
025500070629      * recupero dalla tabella 3S i giorni di tenuta massima dei membri
025600070629     c                   Eval      Cod = '3S'
025700070629     c                   Eval      Key = 'MTC   '
025800070629
025900070703     c     Keytab        Chain     Tabel00f
026000070629
026100070703    1c                   If        %found(tabel00f) and tblflg = ' '
026200070629     c                   movel     tbluni        Ds3s
026300070629
026400070629      * Nome file
026500070629     c                   movel(P)  §3sars        File
026600070629      * Nome libreria
026700070629      * Se sono in ambiente buono - GAITRAAZM
026800070703    2c                   If        knsif = 'GAITRA201 '
026900070629     c                   Eval      libr = 'GAITRAAZM '
027000070629      *  Se sono in ambiente di prova - GAITRAFIL
027100070629     c                   Else
027200070629     c                   Eval      libr = 'GAITRAFIL'
027300070703    2c                   EndIf
027400070629      * Data limite Pulizia
027500070629     c     giorni        SUB       §3SCAD        GIOTGI
027600070629     c                   CALL      'XSRGI8'
027700070629     c                   PARM                    WGIDAT
027800070629      *
027900070629     c                   z-add     gioinv        data
028000070629     c                   movel     param         kpjbu
028100070629     c                   CALL      'TRTC21C'
028200070629     c                   PARM                    KPJBA
028300070703
028400070703    1c                   EndIf
028500070629
028600070629     c                   endsr
