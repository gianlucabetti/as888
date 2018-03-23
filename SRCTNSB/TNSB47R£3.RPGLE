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
002900150929     d savmus          S                   like(knmus)
003000070629
003100060213     d Datadmy         s               d   datfmt(*dmy)
003200060213     d dataiso         s               d   datfmt(*iso)
003300060301     d dataiso1        s               d   datfmt(*iso)
003400060213     d dataiso2        s               d   datfmt(*iso)
003500150929     d dataisoST       s               d   datfmt(*iso)
003600060105      *------------------------------------------------------------------------*
003700060105      *   S C H I E R E
003800060105      *------------------------------------------------------------------------*
003900060105
004000060105      *------------------------------------------------------------------------*
004100060105      *   D S   I N T E R N E / E S T E R N E
004200060105      *------------------------------------------------------------------------*
004300060105
004400070629     d Param           DS
004500070629     d  Libr                   1     10
004600070629     d  File                  11     20
004700070629     d  Data                  21     26  0
004800070629
004900060105     d wlbdat          ds                  Inz
005000060105     d  g02dat                 1      8  0
005100060105     d  g02inv                 9     16  0
005200060105     d  g02err                17     17
005300060105     d  g02tgi                18     22  0
005400070629
005500070629     d Wgidat          DS                  inz
005600070629     d  giodat                 1      8  0
005700070629     d  gioinv                 9     16  0
005800070629     d  giotgi                17     21  0
005900060213
006000060213     d                 ds
006100060213     d Wdata                   1      8  0
006200060221     d dtagg                   1      2  0
006300060213     d dtaggmm                 1      4  0
006400060213     d dtaaa                   7      8  0
006500060207
006600060206
006700060213     d Wdata8          DS
006800060213     d  dadata                 1      8  0
006900060213     d  adata                  9     16  0
007000060213     d  GioLav                17     21  0
007100060213
007200060222      * Data e ora shedulazione per Kpjba
007300060222     d Scheds          DS
007400060222     d  D1Dats                 1      6
007500060222     d  D1Oras                 7     10
007600150929     D*-------------------
007700150929     D* DS  V O L U M I
007800150929     D*-------------------
007900150929     D PARVOL          DS                  INZ
008000150929     D  VOLTLA                 1      1
008100150929     D  VOLANN                 2      5  0
008200150929     D  VOLMMI                 6      7  0
008300150929     D  VOLMMF                 8      9  0
008400150929     D  VOLSNR                10     10
008500150929     D  VOLPID                27     31  2
008600150929     D  VOLDIV                32     32
008700150929     D  VOLARE                33     35  0
008800150929     D  VOLFIL                36     38  0
008900150929     D  VOLDET                39     39
009000150929     D  VOLTOT                40     40
009100150929     D  VOLRG1                41     44  1
009200150929     D  VOLRG2                45     48  1
009300150929     D  VOLPIA                49     53  2
009400150929     D  VOLPSD                54     58  2
009500150929     D  VOLPSA                59     63  2
009600150929     D  VOLREA                64     64
009700150929     D  VOLOK                 65     66
009800150929     D  VOLMIN                67     71  3
009900150929     D  VOLPOS                72     72
010000150929     D  VOLNT1                73     75    inz(*blanks)
010100150929     D  VOLNT2                76     78    inz(*blanks)
010200150929     D  VOLNT3                79     81    inz(*blanks)
010300150929     D  VOLNT4                82     84    inz(*blanks)
010400150929     D  VOLSAL                85     85
010500150929     D*-------------------
010600150929     D* DS  P E S I
010700150929     D*-------------------
010800150929     D PARPES          DS                  INZ
010900150929     D  PESTLA                 1      1
011000150929     D  PESANN                 2      5  0
011100150929     D  PESMMI                 6      7  0
011200150929     D  PESMMF                 8      9  0
011300150929     D  PESSNR                10     10
011400150929     D  PESPEI                27     31  2
011500150929     D  PESDIV                32     32
011600150929     D  PESARE                33     35  0
011700150929     D  PESFIL                36     38  0
011800150929     D  PESDET                39     39
011900150929     D  PESTOT                40     40
012000150929     D  PESRG1                41     44  1
012100150929     D  PESRG2                45     48  1
012200150929     D  PESTAR                49     53  3
012300150929     D  PESPES                54     58  2
012400150929     D  PESMIN                59     63  3
012500150929     D  PESPOS                64     64
012600150929     D  PESNEG                65     65
012700150929     D  PESSAL                66     66
012800060222      *
012900060206     d Tnsb44ds      e ds
013000070629
013100070629     d Ds3s          e dS
013200150929     d DSTP          e dS
013300060206
013400060105     d Kpjba         e ds
013500060105
013600060105      *------------------------------------------------------------------------*
013700060105      *   I N D I C A T O R I
013800060105      *------------------------------------------------------------------------*
013900060105
014000060105      *------------------------------------------------------------------------*
014100060105      * CICLO PRINCIPALE
014200060105      *------------------------------------------------------------------------*
014300070704      *------------------------------------------------------------------------*
014400070704      * KLIST
014500070704      *------------------------------------------------------------------------*
014600070704      *---- File Tabel00f
014700070704
014800070704     c     KeyTab        Klist
014900070704     c                   Kfld                    Codut
015000070704     c                   Kfld                    cod
015100070704     c                   Kfld                    key
015200060213
015300060109     c     *entry        plist
015400060109     c                   parm                    Kpjba
015500060307     c                   clear                   kbuff
015600070629     c                   z-add     1             codut             1 0
015700070629      *
015800070704     c                   clear                   tnsb44ds
015900070704      * Reperisco data corrente
016000060109     c                   time                    w0140
016100060109     c                   move      w0140         wData
016200060222      * ora
016300060222     c                   movel     w0140         wora
016400060206
016500060213     c                   clear                   WLBdat
016600060213     c                   eval      G02dat = wData
016700060213     c                   call      'XSRDA8'
016800060213     c                   parm                    WLBdat
016900060213     c                   move      G02inv        Dataiso
017000060307     c                   z-add     G02inv        DateU
017100070629     c                   z-add     G02tgi        giorni            5 0
017200060213
017300060213      * controllo se udate è festivo non lancio l'elaborazione
017400060213     c                   move      dataiso       dadata
017500060213     c                   move      dataiso       adata
017600060213     c                   CALL      'XSRLAV8'
017700060213     c                   PARM                    Wdata8
017800060213     c                   if        giolav = 0
017900060221     c                   eval      festa = 'SI'
018000060213     c                   endif
018100060213      * verifico che giorno è della settimana
018200060213     c                   Clear                   wgioset
018300060213     c                   Movel     dtaggmm       wggmmaa
018400060213     c                   Move      dtaaa         wggmmaa
018500060213     c                   Call      'XGIOSE1'
018600060213     c                   Parm                    wggmmaa
018700060213     c                   Parm                    wgioset
018800060202
018900060921      * se è lunedì  non si lancia il giornaliero
019000060213
019100060213     c                   If        wgioset = '1'
019200060221     c                   eval      mensile = 'SI'
019300060213     c                   endif
019400060221
019500060221      * se primo giorno  del mese
019600060221     c                   If        dtagg = 01
019700060221     c                   eval      mensile = 'SI'
019800070629      * faccio la pulizia dei membri del file di lavoro manca tariffa
019900070629     c                   exsr      SR_pulmbr
020000070629
020100060221     c                   endif
020200060221
020300060221      * se giorno di festa e non devo fare l'elaborazione mensile vado a fine
020400060221     c                   If        festa = 'SI' and mensile = '  '
020500060221     c                   goto      fine
020600060221     c                   endif
020700150929     c
020800150929     c* vedo se elaborare le statistiche pesi e volumi mensili
020900150929     c                   exsr      STATPESVOL
021000060213
021100060221      * preparo le date di elaborazione se giornaliera
021200060213      * calcolo partendo da udate cercando 4 giorni lavorativi
021300060213      * all'indietro
021400060221     c                   If        mensile = '  '
021500060213     c     dataiso       subdur    1:*d          dataiso2
021600060213     c                   do        *hival
021700060213     c                   move      dataiso2      dadata
021800060213     c                   move      dataiso       adata
021900060213     c                   CALL      'XSRLAV8'
022000060213     c                   PARM                    Wdata8
022100060213      * se corrisponde ho trovato la data al
022200060213     c                   if        giolav = 4
022300060213     c                   leave
022400060213     c                   else
022500060213      * altrimenti continuo a sottrarre i giorni da udate
022600060213     c     dataiso2      subdur    1:*d          dataiso2
022700060213     c                   iter
022800060213     c                   endif
022900060213     c                   enddo
023000060213      * ho stabilito la data spedizione dal
023100060213     c                   move      dataiso2      D44dsd
023200060301      * ho stabilito la data spedizione al
023300060301     c     dataiso       subdur    1:*d          dataiso1
023400060301     c                   move      dataiso1      D44dsa
023500060921      *
023600060222      * valorizza la data e l'ora di schedulazione  per il giornaliero se ora minore delle 6
023700060308     c                   if        wora < 60000
023800060222     c                   clear                   kbuff
023900060222     c                   move      udate         d1dats
024000060308     c                   move      '0600'        d1oras
024100060222     c                   movel     Scheds        kbuff
024200060222     c                   endif
024300060228      * stampa imponibile
024400060228     c                   eval      d44fsi = 'N'
024500060228      * stampa separata per P.O.
024600060228     c                   eval      d44spp = 'S'
024700060228      * stampa unica
024800060228     c                   eval      d44sun = 'N'
024900060228      * coda di stampa
025000060228     c                   eval      d44out = 'O'
025100060228      * creazione file
025200060228     c                   eval      d44crf = 'N'
025300060228      * totali per area
025400060228     c                   eval      d44tot = 'N'
025500060228      * stampante totali per area
025600060228     c                   eval      d44Tou = 'T'
025700060921      * valorizzo il flag della giornaliera
025800060921     c                   eval      d44gio = 'S'
025900060228
026000060221
026100060221     c                   else
026200060221      * MENSILE
026300060301     c                   move      19900101      D44dsd
026400060301      * ho stabilito la data spedizione al
026500060301     c     dataiso       subdur    1:*d          dataiso1
026600060301     c                   move      dataiso1      D44dsa
026700060213      * stampa imponibile
026800060213     c                   eval      d44fsi = 'N'
026900060213      * stampa separata per P.O.
027000060213     c                   eval      d44spp = 'S'
027100060213      * stampa unica
027200060213     c                   eval      d44sun = 'N'
027300060213      * coda di stampa
027400060213     c                   eval      d44out = 'O'
027500060221      * creazione file
027600060221     c                   eval      d44crf = 'S'
027700060307     c                   movel     dateu         com8a
027800060228     c                   eval      D44mbr = 'M'+ com8a
027900060221      * totali per area
028000060221     c                   eval      d44tot = 'S'
028100060222      * stampante totali per area
028200060222     c                   eval      d44Tou = 'T'
028300060921      * pulisco il flag della giornaliera
028400060921     c                   eval      d44gio = ' '
028500150929
028600150929      * valorizza la data e l'ora di schedulazione  per il giornaliero se ora minore delle 6
028700150929     c                   clear                   kbuff
028800150929     c                   if        wora < 30000
028900150929     c                   move      udate         d1dats
029000150929     c                   move      '0300'        d1oras
029100150929     c                   movel     Scheds        kbuff
029200150929     c                   endif
029300060222
029400060222     c                   endif
029500060213
029600060213      * LANCIO
029700060213     c                   movel     tnsb44ds      Kpjbu
029800060213     c                   movel     'SB45'        Kcoaz
029900060213     c                   call      'BCH10'
030000060213     c                   parm                    Kpjba
030100120402      *
030200120402      *
030300120402      * - Elenco spedizioni "Manca Tariffa" a SAS
030400120402      *   (solo mensile)
030500120402      *
030600120402if  1c                   If             mensile = 'SI'
030700120402     c                             and  dtagg   = 01
030800120402     c                   clear                   Kpjbu
030900120402     c                   movel     'SB66'        Kcoaz
031000120402     c                   call      'BCH10'
031100120402     c                   parm                    Kpjba
031200120402e   1c                   EndIf
031300060213
031400060213     c     fine          tag
031500060213     c                   seton                                        lr
031600070629      *------------------------------------------------------------------------*
031700070629      *      Routine di richiamo pulizia membri WFMTC00F vecchi                *
031800070629      *------------------------------------------------------------------------*
031900070629     c     Sr_pulmbr     Begsr
032000070629
032100070629      * recupero dalla tabella 3S i giorni di tenuta massima dei membri
032200070629     c                   Eval      Cod = '3S'
032300070629     c                   Eval      Key = 'MTC   '
032400070629
032500070703     c     Keytab        Chain     Tabel00f
032600070629
032700070703    1c                   If        %found(tabel00f) and tblflg = ' '
032800070629     c                   movel     tbluni        Ds3s
032900070629
033000070629      * Nome file
033100070629     c                   movel(P)  §3sars        File
033200070629      * Nome libreria
033300070629      * Se sono in ambiente buono - GAITRAAZM
033400070703    2c                   If        knsif = 'GAITRA201 '
033500070629     c                   Eval      libr = 'GAITRAAZM '
033600070629      *  Se sono in ambiente di prova - GAITRAFIL
033700070629     c                   Else
033800070629     c                   Eval      libr = 'GAITRAFIL'
033900070703    2c                   EndIf
034000070629      * Data limite Pulizia
034100070629     c     giorni        SUB       §3SCAD        GIOTGI
034200070629     c                   CALL      'XSRGI8'
034300070629     c                   PARM                    WGIDAT
034400070629      *
034500070629     c                   z-add     gioinv        data
034600070629     c                   movel     param         kpjbu
034700070629     c                   CALL      'TRTC21C'
034800070629     c                   PARM                    KPJBA
034900070703
035000070703    1c                   EndIf
035100070629
035200070629     c                   endsr
035300150929      *------------------------------------------------------------------------*
035400150929      *      Routine di lancio statistiche pesi e volumi                       *
035500150929      *------------------------------------------------------------------------*
035600150929     c     statPesVol    Begsr
035700150929     c* Lo devo fare il secondo giorno lavorativo del mese
035800150929      * controllo se udate è festivo non lancio l'elaborazione
035900150929     c                   move      dateu         DataisoST
036000150929     c                   move      dataisoST     adata
036100150929     c                   z-add     dateu         iniziom           8 0
036200150929     c                   move      01            iniziom
036300150929     c                   move      iniziom       DataisoST
036400150929     c                   move      dataisoST     dadata
036500150929     c                   CALL      'XSRLAV8'
036600150929     c                   PARM                    Wdata8
036700150929     c
036800150929     c                   if        giolav = 2
036900150929     c* Sottometto le statistiche pesi e volumi
037000150929     c* del mese precedente
037100150929     c                   move      dateu         DataisoST
037200150929     c     dataisost     subdur    1:*m          dataisost
037300150929     c                   movel     dataisoST     w0080             8 0
037400150929     c                   movel     w0080         datalanc          6 0
037500150929     c                   eval      savmus=knmus
037600150929     c* forzo profilo EDPSTA
037700150929     c                   eval      knmus='EDPSTA    '
037800150929     c                   exsr      lanvol
037900150929     c                   exsr      lanpes
038000150929     c                   eval      knmus =savmus
038100150929     c                   endif
038200150929     c*
038300150929     c                   endsr
038400150929     C*------------------------------------------------------------------------*
038500150929     C* LANVOL - LANCIA LA STATISTICA VOLUMI
038600150929     C*------------------------------------------------------------------------*
038700150929     C     LANVOL        BEGSR
038800150929     C***
038900150929     C* 1° LANCIO
039000150929     C***
039100150929     C                   CLEAR                   KPJBU
039200150929     C                   movel     datalanc      VOLANN                         *ANNO
039300150929     C                   Z-ADD     01            VOLMMI                         *MESE INIZIALE
039400150929     C                   Z-ADD     datalanc      VOLMMF                         *MESE FINALE
039500150929     C                   MOVEL     'S'           VOLSNR                         *RIGENERA WFILE
039600150929     C                   MOVEL     'S'           VOLSAL                         *RIGENERA WRKF Saldi
039700150929     C                   MOVEL     'N'           VOLPOS                         *NO DATI POSTE
039800150929     C* INSERISCO DATI DFT DA TABELLA "TP"
039900150929     c                   Eval      Cod = 'TP'
040000150929     c                   Eval      Key = '1       '
040100150929     C     KEYTAB        CHAIN     TABEL00F                           98
040200150929IF  1c                   if        *in98
040300150929     c                   leavesr
040400150929     c                   endif
040500150929     C                   MOVEL     TBLUNI        DSTP
040600150929     C                   Z-ADD     §TVDPD        VOLRG1
040700150929     C     VOLRG1        MULT      -1            VOLRG1
040800150929     C                   Z-ADD     §TVDPA        VOLRG2
040900150929     C                   Z-ADD     §TVCID        VOLPID
041000150929     C                   Z-ADD     §TVCSD        VOLPSD
041100150929     C                   Z-ADD     §TVMIN        VOLMIN
041200150929     C                   MOVEL     'S'           VOLDET                         *SINTETICA
041300150929     C                   MOVEL     'N'           VOLTOT                         *NO TOTALI
041400150929     C                   MOVEL     'N'           VOLREA                         *NO "REALI"
041500150929     C                   MOVEL     'ST19'        KCOAZ
041600150929     C                   MOVEL     PARVOL        KPJBU
041700150929     C                   CALL      'BCH10'
041800150929     C                   PARM                    KPJBA
041900150929     C***
042000150929     C* 2° LANCIO
042100150929     C***
042200150929     C                   MOVEL     'N'           VOLSNR                         *NON RIGENERA WFILE
042300150929     C                   MOVEL     'N'           VOLSAL                         *NON RIGENERA WRKF S
042400150929     C                   MOVEL     'D'           VOLDET                         *DETTAGLIO
042500150929     C                   MOVEL     'ST19'        KCOAZ
042600150929     C                   MOVEL     PARVOL        KPJBU
042700150929     C                   CALL      'BCH10'
042800150929     C                   PARM                    KPJBA
042900150929     C*
043000150929     C                   ENDSR
043100150929     C*------------------------------------------------------------------------*
043200150929     C* LANPES - LANCIA LA STATISTICA PESI
043300150929     C*------------------------------------------------------------------------*
043400150929     C     LANPES        BEGSR
043500150929     C***
043600150929     C* 1° LANCIO
043700150929     C***
043800150929     C                   CLEAR                   KPJBU
043900150929     C                   movel     datalanc      PESANN                         *ANNO
044000150929     C                   Z-ADD     01            PESMMI                         *MESE INIZIALE
044100150929     C                   Z-ADD     datalanc      PESMMF                         *MESE FINALE
044200150929     C                   MOVEL     'S'           PESSNR                         *RIGENERA WFILE
044300150929     C                   MOVEL     'S'           PESSAL                         *RIGENERA WRKF Saldi
044400150929     C                   MOVEL     'N'           PESPOS                         *NO DATI POSTE
044500150929     C                   MOVEL     'N'           PESNEG                         *NO DATI POSTE
044600150929     C* INSERISCO DATI DFT DA TABELLA "TP"
044700150929     c                   Eval      Cod = 'TP'
044800150929     c                   Eval      Key = '1       '
044900150929     C     KEYTAB        CHAIN     TABEL00F                           98
045000150929IF  1c                   if        *in98
045100150929     c                   leavesr
045200150929     c                   endif
045300150929     C                   MOVEL     TBLUNI        DSTP
045400150929     C                   Z-ADD     §TPDPD        PESRG1
045500150929     C     PESRG1        MULT      -1            PESRG1
045600150929     C                   Z-ADD     §TPDPA        PESRG2
045700150929     C                   Z-ADD     §TPCOI        PESPEI
045800150929     C                   Z-ADD     §TPCOS        PESPES
045900150929     C                   Z-ADD     §TPTAR        PESTAR
046000150929     C                   Z-ADD     §TPMIN        PESMIN
046100150929     C                   MOVEL     'S'           PESDET                         *SINTETICA
046200150929     C                   MOVEL     'N'           PESTOT                         *NO TOTALI
046300150929     C                   MOVEL     'ST16'        KCOAZ
046400150929     C                   MOVEL     PARPES        KPJBU
046500150929     C                   CALL      'BCH10'
046600150929     C                   PARM                    KPJBA
046700150929     C***
046800150929     C* 2° LANCIO
046900150929     C***
047000150929     C                   MOVEL     'N'           PESSNR                         *NON RIGENERA WFILE
047100150929     C                   MOVEL     'N'           PESSAL                         *NON RIGENERA WRKF S
047200150929     C                   MOVEL     'D'           PESDET                         *DETTAGLIO
047300150929     C                   MOVEL     'ST16'        KCOAZ
047400150929     C                   MOVEL     PARPES        KPJBU
047500150929     C                   CALL      'BCH10'
047600150929     C                   PARM                    KPJBA
047700150929     C*
047800150929     C                   ENDSR
