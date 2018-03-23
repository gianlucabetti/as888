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
003600160222     d DataIsoP        s               d   datfmt(*iso)
003700060105      *------------------------------------------------------------------------*
003800060105      *   S C H I E R E
003900060105      *------------------------------------------------------------------------*
004000060105
004100060105      *------------------------------------------------------------------------*
004200060105      *   D S   I N T E R N E / E S T E R N E
004300060105      *------------------------------------------------------------------------*
004400070629
004500060105     d wlbdat          ds                  Inz
004600060105     d  g02dat                 1      8  0
004700060105     d  g02inv                 9     16  0
004800060105     d  g02err                17     17
004900060105     d  g02tgi                18     22  0
005000060213
005100060213     d                 ds
005200060213     d Wdata                   1      8  0
005300060221     d dtagg                   1      2  0
005400060213     d dtaggmm                 1      4  0
005500060213     d dtaaa                   7      8  0
005600060207
005700060206
005800060213     d Wdata8          DS
005900060213     d  dadata                 1      8  0
006000060213     d  adata                  9     16  0
006100060213     d  GioLav                17     21  0
006200060213
006300060222      * Data e ora shedulazione per Kpjba
006400060222     d Scheds          DS
006500060222     d  D1Dats                 1      6
006600060222     d  D1Oras                 7     10
006700150929     D*-------------------
006800150929     D* DS  V O L U M I
006900150929     D*-------------------
007000150929     D PARVOL          DS                  INZ
007100150929     D  VOLTLA                 1      1
007200150929     D  VOLANN                 2      5  0
007300150929     D  VOLMMI                 6      7  0
007400150929     D  VOLMMF                 8      9  0
007500150929     D  VOLSNR                10     10
007600150929     D  VOLPID                27     31  2
007700150929     D  VOLDIV                32     32
007800150929     D  VOLARE                33     35  0
007900150929     D  VOLFIL                36     38  0
008000150929     D  VOLDET                39     39
008100150929     D  VOLTOT                40     40
008200150929     D  VOLRG1                41     44  1
008300150929     D  VOLRG2                45     48  1
008400150929     D  VOLPIA                49     53  2
008500150929     D  VOLPSD                54     58  2
008600150929     D  VOLPSA                59     63  2
008700150929     D  VOLREA                64     64
008800150929     D  VOLOK                 65     66
008900150929     D  VOLMIN                67     71  3
009000150929     D  VOLPOS                72     72
009100150929     D  VOLNT1                73     75    inz(*blanks)
009200150929     D  VOLNT2                76     78    inz(*blanks)
009300150929     D  VOLNT3                79     81    inz(*blanks)
009400150929     D  VOLNT4                82     84    inz(*blanks)
009500150929     D  VOLSAL                85     85
009600150929     D*-------------------
009700150929     D* DS  P E S I
009800150929     D*-------------------
009900150929     D PARPES          DS                  INZ
010000150929     D  PESTLA                 1      1
010100150929     D  PESANN                 2      5  0
010200150929     D  PESMMI                 6      7  0
010300150929     D  PESMMF                 8      9  0
010400150929     D  PESSNR                10     10
010500150929     D  PESPEI                27     31  2
010600150929     D  PESDIV                32     32
010700150929     D  PESARE                33     35  0
010800150929     D  PESFIL                36     38  0
010900150929     D  PESDET                39     39
011000150929     D  PESTOT                40     40
011100150929     D  PESRG1                41     44  1
011200150929     D  PESRG2                45     48  1
011300150929     D  PESTAR                49     53  3
011400150929     D  PESPES                54     58  2
011500150929     D  PESMIN                59     63  3
011600150929     D  PESPOS                64     64
011700150929     D  PESNEG                65     65
011800150929     D  PESSAL                66     66
011900060222      *
012000060206     d Tnsb44ds      e ds
012100070629
012200150929     d DSTP          e dS
012300060206
012400060105     d Kpjba         e ds
012500160222
012600160222      // - Parametri pgm pulizia
012700160222     d ParmTNSF10      DS
012800160222     d  Libreria               1     10
012900160222     d  File                  11     20
013000160222     d  Data                  21     28  0
013100160222
013200160222      //---------------------------------------------------------------
013300160222      //?Definizione procedure esterne.
013400160222      //---------------------------------------------------------------
013500160222      // - Pulizia File
013600160222     d tnsf10c         pr                  extpgm('TNSF10C')
013700160222     d  kpjba                              likeds(kpjba)
013800060105
013900060105      *------------------------------------------------------------------------*
014000060105      *   I N D I C A T O R I
014100060105      *------------------------------------------------------------------------*
014200060105
014300060105      *------------------------------------------------------------------------*
014400060105      * CICLO PRINCIPALE
014500060105      *------------------------------------------------------------------------*
014600070704      *------------------------------------------------------------------------*
014700070704      * KLIST
014800070704      *------------------------------------------------------------------------*
014900070704      *---- File Tabel00f
015000070704
015100070704     c     KeyTab        Klist
015200070704     c                   Kfld                    Codut
015300070704     c                   Kfld                    cod
015400070704     c                   Kfld                    key
015500060213
015600060109     c     *entry        plist
015700060109     c                   parm                    Kpjba
015800060307     c                   clear                   kbuff
015900070629     c                   z-add     1             codut             1 0
016000070629      *
016100070704     c                   clear                   tnsb44ds
016200070704      * Reperisco data corrente
016300060109     c                   time                    w0140
016400060109     c                   move      w0140         wData
016500060222      * ora
016600060222     c                   movel     w0140         wora
016700060206
016800060213     c                   clear                   WLBdat
016900060213     c                   eval      G02dat = wData
017000060213     c                   call      'XSRDA8'
017100060213     c                   parm                    WLBdat
017200060213     c                   move      G02inv        Dataiso
017300060307     c                   z-add     G02inv        DateU
017400070629     c                   z-add     G02tgi        giorni            5 0
017500060213
017600060213      * controllo se udate è festivo non lancio l'elaborazione
017700060213     c                   move      dataiso       dadata
017800060213     c                   move      dataiso       adata
017900060213     c                   CALL      'XSRLAV8'
018000060213     c                   PARM                    Wdata8
018100060213     c                   if        giolav = 0
018200060221     c                   eval      festa = 'SI'
018300060213     c                   endif
018400060213      * verifico che giorno è della settimana
018500060213     c                   Clear                   wgioset
018600060213     c                   Movel     dtaggmm       wggmmaa
018700060213     c                   Move      dtaaa         wggmmaa
018800060213     c                   Call      'XGIOSE1'
018900060213     c                   Parm                    wggmmaa
019000060213     c                   Parm                    wgioset
019100060202
019200060921      * se è lunedì  non si lancia il giornaliero
019300060213
019400060213     c                   If        wgioset = '1'
019500060221     c                   eval      mensile = 'SI'
019600060213     c                   endif
019700060221
019800060221      * se primo giorno  del mese
019900060221     c                   If        dtagg = 01
020000060221     c                   eval      mensile = 'SI'
020100070629      * faccio la pulizia dei membri del file di lavoro manca tariffa
020200070629     c                   exsr      SR_pulmbr
020300070629
020400060221     c                   endif
020500060221
020600060221      * se giorno di festa e non devo fare l'elaborazione mensile vado a fine
020700060221     c                   If        festa = 'SI' and mensile = '  '
020800060221     c                   goto      fine
020900060221     c                   endif
021000150929     c
021100150929     c* vedo se elaborare le statistiche pesi e volumi mensili
021200150929     c                   exsr      STATPESVOL
021300060213
021400060221      * preparo le date di elaborazione se giornaliera
021500060213      * calcolo partendo da udate cercando 4 giorni lavorativi
021600060213      * all'indietro
021700060221     c                   If        mensile = '  '
021800060213     c     dataiso       subdur    1:*d          dataiso2
021900060213     c                   do        *hival
022000060213     c                   move      dataiso2      dadata
022100060213     c                   move      dataiso       adata
022200060213     c                   CALL      'XSRLAV8'
022300060213     c                   PARM                    Wdata8
022400060213      * se corrisponde ho trovato la data al
022500060213     c                   if        giolav = 4
022600060213     c                   leave
022700060213     c                   else
022800060213      * altrimenti continuo a sottrarre i giorni da udate
022900060213     c     dataiso2      subdur    1:*d          dataiso2
023000060213     c                   iter
023100060213     c                   endif
023200060213     c                   enddo
023300060213      * ho stabilito la data spedizione dal
023400060213     c                   move      dataiso2      D44dsd
023500060301      * ho stabilito la data spedizione al
023600060301     c     dataiso       subdur    1:*d          dataiso1
023700060301     c                   move      dataiso1      D44dsa
023800060921      *
023900060222      * valorizza la data e l'ora di schedulazione  per il giornaliero se ora minore delle 6
024000060308     c                   if        wora < 60000
024100060222     c                   clear                   kbuff
024200060222     c                   move      udate         d1dats
024300060308     c                   move      '0600'        d1oras
024400060222     c                   movel     Scheds        kbuff
024500060222     c                   endif
024600060228      * stampa imponibile
024700060228     c                   eval      d44fsi = 'N'
024800060228      * stampa separata per P.O.
024900060228     c                   eval      d44spp = 'S'
025000060228      * stampa unica
025100060228     c                   eval      d44sun = 'N'
025200060228      * coda di stampa
025300060228     c                   eval      d44out = 'O'
025400060228      * creazione file
025500060228     c                   eval      d44crf = 'N'
025600060228      * totali per area
025700060228     c                   eval      d44tot = 'N'
025800060228      * stampante totali per area
025900060228     c                   eval      d44Tou = 'T'
026000060921      * valorizzo il flag della giornaliera
026100060921     c                   eval      d44gio = 'S'
026200060228
026300060221
026400060221     c                   else
026500060221      * MENSILE
026600060301     c                   move      19900101      D44dsd
026700060301      * ho stabilito la data spedizione al
026800060301     c     dataiso       subdur    1:*d          dataiso1
026900060301     c                   move      dataiso1      D44dsa
027000060213      * stampa imponibile
027100060213     c                   eval      d44fsi = 'N'
027200060213      * stampa separata per P.O.
027300060213     c                   eval      d44spp = 'S'
027400060213      * stampa unica
027500060213     c                   eval      d44sun = 'N'
027600060213      * coda di stampa
027700060213     c                   eval      d44out = 'O'
027800060221      * creazione file
027900060221     c                   eval      d44crf = 'S'
028000060307     c                   movel     dateu         com8a
028100060228     c                   eval      D44mbr = 'M'+ com8a
028200060221      * totali per area
028300060221     c                   eval      d44tot = 'S'
028400060222      * stampante totali per area
028500060222     c                   eval      d44Tou = 'T'
028600060921      * pulisco il flag della giornaliera
028700060921     c                   eval      d44gio = ' '
028800150929
028900150929      * valorizza la data e l'ora di schedulazione  per il giornaliero se ora minore delle 6
029000161206      * per il mensile se ora minore delle 3
029100150929     c                   clear                   kbuff
029200161206     c                   if        wora < 30000
029300150929     c                   move      udate         d1dats
029400161206     c                   move      '0300'        d1oras
029500150929     c                   movel     Scheds        kbuff
029600150929     c                   endif
029700161206      * se secondo giorno lavorativa del mese lo devo sottomettere con priorità 6
029800161206     c                   IF        giolav = 2
029900161206     c                   eval      kcojb = 'P6'
030000161206     c                   ENDIF
030100060222
030200060222     c                   endif
030300060213
030400060213      * LANCIO
030500060213     c                   movel     tnsb44ds      Kpjbu
030600060213     c                   movel     'SB45'        Kcoaz
030700060213     c                   call      'BCH10'
030800060213     c                   parm                    Kpjba
030900120402      *
031000120402      *
031100120402      * - Elenco spedizioni "Manca Tariffa" a SAS
031200120402      *   (solo mensile)
031300120402      *
031400120402if  1c                   If             mensile = 'SI'
031500120402     c                             and  dtagg   = 01
031600120402     c                   clear                   Kpjbu
031700120402     c                   movel     'SB66'        Kcoaz
031800120402     c                   call      'BCH10'
031900120402     c                   parm                    Kpjba
032000120402e   1c                   EndIf
032100060213
032200060213     c     fine          tag
032300060213     c                   seton                                        lr
032400070629      *------------------------------------------------------------------------*
032500070629      *      Routine di richiamo pulizia membri WFMTC00F vecchi                *
032600070629      *------------------------------------------------------------------------*
032700070629     c     Sr_pulmbr     Begsr
032800070629
032900160222      /free
033000160222      //?Imposto il file
033100160222         File = 'WFMTC00F';
033200160222
033300160222      //?Imposto la libreria
033400160222         IF  %subst(knsif:7:1) = 'P';
033500160222           Libreria = 'GAITRAFIL';
033600160222         ELSE;
033700160222           Libreria = 'GAITRAAZM';
033800160222         ENDIF;
033900160222
034000160222      //?Calcolo la data
034100160222      //?Oggi - 30 gg.
034200160222         DataIsoP = DataISO - %days(30);
034300160222         Data = %dec(DataIsoP);
034400160222
034500160222      //?Lancio la pulizia
034600160222        kpjbu = parmTNSF10;
034700160222        tnsf10c (kpjba);
034800160222
034900160222      /end-free
035000070629
035100070629     c                   endsr
035200150929      *------------------------------------------------------------------------*
035300150929      *      Routine di lancio statistiche pesi e volumi                       *
035400150929      *------------------------------------------------------------------------*
035500150929     c     statPesVol    Begsr
035600150929     c* Lo devo fare il secondo giorno lavorativo del mese
035700150929      * controllo se udate è festivo non lancio l'elaborazione
035800150929     c                   move      dateu         DataisoST
035900150929     c                   move      dataisoST     adata
036000150929     c                   z-add     dateu         iniziom           8 0
036100150929     c                   move      01            iniziom
036200150929     c                   move      iniziom       DataisoST
036300150929     c                   move      dataisoST     dadata
036400150929     c                   CALL      'XSRLAV8'
036500150929     c                   PARM                    Wdata8
036600150929     c
036700150929     c                   if        giolav = 2
036800150929     c* Sottometto le statistiche pesi e volumi
036900150929     c* del mese precedente
037000150929     c                   move      dateu         DataisoST
037100150929     c     dataisost     subdur    1:*m          dataisost
037200150929     c                   movel     dataisoST     w0080             8 0
037300150929     c                   movel     w0080         datalanc          6 0
037400150929     c                   eval      savmus=knmus
037500150929     c* forzo profilo EDPSTA
037600150929     c                   eval      knmus='EDPSTA    '
037700150929     c                   exsr      lanvol
037800150929     c                   exsr      lanpes
037900150929     c                   eval      knmus =savmus
038000161205      * dopo aver sottomesso la statistica pesi e volumi
038100161205      * imposto che il manca tariffa da lanciare subito dopo è mensile
038200161205     c                   eval      mensile = 'SI'
038300150929     c                   endif
038400150929     c*
038500150929     c                   endsr
038600150929     C*------------------------------------------------------------------------*
038700150929     C* LANVOL - LANCIA LA STATISTICA VOLUMI
038800150929     C*------------------------------------------------------------------------*
038900150929     C     LANVOL        BEGSR
039000150929     C***
039100150929     C* 1° LANCIO
039200150929     C***
039300150929     C                   CLEAR                   KPJBU
039400150929     C                   movel     datalanc      VOLANN                         *ANNO
039500150929     C                   Z-ADD     01            VOLMMI                         *MESE INIZIALE
039600150929     C                   Z-ADD     datalanc      VOLMMF                         *MESE FINALE
039700150929     C                   MOVEL     'S'           VOLSNR                         *RIGENERA WFILE
039800150929     C                   MOVEL     'S'           VOLSAL                         *RIGENERA WRKF Saldi
039900150929     C                   MOVEL     'N'           VOLPOS                         *NO DATI POSTE
040000150929     C* INSERISCO DATI DFT DA TABELLA "TP"
040100150929     c                   Eval      Cod = 'TP'
040200150929     c                   Eval      Key = '1       '
040300150929     C     KEYTAB        CHAIN     TABEL00F                           98
040400150929IF  1c                   if        *in98
040500150929     c                   leavesr
040600150929     c                   endif
040700150929     C                   MOVEL     TBLUNI        DSTP
040800150929     C                   Z-ADD     §TVDPD        VOLRG1
040900150929     C     VOLRG1        MULT      -1            VOLRG1
041000150929     C                   Z-ADD     §TVDPA        VOLRG2
041100150929     C                   Z-ADD     §TVCID        VOLPID
041200150929     C                   Z-ADD     §TVCSD        VOLPSD
041300150929     C                   Z-ADD     §TVMIN        VOLMIN
041400150929     C                   MOVEL     'S'           VOLDET                         *SINTETICA
041500150929     C                   MOVEL     'N'           VOLTOT                         *NO TOTALI
041600150929     C                   MOVEL     'N'           VOLREA                         *NO "REALI"
041700150929     C                   MOVEL     'ST19'        KCOAZ
041800150929     C                   MOVEL     PARVOL        KPJBU
041900150929     C                   CALL      'BCH10'
042000150929     C                   PARM                    KPJBA
042100150929     C***
042200150929     C* 2° LANCIO
042300150929     C***
042400150929     C                   MOVEL     'N'           VOLSNR                         *NON RIGENERA WFILE
042500150929     C                   MOVEL     'N'           VOLSAL                         *NON RIGENERA WRKF S
042600150929     C                   MOVEL     'D'           VOLDET                         *DETTAGLIO
042700150929     C                   MOVEL     'ST19'        KCOAZ
042800150929     C                   MOVEL     PARVOL        KPJBU
042900150929     C                   CALL      'BCH10'
043000150929     C                   PARM                    KPJBA
043100150929     C*
043200150929     C                   ENDSR
043300150929     C*------------------------------------------------------------------------*
043400150929     C* LANPES - LANCIA LA STATISTICA PESI
043500150929     C*------------------------------------------------------------------------*
043600150929     C     LANPES        BEGSR
043700150929     C***
043800150929     C* 1° LANCIO
043900150929     C***
044000150929     C                   CLEAR                   KPJBU
044100150929     C                   movel     datalanc      PESANN                         *ANNO
044200150929     C                   Z-ADD     01            PESMMI                         *MESE INIZIALE
044300150929     C                   Z-ADD     datalanc      PESMMF                         *MESE FINALE
044400150929     C                   MOVEL     'S'           PESSNR                         *RIGENERA WFILE
044500150929     C                   MOVEL     'S'           PESSAL                         *RIGENERA WRKF Saldi
044600150929     C                   MOVEL     'N'           PESPOS                         *NO DATI POSTE
044700150929     C                   MOVEL     'N'           PESNEG                         *NO DATI POSTE
044800150929     C* INSERISCO DATI DFT DA TABELLA "TP"
044900150929     c                   Eval      Cod = 'TP'
045000150929     c                   Eval      Key = '1       '
045100150929     C     KEYTAB        CHAIN     TABEL00F                           98
045200150929IF  1c                   if        *in98
045300150929     c                   leavesr
045400150929     c                   endif
045500150929     C                   MOVEL     TBLUNI        DSTP
045600150929     C                   Z-ADD     §TPDPD        PESRG1
045700150929     C     PESRG1        MULT      -1            PESRG1
045800150929     C                   Z-ADD     §TPDPA        PESRG2
045900150929     C                   Z-ADD     §TPCOI        PESPEI
046000150929     C                   Z-ADD     §TPCOS        PESPES
046100150929     C                   Z-ADD     §TPTAR        PESTAR
046200150929     C                   Z-ADD     §TPMIN        PESMIN
046300150929     C                   MOVEL     'S'           PESDET                         *SINTETICA
046400150929     C                   MOVEL     'N'           PESTOT                         *NO TOTALI
046500150929     C                   MOVEL     'ST16'        KCOAZ
046600150929     C                   MOVEL     PARPES        KPJBU
046700150929     C                   CALL      'BCH10'
046800150929     C                   PARM                    KPJBA
046900150929     C***
047000150929     C* 2° LANCIO
047100150929     C***
047200150929     C                   MOVEL     'N'           PESSNR                         *NON RIGENERA WFILE
047300150929     C                   MOVEL     'N'           PESSAL                         *NON RIGENERA WRKF S
047400150929     C                   MOVEL     'D'           PESDET                         *DETTAGLIO
047500150929     C                   MOVEL     'ST16'        KCOAZ
047600150929     C                   MOVEL     PARPES        KPJBU
047700150929     C                   CALL      'BCH10'
047800150929     C                   PARM                    KPJBA
047900150929     C*
048000150929     C                   ENDSR
