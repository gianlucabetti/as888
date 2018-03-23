000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020605     F*          passaggio dati da PC ad AS x AGIP         *
000300020715     Fagip01L   Ip   E           K DISK    rename(agip:agip000)
000400020715     Fagiperr   o    E           K DISK    rename(agip:agip001)
000500020715     Fmzagi00f  o    E           K DISK
000600020715     Fmzagi01l  Uf   E           K DISK    prefix(£)
000700020709     F                                     rename(mzagi000:mzagi01)
000800020709     fqsysprt   o    f  132        printer oflind(*inof)
000900020709     D kpjba         E DS
001000020709     D ut§dse        E DS                  extname(ut§dse0f)
001100020709     d agidtus         s                   like(agidtu)
001200020709     d dataiso         s               d
001300020709     d dataeur         s               d   datfmt(*eur)
001400020709     d rsut            s             20
001500020605     c* flag annullamento
001600020605     c                   clear                   agiann
001700020605     c* numero fattura
001800020605     c                   testn                   numer00001           01
001900020605     c                   if        not *in01
002000020801     c                   clear                   aginrf
002100020605     c                   else
002200020605     c                   move      numer00001    aginrf
002300020605     c                   end
002400020605     c* data fattura
002500020605     c                   testn                   data_00001           01
002600020605     c                   if        not *in01
002700020605     c                   z-add     0             agidtf
002800020605     c                   else
002900020605     c                   move      data_00001    agidtf
003000020605     c                   end
003100020605     c* targa
003200020605     c                   movel     nominativo    agitga
003300020605     c                   eval      agitgn = %subst(nominativo: 3: 3)+' ' +
003400020605     c                             %subst(nominativo: 6: 2)
003500020605     c* data rifornimento
003600020605     c                   testn                   data_00003           01
003700020605     c                   if        not *in01
003800020605     c                   z-add     0             agidtu
003900020605     c                   else
004000020605     c                   move      data_00003    agidtu
004100020605     c                   end
004200020605     c* umità di misura
004300020605     c                   movel     un_mi00001    agiumi
004400020605     c* quantità
004500020605     c                   z-add     quantita      agiqta
004600020605     c* prezzo unitario
004700020605     c                   z-add     prezzo        agipru
004800020605     c* importo
004900020605     c                   z-add     importo       agiprt
005000020605     c* sconto
005100020605     c                   z-add     sconto_lt     agisc1
005200020605     c                   z-add     sconto        agisc2
005300020605     c* aliquota iva
005400021209     c                   MOVE      aliqu00001    agiali
005500020605     c* importo iva
005600020801     c                   z-add     impor00001    agiiiv
005700020605     c* imponibile
005800020605     c                   z-add     imponibile    agiimp
005900020605     c* km
006000020605     c                   z-add     chilometri    agikm
006100020605     c* divisa
006200020605     c                   movel     valuta        agidiv
006300020715     c* controllo che i km siano in sequenza sia per i record nuovi che per
006400020715     c* quelli già inseriti
006500020715     c                   exsr      srctr
006600020715     c                   if        not *in99
006700020715     c                   movel     'A'           AGIann
006800020715     c                   exsr      srctr
006900020715     c                   end
007000020715     c* scrivo MZAGI00F
007100020715     c                   if        not *in99
007200020715     c                   clear                   AGIann
007300020715     c     kagi          chain     mzagi01l
007400020715     c                   if        %found(mzagi01l)
007500020715     c                   add       agiqta        £agiqta
007600020715     c                   z-add     agikm         £agikm
007700020715     c                   update    mzagi01
007800020715     c                   else
007900020715     c                   write     mzagi000
008000020715     c                   end
008100020715     c                   end
008101021025     c* se non esistono anomalie lo stampo
008102021025     clr                 if        not *in90
008103021025     clr                 seton                                        02
008104021025     clr                 exsr      stampa
008105021025     clr                 end
008200020709     C**********************************************************************
008300020715     c* controlli
008400020709     C**********************************************************************
008500020715     C     srctr         BEGSR
008600020715     c                   setoff                                       99
008700020715     c     kagi          setgt     mzagi01l
008800020715     c     kagi          readpe    mzagi01l
008900020715     c                   if        not %eof(mzagi01l) and
009000020715     c                             £agikm > agikm
009001021025     c                   seton                                        1090
009100020715     c                   exsr      stampa
009200020715     c                   seton                                        99
009300020715     c                   end
009400020715     c                   endsr
009500020715     C**********************************************************************
009600020715     c* stampa errori
009700020715     C**********************************************************************
009800020715     C     stampa        BEGSR
009900020709     c*
010000020709     c   of              except    testa
010100020709     c                   setoff                                       of
010200020709     c                   move      agidtu        dataiso
010300020709     c                   move      dataiso       dataeur
010400020709     c                   move      dataeur       agidtus
010500021025     c   10              except    errore
010501021025     c   02              except    fine
010502021025     c                   if        not *in02
010600020715     c                   write     agip001
010601021025     c                   end
010602021025     c                   setoff                                       1002
010700020709     c*
010800020709     C                   ENDSR
010900020709     C**********************************************************************
011000020709     C     *inzsr        BEGSR
011100020709     C**********************************************************************
011200020709     C     *ENTRY        PLIST
011300020709     C                   PARM                    KPJBA
011400020709     C                   Z-ADD     1             CODUT
011500020709     C                   CALL      'X§PARUT'
011600020709     C                   PARM                    UT§DSE
011700020709     C                   MOVEL     RAGUT         RSUT
011800020709     c                   seton                                        of
011900020709     C     Kagi          KLIST                                                  *
012000020709     C                   KFLD                    agiann                         *
012100020709     C                   KFLD                    agitga                         *
012200020709     C                   KFLD                    agitgn                         *
012300020709     C                   KFLD                    agidtu                         *
012400020709     C                   ENDSR
012500020709     OQSYSPRT   E            TESTA          2 02
012600020709     O                       RSUT                20
012700020709     O                                         + 25 'LISTA ERRORI DURANTE'
012800020709     O                                         +  1 'IL CARICAMENTO DEL F'
012900020709     O                                         +  0 'ILE AGIP'
013000020709     O                                          113 'TRMZ50R'
013100020709     O                       UDATE              127 '  /  /  '
013200020709     O                       PAGE1         Z    132
013300020709     O          e            errore      1
013400020715     O                                         +  0 'Esiste un '
013500020715     O                                         +  0 'movimento con incogruenze '
013600020709     O                                         +  0 'Rif:'
013700020709     o                       agitga            +  1
013800020709     o                       agitgn            +  1
013900020709     O                                         +  1 'data'
014000020709     o                       agidtus       y   +  1
014100021025     O          e            fine        1
014200021025     O                                         + 50 '*** NESSUNA ANOMALIA ***'
