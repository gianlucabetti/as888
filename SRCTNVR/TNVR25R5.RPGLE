000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116      * TNVR23R *----------------------------------------------------*
000300020114      * RINUMERAZIONE CODICE VISITE E FILES COLLEGATI
000400020114      *--------------------------------------------------------------*
000500020117      *
000600020117      * Visite
000700020313     FEXTNVIS1  IF   E           K DISK    RENAME(TNVIS000:TNVIS1)
000800020313     F                                     prefix(B_)
000900020313     FTnVis05l  IF   E           K DISK
001000020313     Fazorg01l  IF   E           K DISK
001100020115      *
001200020111      * Note
001300020313     FTfNtc03l  iF   E           K DISK
001400020115      *
001500020115      * File con vecchi e nuovi numeri visita
001600020313     FTnWNr01l  IF   E           K DISK
001700020118      *
001800020313     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
001900020115      *----------------------------------------------------------------
002000020125      *
002100020125     D                 DS
002200020125     D  VisNew                 1      7  0
002300020313     D  w0060                  1      6  0
002400020313     D  w0010                  7      7  0
002500020118      *
002600020313     D Dsntc         E DS                  EXTNAME(tfntc00F)
002700020313     D DsntcB        E DS                  EXTNAME(tfntc00F) PREFIX(B_)
002800020118      *
002900020118      *
003000921027     C*****************************************************************
003100020313     c                   movel     'V'           kapl
003200020314     C* METTO A POSTO LE NOTE CHE TROVO IN EXTNVIS E NON IN TINVS05L
003300020314     c     kapl          setll     tfntc03L
003400020314     c     kapl          reade     tfntc03L                               30
003500020313     c                   clear                   savke1
003600020118      *
003700020313    1c     *in30         doweq     *off
003800020313     c* a rottura
003900020313     c                   clear                   novisita          1
004000020313     c                   clear                   visita
004100020313     c
004200020313     c**
004300020314     c                   move      ntcnk1        W0020             2 0
004400020314     c                   move      ntcnk1        w0070             7 0
004500020313     c
004600020313     c     w0070         chain     tnwnr01l                           31
004700020313    3c                   dow       not *in31 and visita=0
004800020313     c* se stesso ramo aziendale ok
004900020313     c     wnrpou        chain     azorg01l
005000020314     c     orgdit        ifeq      'SDI'
005100020313     c                   eval      visita=wnrnew
005200020313     c                   else
005300020313     c     w0070         reade     tnwnr01l                               31
005400020313     c                   endif
005500020313     c                   enddo
005600020313     c*
005700020313     c                   if        *in31
005800020314     c* se ultimi 2 numeri > 09 salto: si tratta di visita nuova
005900020314     c     w0020         ifgt      9
006000020314     c  n30
006100020314     can 30              except    stampa7
006200020314     c                   goto      sotto
006300020314     c                   else
006400020313     c                   z-add     w0020         w0010             1 0
006500020313     c                   movel     w0070         w0050             5 0
006600020313     c                   z-add     w0050         w0060             6 0
006700020313     c                   eval      visita=visnew
006800020313    3c                   endif
006900020314    3c                   endif
007000020314     C**
007100020314     C**
007200020314     C                   CLEAR                   ESISTEOLD         1
007300020314     c                   CLEAR                   CONVALIDA
007400020314     C                   EVAL      VISITAOLD=W0070
007500020314     C**
007600020314     c     visita        chain     tnvis05l                           32
007700020314     c**
007800020314     c* inesistente: cancellata??
007900020314    3c                   if        *in32
008000020314     c     visitaold     chain     tnvis05l                           42
008100020314     c                   if        *in42
008200020314     c                   eval      novisita='S'
008300020314     c  n30
008400020314     can 30              except    stampa2
008500020314     c* posso deletare il record
008600020314     c                   else
008700020314     c* si tratta di vistia scritta a modo nuovo
008800020314     c  n30
008900020314     can 30              except    stampa8
009000020314     c                   endif
009100020314     c
009200020314   x3c                   else
009300020314     C** CONTROLLO PRIMA SE ESISTE NE EXTNVIS: SE NON C'E' NON FACCIO NIENTE
009400020314     C     VISITAOLD     CHAIN     EXTNVIS1                           35
009500020314    4C                   IF        *IN35
009600020314     C                   EVAL      ESISTEOLD='N'
009700020314     c  n30
009800020314     can 30              except    stampa3
009900020314   X4C                   ELSE
010000020314     c
010100020314     c* memorizzo se e' stata convaLIDATA DOPO LA CONVERSIONE
010200020314    5c     visfsv        ifeq      'C'
010300020314     c     B_visfsv      ANDNE     'C'
010400020314     c                   MOVEL     'S'           CONVALIDA         1
010500020314    5c                   endif
010600020314     C*
010700020314     C* SE IL NUMERO VECCHIO NON ESISTE NEL NUOVO: E' SICURAMENTE DA AGG
010800020314     C     VISITAOLD     CHAIN     TNVIS05L                           36
010900020314     C*
011000020314    5C                   IF        *IN36
011100020314     C
011200020313     C                   IF        CONVALIDA='S'
011300020314     c  n30
011400020314     cAN 30              except    stampa
011500020313     C                   ENDIF
011600020314     c  n30
011700020314     can 30              except    stampa6
011800020313     c                   move      visita        ntcnk1
011900020313     c*******            update    tfntc
012000020314   x5C                   ELSE
012100020314     C*
012200020314     c                   move      ntcnk1        ntcnk1
012300020314     c                   except    stampa4
012400020314    5c                   endif
012500020314    4c                   endif
012600020313    3c                   endif
012700020313     c
012800020314     c     sotto         tag
012900020314     c     kapl          reade     tfntc03l                               30
013000020313     c                   enddo
013100020117      *
013200020115      *
013300020115     C                   SETON                                        LR
013400020114      *
013500020115      *---------------------------------------------------------------*
013600020114      *
013700020114      * DEFINIZIONE CAMPI
013800020115      *
013900020125      *---------------------------------------------------------------*
014000020314     C     *like         Define    visnrv        visitaold
014100020313     C     *like         Define    ntcapl        kapl
014200020313     C     *like         Define    visnrv        visita
014300020313     C     *like         Define    ntcnk1        savke1
014400020314     C     *like         Define    ntcnk1        knk1
014500020111      *
014600020115      *Tfntc
014700020313     C     KNtcold       Klist
014800020313     C                   Kfld                    B_NtcApl
014900020313     C                   Kfld                    KNK1
015000020313     C                   Kfld                    B_NtcNk2
015100020313     C                   Kfld                    B_ntctnt
015200020313     C     KNtc2         Klist
015300020313     C                   Kfld                    kapl
015400020313     C                   Kfld                    B_NtcNk1
015500020313     C     KNtc3         Klist
015600020313     C                   Kfld                    B_NtcApl
015700020313     C                   Kfld                    B_NtcNk1
015800020313     C                   Kfld                    B_NtcNk2
015900020313     C                   Kfld                    B_ntctnt
016000020313     C                   Kfld                    B_ntcrnt
016100020313     C                   Kfld                    B_ntcsns
016200020313     Oprtf198   E            stampa      1
016300020314     O                                         +  0 'visita NEW convalidata'
016400020314     O                                         +  0 '             '
016500020314     O                       VISITA            +  2 '      / '
016600020313     O          E            stampa2     1
016700020314     O                                         +  0 'Visita NEW inesistente'
016800020314     O                                         +  0 ':cancellata??'
016900020313     O                       visita            +  2 '      / '
017000020314     O                       NTCNK1            +  1
017100020314     O                       NTCTNT            +  1
017200020314     O                       NTCrNT            +  1
017300020314     O          E            stampa3     1
017400020314     O                                         +  0 'Visita OLD inesistente'
017500020314     O                                         +  0 '             '
017600020314     O                       visitaold         +  2 '      / '
017700020314     O                       NTCNK1            +  1
017800020314     O                       NTCTNT            +  1
017900020314     O                       NTCrNT            +  1
018000020314     O          E            stampa4     1
018100020314     O                                         +  0 'su quale visita va la '
018200020314     O                                         +  0 'nota? '
018300020314     O                       visitaold         +  2 '      / '
018400020314     O                       visita            +  2 '      / '
018500020314     O                                         +  2 'ora si trova sulla '
018600020314     O                                         +  0 'visita:'
018700020314     O                       visitaold         +  2 '      / '
018800020314     O                       NTCTNT            +  2
018900020314     O                       NTCrNT            +  1
019000020313     O          E            stampa6     1
019100020313     O                                         +  0 'Record aggiornato     '
019200020313     O                                         +  0 'in TFNTC '
019300020314     O                       visita            +  2 '      / '
019400020314     O                       NTCNK1            +  1
019500020314     O                       NTCTNT            +  1
019600020314     O                       NTCrNT            +  1
019700020314     O          E            stampa7     1
019800020314     O                                         +  0 'Record gia'' a modo NU'
019900020314     O                                         +  0 'OVO:  per PRG   '
020000020314     O                       w0050             +  2
020100020314     O                       w0020             +  2
020200020314     O                       NTCNK1            +  1
020300020314     O                       NTCTNT            +  1
020400020314     O                       NTCrNT            +  1
020500020314     O          E            stampa8     1
020600020314     O                                         +  0 'Record gia'' a modo NU'
020700020314     O                                         +  0 'OVO:old in tnvis'
020800020314     O                       visitaold         +  2 '      / '
020900020314     O                       NTCNK1            +  1
021000020314     O                       NTCTNT            +  1
021100020314     O                       NTCrNT            +  1
