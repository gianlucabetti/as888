000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116      * TNVR23R *----------------------------------------------------*
000300020114      * RINUMERAZIONE CODICE VISITE E FILES COLLEGATI
000400020114      *--------------------------------------------------------------*
000500020117      *
000600020313     ftfntcbar1 IF   E           K DISK    rename(TFNTC:TFNTC1)
000700020313     F                                     prefix(B_)
000800020117      *
000900020117      * Visite
001000020313     FEXTNVIS1  IF   E           K DISK    RENAME(TNVIS000:TNVIS1)
001100020313     F                                     prefix(B_)
001200020313     FTnVis05l  IF   E           K DISK
001300020313     Fazorg01l  IF   E           K DISK
001400020115      *
001500020111      * Note
001600020313     FTfNtc03l  UF A E           K DISK
001700020115      *
001800020115      * File con vecchi e nuovi numeri visita
001900020313     FTnWNr01l  IF   E           K DISK
002000020118      *
002100020313     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
002200020115      *----------------------------------------------------------------
002300020125      *
002400020125     D                 DS
002500020125     D  VisNew                 1      7  0
002600020313     D  w0060                  1      6  0
002700020313     D  w0010                  7      7  0
002800020118      *
002900020313     D Dsntc         E DS                  EXTNAME(tfntc00F)
003000020313     D DsntcB        E DS                  EXTNAME(tfntc00F) PREFIX(B_)
003100020118      *
003200020118      *
003300921027     C*****************************************************************
003400020313     c                   movel     'V'           kapl
003500020313     c     kapl          setll     tfntcbar1
003600020313     c     kapl          reade     tfntcbar1                              30
003700020313     c                   clear                   savke1
003800020118      *
003900020313    1c     *in30         doweq     *off
004000020313     c* a rottura
004100020313    2c     b_ntcnk1      ifne      savke1
004200020313     c                   eval      savke1=b_ntcnk1
004300020313     c                   clear                   novisita          1
004400020313     c                   clear                   visita
004500020313     c
004600020313     c**
004700020313     c                   move      B_ntcnk1      w0020             2 0
004800020313     c                   move      b_ntcnk1      w0070             7 0
004900020313     c
005000020313     c     w0070         chain     tnwnr01l                           31
005100020313    3c                   dow       not *in31 and visita=0
005200020313     c* se stesso ramo aziendale ok
005300020313     c     wnrpou        chain     azorg01l
005400020313     c     orgdit        ifeq      'SDI'
005500020313     c                   eval      visita=wnrnew
005600020313     c                   else
005700020313     c     w0070         reade     tnwnr01l                               31
005800020313     c                   endif
005900020313     c                   enddo
006000020313     c*
006100020313     c                   if        *in31
006200020313     c                   z-add     w0020         w0010             1 0
006300020313     c                   movel     w0070         w0050             5 0
006400020313     c                   z-add     w0050         w0060             6 0
006500020313     c                   eval      visita=visnew
006600020313    3c                   endif
006700020313     c**
006800020313     C                   CLEAR                   CONVALIDA
006900020313     c     visita        chain     tnvis05l                           32
007000020313     c**
007100020313     c* inesistente: cancellata??
007200020313    3c                   if        *in32
007300020313     c                   eval      novisita='S'
007400020313     c                   except    stampa2
007500020313     c* cancelliamo il record in tfntc
007600020313     c     kntc3         delete    tfntc03l                           40
007700020313   x3c                   else
007800020313     c
007900020313     c* stampare se convalidata
008000020313    3c     visfsv        ifeq      'C'
008100020313    2c                   move      b_ntcnk1      visitaold         7 0
008200020313     c     visitaold     chain     extnvis1                           35
008300020313    3c  N35B_visfsv      ifne      'C'
008400020313     c                   MOVEL     'S'           CONVALIDA         1
008500020313    3c                   endif
008600020313    3c                   endif
008700020313     c**
008800020313     c* mettiamo le note
008900020313     c* il 1 record ci dovrebbe essere
009000020313     C                   MOVE      B_NTCNK1      KNK1
009100020313     C                   MOVE      VISITA        KNK1
009200020313     c     kntcold       chain     tfntc03l                           33
009300020313    4c                   if        *in33
009400020313    4c                   if        B_NTCFLT<>'A'
009500020313     c* stampa : non puo' essere!!!!
009600020313     C                   IF        CONVALIDA='S'
009700020313     c                   except    stampa
009800020313     C                   ENDIF
009900020313     c                   except    stampa3
010000020313     C                   ENDIF
010100020313     C
010200020313   x4c                   else
010300020313    5c     B_ntcrnt      ifne      ntcrnt
010400020313    5c     B_ntctnt      orne      ntctnt
010500020313     C                   IF        CONVALIDA='S'
010600020313     c                   except    stampa
010700020313     C                   ENDIF
010800020313     c                   except    stampa4
010900020313     c* lo scrivo lo stesso
011000020313     C                   MOVEL     DSNTCB        DSNTC
011100020313     C                   MOVE      VISITA        ntcnk1
011200020313     c********           write     tfntc
011300020313    5c                   endif
011400020313    4c                   endif
011500020313    3c                   endif
011600020313   x2c                   else
011700020313     c* se record della stessa visita dal 2 in poi
011800020313    3c                   if        novisita=*blanks AND B_NTCFLT<>'A'
011900020313     c     kntc3         chain     tfntc03l                           40
012000020313    4c                   if        *in40
012100020313     c* NON TROVATO LO SCRIVO
012200020313     C                   IF        CONVALIDA='S'
012300020313     c                   except    stampa
012400020313     C                   ENDIF
012500020313     c                   except    stampa5
012600020313     C                   MOVEL     DSNTCB        DSNTC
012700020313     C                   MOVE      VISITA        ntcnk1
012800020313     c                   write     tfntc
012900020313   x4c                   else
013000020313     C                   IF        CONVALIDA='S'
013100020313     c                   except    stampa
013200020313     C                   ENDIF
013300020313     c                   except    stampa6
013400020313     c                   move      visita        ntcnk1
013500020313     c                   update    tfntc
013600020313    4c                   endif
013700020313   x3c                   else
013800020313    4c                   if        novisita='S'
013900020313     c     kntc3         delete    tfntc03l                           40
014000020313    4c                   endif
014100020313    3c                   endif
014200020313    2c                   endif
014300020313     c
014400020313     c     kapl          reade     tfntcbar1                              30
014500020313     c                   enddo
014600020117      *
014700020115      *
014800020115     C                   SETON                                        LR
014900020114      *
015000020115      *---------------------------------------------------------------*
015100020114      *
015200020114      * DEFINIZIONE CAMPI
015300020115      *
015400020125      *---------------------------------------------------------------*
015500020313     C     *like         Define    NTCNK1        kNK1
015600020313     C     *like         Define    ntcapl        kapl
015700020313     C     *like         Define    visnrv        visita
015800020313     C     *like         Define    ntcnk1        savke1
015900020111      *
016000020115      *Tfntc
016100020313     C     KNtcold       Klist
016200020313     C                   Kfld                    B_NtcApl
016300020313     C                   Kfld                    KNK1
016400020313     C                   Kfld                    B_NtcNk2
016500020313     C                   Kfld                    B_ntctnt
016600020313     C     KNtc2         Klist
016700020313     C                   Kfld                    kapl
016800020313     C                   Kfld                    B_NtcNk1
016900020313     C     KNtc3         Klist
017000020313     C                   Kfld                    B_NtcApl
017100020313     C                   Kfld                    B_NtcNk1
017200020313     C                   Kfld                    B_NtcNk2
017300020313     C                   Kfld                    B_ntctnt
017400020313     C                   Kfld                    B_ntcrnt
017500020313     C                   Kfld                    B_ntcsns
017600020313     Oprtf198   E            stampa      1
017700020313     O                                         +  0 'visita convalidata:   '
017800020313     O                                         +  0 '         '
017900020313     O                       visnrv            +  2 '      / '
018000020313     O          E            stampa2     1
018100020313     O                                         +  0 'Visita inesitente: can'
018200020313     O                                         +  0 'cellata??'
018300020313     O                       visita            +  2 '      / '
018400020313     O                       B_NTCNK1          +  1
018500020313     O                       B_NTCTNT          +  1
018600020313     O                       B_NTCrNT          +  1
018700020313     O          E            stampa3     1
018800020313     O                                         +  0 'Non esiste 1° rec in T'
018900020313     O                                         +  0 'FNTC     '
019000020313     O                       visita            +  2 '      / '
019100020313     O                       B_NTCNK1          +  1
019200020313     O                       B_NTCTNT          +  1
019300020313     O                       B_NTCrNT          +  1
019400020313     O          E            stampa4     1
019500020313     O                                         +  0 'Diverso il 1° rec in T'
019600020313     O                                         +  0 'FNTC     '
019700020313     O                       visita            +  2 '      / '
019800020313     O                       B_NTCNK1          +  1
019900020313     O                       B_NTCTNT          +  1
020000020313     O                       B_NTCrNT          +  1
020100020313     O                       B_NTCflt          +  1
020200020313     O                       NTCtNT            +  1
020300020313     O                       NTCrNT            +  1
020400020313     O          E            stampa5     1
020500020313     O                                         +  0 'Non trovato in TFNTC i'
020600020313     O                                         +  0 'l rec di TFNTCBAR'
020700020313     O                       visita            +  2 '      / '
020800020313     O                       B_NTCNK1          +  1
020900020313     O                       B_NTCTNT          +  1
021000020313     O                       B_NTCrNT          +  1
021100020313     O          E            stampa6     1
021200020313     O                                         +  0 'Record aggiornato     '
021300020313     O                                         +  0 'in TFNTC '
021400020313     O                       visita            +  2 '      / '
021500020313     O                       B_NTCNK1          +  1
021600020313     O                       B_NTCTNT          +  1
021700020313     O                       B_NTCrNT          +  1
