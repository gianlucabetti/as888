000100020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200020603     Fqsysprt   o    f  132        printer
000300070528     Ffiar400f  IF   E             DISK
000400070528     Ftita430c  IF   E           K DISK
000500070528     Ftitas30c  IF   E           K DISK
000600070528     Ffnblp01l  IF   E           K DISK    usropn
000700070528     FFNARBD0T  O  A E             DISK    USROPN  RENAME(FNARBD00:FNARBDTT)
000800070528     D C22             S              1    DIM(61) CTDATA PERRCD(61)
000900070528     D C23             S              1    DIM(61) CTDATA PERRCD(61)
001000070528     D WLBDAT          DS                  INZ
001100070528     D  G02DAT                 1      8  0
001200070528     D  G02INV                 9     16  0
001300070528     D  G02ERR                17     17
001400070528     D  G02TGI                18     22  0
001500070606     c     *entry        plist
001600070606     c                   parm                    AGGIO             1
001700070528      *  lettura FIAR400f
001800070528     c     kta4          klist
001900070528     c                   kfld                    ar4aas
002000070528     c                   kfld                    ar4lnp
002100070528     c                   kfld                    ar4nrs
002200070528     c                   kfld                    ar4nsp
002300070528     c                   kfld                    ar4trc
002400070528     c     kblp          klist
002500070528     c                   kfld                    ar4aas
002600070528     c                   kfld                    ar4lnp
002700070528     c                   kfld                    ar4nrs
002800070528     c                   kfld                    ar4nsp
002900070528     c*
003000070528     c                   z-add     61            lung
003100070528     c
003200070528     C                   MOVEL     *BLANKS       COMMAN           80
003300070528     C                   MOVEA     c22           COMMAN
003400070528     C                   CALL      'QCMDEXC'
003500070528     C                   PARM                    COMMAN
003600070528     C                   PARM                    LUNG             15 5
003700070528     c
003800070528     c                   open      fnarbd0t
003900070528     c
004000070528     C                   MOVEL     *BLANKS       COMMAN           80
004100070528     C                   MOVEA     c23           COMMAN
004200070528     C                   CALL      'QCMDEXC'
004300070528     C                   PARM                    COMMAN
004400070528     C                   PARM                    LUNG             15 5
004500070528     c
004600070528     c                   open      fnblp01l
004700070528     c
004800020603     c                   do        *hival
004900070528     c                   read      fiar400f
005000070528     c                   if        %eof(fiar400f)
005100020603     c                   leave
005200020603     c                   end
005300070528     c
005400070528     c* Verifico se presente mittente o destinatario
005500070528     c                   movel     ar4not        fiscmitt         16
005600070528     c                   move      ar4not        fiscdest         16
005700070528     c
005800070528     c* Verifico se presente in sede
005900070528     c     kta4          chain     tita430c
006000070528     c
006100070528     c* MITTENTE
006200070528     c                   if        fiscmitt<>*blanks
006300070528     c
006400070528     c                   if        %found(tita430c)
006500070528     c                   movel     ta4not        mittsede         16
006600070528     c                   else
006700070528     c                   clear                   mittsede
006800070528     c                   endif
006900070528     c
007000070528     c                   if        mittsede=*blanks
007100070528     c* Trasmetto la variazione solo se bolla in blp già trasmessa
007200070528     c     kblp          chain     fnblp01l
007300070528     c                   if        not %found(fnblp01l) or
007400070528     c                             (blpft2<>'N' and blpft2<>' ')
007500070528     c
007600070528     c                   if        not %found(fnblp01l)
007700070528     c     kblp          chain     titas30c
007800070528     c                   if        not %found(titas30c)
007900070528     c                   clear                   blpcbo
008000070528     c                   else
008100070528     c                   movel     tascbo        blpcbo
008200070528     c                   endif
008300070528     c                   endif
008400070528     c
008500070606     c
008600070606     c                   if        AGGIO=' '
008700070606     c                   movel     'M'           tipo              1
008800070528     c                   exsr      TRASMI
008900070606     c                   else
009000070606     c
009100070606     c* Stampo l'aggiornamento da fare
009200070606     c                   except    rigadivm
009300070606     c                   add       1             mitt              5 0
009400070528     c                   endif
009500070606     c                   endif
009600070528     c
009700070528     c                   else
009800070528     c                   if        mittsede<>fiscmitt  and mittsede<>*blanks
009900070528     c* Stampo che è diverso e non faccio nulla
010000070528     c                   except    rigadivm
010100070528     c                   endif
010200070528     c                   endif
010300070528     c                   endif
010400070528     c* DESITNATARIO
010500070528     c                   if        fiscdest<>*blanks
010600070528     c
010700070528     c                   if        %found(tita430c)
010800070528     c                   move      ta4not        destsede         16
010900070528     c                   else
011000070528     c                   clear                   destsede
011100070528     c                   endif
011200070528     c
011300070528     c                   if        destsede=*blanks
011400070528     c* Trasmetto la variazione
011500070528     c     kblp          chain     fnblp01l
011600070528     c                   if        not %found(fnblp01l)
011700070528     c     kblp          chain     titas30c
011800070528     c
011900070528     c                   if        not %found(titas30c)
012000070528     c                   clear                   blpcbo
012100070528     c                   else
012200070528     c                   movel     tascbo        blpcbo
012300070528     c                   endif
012400070528     c                   endif
012500070528     c
012600070606     c                   if        AGGIO=' '
012700070528     c                   movel     'D'           tipo              1
012800070528     c                   exsr      TRASMI
012900070606     c                   else
013000070606     c                   except    rigadivd
013100070606     c                   add       1             dest              5 0
013200070606     c                   endif
013300070606     c
013400070528     c                   else
013500070528     c                   if        destsede<>fiscdest  and destsede<>*blanks
013600070528     c* Stampo che è diverso e non faccio nulla
013700070528     c                   except    rigadivd
013800070528     c                   endif
013900070528     c                   endif
014000070528     c                   endif
014100070528     c
014200070528     c                   enddo
014300070528     c
014400070528     c                   close     fnarbd0t
014500070606     c
014600070606     c                   if        AGGIO<>' '
014700070606     c                   except    fine
014800070606     c                   endif
014900070528     c
015000020603     c                   seton                                        lr
015100070528     c****************************************************************
015200070528     c     TRASMI        BEGSR
015300070528     c                   movel     ar4aas        arbaas
015400070528     c                   movel     ar4lnp        arblnp
015500070528     c                   movel     ar4nrs        arbnrs
015600070528     c                   movel     ar4nsp        arbnsp
015700070528     c                   movel     blpcbo        arbcbo
015800070528     C*  GIRO UDATE
015900070528     C                   TIME                    W0140            14 0
016000070528     C* UDATE IN GGMMAAAA
016100070528     C                   MOVE      W0140         WDTGIO            8 0
016200070528     C*
016300070528     C* UDATE IN AAAAMMGG
016400070528     C                   Z-ADD     WDTGIO        G02DAT
016500070528     C                   MOVEL     *BLANK        G02ERR
016600070528     C                   CALL      'XSRDA8'
016700070528     C                   PARM                    WLBDAT
016800070528     C                   MOVEL     G02INV        DATEU             8 0
016900070528     C                   MOVEL     'EDPES    '   ARBPRU
017000070528     C                   Z-ADD     DATEU         ARBDTV
017100070528     C                   TIME                    ARBORV
017200070528     C                   MOVEL     'FI'          ARBCVB
017300070528     c                   movel     tipo          arbrd2
017400070528     c                   if        tipo='M'
017500070528     c                   movel     fiscmitt      arbcpi
017600070528     c                   move      'P'           ARBPRU
017700070528     c                   else
017800070528     c                   movel     fiscdest      arbcpi
017900070528     c                   move      'A'           ARBPRU
018000070528     c                   endif
018100070528     c
018200070528     c
018300070528     c* clearo gli altri campi del file che non mi interessano
018400070528     c                   clear                   arbrsd
018500070528     c                   clear                   arbind
018600070528     c                   clear                   arbcad
018700070528     c                   clear                   arblod
018800070528     c                   clear                   arbprd
018900070528     c                   clear                   arbnzd
019000070528     c                   clear                   arbfin
019100070528     c                   clear                   arbpkb
019200070528     c                   clear                   arbpkf
019300070528     c                   clear                   arbvlb
019400070528     c                   clear                   arbvlf
019500070528     c                   clear                   arbfvb
019600070528     c                   clear                   arbfvf
019700070528     c                   clear                   arbffd
019800070528     c                   clear                   arbfst
019900070528     C                   WRITE     FNARBDTT
020000070528     c                   ENDSR
020100070528     C*
020200070528     c
020300070528     OQSYSPRT   E            rigadivm    1
020400020603     o                                           +0 'Spedizione'
020500020603     o                       ar4aas              +1
020600020603     o                       ar4lnp              +1
020700070528     o                       ar4nrs        z     +1
020800070528     o                       ar4nsp        2     +1
020900070528     o                                           +1 'Mitt.Sede'
021000070528     o                       mittsede            +1
021100070528     o                                           +1 'Mitt.Fil'
021200070528     o                       fiscmitt            +1
021300070528     OQSYSPRT   E            rigadivd    1
021400070528     o                                           +0 'Spedizione'
021500070528     o                       ar4aas              +1
021600070528     o                       ar4lnp              +1
021700070528     o                       ar4nrs        z     +1
021800070528     o                       ar4nsp        2     +1
021900070528     o                                           +1 'Dest.Sede'
022000070528     o                       destsede            +1
022100070528     o                                           +1 'Dest.Fil'
022200070528     o                       fiscdest            +1
022300070606     OQSYSPRT   E            FINE        1
022400070606     o                                           +0 'Aggiorna mittenti'
022500070606     o                       mitt          z     +1
022600070606     o                                           +2 'Aggiorna destinat'
022700070606     o                       dest          z     +1
022800070528**         C22
022900070528OVRDBF FILE(FNARBD0T) TOFILE(FILTRA201/FNARBD0T) MBR(M202046)
023000070528**         C23
023100070528OVRDBF FILE(FNBLP01L) TOFILE(FILTRA201/FNBLP01L)
