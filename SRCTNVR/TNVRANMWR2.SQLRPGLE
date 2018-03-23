000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300120111     FFNARB01L  IF   E           K DISK
000400150126     FFNbrv07L  IF   E           K DISK
000500150121     FFNANMWUF  o    E             DISK
000600150121     FFnanm04L  iF   E           K DISK
000700150127     FFnanm02L  iF   E           K DISK    rename(fnanm000:fnanm002)
000800150121     f                                     prefix(an2:3)
000900000602      *--------------------------------------------------
001000150126     D WLBDAT          DS                  INZ
001100150126     D  G02DAT                 1      8  0
001200150126     D  G02INV                 9     16  0
001300150126     D  G02ERR                17     17
001400150126     D  G02TGI                18     22  0
001500150126     D WDAT8           DS
001600150126     D  DATADA                 1      8  0
001700150126     D  DATAA                  9     16  0
001800150126     D  GGL                   17     21  0
001900150126
002000150126     D dsart         E DS                  EXTNAME(fnart00f)
002100150126      *--------------------------------------------------
002200120111     c     karb          klist
002300150126     c                   kfld                    artaas
002400150126     c                   kfld                    artlnp
002500150126     c                   kfld                    artnrs
002600150126     c                   kfld                    artnsp
002700150126     c     kbrv          klist
002800150126     c                   kfld                    anmfls
002900150126     c                   kfld                    anmlna
003000150126     c                   kfld                    anmnrs
003100150126     c                   kfld                    anmscn
003200150127     c     kanm2         klist
003300150127     c                   kfld                    anmfls
003400150127     c                   kfld                    anmlna
003500150127     c                   kfld                    anmnrs
003600150127     c                   kfld                    anmscn
003700150127     c                   kfld                    kcaa
003800150121     c     kanm15        klist
003900150121     c                   kfld                    kcaa              3 0
004000150121     c                   kfld                    anmscn
004100150127
004200150126     c                   z-add     105           kcaa
004300150127     c
004400150126     C                   TIME                    W0140            14 0
004500150126     C* UDATE IN GGMMAAAA
004600150126     C                   MOVE      W0140         WDTGIO            8 0
004700150126     C* UDATE IN AAAAMMGG
004800150126     C                   Z-ADD     WDTGIO        G02DAT
004900150126     C                   MOVEL     *BLANK        G02ERR
005000150126     C                   CALL      'XSRDA8'
005100150126     C                   PARM                    WLBDAT
005200150126     C                   MOVEL     G02INV        DATEU             8 0
005300150126
005400120111     C*
005500150126     C/EXEC SQL
005600150126     C+ delete from fnanmwuf
005700150126     C/END-EXEC
005800120111     c                   clear                   conta             7 0
005900021216      *
006000150126     C     105           setll     fnanm04l
006100150126     C     105           reade     fnanm04l
006200120111    1c                   DOW       not %EOF(FNAnm04l)
006300150121     c*
006400150126     c                   if        anmdch=0
006500150126    3c                   if        (anmnrs=40 and anmfls=63) or
006600150121     c                             (anmnrs=40 and anmfls=43)
006700150126     c                   clear                   wdat8
006800150126     c* deve essere spuntata oggi o ieri (lavorativo)
006900150126     c     kbrv          setgt     fnbrv07l
007000150126     c     kbrv          readpe    fnbrv07l
007100150126    4c                   dow       not %eof(fnbrv07l)
007200150126    5c                   if        brvdcs>0
007300150126    6c                   if        brvdcs=dateu
007400150127     c                   clear                   ggl
007500150126     c                   leave
007600150126    6c                   endif
007700150126    6c                   if        brvdcs<dateu
007800150121     c* verifico se c'è anomalia 010
007900150126     c                   clear                   wdat8
008000150126     c                   eval      datada=dateu
008100150126     c                   eval      dataa =brvdcs
008200150126     c                   call      'XSRLAV8'
008300150126     c                   parm                    wdat8
008400150127    7c                   if        ggl<=2
008500150126     c                   leave
008600150126    7c                   endif
008700150126    6c                   endif
008800150126    5c                   endif
008900150126
009000150126     c     kbrv          readpe    fnbrv07l
009100150126    4c                   enddo
009200150121
009300150126    4c                   if        not %eof(fnbrv07l)
009400150127     c                   if        ggl<=2
009500150126     c                   add       1             conta
009600150126     C/EXEC SQL
009700150126     C+ DECLARE fnart CURSOR FOR
009800150126     C+ SELECT * from fnart00f
009900150126     C+ WHERE artlna=:brvlna and artnrs=40 and artnsc=:brvnsc
010000150126     C+ FOR READ ONLY
010100150126     C/END-EXEC
010200150126     C
010300150126     C/EXEC SQL
010400150126     C+ OPEN fnart
010500150126     C/END-EXEC
010600150126    5C                   DOU       SqlCod <> 0
010700150126     C/EXEC SQL
010800150126     C+ FETCH NEXT FROM fnart INTO :dsart
010900150126     C/END-EXEC
011000150126    6C                   if        SqlCod = 100
011100150127     c                   write     wnanm000
011200150126     c                   leave
011300150126   x6C                   else
011400150126     c* leggo fnarb
011500150126     c     karb          chain     fnarb01l
011600150126    7c                   if        %found(fnarb01l)
011700150126     c
011800150126     c                   leave
011900021213      *
012000150126    7c                   endif
012100150126    6c                   endif
012200150126    5c                   enddo
012300150126    4c                   endif
012400150126    4c                   endif
012500150126    3c                   endif
012600150126    2c                   endif
012700150126
012800150126     C     105           reade     fnanm04l
012900120111    1c                   ENDDO
013000150127     C/EXEC SQL
013100150127     C+ CLOSE fnart
013200150127     C/END-EXEC
013300150127
013400150127     c                   clear                   wnanm000
013500150127     c                   eval      anmflo='TotAnom105'
013600150127     c                   eval      anmscn=conta
013700150127     c                   write     wnanm000
013800150127      *
013900150127     c                   clear                   conta             7 0
014000150127      *
014100150127     c* leggo le anom 10 senza anomalia 105
014200150127     C     010           setll     fnanm04l
014300150127     C     010           reade     fnanm04l
014400150127    1c                   DOW       not %EOF(FNAnm04l)
014500150127     c*
014600150127    2c                   if        anmdch=0
014700150127     c     kanm2         chain     fnanm02l
014800150127    3c                   if        not %found(fnanm02l)
014900150127
015000150127    4c                   if        (anmnrs=40 and anmfls=63) or
015100150127     c                             (anmnrs=40 and anmfls=43)
015200150127     c                   clear                   wdat8
015300150127     c* deve essere spuntata oggi o ieri (lavorativo)
015400150127     c     kbrv          setgt     fnbrv07l
015500150127     c     kbrv          readpe    fnbrv07l
015600150127    5c                   dow       not %eof(fnbrv07l)
015700150127    6c                   if        brvdcs>0
015800150127    7c                   if        brvdcs=dateu
015900150127     c                   clear                   ggl
016000150127     c                   leave
016100150127    7c                   endif
016200150127    7c                   if        brvdcs<dateu
016300150127     c* verifico se c'è anomalia 010
016400150127     c                   clear                   wdat8
016500150127     c                   eval      datada=dateu
016600150127     c                   eval      dataa =brvdcs
016700150127     c                   call      'XSRLAV8'
016800150127     c                   parm                    wdat8
016900150127    8c                   if        ggl<=2
017000150127     c                   leave
017100150127    8c                   endif
017200150127    7c                   endif
017300150127    6c                   endif
017400150127
017500150127     c     kbrv          readpe    fnbrv07l
017600150127    5c                   enddo
017700150127
017800150127    5c                   if        not %eof(fnbrv07l)
017900150127    6c                   if        ggl<=2
018000150127     c                   add       1             conta
018100150127     C/EXEC SQL
018200150127     C+ DECLARE fnart2 CURSOR FOR
018300150127     C+ SELECT * from fnart00f
018400150127     C+ WHERE artlna=:brvlna and artnrs=40 and artnsc=:brvnsc
018500150127     C+ FOR READ ONLY
018600150127     C/END-EXEC
018700150127     C
018800150127     C/EXEC SQL
018900150127     C+ OPEN fnart2
019000150127     C/END-EXEC
019100150127    7C                   DOU       SqlCod <> 0
019200150127     C/EXEC SQL
019300150127     C+ FETCH NEXT FROM fnart2 INTO :dsart
019400150127     C/END-EXEC
019500150127    6C                   if        SqlCod = 100
019600150127     c                   write     wnanm000
019700150127     c                   leave
019800150127   x6C                   else
019900150127     c* leggo fnarb
020000150127     c     karb          chain     fnarb01l
020100150127    7c                   if        %found(fnarb01l)
020200150127     c
020300150127     c                   leave
020400150127      *
020500150127    7c                   endif
020600150127    6c                   endif
020700150127    7c                   enddo
020800150127    6c                   endif
020900150127    5c                   endif
021000150127    4c                   endif
021100150127    3c                   endif
021200150127    2c                   endif
021300150127
021400150127     C     010           reade     fnanm04l
021500150127    1c                   ENDDO
021600150127     C/EXEC SQL
021700150127     C+ CLOSE fnart2
021800150127     C/END-EXEC
021900150126     c                   clear                   wnanm000
022000150126     c                   eval      anmflo='TotAnom105'
022100150126     c                   eval      anmscn=conta
022200150126     c                   write     wnanm000
022300021213      *
022400021213     C                   eval       *inlr = *on
