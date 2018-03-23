000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200140402     H* TISTO3R *----------------------------------------------------*
000300140402     H* STATISTICA controllo orari servizi                           *
000400000000     H*--------------------------------------------------------------*
000500140409     FTABEL00F  IF   E           K DISK
000600140409     FAZORG01L  IF   E           K DISK
000700140402     FTISTO3D   CF   E             WORKSTN
000800140402     d
000900140416     D MSG             S             78    DIM(15) CTDATA PERRCD(1)
001000140416     D or_dal          S              4  0 DIM(10)
001100140415     D or_al           S              4  0 DIM(09)
001200941223     D*
001300920925     D WLBDAT          DS
001400941223     D  G02DAT                 1      8  0
001500941223     D  G02INV                 9     16  0
001600941223     D  G02ERR                17     17
001700941223     D  G02TGI                18     22  0
001800900517     D KPJBA         E DS
001900051114     d Azuteds       e ds                  Extname(Azute00f)
002000051114     d dDatiute      e ds
002100051114     d Tibs34ds      e ds                  Inz
002200140409     d DS05          e ds                  Inz
002300051114     D*-------------------
002400051114     D* DS PARAMETRI DI LANCIO PGM
002500051114     D*-------------------
002600051114     D PARAM           DS
002700140402     D  PARdsd                 1      8  0
002800140402     D  PARdsa                 9     16  0
002900140402     D  v1ctser               17     17
003000140402     D  v1cfil                19     21
003100140411     D  v1ccar                22     24
003200140402     D  v1cperc               25     27  0
003300140408     D  v1cnum                28     30  0
003400140408     D  v1corari              31     31
003500140408     D  v1ccor                32     32
003600140411     D  v1cDET                33     33
003700140416     d* dettaglio orari
003800140416     D  v1c1dal               34     37  0
003900140416     D  v1c2dal               38     41  0
004000140416     D  v1c3dal               42     45  0
004100140416     D  v1c4dal               46     49  0
004200140416     D  v1c5dal               50     53  0
004300140416     D  v1c6dal               54     57  0
004400140416     D  v1c7dal               58     61  0
004500140416     D  v1c8dal               62     65  0
004600140416     D  v1c9dal               66     69  0
004700140416     D  v1coltre              70     73  0
004800051114     d
004900140416     D w0060           S              6  0 inz
005000140416     D orario1         S              4  0 inz
005100140416     D orario2         S              4  0 inz
005200140416     D YY              S              3  0
005300140416     D YY1             S              3  0
005400140416     D YY2             S              3  0
005500140416     D wlin            S              3
005600140409     D codut           S              1  0 inz(1)
005700140409     D errl00          S              1    inz('0')
005800140409     D korfil          S                   LIKE(orgfil) inz
005900140409     D comdsd          S                   LIKE(v1cdsd) inz
006000140402     D comdsa          S                   LIKE(v1cdsa) inz
006100140408     D dal_ver         S                   LIKE(p96ver) inz
006200140409     D KTBCOD          S                   LIKE(tblcod) inz
006300140409     D KTBkey          S                   LIKE(tblkey) inz
006400140411     D tgidsd          S                   LIKE(g02tgi) inz
006500140411     D wdiff           S                   LIKE(g02tgi) inz
006600140416     d Timeiso         s               t   timfmt(*iso)
006700000000     C*---------------------------------------------------------------*
006800000000     C     *ENTRY        PLIST
006900000000     C                   PARM                    KPJBA
007000051114     c     *dtaara       define    §azute        azuteds
007100051114     c     *dtaara       define    §datiute      ddatiute
007200051114     c                   in(E)     *dtaara
007300051114     c                   If        %error  or RSUT = *blanks
007400051114     c                   CLEAR                   tibs34ds
007500051114     c                   CALL      'TIBS34R'
007600051114     c                   PARM                    tibs34ds
007700051114     c                   in        *dtaara
007800051114     c                   EndIf
007900140409     C     KEYTAB        KLIST
008000140409     C                   KFLD                    codut
008100140409     C                   KFLD                    KTBCOD
008200140409     C                   KFLD                    KTBKEY
008300150505     c* SEDE
008400150505     c                   if        dutpou=046
008500150505     c                   seton                                        50
008600150505     c                   endif
008700941223     C*
008800900511     C*---------------------------------------------------------------*
008900920925     C*
009000140402     C                   Z-ADD     0             V1cdsd
009100140402     C                   Z-ADD     0             V1cdsa
009200140402     C                   movel     'C'           V1ctser
009300140411     c                   z-add     20            v1cperc
009400140411     c                   z-add     0             v1cnum
009500140408     C                   movel     'S'           V1corari
009600140408     C                   movel     'F'           V1ccor
009700140411     C                   movel     'S'           V1cdet
009800140415     c                   clear                   v1c1dal
009900140415     c                   clear                   v1c2dal
010000140415     c                   clear                   v1c3dal
010100140415     c                   clear                   v1c4dal
010200140415     c                   clear                   v1c5dal
010300140415     c                   clear                   v1c6dal
010400140415     c                   clear                   v1c7dal
010500140415     c                   clear                   v1c8dal
010600140415     c                   clear                   v1c9dal
010700140415     c                   clear                   v1c1al
010800140415     c                   clear                   v1c2al
010900140415     c                   clear                   v1c3al
011000140415     c                   clear                   v1c4al
011100140415     c                   clear                   v1c5al
011200140415     c                   clear                   v1c6al
011300140415     c                   clear                   v1c7al
011400140415     c                   clear                   v1c8al
011500140415     c                   clear                   v1c9al
011600140415     c                   clear                   v1coltre
011700000615     C*
011800140505     c                   eval      v1c1dal=1000
011900140505     c                   eval      v1c2dal=1030
012000140505     c                   eval      v1c3dal=1100
012100140505     c                   eval      v1c4dal=1130
012200140505     c                   eval      v1c5dal=1430
012300140505     c                   eval      v1c6dal=1500
012400140505     c                   eval      v1c7dal=1530
012500140505     c                   eval      v1c8dal=1600
012600140505     c                   eval      v1c9dal=1630
012700140505     c                   eval      v1coltre=1700
012800900509     C     FOR01         TAG
012900140402     C                   EXFMT     sto3D01
013000140416     c                   setoff                                       289046
013100140415     c                   setoff                                       404142
013200140415     c                   setoff                                       434445
013300140515     c                   setoff                                       47
013400941223     C*
013500140402     C** F3 - FINE LAVORO
013600900509     C   KC              GOTO      FINE
013700150505     C** F9 - skedulazione
013800150505     c                   if        *inki
013900150505     c                   exsr      F09GES
014000150505     c                   goto      for01
014100150505     c                   endif
014200900524     C* CONTROLLO
014300900524     C                   EXSR      CONTR
014400941223     C  NKF
014500900524     COR 90              GOTO      FOR01
014600140415     C** F6- elaborazione
014700140402     C                   Z-ADD     comdsd        pardsd
014800140402     C                   Z-ADD     comdsa        pardsa
014900000606     C                   CLEAR                   KPJBU
015000140404     C                   IF        V1CFIL<=*ZEROS
015100140404     C                   MOVEL     '000'         V1CFIL
015200140404     C                   ENDIF
015300140515     C                   IF        V1Ccar<=*ZEROS
015400140515     C                   MOVEL     '000'         V1Ccar
015500140515     C                   ENDIF
015600051114     C                   MOVEL     param         KPJBU
015700140402     C                   MOVEL     'STO4'        KCOAZ
015800140508     C****               CALL      'TISTO4R'
015900140508     C                   CALL      'BCH10'
016000900523     C                   PARM                    KPJBA
016100920925     C*
016200920925     C     FINE          TAG
016300000000     C                   SETON                                        LR
016400900524     C*
016500900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
016600900524     C     CONTR         BEGSR
016700900524     C                   SETOFF                                       90
016800140409     C                   MOVEL     '0'           ERRL00
016900140408     c
017000140402     C* DATA consegna   DAL OBBLIGATORIA
017100140402     C                   MOVE      V1cdsd        G02DAT
017200920925     C                   MOVEL     *BLANK        G02ERR
017300941223     C                   CALL      'XSRDA8'
017400920925     C                   PARM                    WLBDAT
017500920925     C     G02ERR        IFEQ      '1'
017600140402     C                   SETON                                        402890
017700140402     c                   eval      v1cmsg=msg(1)
017800920925     C                   GOTO      ENDCTR
017900920925     C                   END
018000140402     c
018100140402     C                   Z-ADD     G02INV        comdsd
018200140411     C                   Z-ADD     G02tgi        tgidsd
018300140408     c* verifico versione cappario
018400140408     C                   Z-ADD     comdsd        P96DRI            8 0
018500140408     C                   Z-ADD     *ZEROS        P96VER            5 0
018600140408     C                   CALL      'TISI96R'
018700140408     C                   PARM                    P96DRI
018800140408     C                   PARM                    P96VER
018900140408     C*
019000140408     c                   eval      dal_ver=p96ver
019100140408     c*
019200140402     c
019300941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
019400140402     C                   Z-ADD     G02DAT        v1cdsd
019500900524     C*
019600140402     C* DATA            AL
019700140402     c                   clear                   comdsa
019800140402     c                   if        v1cdsa >0
019900140402     C                   MOVE      V1cdsa        G02DAT
020000051114     C                   MOVEL     *BLANK        G02ERR
020100051114     C                   CALL      'XSRDA8'
020200051114     C                   PARM                    WLBDAT
020300051114     C     G02ERR        IFEQ      '1'
020400140402     C                   SETON                                        412890
020500140402     c                   eval      v1cmsg=msg(1)
020600051114     C                   GOTO      ENDCTR
020700051114     C                   END
020800140402     C                   Z-ADD     G02INV        comdsa
020900051114     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
021000140402     C                   Z-ADD     G02DAT        V1cdsa
021100941223     C*
021200900524     C* DATA DAL NO MAGGIORE DATA AL
021300140402     C     comdsd        IFGT      comdsa
021400140402     C                   SETON                                        419028
021500140402     c                   eval      v1cmsg=msg(2)
021600900524     C                   GOTO      ENDCTR
021700900524     C                   END
021800140408     c* verifico versione cappario
021900140408     C                   Z-ADD     comdsa        P96DRI            8 0
022000140408     C                   Z-ADD     *ZEROS        P96VER            5 0
022100140408     C                   CALL      'TISI96R'
022200140408     C                   PARM                    P96DRI
022300140408     C                   PARM                    P96VER
022400140408     c
022500140408     C     dal_ver       IFne      p96ver
022600140408     C                   SETON                                        419028
022700140408     c                   eval      v1cmsg=msg(3)
022800140408     C                   GOTO      ENDCTR
022900140411     C                   END
023000140411     c* non si può chiedere periodo > 62 giorni
023100140411     c                   eval      wdiff=g02tgi-tgidsd
023200140411     c                   if        wdiff>62
023300140411     C                   SETON                                        419028
023400140411     c                   eval      v1cmsg=msg(6)
023500140411     C                   GOTO      ENDCTR
023600140411     C                   END
023700140408     C*
023800140402     C                   END
023900140408     c*
024000140411     c* filiale o area obbligatori
024100140411     c                   if        dutpou<>046 and (v1cfil=*blanks or
024200140411     c                             v1cfil=*zeros) and
024300140515     c                             (v1ccar=*blanks or v1ccar=*zeros)
024400140411     C                   SETON                                        439028
024500140411     c                   eval      v1cmsg=msg(7)
024600140411     C                   GOTO      ENDCTR
024700140411     C                   END
024800140409     c* controllo filiale
024900140409     C                   MOVEL     *BLANKS       V1Dfil
025000140409     c                   if        v1cfil<>*blanks and v1cfil<>'000'
025100140409     C                   MOVE      V1cfil        WLIN
025200140409     C                   EXSR      CHKLIN
025300140409     c* errore
025400140409     C     ERRL00        IFEQ      '1'
025500140409     C                   SETON                                        432890
025600140409     c                   eval      v1cmsg=msg(5)
025700140409     C                   GOTO      ENDCTR
025800140409     C                   END
025900140409     c* ricerca'?'
026000140409     c   90              goto      ENDCTR
026100140409     C                   END
026200140409
026300140409     C* CONTROLLO L'AREA
026400140409     C                   MOVEL     *BLANKS       V1Dcar
026500140409     c                   if        v1ccar<>*blanks and v1ccar<>'000'
026600140409     C                   EXSR      CHKARE
026700140409     c* errore
026800140409     C     ERRL00        IFEQ      '1'
026900140409     C                   SETON                                        422890
027000140409     c                   eval      v1cmsg=msg(4)
027100140409     C                   GOTO      ENDCTR
027200140409     C                   END
027300140409     c* ricerca'?'
027400140409     c   90              goto      ENDCTR
027500140409     C                   END
027600140515     c* Base di calcolo % "Minimo/massimo" non valida per i ritiri
027700140515     c                   if        v1ctser='R' and v1corari='M'
027800140515     C                   SETON                                        472890
027900140515     c                   eval      v1cmsg=msg(13)
028000140515     C                   GOTO      ENDCTR
028100140515     c                   endif
028200140415
028300140415     c* Se richiesto dettaglio controllo le fasce orario messe
028400140415     c                   eval      or_dal(1)=v1c1dal
028500140415     c                   eval      or_dal(2)=v1c2dal
028600140415     c                   eval      or_dal(3)=v1c3dal
028700140415     c                   eval      or_dal(4)=v1c4dal
028800140415     c                   eval      or_dal(5)=v1c5dal
028900140415     c                   eval      or_dal(6)=v1c6dal
029000140415     c                   eval      or_dal(7)=v1c7dal
029100140415     c                   eval      or_dal(8)=v1c8dal
029200140415     c                   eval      or_dal(9)=v1c9dal
029300140416     c                   eval      or_dal(10)=v1coltre
029400140416     c                   clear                   v1c1al
029500140416     c                   clear                   v1c2al
029600140416     c                   clear                   v1c3al
029700140416     c                   clear                   v1c4al
029800140416     c                   clear                   v1c5al
029900140416     c                   clear                   v1c6al
030000140416     c                   clear                   v1c7al
030100140416     c                   clear                   v1c8al
030200140416     c                   clear                   v1c9al
030300140416     c                   clear                   orario1
030400140416     c                   clear                   orario2
030500140416     c
030600140415     c                   xfoot     or_dal        totale           11 0
030700140416     c                   if        (v1cdet<>'S' and totale>0)
030800140415     C                   SETON                                        442890
030900140415     c                   eval      v1cmsg=msg(8)
031000140415     C                   GOTO      ENDCTR
031100140415     C                   END
031200140416     c
031300140416     c* Primo orario e oltre sempre obbligatori
031400140416    1c                   if        v1cdet='S'
031500140416     c
031600140416    2c                   if        v1coltre=0
031700140416     C                   SETON                                        462890
031800140416     c                   eval      v1cmsg=msg(9)
031900140416     C                   GOTO      ENDCTR
032000140416    2C                   END
032100140416    2c                   if        totale  =0
032200140416     C                   SETON                                        442890
032300140416     c                   eval      v1cmsg=msg(10)
032400140416     C                   GOTO      ENDCTR
032500140416    2C                   END
032600140416     c
032700140416     c                   z-add     1             yy
032800140416    2c                   do        10
032900140416    3c                   if        or_dal(yy)>0
033000140416     c* controllo che sia arrotondato ai 15 minuti
033100140416     c                   move      or_dal(yy)    wmm               2 0
033200140416    4c                   if        wmm<>00 and wmm<>15 and wmm<>30 and
033300140416     c                             wmm<>45
033400140416     C                   SETON                                        452890
033500140416     c                   eval      v1cmsg=msg(12)
033600140416     C                   GOTO      ENDCTR
033700140416    4c                   endif
033800140416     c
033900140416    4c                   select
034000140416     c                   when      orario1=0
034100140416     c                   eval      orario1=or_dal(yy)
034200140416     c                   eval      yy1=yy
034300140416     c
034400140416     c                   when      orario2=0
034500140416     c                   eval      orario2=or_dal(yy)
034600140416     c                   eval      yy2=yy
034700140416    4c                   endsl
034800140416    3c                   endif
034900140416     c
035000140416    3c                   if        orario1>0 and orario2>0
035100140424    4c                   if        orario2   <=orario1
035200140416     C                   SETON                                        452890
035300140416     c                   eval      v1cmsg=msg(11)
035400140416     C                   GOTO      ENDCTR
035500140416   x4c                   else
035600140416     c* imposto orario AL
035700140416     c                   movel     orario2       w0060
035800140416     c                   move      w0060         timeiso
035900140422     c                   if        yy<10
036000140416     c                   subdur    1:*mn         timeiso
036100140422     c                   endif
036200140416     c                   eval      w0060     =%dec(timeiso)
036300140416     c                   movel     w0060         or_al(yy1)
036400140416
036500140416     c                   eval      orario1=orario2
036600140416     c                   clear                   orario2
036700140416     c                   eval      yy1    =yy2
036800140416     c
036900140416    4c                   endif
037000140416    3c                   endif
037100140416     c
037200140416     c                   add       1             yy
037300140416    2c                   enddo
037400140416     c
037500140416     c                   eval      v1c1al=or_al(1)
037600140416     c                   eval      v1c2al=or_al(2)
037700140416     c                   eval      v1c3al=or_al(3)
037800140416     c                   eval      v1c4al=or_al(4)
037900140416     c                   eval      v1c5al=or_al(5)
038000140416     c                   eval      v1c6al=or_al(6)
038100140416     c                   eval      v1c7al=or_al(7)
038200140416     c                   eval      v1c8al=or_al(8)
038300140416     c                   eval      v1c9al=or_al(9)
038400140416     c
038500140416    1C                   ENDIF
038600941223     C*
038700900524     C     ENDCTR        ENDSR
038800140409     C*------------------------------------------------------------------------*
038900140409     C*  CHKARE - CONTROLLA L'AREA
039000140409     C*------------------------------------------------------------------------*
039100140409     C     CHKARE        BEGSR
039200140409     C*
039300140409     C* CONTROLLO SE E' STATO INSERITO '?'
039400140409     C     '?'           SCAN      V1ccar        A                 1 0    99
039500140409     C*
039600140409     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
039700140409     C     *IN99         IFEQ      '1'                                          --- 1 -->
039800140409     C                   MOVEL     CODUT         §KUT              1 0
039900140409     C                   MOVEL     '05'          §COD              2
040000140409     C                   MOVE      *BLANKS       §DES1
040100140409     C                   CALL      'X§TABER'
040200140409     C                   PARM                    §KUT
040300140409     C                   PARM                    §COD
040400140409     C                   PARM                    §KEY              8
040500140409     C                   PARM                    §DES             25
040600140409     C                   MOVEL     §KEY          V1ccar
040700140409     C                   MOVEL     §DES          V1dcar
040800140409     c                   seton                                        4290
040900140409     C*
041000140409     C                   ELSE                                                   <-- 1 -->
041100140409     C*
041200140409     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
041300140409     C                   MOVE      '05'          KTBCOD
041400140409     C                   MOVE      *BLANKS       KTBKEY
041500140409     C                   MOVEL     V1ccar        KTBKEY
041600140409     C     KEYTAB        CHAIN     TABEL                              99
041700140409     C     *IN99         IFEQ      '1'                                          --- 2 -->
041800140409     C     TBLFLG        OREQ      '*'
041900140409     C                   MOVEL     '1'           ERRL00
042000140409     C                   ELSE                                                   <-- 2 -->
042100140409     C                   MOVEL     TBLUNI        DS05
042200140409     C                   MOVEL     §05DES        V1Dcar
042300140409     C                   END                                                    <-- 2 ---
042400140409     C*
042500140409     C                   END                                                    <-- 1 ---
042600140409     C*
042700140409     C                   ENDSR
042800140409     C*------------------------------------------------------------------------*
042900140409     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
043000140409     C*------------------------------------------------------------------------*
043100140409     C     CHKLIN        BEGSR
043200140409     C*
043300140409     C* CONTROLLO SE E' STATO INSERITO '?'
043400140409     C     '?'           SCAN      WLIN          A                 1 0    99
043500140409     C*
043600140409     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
043700140409IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
043800140409     C                   MOVEL     *BLANKS       §COD1             3
043900140409     C                   MOVE      *BLANKS       §DES1
044000140409     C                   MOVE      *BLANKS       §PRA
044100140409     C                   CALL      'TNSD24R'
044200140409     C                   PARM                    §COD1
044300140409     C                   PARM                    §TIP              1
044400140409     C                   PARM                    §DES1            25
044500140409     C                   PARM                    §PRA              3
044600140409     C                   MOVEL     §COD1         v1cfil
044700140409     C                   MOVEL     §DES1         v1dfil
044800140409     c                   seton                                        4390
044900140409     C*
045000140409X   1C                   ELSE
045100140409     C*
045200140409     C* CARATTERI ALFABETICI NON VALIDI
045300140409IF  2C     WLIN          IFLT      *ZEROS
045400140409     C                   MOVEL     '1'           ERRL00
045500140409X   2C                   ELSE
045600140409     C*
045700140409     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
045800140409     C                   MOVE      WLIN          KORFIL
045900140409     C     korfil        CHAIN     AZORG
046000140409IF  3C                   if        %found(azorg01l)                             --- 2 -->
046100140409     C                   MOVEL     ORGDES        v1dfil
046200140409IF  4C     ORGFAG        IFEQ      'V'
046300140409     C                   MOVEL     '1'           ERRL00
046400140409E   4C                   END
046500140409
046600140409X   3C                   ELSE
046700140409     C                   MOVEL     '1'           ERRL00
046800140409E   3C                   END
046900140409
047000140409E   2C                   END
047100140409E   1C                   END
047200140409     C                   ENDSR
047300150505     C*------------------------------------------------------------------------*
047400150505     C* F09GEs - GEST. TASTO FUNZ. F09          ==> skedulazione
047500150505     C*------------------------------------------------------------------------*
047600150505     C     F09GES        BEGSR
047700150505     C*
047800150505     C                   MOVE      '1'           KRITB
047900150505     C                   MOVEL     'STO4'        KCOAZ
048000150505     c
048100150505     c                   call      'BCH09'
048200150505     c                   parm                    kpjba
048300150505     C*
048400150505     C                   ENDSR
048500920925     C*
048600140402** MSG - MESSAGGI
048700140416Data errata                                                                       1
048800140416Data DAL maggiore data AL                                                         2
048900140416Selezionare un periodo di tempo della stessa versione cappario                    3
049000140416Codice AREA errato                                                                4
049100140416Filiale errata                                                                    5
049200140416Non si può chiedere un periodo superiore a 2 mesi                                 6
049300140416Richiedere o una filiale o una area                                               7
049400140416Richiedere il dettaglio per indicare le fasce orarie                              8
049500140416Ultimo orario OLTRE sempre obbligatorio                                           9
049600140416Indicare almeno un orario                                                        10
049700140424Gli orari devono essere in ordine cronologico e non ripetuti                     11
049800140416Si possono inserire soltanto orari arrotondati ai quarti d'ora                   12
049900140515Scelta non valida per Tipo servizio Ritiro                                       13
