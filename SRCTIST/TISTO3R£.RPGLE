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
008300941223     C*
008400900511     C*---------------------------------------------------------------*
008500920925     C*
008600140402     C                   Z-ADD     0             V1cdsd
008700140402     C                   Z-ADD     0             V1cdsa
008800140402     C                   movel     'C'           V1ctser
008900140411     c                   z-add     20            v1cperc
009000140411     c                   z-add     0             v1cnum
009100140408     C                   movel     'S'           V1corari
009200140408     C                   movel     'F'           V1ccor
009300140411     C                   movel     'S'           V1cdet
009400140415     c                   clear                   v1c1dal
009500140415     c                   clear                   v1c2dal
009600140415     c                   clear                   v1c3dal
009700140415     c                   clear                   v1c4dal
009800140415     c                   clear                   v1c5dal
009900140415     c                   clear                   v1c6dal
010000140415     c                   clear                   v1c7dal
010100140415     c                   clear                   v1c8dal
010200140415     c                   clear                   v1c9dal
010300140415     c                   clear                   v1c1al
010400140415     c                   clear                   v1c2al
010500140415     c                   clear                   v1c3al
010600140415     c                   clear                   v1c4al
010700140415     c                   clear                   v1c5al
010800140415     c                   clear                   v1c6al
010900140415     c                   clear                   v1c7al
011000140415     c                   clear                   v1c8al
011100140415     c                   clear                   v1c9al
011200140415     c                   clear                   v1coltre
011300000615     C*
011400140505     c                   eval      v1c1dal=1000
011500140505     c                   eval      v1c2dal=1030
011600140505     c                   eval      v1c3dal=1100
011700140505     c                   eval      v1c4dal=1130
011800140505     c                   eval      v1c5dal=1430
011900140505     c                   eval      v1c6dal=1500
012000140505     c                   eval      v1c7dal=1530
012100140505     c                   eval      v1c8dal=1600
012200140505     c                   eval      v1c9dal=1630
012300140505     c                   eval      v1coltre=1700
012400900509     C     FOR01         TAG
012500140402     C                   EXFMT     sto3D01
012600140416     c                   setoff                                       289046
012700140415     c                   setoff                                       404142
012800140415     c                   setoff                                       434445
012801140515     c                   setoff                                       47
012900941223     C*
013000140402     C** F3 - FINE LAVORO
013100900509     C   KC              GOTO      FINE
013200900524     C* CONTROLLO
013300900524     C                   EXSR      CONTR
013400941223     C  NKF
013500900524     COR 90              GOTO      FOR01
013600140415     C** F6- elaborazione
013700140402     C                   Z-ADD     comdsd        pardsd
013800140402     C                   Z-ADD     comdsa        pardsa
013900000606     C                   CLEAR                   KPJBU
014000140404     C                   IF        V1CFIL<=*ZEROS
014100140404     C                   MOVEL     '000'         V1CFIL
014200140404     C                   ENDIF
014201140515     C                   IF        V1Ccar<=*ZEROS
014202140515     C                   MOVEL     '000'         V1Ccar
014203140515     C                   ENDIF
014300051114     C                   MOVEL     param         KPJBU
014400140402     C                   MOVEL     'STO4'        KCOAZ
014500140508     C****               CALL      'TISTO4R'
014600140508     C                   CALL      'BCH10'
014700900523     C                   PARM                    KPJBA
014800920925     C*
014900920925     C     FINE          TAG
015000000000     C                   SETON                                        LR
015100900524     C*
015200900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
015300900524     C     CONTR         BEGSR
015400900524     C                   SETOFF                                       90
015500140409     C                   MOVEL     '0'           ERRL00
015600140408     c
015700140402     C* DATA consegna   DAL OBBLIGATORIA
015800140402     C                   MOVE      V1cdsd        G02DAT
015900920925     C                   MOVEL     *BLANK        G02ERR
016000941223     C                   CALL      'XSRDA8'
016100920925     C                   PARM                    WLBDAT
016200920925     C     G02ERR        IFEQ      '1'
016300140402     C                   SETON                                        402890
016400140402     c                   eval      v1cmsg=msg(1)
016500920925     C                   GOTO      ENDCTR
016600920925     C                   END
016700140402     c
016800140402     C                   Z-ADD     G02INV        comdsd
016900140411     C                   Z-ADD     G02tgi        tgidsd
017000140408     c* verifico versione cappario
017100140408     C                   Z-ADD     comdsd        P96DRI            8 0
017200140408     C                   Z-ADD     *ZEROS        P96VER            5 0
017300140408     C                   CALL      'TISI96R'
017400140408     C                   PARM                    P96DRI
017500140408     C                   PARM                    P96VER
017600140408     C*
017700140408     c                   eval      dal_ver=p96ver
017800140408     c*
017900140402     c
018000941223     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
018100140402     C                   Z-ADD     G02DAT        v1cdsd
018200900524     C*
018300140402     C* DATA            AL
018400140402     c                   clear                   comdsa
018500140402     c                   if        v1cdsa >0
018600140402     C                   MOVE      V1cdsa        G02DAT
018700051114     C                   MOVEL     *BLANK        G02ERR
018800051114     C                   CALL      'XSRDA8'
018900051114     C                   PARM                    WLBDAT
019000051114     C     G02ERR        IFEQ      '1'
019100140402     C                   SETON                                        412890
019200140402     c                   eval      v1cmsg=msg(1)
019300051114     C                   GOTO      ENDCTR
019400051114     C                   END
019500140402     C                   Z-ADD     G02INV        comdsa
019600051114     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
019700140402     C                   Z-ADD     G02DAT        V1cdsa
019800941223     C*
019900900524     C* DATA DAL NO MAGGIORE DATA AL
020000140402     C     comdsd        IFGT      comdsa
020100140402     C                   SETON                                        419028
020200140402     c                   eval      v1cmsg=msg(2)
020300900524     C                   GOTO      ENDCTR
020400900524     C                   END
020500140408     c* verifico versione cappario
020600140408     C                   Z-ADD     comdsa        P96DRI            8 0
020700140408     C                   Z-ADD     *ZEROS        P96VER            5 0
020800140408     C                   CALL      'TISI96R'
020900140408     C                   PARM                    P96DRI
021000140408     C                   PARM                    P96VER
021100140408     c
021200140408     C     dal_ver       IFne      p96ver
021300140408     C                   SETON                                        419028
021400140408     c                   eval      v1cmsg=msg(3)
021500140408     C                   GOTO      ENDCTR
021600140411     C                   END
021700140411     c* non si può chiedere periodo > 62 giorni
021800140411     c                   eval      wdiff=g02tgi-tgidsd
021900140411     c                   if        wdiff>62
022000140411     C                   SETON                                        419028
022100140411     c                   eval      v1cmsg=msg(6)
022200140411     C                   GOTO      ENDCTR
022300140411     C                   END
022400140408     C*
022500140402     C                   END
022600140408     c*
022700140411     c* filiale o area obbligatori
022800140411     c                   if        dutpou<>046 and (v1cfil=*blanks or
022900140411     c                             v1cfil=*zeros) and
023000140515     c                             (v1ccar=*blanks or v1ccar=*zeros)
023100140411     C                   SETON                                        439028
023200140411     c                   eval      v1cmsg=msg(7)
023300140411     C                   GOTO      ENDCTR
023400140411     C                   END
023500140409     c* controllo filiale
023600140409     C                   MOVEL     *BLANKS       V1Dfil
023700140409     c                   if        v1cfil<>*blanks and v1cfil<>'000'
023800140409     C                   MOVE      V1cfil        WLIN
023900140409     C                   EXSR      CHKLIN
024000140409     c* errore
024100140409     C     ERRL00        IFEQ      '1'
024200140409     C                   SETON                                        432890
024300140409     c                   eval      v1cmsg=msg(5)
024400140409     C                   GOTO      ENDCTR
024500140409     C                   END
024600140409     c* ricerca'?'
024700140409     c   90              goto      ENDCTR
024800140409     C                   END
024900140409
025000140409     C* CONTROLLO L'AREA
025100140409     C                   MOVEL     *BLANKS       V1Dcar
025200140409     c                   if        v1ccar<>*blanks and v1ccar<>'000'
025300140409     C                   EXSR      CHKARE
025400140409     c* errore
025500140409     C     ERRL00        IFEQ      '1'
025600140409     C                   SETON                                        422890
025700140409     c                   eval      v1cmsg=msg(4)
025800140409     C                   GOTO      ENDCTR
025900140409     C                   END
026000140409     c* ricerca'?'
026100140409     c   90              goto      ENDCTR
026200140409     C                   END
026201140515     c* Base di calcolo % "Minimo/massimo" non valida per i ritiri
026202140515     c                   if        v1ctser='R' and v1corari='M'
026203140515     C                   SETON                                        472890
026204140515     c                   eval      v1cmsg=msg(13)
026205140515     C                   GOTO      ENDCTR
026206140515     c                   endif
026300140415
026400140415     c* Se richiesto dettaglio controllo le fasce orario messe
026500140415     c                   eval      or_dal(1)=v1c1dal
026600140415     c                   eval      or_dal(2)=v1c2dal
026700140415     c                   eval      or_dal(3)=v1c3dal
026800140415     c                   eval      or_dal(4)=v1c4dal
026900140415     c                   eval      or_dal(5)=v1c5dal
027000140415     c                   eval      or_dal(6)=v1c6dal
027100140415     c                   eval      or_dal(7)=v1c7dal
027200140415     c                   eval      or_dal(8)=v1c8dal
027300140415     c                   eval      or_dal(9)=v1c9dal
027400140416     c                   eval      or_dal(10)=v1coltre
027500140416     c                   clear                   v1c1al
027600140416     c                   clear                   v1c2al
027700140416     c                   clear                   v1c3al
027800140416     c                   clear                   v1c4al
027900140416     c                   clear                   v1c5al
028000140416     c                   clear                   v1c6al
028100140416     c                   clear                   v1c7al
028200140416     c                   clear                   v1c8al
028300140416     c                   clear                   v1c9al
028400140416     c                   clear                   orario1
028500140416     c                   clear                   orario2
028600140416     c
028700140415     c                   xfoot     or_dal        totale           11 0
028800140416     c                   if        (v1cdet<>'S' and totale>0)
028900140415     C                   SETON                                        442890
029000140415     c                   eval      v1cmsg=msg(8)
029100140415     C                   GOTO      ENDCTR
029200140415     C                   END
029300140416     c
029400140416     c* Primo orario e oltre sempre obbligatori
029500140416    1c                   if        v1cdet='S'
029600140416     c
029700140416    2c                   if        v1coltre=0
029800140416     C                   SETON                                        462890
029900140416     c                   eval      v1cmsg=msg(9)
030000140416     C                   GOTO      ENDCTR
030100140416    2C                   END
030200140416    2c                   if        totale  =0
030300140416     C                   SETON                                        442890
030400140416     c                   eval      v1cmsg=msg(10)
030500140416     C                   GOTO      ENDCTR
030600140416    2C                   END
030700140416     c
030800140416     c                   z-add     1             yy
030900140416    2c                   do        10
031000140416    3c                   if        or_dal(yy)>0
031100140416     c* controllo che sia arrotondato ai 15 minuti
031200140416     c                   move      or_dal(yy)    wmm               2 0
031300140416    4c                   if        wmm<>00 and wmm<>15 and wmm<>30 and
031400140416     c                             wmm<>45
031500140416     C                   SETON                                        452890
031600140416     c                   eval      v1cmsg=msg(12)
031700140416     C                   GOTO      ENDCTR
031800140416    4c                   endif
031900140416     c
032000140416    4c                   select
032100140416     c                   when      orario1=0
032200140416     c                   eval      orario1=or_dal(yy)
032300140416     c                   eval      yy1=yy
032400140416     c
032500140416     c                   when      orario2=0
032600140416     c                   eval      orario2=or_dal(yy)
032700140416     c                   eval      yy2=yy
032800140416    4c                   endsl
032900140416    3c                   endif
033000140416     c
033100140416    3c                   if        orario1>0 and orario2>0
033200140424    4c                   if        orario2   <=orario1
033300140416     C                   SETON                                        452890
033400140416     c                   eval      v1cmsg=msg(11)
033500140416     C                   GOTO      ENDCTR
033600140416   x4c                   else
033700140416     c* imposto orario AL
033800140416     c                   movel     orario2       w0060
033900140416     c                   move      w0060         timeiso
034000140422     c                   if        yy<10
034100140416     c                   subdur    1:*mn         timeiso
034200140422     c                   endif
034300140416     c                   eval      w0060     =%dec(timeiso)
034400140416     c                   movel     w0060         or_al(yy1)
034500140416
034600140416     c                   eval      orario1=orario2
034700140416     c                   clear                   orario2
034800140416     c                   eval      yy1    =yy2
034900140416     c
035000140416    4c                   endif
035100140416    3c                   endif
035200140416     c
035300140416     c                   add       1             yy
035400140416    2c                   enddo
035500140416     c
035600140416     c                   eval      v1c1al=or_al(1)
035700140416     c                   eval      v1c2al=or_al(2)
035800140416     c                   eval      v1c3al=or_al(3)
035900140416     c                   eval      v1c4al=or_al(4)
036000140416     c                   eval      v1c5al=or_al(5)
036100140416     c                   eval      v1c6al=or_al(6)
036200140416     c                   eval      v1c7al=or_al(7)
036300140416     c                   eval      v1c8al=or_al(8)
036400140416     c                   eval      v1c9al=or_al(9)
036500140416     c
036600140416    1C                   ENDIF
036700941223     C*
036800900524     C     ENDCTR        ENDSR
036900140409     C*------------------------------------------------------------------------*
037000140409     C*  CHKARE - CONTROLLA L'AREA
037100140409     C*------------------------------------------------------------------------*
037200140409     C     CHKARE        BEGSR
037300140409     C*
037400140409     C* CONTROLLO SE E' STATO INSERITO '?'
037500140409     C     '?'           SCAN      V1ccar        A                 1 0    99
037600140409     C*
037700140409     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
037800140409     C     *IN99         IFEQ      '1'                                          --- 1 -->
037900140409     C                   MOVEL     CODUT         §KUT              1 0
038000140409     C                   MOVEL     '05'          §COD              2
038100140409     C                   MOVE      *BLANKS       §DES1
038200140409     C                   CALL      'X§TABER'
038300140409     C                   PARM                    §KUT
038400140409     C                   PARM                    §COD
038500140409     C                   PARM                    §KEY              8
038600140409     C                   PARM                    §DES             25
038700140409     C                   MOVEL     §KEY          V1ccar
038800140409     C                   MOVEL     §DES          V1dcar
038900140409     c                   seton                                        4290
039000140409     C*
039100140409     C                   ELSE                                                   <-- 1 -->
039200140409     C*
039300140409     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
039400140409     C                   MOVE      '05'          KTBCOD
039500140409     C                   MOVE      *BLANKS       KTBKEY
039600140409     C                   MOVEL     V1ccar        KTBKEY
039700140409     C     KEYTAB        CHAIN     TABEL                              99
039800140409     C     *IN99         IFEQ      '1'                                          --- 2 -->
039900140409     C     TBLFLG        OREQ      '*'
040000140409     C                   MOVEL     '1'           ERRL00
040100140409     C                   ELSE                                                   <-- 2 -->
040200140409     C                   MOVEL     TBLUNI        DS05
040300140409     C                   MOVEL     §05DES        V1Dcar
040400140409     C                   END                                                    <-- 2 ---
040500140409     C*
040600140409     C                   END                                                    <-- 1 ---
040700140409     C*
040800140409     C                   ENDSR
040900140409     C*------------------------------------------------------------------------*
041000140409     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
041100140409     C*------------------------------------------------------------------------*
041200140409     C     CHKLIN        BEGSR
041300140409     C*
041400140409     C* CONTROLLO SE E' STATO INSERITO '?'
041500140409     C     '?'           SCAN      WLIN          A                 1 0    99
041600140409     C*
041700140409     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
041800140409IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
041900140409     C                   MOVEL     *BLANKS       §COD1             3
042000140409     C                   MOVE      *BLANKS       §DES1
042100140409     C                   MOVE      *BLANKS       §PRA
042200140409     C                   CALL      'TNSD24R'
042300140409     C                   PARM                    §COD1
042400140409     C                   PARM                    §TIP              1
042500140409     C                   PARM                    §DES1            25
042600140409     C                   PARM                    §PRA              3
042700140409     C                   MOVEL     §COD1         v1cfil
042800140409     C                   MOVEL     §DES1         v1dfil
042900140409     c                   seton                                        4390
043000140409     C*
043100140409X   1C                   ELSE
043200140409     C*
043300140409     C* CARATTERI ALFABETICI NON VALIDI
043400140409IF  2C     WLIN          IFLT      *ZEROS
043500140409     C                   MOVEL     '1'           ERRL00
043600140409X   2C                   ELSE
043700140409     C*
043800140409     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
043900140409     C                   MOVE      WLIN          KORFIL
044000140409     C     korfil        CHAIN     AZORG
044100140409IF  3C                   if        %found(azorg01l)                             --- 2 -->
044200140409     C                   MOVEL     ORGDES        v1dfil
044300140409IF  4C     ORGFAG        IFEQ      'V'
044400140409     C                   MOVEL     '1'           ERRL00
044500140409E   4C                   END
044600140409
044700140409X   3C                   ELSE
044800140409     C                   MOVEL     '1'           ERRL00
044900140409E   3C                   END
045000140409
045100140409E   2C                   END
045200140409E   1C                   END
045300140409     C                   ENDSR
045400920925     C*
045500140402** MSG - MESSAGGI
045600140416Data errata                                                                       1
045700140416Data DAL maggiore data AL                                                         2
045800140416Selezionare un periodo di tempo della stessa versione cappario                    3
045900140416Codice AREA errato                                                                4
046000140416Filiale errata                                                                    5
046100140416Non si può chiedere un periodo superiore a 2 mesi                                 6
046200140416Richiedere o una filiale o una area                                               7
046300140416Richiedere il dettaglio per indicare le fasce orarie                              8
046400140416Ultimo orario OLTRE sempre obbligatorio                                           9
046500140416Indicare almeno un orario                                                        10
046600140424Gli orari devono essere in ordine cronologico e non ripetuti                     11
046700140416Si possono inserire soltanto orari arrotondati ai quarti d'ora                   12
046800140515Scelta non valida per Tipo servizio Ritiro                                       13
