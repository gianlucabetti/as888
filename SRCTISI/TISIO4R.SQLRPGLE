000100051116      *PARMS  TGTRLS(*PRV)
000200000000     H*------------------------------------------------------------------------*
000300140213     H* Spalmatura orari servizi
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900000000     FTABEL00F  IF   E           K DISK
001000140129     Fazorg01l  IF   E           K DISK
001100131024     FTNTBE01L  iF   E           K DISK
001200970707     FAZCPC01L  IF   E           K DISK
001300980210     FAZCPL01L  IF   E           K DISK
001400140410     FAZCPE01L  IF   E           K DISK
001500140507     FAZcos03l  uF a E           K DISK
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900140214     d lna_prev        s              3  0 dim(14)
002000000000     D*------------------------------------------------------------------------*
002100000000     D* INPUT
002200000000     D*------------------------------------------------------------------------*
002300980210     D* DS REPERIMENTO DATI UTENTE
002400980210     D*-------------------
002500020919     D TIBS34DS      E DS                                                       *Profili utente
002600020919     D DDATIUTE      E DS                                                       *Dati utente
002700020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
002800980210     D*------------------
002900980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000980210     D*------------------
003100980210     D WLBDA8          DS
003200980210     D  G08DAT                 1      8  0
003300980210     D  G08INV                 9     16  0
003400980210     D  G08ERR                17     17
003500980210     D  G08TGI                18     22  0
003600000000     D*-------------------
003700000000     D* DS PARAMETRI DI LANCIO
003800000000     D*-------------------
003900140213     D tisio4ds        DS
004000140213     D  isio4dri               1      8  0
004100140213     D  isio4ver               9     13  0
004200140506     D  isio4CAP              14     22
004300131024
004400140109     D tisio9ds      e DS
004500980210     D*-------------------
004600980210     D KPJBA         E DS
004700051116      *
004800051116      * V A R I A B I L I
004900051116      *
005000140410     d Indx            s              3  0 inz
005100140409     d kloc            s                   like(cosloc)
005200140304     d klna            s                   like(cpllna)
005300140304     d ktser           s                   like(costser)
005400140505     d kserv           s                   like(cosserv)
005500140410     d wtser           s                   like(costser)
005600140410     d wserv           s                   like(cosserv)
005700140410     d max_loc         s                   like(cplloc)
005800140507     d sav_loc         s                   like(cplloc)
005900140410     d wloc            s                   like(cplloc)
006000140214     d YY              s              3  0
006100140214     d XX              s              3  0
006200140410     d totspe          s              9  0
006300140410     d savspe          s              9  0
006400140505     d Wesist          s              3
006500140507     d CT_ostis        s                   like(COSOSTIS)
006600140507     d CT_ostfs        s                   like(COSOSTfS)
006700140507     d CT_omiis        s                   like(COSOmiis)
006800140507     d CT_omxfs        s                   like(COSOmxfs)
006900140507     d CT_olrsc        s                   like(COSOlrsc)
007000140507     d CT_olrnc        s                   like(COSOlrnc)
007100140507     d CT_olrnk        s                   like(COSOlrnk)
007200140507     d CT_trzs         s                   like(costrzs)
007300140507     d CP_ostis        s                   like(COSOSTIS)
007400140507     d CP_ostfs        s                   like(COSOSTfS)
007500140507     d CP_omiis        s                   like(COSOmiis)
007600140507     d CP_omxfs        s                   like(COSOmxfs)
007700140507     d CP_olrsc        s                   like(COSOlrsc)
007800140507     d CP_olrnc        s                   like(COSOlrnc)
007900140507     d CP_olrnk        s                   like(COSOlrnk)
008000140507     d CP_trzs         s                   like(costrzs)
008100140507     d RT_ostis        s                   like(COSOSTIS)
008200140507     d RT_ostfs        s                   like(COSOSTfS)
008300140507     d RT_omiis        s                   like(COSOmiis)
008400140507     d RT_omxfs        s                   like(COSOmxfs)
008500140507     d RT_olrsc        s                   like(COSOlrsc)
008600140507     d RT_olrnc        s                   like(COSOlrnc)
008700140507     d RT_olrnk        s                   like(COSOlrnk)
008800140507     d RT_trzs         s                   like(costrzs)
008900000000     C*------------------------------------------------------------------------*
009000000000     C* MAIN LINE
009100000000     C*------------------------------------------------------------------------*
009200020925     C*
009300140429     c* Elaboro CAP per CAP
009400140506     c                   if        isio4cap=*blanks
009500140410     c     isio4ver      setll     azcpc01l
009600140410     c     isio4ver      reade     azcpc01l
009700140506     c
009800140506     c* se passat il CAP elaboro solo quello
009900140506     c                   else
010000140506     c                   clear                   knar
010100140506     c     kcpcLet       setll     azcpc01l
010200140506     c     kcpcLet       reade     azcpc01l
010300140506     c                   endif
010400140409    1c                   dow       not %eof(azcpc01l)
010500140304     c
010600140304     c                   exsr      Chkrec
010700140304
010800140409    2c                   if        wrecOK='S'
010900140214     c
011000140214     c                   exsr      Linee_cap
011100140213     c
011200140304     c* Per ogni linea prevista
011300140304     c* se il CAP ha AZCOS sia "C" che "R" inserito --> salto il record
011400140304     c                   z-add     1             yy
011500140410    3c                   dow       lna_prev(YY)>0
011600140304     c
011700140304     c                   clear                   wesist
011800140304     c                   eval      klna=lna_prev(YY)
011900140505     c* Verifico se la filiale ha la soglia
012000140507     c                   clear                   klnasgl           1
012100140505     c                   clear                   tisio9ds
012200140505     c                   eval      DO0TSER='C'
012300140505     c                   move      klna          DO0LNA
012400140505     c                   call      'TISIO9R'
012500140505     C                   PARM                    KPJBA
012600140505     C                   PARM                    tiSIO9ds
012700140505     c*
012800140505    4c                   if        do0fcsgl='S'
012900140505     c                   eval      klnaSGL='S'
013000140505    4c                   endif
013100140505     c
013200140505     c                   eval      kloc=*blanks
013300140304     c                   eval      ktser='C'
013400140505     c                   clear                   kserv
013500140507     c     kcos          chain(N)  azcos03l
013600140505    4c                   if        %found(azcos03l)
013700140505     c                   eval      wesist='C  '
013800140409    4c                   endif
013900140505     c* soglia
014000140505    4c                   if        klnasgl=' '
014100140505     c                   eval      %subst(wesist:2:1)='X'
014200140505   x4c                   else
014300140505     c                   eval      kserv='P '
014400140507     c     kcos          chain(n)  azcos03l
014500140505    5c                   if        %found(azcos03l)
014600140505     c                   eval      %subst(wesist:2:1)='P'
014700140505    5c                   endif
014800140505    4c                   endif
014900140304     c
015000140304     c                   eval      ktser='R'
015100140507     c                   clear                   kserv
015200140507     c     kcos          chain(n)  azcos03l
015300140505    4c                   if        %found(azcos03l)
015400140505     c                   eval      %subst(wesist:3:1)='R'
015500140409    4c                   endif
015600140213     c
015700140505    4c                   if        %scan(' ':wesist)>0
015800140410     c                   clear                   max_loc
015900140507     c                   clear                   sav_loc
016000140410     c                   clear                   totspe
016100140410     c                   clear                   savspe
016200140304     c                   exsr      Spalma_loc
016300140410     c
016400140410     c                   exsr      aggio_cap
016500140409    4c                   endif
016600140507     c
016700140507     c* rileggo e cancello gli orari di località che sono uguali al CAP
016800140507     c                   exsr      Deleta_or
016900140409     c
017000140409     c                   add       1             YY
017100140409    3c                   enddo
017200140409    2c                   endif
017300140304     c
017400140506     c                   if        isio4cap=*blanks
017500140410     c     isio4ver      reade     azcpc01l
017600140506     c                   else
017700140506     c     kcpcLet       reade     azcpc01l
017800140506     c                   endif
017900140506     c
018000140409    1c                   enddo
018100140505     c
018200000000     C*
018300000000     C                   SETON                                        LR
018400140214     C*------------------------------------------------------------------------*
018500140214     C* Carico in skiera le linee del CAP
018600140214     C*------------------------------------------------------------------------*
018700140214     C     Linee_cap     BEGSR
018800140214     c                   z-add     1             yy                3 0
018900140214     c                   clear                   lna_prev
019000140214     c                   z-add     cpclna        lna_prev(yy)
019100140429    1c                   if        cpclos<>cpclna  and cpclos>0
019200140214     c                   add       1             yy
019300140214     c                   z-add     cpclos        lna_prev(yy)
019400140429    1c                   endif
019500140429    1c                   if        cpclol<>cpclna and cpclos<>cpclol and
019600140429     c                             cpclol>0
019700140214     c                   add       1             yy
019800140214     c                   z-add     cpclol        lna_prev(yy)
019900140429    1c                   endif
020000140214     c
020100140214     c* Carico eventuali eccezioni di linea se presenti
020200140214     C     Kcpc          setll     AZCPe01L
020300140214     C     Kcpc          reade     AZCPe01L
020400140214     c
020500140214    1c                   dow       not %eof(azcpe01l)
020600140214    2c                   if        cpelna>0
020700140214     c                   eval      indx=%lookup(cpelna:lna_prev)
020800140214    3c                   if        Indx=0
020900140214     c                   add       1             yy
021000140214     c                   z-add     cpelna        lna_prev(yy)
021100140214    3c                   endif
021200140214    2c                   endif
021300140214     c
021400140214    2c                   if        cpelos>0
021500140214     c                   eval      indx=%lookup(cpelos:lna_prev)
021600140214    3c                   if        Indx=0
021700140214     c                   add       1             yy
021800140214     c                   z-add     cpelos        lna_prev(yy)
021900140214    3c                   endif
022000140214    2c                   endif
022100140214     c
022200140214    2c                   if        cpelol>0
022300140214    3c                   eval      indx=%lookup(cpelol:lna_prev)
022400140214     c                   if        Indx=0
022500140214     c                   add       1             yy
022600140214     c                   z-add     cpelol        lna_prev(yy)
022700140214    3c                   endif
022800140214    2c                   endif
022900140214     c
023000140214     C     Kcpc          reade     AZCPe01L
023100140214    1c                   enddo
023200140214     c
023300140214     c                   ENDSR
023400140214     C*------------------------------------------------------------------------*
023500140214     C* Spalma sulle località
023600140214     C*------------------------------------------------------------------------*
023700140214     C     Spalma_loc    BEGSR
023800140214     c
023900140214     c* Leggo le località e per ogni linea vedo se presenti gli orari
024000140304     c* per il tipo servizio che devo elaborare
024100140304     c     kcpc          setll     azcpl01l
024200140214     c     kcpc          reade     azcpl01l
024300140410    1c                   dow       not %eof(azcpl01l)
024400140410
024500140410     c* Se linee a livello di LOC verifico se c'è la linea che sto elaborando
024600140410    2c                   if        cplliv='C' or cpllna=klna or cpllos=klna or
024700140410     c                             cpllol=klna
024800140505     c* CONSEGNE "  "
024900140505    3c                   if        %subst(wesist:1:1)= ' '
025000140410     c                   eval      wtser='C'
025100140304     c
025200140214     c                   clear                   tisio9ds
025300140410     c                   eval      DO0VER = cpcver
025400140410     c                   eval      DO0NAR = cpcnar
025500140410     c                   eval      DO0CAP =cpccap
025600140214     c                   eval      DO0LOC =cplloc
025700140410     c                   eval      DO0LNA =klna
025800140410     c                   eval      DO0TSER=WTSER
025900140505     c                   eval      DO0soglia='T'
026000140214     c                   call      'TISIO9R'
026100140214     C                   PARM                    KPJBA
026200140214     C                   PARM                    tiSIO9ds
026300140410     c* Se gli orari NON sono a livello di località li devo scrivere dai dati
026400140410     c*  di riferimiento presi
026500140410    4c                   if        do0rif<>'L'
026600140410     c                   eval      wserv=*blanks
026700140410     c                   eval      wloc=cplloc
026800140410     C                   exsr      scri_azcos
026900140410    4c                   endif
027000140410     c
027100140410    3c                   endif
027200140505     c* CONSEGNE "P  "
027300140505    3c                   if        %subst(wesist:2:1)= ' '
027400140505     c                   eval      wtser='C'
027500140505     c
027600140505     c                   clear                   tisio9ds
027700140505     c                   eval      DO0VER = cpcver
027800140505     c                   eval      DO0NAR = cpcnar
027900140505     c                   eval      DO0CAP =cpccap
028000140505     c                   eval      DO0LOC =cplloc
028100140505     c                   eval      DO0LNA =klna
028200140505     c                   eval      DO0TSER=WTSER
028300140505     c                   eval      DO0soglia='P'
028400140505     c                   call      'TISIO9R'
028500140505     C                   PARM                    KPJBA
028600140505     C                   PARM                    tiSIO9ds
028700140505     c* Se gli orari NON sono a livello di località li devo scrivere dai dati
028800140505     c*  di riferimiento presi
028900140505    4c                   if        do0rif<>'L'
029000140505     c                   eval      wserv=*blanks
029100140505     c                   eval      wloc=cplloc
029200140505     c                   eval      wserv='P '
029300140505     C                   exsr      scri_azcos
029400140505    4c                   endif
029500140505    3c                   endif
029600140410     c* RITIRI
029700140505    3c                   if        %subst(wesist:3:1)= ' '
029800140410     c                   eval      wtser='R'
029900140410     c
030000140410     c                   clear                   tisio9ds
030100140410     c                   eval      DO0VER = cpcver
030200140410     c                   eval      DO0NAR = cpcnar
030300140410     c                   eval      DO0CAP =cpccap
030400140410     c                   eval      DO0LOC =cplloc
030500140410     c                   eval      DO0LNA =klna
030600140410     c                   eval      DO0TSER=WTSER
030700140410     c                   call      'TISIO9R'
030800140410     C                   PARM                    KPJBA
030900140410     C                   PARM                    tiSIO9ds
031000140410     c* Se gli orari NON sono a livello di località li devo scrivere dai dati
031100140410     c*  di riferimiento presi
031200140410    4c                   if        do0rif<>'L'
031300140410     c                   eval      wserv=*blanks
031400140505     c                   eval      wloc=cplloc
031500140410     C                   exsr      scri_azcos
031600140410    4c                   endif
031700140410    3c                   endif
031800140507     c
031900140507     c                   if        sav_loc=*blanks
032000140507     c                   eval      sav_loc=cplloc
032100140507     c                   endif
032200140410     c
032300140410     c* Memorizzo la località con le maggiori consegne vado sul tIALA
032400140410      /free
032500140410         exec sql
032600140410          declare ALA cursor for
032700140410          select sum(alatos) from tiala00f
032800140410           where alacap=:cplcap and alaloc=:cplloc and alanaz=:cplnar
032900140410           and alalna=:klna ;
033000140410
033100140410         exec sql
033200140410           open ALA;
033300140410
033400140410           IF sqlcode = 0;
033500140410           exec sql
033600140410             fetch next from ALA into :totspe  ;
033700140410             IF sqlcod = 100 or sqlcod < 0;
033800140410               else         ;
033900140410             if totspe >savspe   or max_loc=*blanks ;
034000140410              max_loc=cplloc  ;
034100140507              savspe=totspe   ;
034200140410             ENDIF;
034300140410             ENDIF;
034400140410           ENDIF;
034500140410
034600140410         exec sql close ALA;
034700140410
034800140410      /end-free
034900140410     c
035000140410    2c                   endif
035100140214     c
035200140214     c     kcpc          reade     azcpl01l
035300140410    1c                   enddo
035400140214     c
035500140214     c                   ENDSR
035600140410     C*------------------------------------------------------------------------*
035700140410     C* Aggiorno gli orari del CAP  con gli orari della località più numerosa
035800140410     C*------------------------------------------------------------------------*
035900140410     C     Aggio_cap     BEGSR
036000140507     c
036100140507     c* Se max_loc è vuoto devo impo la prima località trovata
036200140507     c                   if        max_loc=*blanks
036300140507     c                   eval      max_loc=sav_loc
036400140507     c                   endif
036500140410     c
036600140505     c* CONSEGNE "  "
036700140505    3c                   if        %subst(wesist:1:1)= ' '
036800140410     c                   eval      wtser='C'
036900140410     c                   clear                   wserv
037000140410     c                   clear                   tisio9ds
037100140410     c                   eval      DO0VER = cpcver
037200140410     c                   eval      DO0NAR = cpcnar
037300140410     c                   eval      DO0CAP =cpccap
037400140410     c                   eval      DO0LOC =max_loc
037500140410     c                   eval      DO0LNA =klna
037600140410     c                   eval      DO0TSER=WTSER
037700140505     c                   eval      DO0soglia='T'
037800140410     c                   call      'TISIO9R'
037900140410     C                   PARM                    KPJBA
038000140410     C                   PARM                    tiSIO9ds
038100140410
038200140410     c* scrivo gli orari per il CAP
038300140410     c                   clear                   wloc
038400140410     c                   eval      wserv=*blanks
038500140410     C                   exsr      scri_azcos
038600140410    1c                   endif
038700140505     c* CONSEGNE "P "
038800140505    3c                   if        %subst(wesist:2:1)= ' '
038900140505     c                   eval      wtser='C'
039000140505     c                   clear                   wserv
039100140505     c                   clear                   tisio9ds
039200140505     c                   eval      DO0VER = cpcver
039300140505     c                   eval      DO0NAR = cpcnar
039400140505     c                   eval      DO0CAP =cpccap
039500140505     c                   eval      DO0LOC =max_loc
039600140505     c                   eval      DO0LNA =klna
039700140505     c                   eval      DO0TSER=WTSER
039800140505     c                   eval      DO0soglia='P'
039900140505     c                   call      'TISIO9R'
040000140505     C                   PARM                    KPJBA
040100140505     C                   PARM                    tiSIO9ds
040200140505
040300140505     c* scrivo gli orari per il CAP
040400140505     c                   clear                   wloc
040500140505     c                   eval      wserv='P '
040600140505     C                   exsr      scri_azcos
040700140505     c                   endif
040800140410     c* RITIRI
040900140505    3c                   if        %subst(wesist:3:1)= ' '
041000140410     c                   eval      wtser='R'
041100140410     c
041200140410     c                   clear                   tisio9ds
041300140410     c                   eval      DO0VER = cpcver
041400140410     c                   eval      DO0NAR = cpcnar
041500140410     c                   eval      DO0CAP =cpccap
041600140410     c                   eval      DO0LOC =max_loc
041700140410     c                   eval      DO0LNA =klna
041800140410     c                   eval      DO0TSER=WTSER
041900140410     c                   call      'TISIO9R'
042000140410     C                   PARM                    KPJBA
042100140410     C                   PARM                    tiSIO9ds
042200140410     c* scrivo gli orari per il CAP
042300140410     c                   clear                   wloc
042400140410     c                   eval      wserv=*blanks
042500140410     C                   exsr      scri_azcos
042600140410     c
042700140410    1c                   endif
042800140410     c
042900140410     c                   ENDSR
043000000000     C*------------------------------------------------------------------------*
043100980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
043200000000     C*------------------------------------------------------------------------*
043300000000     C     CHKREC        BEGSR
043400000000     C*
043500000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
043600000000     C*
043700000000     C* CAP ESTERO, ESCLUDE
043800970707IF  1C     CPCNAR        IFNE      *BLANKS
043900000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
044000000000E   1C                   ENDIF
044100000000     C*
044200000000     C* CAP FITTIZIO, ESCLUDE
044300970707IF  1C     CPCFIT        IFEQ      'S'
044400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
044500000000E   1C                   ENDIF
044600000000     C*
044700000000     C                   ENDSR
044800140410     C*--------------------------------------------------------------------------------------------*
044900140410     C* scrivo record in AZCOS
045000140410     C*--------------------------------------------------------------------------------------------*
045100140410     C     Scri_azcos    BEGSR
045200140410     c                   clear                   azcos000
045300140410     c                   eval      cosver=cpcver
045400140410     c                   eval      coscap=cpccap
045500140410     c                   eval      cosnar=cpcnar
045600140410     c                   eval      cosloc=Wloc
045700140410     c                   eval      costser=wtser
045800140410     c                   eval      COSOSTIS=DO0OSTIS
045900140410     c                   eval      COSOSTfS=DO0OSTfS
046000140410     c                   eval      COSOMIIS=DO0Omiis
046100140410     c                   eval      COSOMxfS=DO0Omxfs
046200140410     c                   eval      COSOlrsc=DO0OLRSC
046300140410     c                   eval      COSOlrnc=DO0OLRnC
046400140410     c                   eval      COSOlrnk=DO0OLRnk
046500140410     c                   eval      COStrzs =DO0trzs
046600140410     c                   eval      COSduv  =datcor
046700140410     c                   eval      COSserv =wserv
046800140429     c                   eval      COSlna  =klna
046900140410     c
047000140410     c                   write     azcos000
047100140410     c
047200140410     c                   ENDSR
047300140507     C*------------------------------------------------------------------------*
047400140507     C* Deleta gli orari di località uguale al CAP
047500140507     C*------------------------------------------------------------------------*
047600140507     C     deleta_or     BEGSR
047700140507     c* Mi salvo gli orari
047800140507     c* consegne "  "
047900140507     c                   clear                   tisio9ds
048000140507     c                   eval      DO0VER = cpcver
048100140507     c                   eval      DO0NAR = cpcnar
048200140507     c                   eval      DO0CAP =cpccap
048300140507     c                   eval      DO0LOC =*blanks
048400140507     c                   eval      DO0LNA =klna
048500140507     c                   eval      DO0TSER='C'
048600140507     c                   eval      DO0soglia='T'
048700140507     c                   call      'TISIO9R'
048800140507     C                   PARM                    KPJBA
048900140507     C                   PARM                    tiSIO9ds
049000140507     c*
049100140507      /free
049200140507        CT_ostis=do0ostis  ;
049300140507        CT_ostfs=do0ostfs  ;
049400140507        CT_omiis=do0omiis  ;
049500140507        CT_omxfs=do0omxfs  ;
049600140507        CT_olrsc=do0olrsc  ;
049700140507        CT_olrnc=do0olrnc  ;
049800140507        CT_olrnk=do0olrnk  ;
049900140507        CT_trzs =do0trzs   ;
050000140507      /end-free
050100140507     c
050200140507     c* consegne "P "
050300140507     c                   if        klnasgl='S'
050400140507     c                   clear                   tisio9ds
050500140507     c                   eval      DO0VER = cpcver
050600140507     c                   eval      DO0NAR = cpcnar
050700140507     c                   eval      DO0CAP =cpccap
050800140507     c                   eval      DO0LOC =*blanks
050900140507     c                   eval      DO0LNA =klna
051000140507     c                   eval      DO0TSER='C'
051100140507     c                   eval      DO0soglia='P'
051200140507     c                   call      'TISIO9R'
051300140507     C                   PARM                    KPJBA
051400140507     C                   PARM                    tiSIO9ds
051500140507     c*
051600140507      /free
051700140507        CP_ostis=do0ostis  ;
051800140507        CP_ostfs=do0ostfs  ;
051900140507        CP_omiis=do0omiis  ;
052000140507        CP_omxfs=do0omxfs  ;
052100140507        CP_olrsc=do0olrsc  ;
052200140507        CP_olrnc=do0olrnc  ;
052300140507        CP_olrnk=do0olrnk  ;
052400140507        CP_trzs =do0trzs   ;
052500140507      /end-free
052600140507     c                   ENDIF
052700140507     c*
052800140507     c* RITIRI
052900140507     c                   clear                   tisio9ds
053000140507     c                   eval      DO0VER = cpcver
053100140507     c                   eval      DO0NAR = cpcnar
053200140507     c                   eval      DO0CAP =cpccap
053300140507     c                   eval      DO0LOC =*blanks
053400140507     c                   eval      DO0LNA =klna
053500140507     c                   eval      DO0TSER='R'
053600140507     c                   call      'TISIO9R'
053700140507     C                   PARM                    KPJBA
053800140507     C                   PARM                    tiSIO9ds
053900140507     c*
054000140507      /free
054100140507        RT_ostis=do0ostis  ;
054200140507        RT_ostfs=do0ostfs  ;
054300140507        RT_omiis=do0omiis  ;
054400140507        RT_omxfs=do0omxfs  ;
054500140507        RT_olrsc=do0olrsc  ;
054600140507        RT_olrnc=do0olrnc  ;
054700140507        RT_olrnk=do0olrnk  ;
054800140507        RT_trzs =do0trzs   ;
054900140507      /end-free
055000140507     c
055100140507     c* Leggo le località e per ogni linea vedo se presenti gli orari
055200140507     c* per il tipo servizio che devo elaborare
055300140507     c     kcpc          setll     azcpl01l
055400140507     c     kcpc          reade     azcpl01l
055500140507    1c                   dow       not %eof(azcpl01l)
055600140507
055700140507     c* Se linee a livello di LOC verifico se c'è la linea che sto elaborando
055800140507    2c                   if        cplliv='C' or cpllna=klna or cpllos=klna or
055900140507     c                             cpllol=klna
056000140507     c* CONSEGNE "  "
056100140507     c                   eval      ktser='C'
056200140507     c                   eval      kloc=cplloc
056300140507     c                   clear                   kserv
056400140507     c     kcos          chain     azcos03l
056500140507    3c                   if        %found(azcos03l)
056600140507      /free
056700140507  4       if  CT_ostis= cosostis   and
056800140507              CT_ostfs= cosostfs   and
056900140507              CT_omiis= cosomiis   and
057000140507              CT_omxfs= cosomxfs   and
057100140507              CT_olrsc= cosolrsc   and
057200140507              CT_olrnc= cosolrnc   and
057300140507              CT_olrnk= cosolrnk   and
057400140507              CT_trzs = costrzs            ;
057500140507               delete azcos000             ;
057600140507  4           endif  ;
057700140507      /end-free
057800140507     c
057900140507    3c                   endif
058000140507     c* CONSEGNE "P  "
058100140507    3C                   IF        KLNASGL='S'
058200140507     c                   eval      ktser='C'
058300140507     c                   eval      kloc=cplloc
058400140507     c                   EVAL      kserv='P '
058500140507     c     kcos          chain     azcos03l
058600140507    4c                   if        %found(azcos03l)
058700140507      /free
058800140507  5       if  CP_ostis= cosostis   and
058900140507              CP_ostfs= cosostfs   and
059000140507              CP_omiis= cosomiis   and
059100140507              CP_omxfs= cosomxfs   and
059200140507              CP_olrsc= cosolrsc   and
059300140507              CP_olrnc= cosolrnc   and
059400140507              CP_olrnk= cosolrnk   and
059500140507              CP_trzs = costrzs            ;
059600140507               delete azcos000             ;
059700140507  5           endif  ;
059800140507      /end-free
059900140507     c
060000140507    4c                   endif
060100140507    3c                   endif
060200140507     c* Ritiri
060300140507     c                   eval      ktser='R'
060400140507     c                   eval      kloc=cplloc
060500140507     c                   clear                   kserv
060600140507     c     kcos          chain     azcos03l
060700140507    3c                   if        %found(azcos03l)
060800140507      /free
060900140507  4       if  RT_ostis= cosostis   and
061000140507              RT_ostfs= cosostfs   and
061100140507              RT_omiis= cosomiis   and
061200140507              RT_omxfs= cosomxfs   and
061300140507              RT_olrsc= cosolrsc   and
061400140507              RT_olrnc= cosolrnc   and
061500140507              RT_olrnk= cosolrnk   and
061600140507              RT_trzs = costrzs            ;
061700140507               delete azcos000             ;
061800140507  4           endif  ;
061900140507      /end-free
062000140507     c
062100140507    3c                   endif
062200140507     C
062300140507    2c                   endif
062400140507     c
062500140507     c     kcpc          reade     azcpl01l
062600140507    1c                   enddo
062700140507     c
062800140507     c                   ENDSR
062900020919     C*--------------------------------------------------------------------------------------------*
063000020919     C* REPERISCE I DATI UTENTE
063100020919     C*--------------------------------------------------------------------------------------------*
063200020919     C     REPDATIUTE    BEGSR
063300020919     C*
063400020919     C* INIZIALIZZA VARIABILI DI WRK
063500020919     C                   CLEAR                   TIBS34DS
063600020919     C                   CLEAR                   AZUTEDS
063700020919     C                   CLEAR                   DDATIUTE
063800020919     C*
063900020919     C     *DTAARA       DEFINE    §azute        azuteds
064000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
064100020919     C                   IN(E)     *DTAARA
064200020919if  1C                   IF        %Error
064300020919     c                   EVAL      I34Tla = 'L'
064400020919     C                   CALL      'TIBS34R'
064500020919     C                   PARM                    Tibs34Ds
064600020919     C                   IN        *DTAARA
064700020919e   1C                   ENDIF
064800020919     C*
064900020919     C                   ENDSR
065000000000     C*------------------------------------------------------------------------*
065100000000     C* *INZSR - OPERAZIONI INIZIALI
065200000000     C*------------------------------------------------------------------------*
065300000000     C     *INZSR        BEGSR
065400980211     C*---
065500000000     C* RICEVIMENTO PARAMETRI
065600980211     C*---
065700000000     C     *ENTRY        PLIST
065800000000     C                   PARM                    KPJBA
065900140429     C                   MOVEL     KPJBU         tisio4ds
066000980211     C*---
066100140214     c*
066200140506     c     kcpcLet       klist
066300140506     c                   kfld                    isio4ver
066400140506     c                   kfld                    knar              3
066500140506     c                   kfld                    isio4cap
066600140506     c     kcpc          klist
066700140506     c                   kfld                    cpcver
066800140506     c                   kfld                    cpcnar
066900140506     c                   kfld                    cpccap
067000140304     c     kcos          klist
067100140304     c                   kfld                    cpcver
067200140409     c                   kfld                    cpcnar
067300140409     c                   kfld                    cpccap
067400140409     c                   kfld                    kloc
067500140409     c                   kfld                    klna
067600140409     c                   kfld                    ktser
067700140505     c                   kfld                    kserv
067800980211     C*---
067900000000     C* VARIABILI DI CONTROLLO
068000980211     C*---
068100980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
068200980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
068300980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
068400980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
068500050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
068600020527     C*
068700020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
068800020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
068900020527     C                   Z-ADD     N8            G08DAT
069000020527     C                   Z-ADD     *ZEROS        G08INV
069100020527     C                   MOVEL     '0'           G08ERR
069200020527     C                   CALL      'XSRDA8'
069300020527     C                   PARM                    WLBDA8
069400020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
069500020925     C*
069600020925     C* REPERISCE I DATI UTENTE
069700020925     C                   EXSR      REPDATIUTE
069800970707     C*
069900140213     c                   if        isio4ver=0
070000140410     C                   Z-ADD     isio4dri      P96DRI            8 0
070100970707     C                   Z-ADD     *ZEROS        P96VER            5 0
070200970707     C                   CALL      'TISI96R'
070300970707     C                   PARM                    P96DRI
070400970707     C                   PARM                    P96VER
070500140213     c                   eval      isio4ver=p96ver
070600140213     c                   endif
070700960709     C*
070800000000     C                   ENDSR
