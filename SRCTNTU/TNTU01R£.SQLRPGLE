000700051121      /TITLE Anagrafico attivià
000800030123
000900051121     Ftntu01d   CF   E             WORKSTN
001000000612     F                                     SFILE(S01:RRNS01)
001400051121     Ftntua01l  iF   E           k disk
001401051121     Ftntbe01l  iF   E           k disk
001500011228      *
001600020103     D Psds           SDS
001700020103     D  PgmName          *PROC
001800011228     D Kpjba         E DS
001801051121     D dtuc          E DS
001802051121     D dtur          E DS
001803051129     D dtut          E DS
001804051121     D tntb38ds      E DS
001805051122     D tntu02ds      E DS
001806051122     D tntu03ds      E DS
005200030120
005300051121     D tntua           DS
005400011227     d sel0001                       15  0
005500051121     d tuaatb                         1
005501051122     d tuaatt                         3
005600051121     d tuaprg                         3  0
005601051121     d tuades                        30
005701051129     d tuachi                         2
005800051121     d tuarep                         5
005801051213     d tuamult                        3
006100051121     d tuadti                          d
006200051121     d tuadtf                          d
006400011228      *
006500011227     d data            ds            10
006600011228     d aa                      3      4
006700011228     d mm                      6      7
006800011228     d gg                      9     10
006900011228      *
007000011228     D com6            S              6
007100011228     D WrkSqlCmd       S           1024
007200011227     D WrkEofS01       S              1
007300011227     D WrkCarS01       S              1
007400011228     D $VIDEO          S             10
007500011228     D VDESS           S                   LIKE(VDES)
007600011228     D rrns01          S              5i 0
007700011228     D SavOpzione      S                   like(s01opz)
008400051125     D dtaval          S               d
008401051125     D dtaini          S               d
008500011227     D dtafin          S               d
008800011228      **********************************************************************
008900011228      * Constants
009000011228      *     MaxKey - Maximum number of key fields allowed by this program
009100011228      **********************************************************************
009200011228     D MaxKey          C                    1
009300011228     D Descrizione     C                   '1'
009400011228     D Ascendente      C                   1
009500011228     D Carattere       C                   6
009600011228     D Put             C                   1
009700011228     D EndPut          C                   2
009800011228     D Get             C                   3
009900011228      **********************************************************************
010000011228      * Standalone fields
010100011228      *     SizeList   - Size of list
010200011228      *     ReturnSize - Size of list returned by sort API
010300011228      **********************************************************************
010400011228     D NotUsed         S             16A
010500011228     D ReturnSize      S              9B 0
010600011228     D SizeList        S              9B 0
010700011228     D RrnLast         S              5i 0
010800011228     D WrkSort         S              1
010900011228      **********************************************************************
011000011228      * Data Structures
011100011228      *     SflRcd     - The entire subfile record to pass to the sort
011200011228      *     QLGSCB     - The sort request block for the QLGSORT API
011300011228      *     QLGSCB00   - The sort request block for the QLGSRTIO API
011400011228      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
011500011228      *     QUSEC      - Error structure for the QLGSORT API
011600011228      **********************************************************************
011700011228     D SflRcd          DS
011800011228     D  S01opz
011900051121     D  S01atb
012000051121     D  S01att
012100051121     D  S01prg
012200051121     D  S01des
012300051121     D  S01chi
012400051121     D  S01rep
012401051129     D  S01mult
012500011228     D  S01dti
012600011228     D  S01dtf
012700011228     d  hnrr
012800011228     D  Selected                      1A
012900011228
013000011228      /COPY QSYSINC/QRPGLESRC,QLGSORT
013100011228     D QLGKL                         16    DIM(MaxKey)
013200011228     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
013300011228     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
013400011228     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
013500011228     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
013600011228
013700011228      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
013800011228      /COPY QSYSINC/QRPGLESRC,QUSEC
013900051121      *---------------- Gestione formato video v01 -------------------------
014000051121     C                   EXSR      Inzv01
014100051121     C                   DOU       $Video <> 'V01'
014200000608      *----------- Visualizzo dati di output in caso di errori.-------------
014300000608     C                   IF        *IN99
014400000608     C                   EVAL      *IN99 = *OFF
014500051121     C                   WRITE     V01
014600000608     C                   EVAL      *IN99 = *ON
014700000608     C                   ENDIF
014800000608      *
014900051121     C                   EXFMT     v01
015000000608      *
015100011227     C                   SELECT
015200011227     C                   WHEN      *INKC
015300011227     C                             OR
015400011227     C                             *INKL
015500011227     C                   EVAL      $Video = *BLANKS
015600011227
015700011228     C                   WHEN      *INKD
015800011228     C                   EXSR      Search
015801051124     C                   EXSR      Chkv01
016100011227     C                   OTHER
016200051121     C                   EXSR      Chkv01
016300011227     C  N99              EXSR      WrkC01
016400011227     C                   ENDSL
016500011227      *
016600011227     C                   ENDDO
016700011227      *
016800011227     C                   EXSR      Uscita
016900011227     c**********************************************************************
017000011227     c* uscita
017100011227     c**********************************************************************
017200011227     C     Uscita        BEGSR
017300011227      *
017400011227     C                   EVAL      *INLR = *ON
017500011227     C                   RETURN
017600011227      *
017700011227     C                   ENDSR
017800011227     c**********************************************************************
017900011227     c* gestione sfl
018000011227     c**********************************************************************
018100000627     C     WrkC01        BEGSR
018200011227     c* compone comando sql
018300011227     c                   exsr      srsql
018400000627      *
018500011227     C                   EVAL      WrkCarS01 = *ON
018600011227     C                   EVAL      $Video = 'C01'
018700011227      *
018800011227     C                   DOU       $Video <> 'C01'
018900000627     C                   EXSR      CarS01
019000011228     c*Descrizione della ricerca
019100020213     c*                  if        wrksort = ' '
019200020213     c*                  eval      destps = 'Descrizione'
019300020213     c*                  else
019400020213     c*                  eval      destps = 'Codice'
019500020213     c*                  end
019600000627      *
019700011227     C                   WRITE     r03
019800000607     C                   EXFMT     C01
019900011227     C                   SELECT
020000011228     c* fine
020100011227     C                   WHEN      *INKC=*ON
020200011227     C                   EVAL      $VIDEO=*BLANKS
020300011228     c* guida
020400011227     C                   WHEN      *INKL=*ON
020500051121     C                   EVAL      $VIDEO='V01'
020600011228     c* ripristino
020700011227     C                   WHEN      *INKE=*ON
020800011227     C                   EVAL      WrkCarS01 = *ON
020900020213     c*Cambio ordinamento (NON ABILITATO)
021000011228     C                   WHEN      *INKK
021100011228     C                   EXSR      Sort
021200011228     c*ripeti opzione (NON ABILITATO)
021300011228     C                   WHEN      *INKM
021400011228     C                   EXSR      RipetiOpz
021500011227
021600011227     C                   OTHER
021700011227
021800011227     C                   EXSR      GestioneSFL
021900011227     C                   ENDSL
022000011228
022100011227     C                   ENDDO
022200011227     C                   ENDSR
022300011227     c**********************************************************************
022400051121     c* Controlli video V01.
022500011227     c**********************************************************************
022600051121     C     Chkv01        BEGSR
022700030123
022900030123     C                   EVAL      *IN99 = *OFF
024600030115
025400051121     c* attività
025500051121     C                   IF        v01att <> *blanks
025600051121     C     keytua        chain     tntua01l
025700030115     C                   IF        NOT %FOUND
025800051121     c                   seton                                        9901
025801051121     c                   else
025802051121     c                   eval      v01des = tuades
025900030115     C                   ENDIF
025901051124     c                   else
025902051124     c                   clear                   v01des
026000000627     C                   ENDIF
026100030123
026200051121     c* p.o.
026300051121     C                   IF        v01chi <> *blanks
026301051121     c                   eval      tbecod='TUC'
026302051124     c                   movel(p)  v01chi        tbeke1
030900051121     C     keytab        chain     tntbe01l
030901051121     C                   IF        NOT %FOUND
031000051121     c                   seton                                        0299
031001051121     c                   else
031002051121     c                   eval      dtuc = tbeuni
031003051121     c                   eval      v01dchi = §tucdes
031004051121     C                   ENDIF
031005051124     c                   else
031006051124     c                   clear                   v01dchi
031100020213     C                   ENDIF
031200030115
031300051121     c* reparto
031400051121     C                   IF        v01rep <> *blanks
031401051121     c                   eval      tbecod='TUR'
031402051124     c                   movel(p)  v01rep        tbeke1
031700051121     C     keytab        chain     tntbe01l
031701051121     C                   IF        NOT %FOUND
031702051121     c                   seton                                        0399
031703051121     c                   else
031704051121     c                   eval      dtur = tbeuni
031705051121     c                   eval      v01drep = §turdes
031706051121     C                   ENDIF
031707051124     c                   else
031708051124     c                   clear                   v01drep
031900020213     C                   ENDIF
031901051129     c* moltiplicatore
031902051129     C                   IF        v01mult<> *blanks
031903051129     c                   eval      tbecod='TUT'
031904051129     c                   movel(p)  v01mult       tbeke1
031905051129     C     keytab        chain     tntbe01l
031906051129     C                   IF        NOT %FOUND
031907051129     c                   seton                                        0799
031908051129     c                   else
031909051129     c                   eval      dtut = tbeuni
031910051129     c                   eval      v01dmult= §tutdes
031911051129     C                   ENDIF
031912051129     c                   else
031913051129     c                   clear                   v01dmult
031914051129     C                   ENDIF
032000030115
032100011227     c* data inizio
032200051121     C                   IF        v01dti <> 0
032300051121     C     *dmy          test(d)                 v01dti                 04
032400051121     c                   if        *in04
032500020103     c                   seton                                        99
032600020103     c                   else
032700051121     c     *dmy          move      v01dti        dtaini
032800020103     c                   end
032900011228     c                   else
033000011228     c                   move      '0001-01-01'  dtaini
033100000627     C                   ENDIF
033200011227     c* data fine
033300051121     C                   IF        v01dtf <> 999999
033400051121     C     *dmy          test(d)                 v01dtf                 05
033500051121     c                   if        *in05
033600020103     c                   seton                                        99
033700020103     c                   else
033800051121     c     *dmy          move      v01dtf        dtafin
033900020103     c                   end
034000020103     c                   else
034100020103     c                   move      '9999-12-31'  dtafin
034200020103     c                   end
034300030115
034400030115     C                   IF        DtaFin < DtaIni
034500051121     c                   seton                                        040599
034600030115     C                   ENDIF
034601051125     c* data validità
034602051125     C                   IF        v01dtv <> 999999
034603051125     C     *dmy          test(d)                 v01dtv                 06
034604051125     c                   if        *in06
034605051125     c                   seton                                        99
034606051125     c                   else
034607051125     c     *dmy          move      v01dtv        dtaval
034608051125     c                   end
034609051125     c                   else
034610051125     c                   move      '0001-01-01'  dtaval
034611051125     C                   ENDIF
034700000608      *
034800000608     C                   ENDSR
034900011227     c**********************************************************************
035000051121     c* inizializza V01
035100011227     c**********************************************************************
035200051121     C     InzV01        BEGSR
035300000608
035400051121     C                   EVAL      $Video = 'V01'
035500051121     C                   clear                   v01att
035501051121     C                   clear                   v01prg
035502051121     C                   clear                   v01des
035503051121     C                   clear                   v01chi
035504051121     C                   clear                   v01dchi
035505051129     C                   clear                   v01mult
035506051129     C                   clear                   v01dmult
035507051121     C                   clear                   v01rep
035508051121     C                   clear                   v01drep
036200051121     C                   EVAL      v01dti = 0
036300051121     C                   EVAL      v01dtf = 999999
036301051125     C                   EVAL      v01dtv = 999999
036400051121     C                   EVAL      v01atb = 'N'
036500051121     C                   EXSR      Chkv01
036600000609
036700000608     C                   ENDSR
036800011228     c**********************************************************************
036900011228     c* ricerca
037000011228     c**********************************************************************
037100011228     C     Search        BEGSR
037400030123
037401051121     C                   clear                   Tntb38DS
037402051121     C                   eval      t38tla = 'L'
037403051121     C                   movel     'R'           t38mod
037404051121     C                   movel     *blanks       t38sif
037500020213     c                   select
037600051121     c* attività
037700051121     C                   when      rtncsrfld = 'V01ATT'
039200051121     c* chi esegue attività
039300051121     C                   when      rtncsrfld = 'V01CHI'
039301051121     C                   movel     'TUC'         t38cod
039302051121     C                   CALL      'TNTB38R'
039303051121     C                   parm                    KPJBA
039304051121     C                   parm                    TNTB38DS
039305051121    2C                   IF        t38err = *blanks
039306051121     C                   movel     t38ke1        v01chi
039307051121     C                   movel     t38uni        dtuc
039308051121     C                   movel     §tucdes       v01dchi
039309051121     C                   else
039310051121     C                   seton                                        0299
039311051121     C                   ENDIF
040300051121     c* reparto
040400051121     C                   when      rtncsrfld = 'V01REP'
040401051121     C                   movel     'TUR'         t38cod
040402051121     C                   CALL      'TNTB38R'
040403051121     C                   parm                    KPJBA
040404051121     C                   parm                    TNTB38DS
040405051121    2C                   IF        t38err = *blanks
040406051121     C                   movel     t38ke1        v01rep
040407051121     C                   movel     t38uni        dtur
040408051121     C                   movel     §turdes       v01drep
040409051121     C                   else
040410051121     C                   seton                                        0399
040411051121     C                   ENDIF
040412051129     c* moltiplicatore
040413051129     C                   when      rtncsrfld = 'V01MULT'
040414051129     C                   movel     'TUT'         t38cod
040415051129     C                   CALL      'TNTB38R'
040416051129     C                   parm                    KPJBA
040417051129     C                   parm                    TNTB38DS
040418051129    2C                   IF        t38err = *blanks
040419051129     C                   movel     t38ke1        v01mult
040420051129     C                   movel     t38uni        dtut
040421051129     C                   movel     §tutdes       v01dmult
040422051129     C                   else
040423051129     C                   seton                                        0799
040424051129     C                   ENDIF
044700020213     c                   endsl
044800020213     C*
044900011228     C                   ENDSR
045000011228     c**********************************************************************
045100011228     c* carica sfl
045200011228     c**********************************************************************
045300011228     C     CarS01        BEGSR
045400011228     C                   IF        WrkCarS01 = *ON
045500011228     C                   EVAL      RRNS01 = 0
045600020218     C                   EVAL      *IN90 = *ON
045700020218     C                   EVAL      *IN91 = *OFF
045800011228     C                   WRITE     C01
045900020218     C                   EVAL      *IN90 = *OFF
046000051121     c* Lettura tntua01l
046100011228     C/EXEC SQL
046200011228     C+ PREPARE S1 FROM :WrkSqlCmd
046300011228     C/END-EXEC
046400011228
046500011228     C/EXEC SQL
046600011228     C+ DECLARE A1 CURSOR FOR S1
046700011228     C/END-EXEC
046800011228
046900011228     C/EXEC SQL
047000011228     C+ OPEN A1
047100011228     C/END-EXEC
047200011228
047300051121     C                   DO        *hival
047400011228     C/EXEC SQL
047500051121     C+ FETCH NEXT FROM A1 INTO :tntua
047600011228     C/END-EXEC
047700011228     c*
047800011228     C                   SELECT
047801051121     c* fine file
047802051121     C                   WHEN      SqlCod = 100
047803051121     c                   leave
047804051121     C                   WHEN      SqlCod < 0
047805051121      * forzo la stampa del JOBLOG.
047806051121     C                   seton                                        h1lr
047807051121     c                   return
047808051121     c* dettaglio
047900011228     C                   WHEN      SqlCod = 0
048000051121     c                   movel     *blanks       s01atb
048100051121     c                   if        tuaatb <> *blanks
048200051121     c                   if        v01atb = 'S'
048300051121     c                   movel     'A'           s01atb
048400020214     c                   else
048500020214     c                   iter
048600020214     c                   end
048700020214     c                   end
048800011228     c                   movel     *blanks       s01opz
048900011228     c                   movel     sel0001       hnrr
049000051121     c                   movel     tuaatt        s01att
049100051121     c                   move      tuaprg        s01prg
049200051121     c                   movel     tuades        s01des
049300051121     c                   movel     tuachi        s01chi
049301051129     c                   movel     tuamult       s01mult
049400051121     c                   movel     tuarep        s01rep
049600011228     c                   z-add     0             s01dti
049700051121     c                   if        tuadti  <> *loval
049800051121     c                   move      tuadti        data
049900011228     c                   eval      com6 = gg + mm + aa
050000011228     c                   move      com6          s01dti
050100011228     c                   end
050200020103     c                   z-add     999999        s01dtf
050300051121     c                   if        tuadtf <> *hival
050400051121     c                   move      tuadtf        data
050500011228     c                   eval      com6 = gg + mm + aa
050600011228     c                   move      com6          s01dtf
050700011228     c                   end
050800011228     C                   EVAL      RRNS01 = RRNS01 + 1
050900011228     C                   WRITE     S01
051400011228     C                   ENDSL
051500011228     C                   ENDDO
051600011228
051700011228     C/EXEC SQL
051800011228     C+ CLOSE A1
051900011228     C/END-EXEC
052000020214     C*                  EVAL      RrnLast = Rrns01
052100020218     c* carico api X CODICE
052200020218     C*                  EVAL      WrkSort = ' '
052300020218     c*                  exsr      sort
052400011228     C                   EVAL      WrkCarS01 = *OFF
052500020218     C                   IF        rrns01 > 0
052600020218     C                   EVAL      *IN91 = *ON
052700011228     C                   ENDIF
052800020218     C                   ENDIF
052900020214     C                   EVAL      RRNS01  = 1
053000020218     C                   EVAL      rcdnbr  = 1
053100020218     C                   EVAL      c01csrrrn = 1
053200011228     C                   ENDSR
053300011227     c**********************************************************************
053400011227     c* gestione sfl
053500011227     c**********************************************************************
053600000613     C     GestioneSFL   BEGSR
053700000627      *
053800000627     C                   EVAL      WrkEofS01 = *OFF
053900000627      * F10 Immissione.
054000000627     C                   IF        *INKJ
054001051122     c                   clear                   tntu02ds
054200051122     c                   move      '1 '          t02mod
054201051122     c                   movel(p)  tntu02ds      kpjbu
054300051121     c                   call      'TNTU02R'
054400051121     c                   parm                    KPJBA
054401051122     c                   movel     kpjbu         tntu02ds
054700000627      * Se il programma chiamato si è chiuso con ...
054800000627     C                   SELECT
054900000627      * F3
055000051122     C                   WHEN      t02cmd = 'C'
055100000627     C                   EXSR      Uscita
055200011227      * F12
055300051122     C                   WHEN      t02cmd = 'L'
055400011227     C                   EVAL      WrkEofS01 = *ON
055500000703      * F6
055600051122     C                   WHEN      t02cmd = 'F'
055700011228     C                   EVAL      WrkCarS01 = *ON
055800000627     C                   ENDSL
055900000627      *
056000011228     C                   Else
056100000627      * Elaborazione opzioni.
056200020219     C                   IF        *IN91
056300011228     c                   do        *hival        X                 4 0
056400011228     c     X             CHAIN     s01                                50
056500011228     C   50              LEAVE
056600011228     c*
056700051121     C                   IF        S01opz <> *blanks
056901060110     c                   if        s01opz <> 'T'
056904051121     c* anagrafica attività
056905051122     c                   clear                   tntu02ds
056906051122     c                   z-add     hnrr          t02nrr
056907051122     c                   move      s01opz        t02mod
056908060110     c* se profilo <> EDP forzo la visualizzazione
056909060110     c                   if        %subst(knmus: 1: 3)<>'EDP'
056910060110     c                   eval      t02mod = '5'
056911060110     c                   end
056912060110     c*
056913051122     c                   movel(p)  tntu02ds      kpjbu
057000051121     c                   call      'TNTU02R'
057100051121     c                   parm                    KPJBA
057301051122     c                   movel     kpjbu         tntu02ds
057302051121     c                   else
057303051121     c* anagrafica tempi
057304051122     c                   clear                   tntu03ds
057305051122     c                   z-add     s01prg        t03prg
057306051122     c                   movel     s01att        t03att
057307051122     c                   movel(p)  tntu03ds      kpjbu
057308051121     c                   call      'TNTU03R'
057309051122     c                   parm                    kpjba
057310051122     c                   movel     kpjbu         tntu03ds
057311051122     c                   movel     t03cmd        t02cmd
057312051121     c                   end
057400000627      * Se il programma chiamato si è chiuso con ...
057500000627     C                   SELECT
057600000627      * F3
057700051122     C                   WHEN      t02cmd = 'C'
057800000627     C                   EXSR      Uscita
057900000627      * F6
058000051122     C                   WHEN      t02cmd = 'F'
058100011227     C                   CLEAR                   S01opz
058200011227     C                   EVAL      WrkCarS01 = *ON
058300020128     C                   UPDATE    S01
058400011228     C                   LEAVE
058500011227      * F12
058600051122     C                   WHEN      t02cmd = 'L'
058700011227     C                   CLEAR                   S01opz
058800011227     C                   EVAL      WrkEofS01 = *ON
058900020128     C                   UPDATE    S01
059000011228     C                   LEAVE
059100000627     C                   ENDSL
059200011228     C                   ENDIF
059300011228     C* CONTROLLO SE EFFETTUATA RICERCA
059400020213     C*                  IF        VDES <> ' ' AND VDES <> VDESS
059500020213     c*                  setoff                                       91
059600020213     C*                  if        (wrksort = ' ' and VDES <= S01DES) or
059700051121     C*                            (wrksort = '1' and VDES <= S01att)
059800020213     c*                  seton                                        91
059900020213     C*                  Z-ADD     X             rcdnbr
060000020213     C*                  MOVEL     VDES          VDESS
060100020213     C*                  LEAVE
060200020213     C*                  END
060300020213     C*                  END
060400000619     C                   ENDDO
060500020213     C*  91              clear                   VDES
060600011228     c                   end
060700020219     c                   end
060800000627      *
060900000613     C                   ENDSR
061000011227     c**********************************************************************
061100000703      * Ripetizione opzione in tutte le righe del subfile.
061200011227     c**********************************************************************
061300000703     C     RipetiOpz     BEGSR
061400000703      *
061500000703     C                   IF        C01CsrRrn > *ZERO
061600000703     C                   EVAL      RrnS01 = C01CsrRrn
061700000703     C     RrnS01        CHAIN     S01
061800011228     C                   IF        %FOUND and s01opz <> *blanks
061900011227     C                   EVAL      SavOpzione = S01opz
062000000703     C                   UPDATE    S01
062100000703     C                   EVAL      WrkEofS01 = *OFF
062200000703     C                   DOU       WrkEofS01 = *ON
062300000703     C                   ADD       1             RrnS01
062400000703     C     RrnS01        CHAIN     S01
062500000703     C                   IF        %FOUND
062600011227     C                   EVAL      S01opz = SavOpzione
062700000703     C                   UPDATE    S01
062800000703     C                   ELSE
062900000703     C                   EVAL      WrkEofS01 = *ON
063000000703     C                   ENDIF
063100000703     C                   ENDDO
063200000703     C                   ENDIF
063300000703     C                   ENDIF
063400000703      *
063500000703     C                   ENDSR
063600011228      **********************************************************************
063700011228      * Subroutine - Sort
063800011228      *   This subroutine sorts the subfile records.
063900011228      **********************************************************************
064000011228     C     Sort          BEGSR
064100011228     C                   CLEAR                   vdes
064200011228     C                   CLEAR                   vdess
064300011228     C                   EVAL      rcdnbr  = 1
064400011228
064500011228      * Initialize the key fields to sort on. There is one extra field not in
064600011228      * the subfile -- Selected -- that is added to the record. This field
064700011228      * is used to place selected records in front of nonselected records.
064800011228
064900011228     C                   CLEAR                   QLGSCB
065000011228     C                   CLEAR                   QLGSCB00
065100011228
065200011228     ** Gestione della scelta ordinamento.
065300011228     C                   SELECT
065400011228     ** Ordinamento per descrizione.
065500011228     C                   WHEN      WrkSort = Descrizione
065600011228     ** 1 campo chiave.
065700011228     C                   EVAL      QLGNBRK    = 1
065800011228     ** La descrizione è
065900051121     ** a posizone 10, 30 byte, char, ascending.
066000051121     C                   EVAL      QLGSP      = 10
066100051121     C                   EVAL      QLGSS      = %SIZE(S01des)
066200011228     C                   EVAL      QLGDT      = Carattere
066300011228     C                   EVAL      QLGSO      = Ascendente
066400011228     C                   EVAL      QLGKL(1)   = QLGSKL
066500011228     ** Al prossimo F11 ordino per CODICE
066600011228     C                   EVAL      WrkSort = ' '
066700011228     ** Ordinamento per Codice
066800011228     C                   WHEN      WrkSort = ' '
066900011228     C                   EVAL      QLGNBRK    = 1
067000011228     ** Il codice è
067100051121     ** a posizione 3, 3 byte, char, ascending
067200011228     C                   EVAL      QLGSP      = 3
067300051121     C                   EVAL      QLGSS      = %SIZE(S01att)
067400011228     C                   EVAL      QLGDT      = Carattere
067500011228     C                   EVAL      QLGSO      = Ascendente
067600011228     C                   EVAL      QLGKL(1)   = QLGSKL
067700011228     ** Al prossimo F11 ordino per descrizione.
067800011228     C                   EVAL      WrkSort = Descrizione
067900011228     C                   ENDSL
068000011228      * Load other sort parameters.
068100011228     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
068200011228     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
068300011228     C                   EVAL      QLGRT     = 8
068400011228     C                   EVAL      QLGOKL    = 80
068500011228     C                   EVAL      QLGLKE    = 16
068600011228     C                   EVAL      QLGLSS    = 290
068700011228      * Initialize Sort I/O API fields.
068800011228     C                   EVAL      QLGRL00  = QLGRL
068900011228     C                   EVAL      QLGRC00  = 1
069000011228     C                   CLEAR                   QUSEI
069100011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
069200011228      * First step - Initialize the sort routine.
069300011228     C                   CALL      'QLGSORT'
069400011228     C                   PARM                    QLGSCB
069500011228     C                   PARM                    NotUsed
069600011228     C                   PARM                    NotUsed
069700011228     C                   PARM                    SizeList
069800011228     C                   PARM                    ReturnSize
069900011228     C                   PARM                    QUSEC
070000011228      * Next step - Write records to I/O routine.
070100011228     C                   EVAL      QLGRT00 = Put
070200011228     C                   DO        RrnLast       x
070300011228     C     x             CHAIN     S01
070400011228     ** Solo le righe con Selected = 'Y' sono riordinate,
070500011228     ** quindi per fare un ordinamento di tutte le righe
070600011228     ** metto 'Y' sempre.
070700011228     C                   EVAL      Selected = 'Y'
070800011228     C                   CLEAR                   QUSEI
070900011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
071000011228     C                   CALL      'QLGSRTIO'
071100011228     C                   PARM                    QLGSCB00
071200011228     C                   PARM                    SflRcd
071300011228     C                   PARM                    NotUsed
071400011228     C                   PARM                    SizeList
071500011228     C                   PARM                    NotUsed
071600011228     C                   PARM                    QUSEC
071700011228     C                   ENDDO
071800011228      * Next step - Signal end of input, clear subfile for reload.
071900011228     C                   EVAL      QLGRT00 = EndPut
072000011228     C                   CLEAR                   QUSEI
072100011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
072200011228
072300011228     C                   CALL      'QLGSRTIO'
072400011228     C                   PARM                    QLGSCB00
072500011228     C                   PARM                    SflRcd
072600011228     C                   PARM                    NotUsed
072700011228     C                   PARM                    SizeList
072800011228     C                   PARM                    NotUsed
072900011228     C                   PARM                    QUSEC
073000011228     c                   seton                                        90
073100011228     C                   WRITE     C01
073200011228     C                   setoff                                       90
073300011228      * Final step - Write the records back to the subfile.
073400011228     C                   EVAL      QLGRT00 = Get
073500011228     C                   DO        RrnLast       x
073600011228     C                   CLEAR                   QUSEI
073700011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
073800011228     C                   CALL      'QLGSRTIO'
073900011228     C                   PARM                    QLGSCB00
074000011228     C                   PARM                    NotUsed
074100011228     C                   PARM                    SflRcd
074200011228     C                   PARM                    QLGRL00
074300011228     C                   PARM                    NotUsed
074400011228     C                   PARM                    QUSEC
074500011228     c                   z-add     x             rrns01
074600011228     C                   WRITE     S01
074700011228     C                   ENDDO
074800011228     C                   ENDSR
074900011227     c**********************************************************************
075000000607     C     *INZSR        BEGSR
075100011227     c**********************************************************************
075200000607      *
075300000607     C     *ENTRY        PLIST
075400000607     C                   PARM                    Kpjba
075500000607      *
075600051121     C     Keytua        KLIST
075700051121     C                   KFLD                    v01att
075800051121     C                   KFLD                    v01prg
076000051121     C     Keytab        KLIST
076100051121     C                   KFLD                    tbecod
076200051121     C                   KFLD                    tbeke1
076201060110     c* abilito la manutenzione solo se profilo EDP*
076202060110     c                   if        %subst(knmus: 1: 3)<>'EDP'
076203060110     c                   seton                                        40
076204060110     c                   end
078600011227     C                   ENDSR
080300011227     c**********************************************************************
080400011227     c     srsql         begsr
080500011227     c**********************************************************************
080600011227     C                   EVAL      WrkSqlCmd
080700011227     C                             =
080800011227     C                             'SELECT '
080900011227     C                             +
081000051121     C                             'rrn(tntua01l), tuaatb, tuaatt, -
081100051121     C                             tuaprg, tuades, tuachi, -
081200051129     C                             tuarep, tuamult, tuadti, tuadtf -
081300051121     C                             FROM TNTUA01L '
081400011227     C                             +
081500051121     C                             ' WHERE TUAATT <> '' '''
081600051121     c*scelta attività
081700051121     C                   IF        v01att <> *blanks
081800030114     C                   EVAL      WrkSqlCmd
081900030114     C                             =
082000030114     C                             %TRIMR(WrkSqlCmd)
082100030114     C                             +
082200051121     C                             ' AND tuaatt = ' + '''' + v01att + ''''
082400051121     c*progressivo
082600011227     C                   EVAL      WrkSqlCmd
082700011227     C                             =
082800011227     C                             %TRIMR(WrkSqlCmd)
082900011227     C                             +
082901060118     C                             ' AND tuaprg = ' + %TRIML(%EDITC(v01prg:'X'))
083100011227     C                   endif
083200051121     c* chi esegue attività
083300051121     C                   IF        v01chi <> *blanks
083400011227     C                   EVAL      WrkSqlCmd
083500011227     C                             =
083600011227     C                             %TRIMR(WrkSqlCmd)
083700011227     C                             +
083800051121     C                             ' AND tuachi = ' + '''' + v01chi + ''''
083900011227     C                   endif
084000051121     ** reparto
084100051121     C                   IF        v01rep <> *blanks
084200030120     C                   EVAL      WrkSqlCmd
084300030120     C                             =
084400030120     C                             %TRIMR(WrkSqlCmd)
084500030120     C                             +
084600051121     C                             ' AND tuarep = ' + '''' + v01rep + ''''
084700030120     C                   endif
084701051129     c* moltiplicatore
084702051129     C                   IF        v01mult<> *blanks
084703051129     C                   EVAL      WrkSqlCmd
084704051129     C                             =
084705051129     C                             %TRIMR(WrkSqlCmd)
084706051129     C                             +
084707051129     C                             ' AND tuamult= ' + '''' + v01mult + ''''
084708051129     C                   endif
087200051121     c*scelta data inizio decorrenza
087300051121     C                   IF        v01dti <> 0
087400051121     c     *dmy          move      v01dti        dtaini
087500011227     c                   move      dtaini        dtainia          10
087600011227     C                   EVAL      WrkSqlCmd
087700011227     C                             =
087800011227     C                             %TRIMR(WrkSqlCmd)
087900011227     C                             +
088000051121     C                             ' AND tuadti >= ' + '''' + dtainia + ''''
088100011227     C                   endif
088200051125     c*scelta data fine decorrenza
088300051125     C                   IF        v01dtv <> 999999
088400051125     c     *dmy          move      v01dtv        dtaval
088600011227     C                   EVAL      WrkSqlCmd
088700011227     C                             =
088800011227     C                             %TRIMR(WrkSqlCmd)
088900011227     C                             +
088901051125     C                             ' AND '
088902051125     C                             +
088903051125     C                             '''' + %char(dtaval)  + ''''
088904051125     C                             +
088905051125     C                             ' between tuadti and tuadtf'
089100011227     C                   endif
089101051125     c*scelta data validità
089102051125     C                   IF        v01dtf <> 999999
089103051125     c     *dmy          move      v01dtf        dtafin
089104051125     c                   move      dtafin        dtafina          10
089105051125     C                   EVAL      WrkSqlCmd
089106051125     C                             =
089107051125     C                             %TRIMR(WrkSqlCmd)
089108051125     C                             +
089109051125     C                             ' AND tuadtf <= ' + '''' + dtafina + ''''
089110051125     C                   endif
089200020214     c* ordinamento
089300020214     C                   EVAL      WrkSqlCmd
089400020214     C                             =
089500020214     C                             %TRIMR(WrkSqlCmd)
089600020214     C                             +
089700051125     C                             ' ORDER BY tuaatt, tuaprg, tuadti, tuadtf'
089800011227     c*
089900011227     C                   ENDSR
090000030123
