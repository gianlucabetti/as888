000100981007      *-------------------------------------------------------------------------*
000200990614      *               RISTAMPA PROGETTO DI LIQUIDAZIONE                         *
000300981007      *-------------------------------------------------------------------------*
000400990909      *  Solo se la C.A. è in fase 700 = Stampato progetto di liquidazione      *
000500990909      *-------------------------------------------------------------------------*
000600990614
000700990909      ****************************************************************
000800990909      *  FASI C.A. FISSATE A PROGRAMMA
000900990909      ****************************************************************
001000990909      *    700 = Stampa progetto di liqidazione               (WFspl)
001100000131      *    690 = Ristampa progetto di liqidazione             (WRspl)
001200990909      ***************************************************************
001300990909
001400980521      ****************************************************************
001500980521      *  RIEPILOGO INDICATORI
001600980521      ***************************************************************
001700980923      * 28 - ERRORE GENERICO DSPF
001800980904      * 30 - COMODO
001900980904      * 31 - COMODO
002000981016      * 32 - COMODO
002100990614      * 40 - ERRORE C.A.
002200980904      ***************************************************************
002300980623
002400980521     H DECEDIT('0,') DATEDIT(*DMY.)
002500980623
002600990614     fFIDN55D   CF   E             WORKSTN
002700000131     fFNDCT01L  UF   E           K DISK
002800000202     fFNDKA01L  UF   E           K DISK
002900050222     fFNDCL01L  Uf   e           k disk    usropn
003000050207     fFNDCF01L  O  A E             DISK
003100980623
003200980521      *------------------------------------------------------------------------*
003300981013     D Kaac            S                   LIKE(DCTaac)
003400981013     D Kfil            S                   LIKE(DCTfil)
003500981013     D Knca            S                   LIKE(DCTnca)
003600000202     D Ktrc            S                   LIKE(DKAtrc)   INZ('I')
003700990909     D WFspl           S                   LIKE(DCTfca)   INZ(700)
003800000131     D WRspl           S                   LIKE(DCTfca)   INZ(690)
003900000204
004000000204     D kcap            s              9
004100000204     D werr            s              1
004200000204     D wfzv            s                   LIKE(E13fzv)
004300000204     D wfz1            s                   LIKE(E13fz1)
004400000204     D wfz2            s                   LIKE(E13fz2)
004500000204     D wfz3            s                   LIKE(E13fz3)
004600000204     D wmsg            s                   LIKE(O13msg)
004700000204     D wsvcap          s                   LIKE(W2Ccap)
004800000204     D wsvloc          s                   LIKE(W2Cloc)
004900000204     D wsvprv          s                   LIKE(W2Cprv)
005000000204     D XX              s              2  0
005100980923
005200980923     D WORA            S              4  0
005300980721     D W0140           S             14  0
005400980521     D Wdtgio          S              8  0
005500980521     D dateu           S              8  0
005600981015     D wdataca         S              8  0
005700980923
005800980622      *  titolo videata (lunghezza massima 34)
005900990614     D TITOLO          C                   CONST(' RISTAMPA PROGETTO DI LIQUIDA-
006000990614     D                                     ZIONE')
006100980521      *   S C H I E R E
006200000204     D MSG             S             78    DIM(8) CTDATA PERRCD(1)              MSG VIDEO
006300980923      *
006400980923     D WLBDAT          DS                  INZ
006500980923     D  G02DAT                 1      8  0
006600980923     D  G02INV                 9     16  0
006700980923     D  G02ERR                17     17
006800980923     D  G02TGI                18     22  0
006900980915      *----------------------------------
007000050207      * - Parametri x Controllo profilo utenti
007100050207     d TIBS34ds      e ds
007200050207      * - Ds di riferimento al file esterno AZUTE00F
007300050207     d AZUTEds       e ds                  extname(AZUTE00F)
007400050207      * - Ds per dati organigramma
007500050207     d dDATIUTE      e ds
007600050207      *
007700981016     D Fidn00DS      E DS
007800000204     d FNLV14DS      E DS
007900000204     d FNLV13DS      E DS
008000000204     d TISI95DS      E DS
008100000204     d TRUL42DS      E DS
008200020129     D DDCT01        E DS                  inz
008300020201     D DDCF01        E DS                  inz
008400050207     d TIBS02ds      e ds
008500050207     d dGEDdn        e ds
008600050207     d dSTD          e ds
008700980521      *
008800980521     D KPJBA         E DS
008900980521      *
009000000204     d                 ds                  INZ
009100000204     d  DS_arbdat              1      8  0
009200000204     d  dctaac                 1      4  0
009300000204     d  dctmgc                 5      8  0
009400000204      *
009500980521     D                SDS
009600980521     D  VTCPGM                 1     10
009700990122     c*---------------------------------------------------------------------------------------------
009800981013     C                   do        *hival
009900980521      * Emissione VIDEO1
010000990614     c                   write     FI55t01
010100990614     c                   write     FI55z01
010200990614     c                   exfmt     FI55d01
010300980923
010400980923     c                   setoff                                       284041
010500990419     c                   setoff                                       422190
010600980923     c                   clear                   v1cmsg
010700980623
010800990429      * f03=Fine
010900990429     c   kc              leave
011000980623
011100981014      * Controlli  videata
011200980521     c                   exsr      ctrd01
011300990429      * se è acceso il  28 ritorno ad emettere il formato video
011400990614     c   28              iter
011500981015
011600981015      * f14=Interrogazione C.A.
011700990429     C                   IF        *INKN
011800981015     c                   exsr      SUB_F14
011900981015     C                   iter
012000981015     c                   ENDIF
012100000202
012200000202      * f08=Gestione dati Intestatario P.L.
012300000202     C                   IF        *INKH
012400000202     c                   exsr      SUB_F08
012500000202     C                   iter
012600000202     c                   ENDIF
012700990429      *
012800981014      * Se non ho confermato o ci sono errori riemetto video
012900990429     c                   IF        *INKF
013000980910     c                   exsr      SUB_confer
013100981015     c                   leave
013200980908     c                   ELSE
013300981013     c                   iter
013400980908     c                   ENDIF
013500981014      *
013600981014     c                   enddo
013700990614
013800990122     c     fine          tag
013900990614      *
014000050207     c                   clear                   TIBS02ds
014100050207     c                   movel     'C'           T02tla
014200050207     c                   call      'TIBS02R'
014300050207     c                   parm                    KPJBA
014400050207     c                   parm                    TIBS02ds
014500980907     c                   EVAL      *INLR = *ON
014600980521      **********************************************************************
014700980521      * CONTROLLI VIDEO 1
014800980521      **********************************************************************
014900980923     C     CTRD01        BEGSR
015000980904
015100990614      * Controllo se inseriti dati validi
015200990614    1C                   IF        V1CFIL = *zeros  OR  V1Caac = *zeros
015300990614    1C                                              OR  V1Cnca = *zeros
015400981013     C                   movel     MSG(2)        V1Cmsg
015500990614     c                   eval      *in28 = *on
015600990614     c                   eval      *in40 = *on
015700990429    1C                   ENDIF
015800990614     c   28              goto      Endct1
015900981012
016000990614      * Controllo Anno C.A. sistemo anno di due cifre
016100990429     C                   if        V1Caac < 100  and  V1Caac > 60
016200981013     C                   ADD       1900          V1Caac
016300990429     C                   endif
016400990429     C                   if        V1Caac < 100  and  V1Caac <= 60
016500981014     C                   ADD       2000          V1Caac
016600990429     C                   endif
016700980923
016800981013      * Controllo N° C.A.
016900980923      *
017000981014     C                   z-add     V1Cfil        Kfil
017100981014     C                   z-add     V1Caac        Kaac
017200981014     C                   z-add     V1Cnca        Knca
017300981014     C     Keynca        CHAIN     FNDCT000                           31
017400980904      *
017500981014    2C                   IF        *in31 = *on
017600980923     C                   movel     MSG(1)        V1Cmsg
017700990614     c                   eval      *in28 = *on
017800990614     c                   eval      *in40 = *on
017900990614    2c                   else
018000020129      *
018100020129     c                   movel     DCTflo        DDCT01
018200990909      *
018300990909      * se la CA non è in fase 700 non ristampo
018400990909     c                   if        DCTfca <> WFspl
018500990429     C                   movel     MSG(3)        V1Cmsg
018600990614     c                   eval      *in28 = *on
018700990614     c                   eval      *in40 = *on
018800990429     c                   endif
018900990429     c                   endif
019000990614      *
019100990614     c   28              goto      Endct1
019200980923
019300990429     c     Endct1        ENDSR
019400000202      ****************************************************************
019500000202      * F08 = Manutenzione Intestatario P.L.
019600000202      ****************************************************************
019700000202     C     SUB_F08       BEGSR
019800000202      *
019900000202      * aggancio il file FNDKA con tipo record "I"
020000000202     c     Keydka        chain     FNDKA01L
020100000202     c*
020200000202     c                   if        %found
020300000202     c* valorizzo i campi del video
020400000202     c                   movel     DKArag        W2Crag
020500000202     c                   movel     DKAvia        W2Cvia
020600000202     c                   movel     DKAcap        W2Ccap
020700000202     c                   movel     DKAloc        W2Cloc
020800000202     c                   movel     DKAprv        W2Cprv
020900000202     c                   movel     DKAnaz        W2Cnaz
021000000202     c                   eval      W2Civa = %subst(DKAiva:1:14)
021100000202     c                   eval      W2Cive = %subst(DKAiva:15:2)
021200000202     c                   movel     DKAtel        W2Ctel
021300000202     c                   movel     DKAfax        W2Cfax
021400000202     c*  gestione della finestrella
021500000202     c                   exsr      mancli
021600000202     c* se non esiste il record errore
021700000202     c                   else
021800000202     c*
021900000202     C                   movel     MSG(1)        V1Cmsg
022000000202     c                   eval      *in28 = *on
022100000202     c                   eval      *in40 = *on
022200000202     c*
022300000202     c                   ENDIF
022400000202     c*
022500000202     c                   ENDSR
022600000202      *****************************************************************
022700000202      *  MANUTENZIONE INTESTATARIO PROGETTO DI LIQUIDAZIONE
022800000202      *****************************************************************
022900000202     C     MANCLI        BEGSR
023000000202      *
023100000202      * EMISSIONE
023200000202     c     emw02         tag
023300000202     c                   exfmt     FI55W02
023400000202      *
023500000202     c                   clear                   w2cmsg
023600000202     c                   eval      *in28 = *off
023700000202      *
023800000202     c   KL              goto      Emancli
023900000202      *
024000000202      * CONTROLLI
024100000202     c                   exsr      ctrw02
024200000204     c  nkf
024300000204     cor 28              goto      emw02
024400000202      *
024500000204     c   KF              exsr      regcli
024600000202      *
024700000202     C     EMANCLI       ENDSR
024800000204      *****************************************************************
024900000204      * AGGIORNO I DATI SU FNDKA variazioni intestatario P.L.
025000000204      *****************************************************************
025100000204     C     REGCLI        BEGSR
025200000204
025300000204     c                   movel     W2Crag        DKArag
025400000204     c                   movel     W2Cvia        DKAvia
025500000204     c                   movel     W2Ccap        DKAcap
025600000204     c                   movel     W2Cloc        DKAloc
025700000204     c                   movel     W2Cprv        DKAprv
025800000204     c                   movel     W2Cnaz        DKAnaz
025900000204     c                   eval      DKAiva = W2Civa + W2Cive
026000000204     c                   movel     W2Ctel        DKAtel
026100000204     c                   movel     W2Cfax        DKAfax
026200000204
026300000204     c                   update    fndka000
026400000204
026500000204     C                   endsr
026600000202      *****************************************************************
026700000202      * CONTROLLI VIDEATA MANUTENZ./INSERIMENTO ANAGRAFICA CLIENTI
026800000202      *****************************************************************
026900000202     C     CTRW02        BEGSR
027000000202
027100000202     c     60            DO        69            XX
027200000202     c                   eval      *IN(XX) = *OFF
027300000202     c                   ENDDO
027400000202
027500000202      * Controllo Ragione Sociale, Indirizzo, Località, Provincia, CAP, Nazione e Partita IVA
027600000202     c                   CLEAR                   FNLV14DS
027700000202     c                   z-add     DS_arbdat     I14dri
027800000202     c                   movel     W2Crag        I14rsc
027900000202     c                   movel     W2Cvia        I14ind
028000000202     c                   movel     W2Cloc        E14loc
028100000202     c                   movel     W2Cprv        E14prv
028200000202     c                   movel     W2Ccap        E14cap
028300000202     c                   movel     W2Cnaz        E14nar
028400000202     c                   eval      E14pip = W2Cive + W2Civa
028500000202      *
028600000202     c                   CALL      'FNLV14R'
028700000202     c                   PARM                    KPJBA
028800000202     c                   PARM                    FNLV14DS
028900000202      *
029000000202     c                   movel     I14ind        W2Cvia
029100000202     c                   movel     E14loc        W2Cloc
029200000202     c                   movel     E14prv        W2Cprv
029300000202     c                   movel     E14cap        W2Ccap
029400000202     c                   movel     E14nar        W2Cnaz
029500000202     c                   eval      W2Cive = %subst(E14pip:1:2)
029600000202     c                   eval      W2Civa = %subst(E14pip:3:14)
029700000202      *
029800000202     c                   eval      *in60 = (O14err = '1')                       Ragione Sociale
029900000202     c                   eval      *in61 = (O14err = '2')                       Indirizzo
030000000202     c                   eval      *in62 = (O14err = '5')                       CAP
030100000202     c                   eval      *in63 = (O14err = '3')                       Località
030200000202     c                   eval      *in64 = (O14err = '4')                       Provincia
030300000202     c                   eval      *in65 = (O14err = '6')                       Nazione
030400000202     c                   eval      *in68 = (O14err = '7')                       P.IVA
030500000202      *
030600000202     c                   IF        O14msg <> *blanks
030700000202     c                   movel     O14msg        W2Cmsg
030800000202     c                   eval      *IN28 = *ON
030900000202     c                   ELSE
031000000202      * Controllo il CAP
031100000202     C                   movel     w2ccap        kcap
031200000202     C                   exsr      CAZIS0
031300000202     c                   ENDIF
031400000202     C   28              goto      ectw02
031500000202
031600000202      * CONTROLLO IL NUMERO FAX
031700000202     C                   IF        W2Cfax <> *BLANKS
031800000202     C                   clear                   TRUL42DS
031900000202     C                   movel     W2Cfax        D42FAX
032000000202     C                   CALL      'TRUL42R'
032100000202     C                   PARM                    TRUL42DS
032200000202     C                   IF        D42ERR = '1'
032300000202     C                   eval      *in66 = *on
032400000202     C                   eval      *in28 = *on
032500000202     C                   movel     D42MSG        w2cmsg
032600000202     C                   ENDIF
032700000202     C                   ENDIF
032800000202     C   28              goto      ectw02
032900000202
033000000202      * CONTROLLO IL NUMERO DI TELEFONO
033100000202     C                   IF        W2Ctel <> *BLANKS
033200000202     C                   clear                   TRUL42DS
033300000202     C                   movel     W2Ctel        D42FAX
033400000202     C                   CALL      'TRUL42R'
033500000202     C                   PARM                    TRUL42DS
033600000202     C                   IF        D42ERR = '1'
033700000202     C                   eval      *in67 = *on
033800000202     C                   eval      *in28 = *on
033900000202     C                   movel     D42MSG        w2cmsg
034000000202     C                   ENDIF
034100000202     C                   ENDIF
034200000202     C   28              goto      ectw02
034300000202
034400000202     c     ECTW02        ENDSR
034500000202      *---------------------------------------------------------------*
034600000202      * ROUTINE DI CONTROLLO ESISTENZA CAP                            *
034700000202      *---------------------------------------------------------------*
034800000202     C     CAZIS0        BEGSR
034900000202      *
035000000202      * CONTROLLO CAP PER INTERO
035100000202     C                   CLEAR                   TISI95DS
035200000202     C                   CLEAR                   FNLV13DS
035300000202     C                   MOVEL     ' '           I95TLA
035400000202     C                   MOVEL     '7'           I95TCN
035500000202     C                   MOVEL     w2cCAP        I95CAP
035600000202     C                   MOVEL     w2cNAZ        I95NAR
035700000202     C                   MOVEL     w2cvia        I95IND
035800000202     C                   MOVEL     w2cloc        I95LOC
035900000202     C                   MOVEL     w2cprv        I95PRV
036000000202     C                   MOVEL     'S'           I13AF0
036100000202     C                   MOVEL     'S'           I13AF1
036200000202     C                   MOVEL     'S'           I13SZ2
036300000202     C                   MOVEL     'S'           I13LA3
036400000202     C                   MOVEL     'S'           I13SZ3
036500000202     C                   MOVEL     'S'           I13CNV
036600000202     C                   MOVEL     'S'           I13SZV
036700000202     C* Gestione errori forzabili: se cambiato CAP/Località
036800000202      * riazzero i valori di errori forzabili
036900000202    2C     w2cCAP        IFNE      WSVCAP
037000000202     C                   CLEAR                   WFZV
037100000202     C                   CLEAR                   WFZ1
037200000202     C                   CLEAR                   WFZ2
037300000202     C                   CLEAR                   WFZ3
037400000202     C                   MOVEL     w2cCAP        WSVCAP
037500000202    2C                   ENDIF
037600000202      * Modifica località
037700000202    2C     w2cLOC        IFNE      WSVLOC
037800000202     C                   MOVEL     w2cLOC        WSVLOC
037900000202     C                   CLEAR                   WFZ1
038000000202     C                   CLEAR                   WFZ2
038100000202     C                   CLEAR                   WFZ3
038200000202    2C                   ENDIF
038300000202      * Modifica PROVINCIA
038400000202    2C     w2cPRv        IFNE      WSVPRv
038500000202     C                   MOVEL     w2cPRv        WSVPRv
038600000202     C                   CLEAR                   WFZ3
038700000202    2C                   ENDIF
038800000202      * CALL
038900000202     C                   MOVEL     WFZV          E13FZV
039000000202     C                   MOVEL     WFZ1          E13FZ1
039100000202     C                   MOVEL     WFZ2          E13FZ2
039200000202     C                   MOVEL     WFZ3          E13FZ3
039300000202     C                   CALL      'FNLV13R'
039400000202     C                   PARM                    KPJBA
039500000202     C                   PARM                    FNLV13DS
039600000202     C                   PARM                    TISI95DS
039700000202     C* Ricontrollo errori
039800000202     C                   MOVEL     O13ERR        WERR
039900000202     C                   MOVEL     O13MSG        WMSG
040000000202     C                   MOVEL     E13FZV        WFZV
040100000202     C                   MOVEL     E13FZ1        WFZ1
040200000202     C                   MOVEL     E13FZ2        WFZ2
040300000202     C                   MOVEL     E13FZ3        WFZ3
040400000202     C*
040500000202     C* Se il livello di affidabilità non è inferiore a 2 e
040600000202     C* il livello di reperimento dati non è inferiore a 3
040700000202     C* reimposto i dati da TISI95DS
040800000202     C     O13RON        IFEQ      'S'
040900000202     C     O13ROC        OREQ      'S'
041000000202     C     O13ROP        OREQ      'S'
041100000202     C     O13ROL        OREQ      'S'
041200000202     C                   MOVEL     O95LOC        w2cLOC
041300000202     C                   MOVEL     O95PRV        w2cprv
041400000202     C                   MOVEL     O95NAR        w2cnaz
041500000202     C                   MOVEL     O95CAP        w2cCAP
041600000202     C                   END
041700000202     C*
041800000202     C                   EXSR      CTRIND
041900000202     C* Se non ci sono errori proseguo
042000000202     C   28              GOTO      FINCTR
042100000202     C*
042200000202     C* Controllo se reimpostata località
042300000202     C     O13ROL        IFEQ      'S'
042400000202     c                   eval      *in63 = *on
042500000202     c                   eval      *in28 = *on
042600000204     C                   MOVEL     msg(5)        w2cmsg
042700000202     C                   GOTO      FINCTR
042800000202     C                   END
042900000202     C* Controllo se reimpostata Provincia
043000000202     C     O13ROP        IFEQ      'S'
043100000202     c                   eval      *in64 = *on
043200000202     c                   eval      *in28 = *on
043300000204     C                   MOVEL     msg(7)        w2cmsg
043400000202     C                   GOTO      FINCTR
043500000202     C                   END
043600000202     C* Controllo se reimpostato CAP
043700000202     C     O13ROC        IFEQ      'S'
043800000202     c                   eval      *in62 = *on
043900000202     c                   eval      *in28 = *on
044000000204     C                   MOVEL     msg(6)        w2cmsg
044100000202     C                   GOTO      FINCTR
044200000202     C                   END
044300000202     C* Controllo se reimpostata Nazione
044400000202     C     O13RON        IFEQ      'S'
044500000202     c                   eval      *in65 = *on
044600000202     c                   eval      *in28 = *on
044700000204     C                   MOVEL     msg(8)        w2cmsg
044800000202     C                   GOTO      FINCTR
044900000202     C                   END
045000000202     C*
045100000202     C     FINCTR        ENDSR
045200000202     C*------------------------------------------------------*
045300000202     C*  Controllo dati indirizzo
045400000202     C*------------------------------------------------------*
045500000202     C     CTRIND        BEGSR
045600000202     C*
045700000202     C* Errore !!!!
045800000202     C     WERR          IFNE      ' '
045900000202     C     WMSG          ANDNE     *BLANKS
046000000202     C* Manca la Ragione sociale
046100000202     C                   SELECT
046200000202     C* Errore su località/città
046300000202     C     WERR          WHENEQ    '3'
046400000202     c                   eval      *in63 = *on
046500000202     c                   eval      *in28 = *on
046600000202     C* Errore su Cap
046700000202     C     WERR          WHENEQ    '5'
046800000202     c                   eval      *in62 = *on
046900000202     c                   eval      *in28 = *on
047000000202     C* Errore su provincia
047100000202     C     WERR          WHENEQ    '4'
047200000202     c                   eval      *in64 = *on
047300000202     c                   eval      *in28 = *on
047400000202     C* Nazione  errata
047500000202     C     WERR          WHENEQ    '6'
047600000202     c                   eval      *in65 = *on
047700000202     c                   eval      *in28 = *on
047800000202    2C                   ENDSL
047900000202      *
048000000202     C                   MOVEL     WMSG          w2cmsg
048100000202     C                   END
048200000202     C*
048300000202     C                   ENDSR
048400981015      ****************************************************************
048500981015      * F14 = INTERROGAZIONE C.A.
048600981015      ****************************************************************
048700981015     C     SUB_F14       BEGSR
048800981016      *
048900981016     c                   clear                   FIDN00DS
049000981016      *
049100981016     c                   movel     'I'           I00mod
049200990429     c                   movel     dctfca        I00fca
049300981016     c                   z-add     dctgfc        I00fgs
049400981016     c                   z-add     dctaac        I00aac
049500981016     c                   z-add     dctfil        I00fil
049600981016     c                   z-add     dctnca        I00nca
049700981016     c                   z-add     dctaas        I00aas
049800981016     c                   z-add     dctlnp        I00lnp
049900981016     c                   z-add     dctnrs        I00nrs
050000981016     c                   z-add     dctnsp        I00nsp
050100981016     c                   z-add     dctlna        I00lna
050200981016     c                   movel     dcttad        I00tad
050300981016     c                   if        dctgfc = 046
050400981016     c                   movel     'S'           I00tpb
050500981016     c                   endif
050600981016      *
050700981016     c                   movel     FIDN00DS      kpjbu
050800981016      *
050900981016     c                   call      'FIDN01R'
051000981016     c                   parm                    kpjba
051100981016      *
051200981015     c                   endsr
051300980608      *****************************************************************
051400980923      * AGGIORNO ARCHIVI
051500980608      *****************************************************************
051600980923     C     SUB_confer    BEGSR
051700981007
051800980923      *
051900000131     c* se è stata richiesta solo la ristampa lancio il pgm di ristampa
052000000131     c                   if        v1cnpl <> 'SI'
052100990429     c                   clear                   FIDN00DS
052200990429      *
052300990429     c                   z-add     dctaac        I00aac
052400990429     c                   z-add     dctfil        I00fil
052500990429     c                   z-add     dctnca        I00nca
052600990429      *
052700990429     c                   movel     FIDN00DS      kpjbu
052800990429      *
052900060316      * non condiziopno + la ristampa del progetto di liquidazione in base all'utente
053000060316     c*****              if        KNMUS = §STDpco
053100050207     c                   call      'FIDN56R'
053200050207     c                   parm                    kpjba
053300060316     c****               else
053400060316     c******             call      'FIDN56R1'
053500060316     c******             parm                    kpjba
053600060316     c******             endif
053700000131     c*
053800000131     c                   else
053900000131     c*
054000000131     c* riporto la CA in fase 690
054100000131     c                   EXSR      SUB_FASE
054200000131     c*
054300000131     c                   endif
054400981009      *
054500980923     c                   ENDSR
054600000131      *****************************************************************
054700000131      * RIPORTO ALLA FASE  690
054800000131      *****************************************************************
054900000131     C     SUB_FASE      BEGSR
055000000131
055100000131      *
055200000131      * aggiorno la testata
055300000131     c                   z-add     WRspl         dctfca
055400000131     c                   clear                   dctft1
055500000131     c                   clear                   dctft2
055600000131     c                   update    fndct000
055700050222      *
055800050222      * aggiorno la liquidazione
055900050222if  1c                   if        KNMUS <> §STDpco
056000050222     c     keynca        CHAIN     FNDCL000
056100050222if  2c                   if        %found(FNDCL01L)
056200050222     c                   eval      DCLfrp = 'S'
056300050222     c                   update    FNDCL000
056400050222e   2c                   endif
056500050222e   1c                   endif
056600000131      *scrivo la fase CA
056700000131      *
056800000131     c                   clear                   fndcf000
056900020201     c                   clear                   DDCF01
057000000131      *
057100000131     c                   z-add     dctaac        DCFaac
057200000131     c                   z-add     dctfil        DCFfil
057300000131     c                   z-add     dctnca        DCFnca
057400000131     c                   z-add     WRspl         DCFfca
057500000131     c                   clear                   DCFptr
057600000131      *
057700000131     c                   z-add     dateu         DCFdfc
057800000131     c                   movel     w0140         DCFhfc
057900000131      *
058000000131     c                   z-add     dctgfc        DCFfev
058100000131     c                   movel     knmus         DCFpru
058200020201     c***                clear                   DCFlet
058300020201     c                   movel     §DCTpfra      §DCFpfra
058400020201     c                   movel     DDCF01        DCFlet
058500000131     c                   clear                   DCFftr
058600000131     c                   clear                   DCFdtr
058700000131      *
058800000131     c                   write     fndcf000
058900000131      *
059000000131     c                   ENDSR
059100000131      *
059200980910      *****************************************************************
059300980527      * ROUTINE INIZIALE
059400980527      *****************************************************************
059500980527     C     *INZSR        BEGSR
059600980615      *
059700980521     C     *ENTRY        PLIST
059800980521     C                   PARM                    KPJBA
059900980907      *
060000050207      * Reperisco nome azienda
060100050207     c     *dtaara       define    §azute        AZUTEds
060200050207     c     *dtaara       define    §datiute      dDATIUTE
060300050207     c                   in(E)     *dtaara
060400050207     c                   if        %ERROR or RSUT = *blanks
060500050207     c                   clear                   Tibs34Ds
060600050207     c                   call      'TIBS34R'
060700050207     c                   parm                    Tibs34Ds
060800050207     c                   in        *dtaara
060900050207     c                   endif
061000050207      ***
061100050207      * Aggancio tabella GED
061200050207      ***
061300050207     c                   clear                   dGEDdn
061400050207     c                   clear                   TIBS02ds
061500050207     c                   movel     'C'           T02mod
061600050207     c                   movel     KNSIF         T02sif
061700050207     c                   movel     'GED'         T02cod
061800050207     c                   movel(p)  'DANNI'       T02ke1
061900050207     c                   call      'TIBS02R'
062000050207     c                   parm                    KPJBA
062100050207     c                   parm                    TIBS02ds
062200050207     c                   if        T02err = *BLANKS
062300050207     c                   movel     T02uni        dGEDdn
062400050207     c                   endif
062500050207      ***
062600050207      * Aggancio tabella con dati Consuldanni
062700050207      ***
062800050207     c                   clear                   dSTD
062900050207     c                   clear                   TIBS02ds
063000050207     c                   movel     'C'           T02mod
063100050207     c                   movel     KNSIF         T02sif
063200050207     c                   movel     'STD'         T02cod
063300050207     c                   movel(p)  '1'           T02ke1
063400050207     c                   movel(p)  §GEDdba       T02ke2
063500050207     c                   call      'TIBS02R'
063600050207     c                   parm                    KPJBA
063700050207     c                   parm                    TIBS02ds
063800050207     c                   if        T02err = *blanks
063900050207     c                   movel     T02uni        dSTD
064000050207     c                   endif
064100050222      *
064200050222if  1c                   if        KNMUS <> §STDpco
064300050222     c                   open      FNDCL01L
064400050222e   1c                   endif
064500981007      *
064600981007      * Imposto il TITOLO
064700990429      *
064800990429     c                   movel     TITOLO        VTCtit
064900980721      * reperisco data e ora
065000980721     C                   TIME                    W0140
065100980521      * UDATE IN GGMMAAAA
065200980521     C                   MOVE      W0140         WDTGIO
065300980923     C                   MOVEL     W0140         WORA
065400980521      * UDATE IN AAAAMMGG
065500980521     C                   Z-ADD     WDTGIO        G02DAT
065600980521     C                   MOVEL     *BLANK        G02ERR
065700980521     C                   CALL      'XSRDA8'
065800980521     C                   PARM                    WLBDAT
065900980521     C                   MOVEL     G02INV        DATEU
066000980923
066100981013      *    Imposto anno C.A.
066200981013     c                   move      wdtgio        V1Caac
066300980721
066400981014     C     Keynca        klist
066500981013     C                   KFLD                    kaac
066600981013     C                   KFLD                    kfil
066700981013     C                   KFLD                    knca
066800000202
066900000202     C     Keydka        klist
067000000202     C                   KFLD                    kaac
067100000202     C                   KFLD                    kfil
067200000202     C                   KFLD                    knca
067300000202     C                   KFLD                    Ktrc
067400980915
067500980521     C                   ENDSR
067600980521      *---------------------------------------------------------------------------------------------
067700980521** MSG  Lungh. 78                                                            *
067800990429Il numero C.A. immesso non esiste                                              1
067900981014Il valore immesso nel campo non è valido                                       2
068000990909La C.A. non è nella fase corretta per la ristampa progetto di liquidazione     3
068100000202Non esiste Intestatario progetto di liquidazione                               4
068200000204Attenzione !! Modificata Località                                              5
068300000204Attenzione !! Modificato CAP                                                   6
068400000204Attenzione !! Modificata Provincia                                             7
068500000204Attenzione !! Modificata Nazione                                               8
