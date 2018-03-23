000100980506      *------------------------------------------------------------------------*
000200981008      * Stampa lettera per invio a sede avarie
000300990304      *------------------------------------------------------------------------*
000400990304      *    Allegato A15
000500980506      *------------------------------------------------------------------------*
000600981028
000700981028      ****************************************************************
000800981028      *  FASI C.A. FISSATE A PROGRAMMA
000900981028      ****************************************************************
001000981028      ***************************************************************
001100981028
001200981028      ****************************************************************
001300981028      *  RIEPILOGO INDICATORI
001400981028      ***************************************************************
001500981028      * 30 - COMODO
001600981028      * 31 - COMODO
001700981028      ***************************************************************
001800981028
001900981028     H DECEDIT('0,') DATEDIT(*DMY.)
002000981028
002100981028     FFIDN00D   CF   E             WORKSTN  sfile(FI00S02:nrr1)
002200981028     FFNDCT01L  UF   E           K DISK
002300981028     FFNDCF01L  UF A E           K DISK
002400981028     FAZORG01L  IF   E           K DISK
002500990304     FPRTF198   O    F  198        PRINTER  oflind(*INOF)  usropn
002600981028      *------------------------------------------------------------------------*
002700981028      *   S C H I E R E
002800981028     D PTF             S              4    DIM(100)                             PTR + TAD + FPE
002900981028     D GFS             S              1    DIM(100)                             P.O. gestione fase
003000981028     D NCA             S              1    DIM(105)                             Comodo per stampa
003100990304      *----------------------------------
003200990304     D CMD             S             80    DIM(4)  CTDATA PERRCD(1)             OVRPRTF
003300981028      *----------------------------------
003400981028     D DATA_oggi       S               D   DATFMT(*eur)
003500981028     D wfpe            S                   LIKE(i05fpe)
003600981028     D Pdalo           S             30
003700981028     D Wkca            S             19
003800981028     D WPTF            S              4
003900981028     D wdataoggi       S             10
004000981028     D wsocie          S             20
004100981028     D WAanno          S              4
004200981028     D WAfil           S              3
004300981028     D WAnum           S              7
004400990304     D command         S             80
004500981028     D W007a           S              7
004600990304     D lung            S             15  5 inz(80)
004700981028     D xx              S              3  0
004800981028     D yy              S              3  0
004900981028     D jj              S              3  0
005000981028     D W0140           S             14  0
005100981028     D Wdtgio          S              8  0
005200981028     D dateu           S              8  0
005300981028     D nrr1            S              4  0
005400981028     D nrr2            S              4  0
005500981028     D WORA            S              4  0
005600981028      *----------------------------------
005700981028     D KBAR            C                   CONST('BARTOLINI  S.p.a.')
005800981028      *----------------------------------
005900981028      *   D S   I N T E R N E / E S T E R N E
006000981028     D WLBDAT          DS                  INZ
006100981028     D  G02DAT                 1      8  0
006200981028     D  G02INV                 9     16  0
006300981028     D  G02ERR                17     17
006400981028     D  G02TGI                18     22  0
006500981028      *
006600981028      *----------------------------------
006700981028     D FIDN00DS      E DS
006800981028     D FIDN05DS      E DS
006900990304     D TIBS02DS      E DS
007000981028     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
007100981028     D KPJBA         E DS
007200981229     D DDCT01        E DS
007300990304     D DTLD          E DS
007400981028      *
007500981028     D                SDS
007600981028     D  VTCPGM                 1     10
007700981028      *------------------------------------------------------------------------*
007800981028      *---------------  C A L C O L O  ----------------------------------------*
007900981028      *------------------------------------------------------------------------*
008000990304
008100990304      * Imposto Tipo Modulo e apro file stampa
008200990304     c                   EXSR      IMPPRT
008300981028
008400981028      * Stampa intestazione e testo lettera
008500981028     c     I00fgs        chain     AZORG                              30
008600981028      *
008700981028     c                   movel     KBAR          Wsocie
008800981028      *
008900981028     C     *eur          movel     DATA_oggi     wdataoggi
009000981028     c                   eval      Pdalo = %TRIM(ORGDES) + ', ' + wdataoggi
009100981028      *
009200981028     C                   EXCEPT    PINTES
009300981028
009400981028      * Stampo C.A. allegate e aggiorno testata C.A. e file fasi
009500981028     c                   IF        i00nce = 1
009600981028     c                   exsr      CA_singola
009700981028     c                   ELSE
009800981028     c                   exsr      CA_multi
009900981028     c                   ENDIF
010000981028      *
010100981028      * Stampo numeri C.A. se non ancora stampati
010200981028     c                   IF        jj > *zeros
010300981028     c                   EXCEPT    PNUMCA
010400981028     c                   ENDIF
010500981028
010600981028      * Stampa parte finale
010700981028     C                   EXCEPT    PFIRMA
010800981028
010900981028     c     fine          tag
011000981028     c                   EVAL      *INLR = *ON
011100981028      *****************************************************************
011200981028      * AGGIORNO FILES SE SELEZIONATA UNA C.A.
011300981028      *****************************************************************
011400981028     C     CA_singola    BEGSR
011500981028
011600981229      * Aggancio testata C.A. e imposto flag evento/non evento
011700981028     C     KDCT00        CHAIN     FNDCT000                           30
011800981028      *
011900981028     c                   if        DCTnev > *zeros
012000981028     c                   movel     'E'           wfpe
012100981028     c                   else
012200981028     c                   movel     'N'           Wfpe
012300981028     c                   endif
012400981028
012500981028      * Ricavo gestore fase successiva
012600981028     c                   EXSR      FASEsucc
012700981028
012800981028      * Imposto campi per trasmissione dati
012900981028     c                   EXSR      RIEfndct
013000981028
013100981028      * Aggiorno testata C.A.
013200981028     c                   EXCEPT    UPDDCT
013300981028
013400981028      * Scrivo fasi C.A.
013500981028     c                   EXSR      WRTDCF
013600981028
013700981028      * Stampo numero C.A.
013800981028     c                   EXSR      PRTnumca
013900981028
014000981028     c                   ENDSR
014100990304      *****************************************************************
014200990304      *   IMPOSTO TIPO MODULO E STAMPANTE
014300990304      *****************************************************************
014400990304     C     IMPPRT        BEGSR
014500990304      *
014600990304     c                   exsr      DECtld
014700990304      *
014800990304     c                   Clear                   COMMAND
014900990304      *
015000990304     c                   IF        §TLDtmod <> *blanks
015100990304     c                   movea     CMD(1)        COMMAND
015200990304     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
015300990304     c                   ELSE
015400990304     c                   movea     CMD(2)        COMMAND
015500990304     c                   ENDIF
015600990304      *
015700990304     c                   call      'QCMDEXC'
015800990304     c                   parm                    COMMAND
015900990304     c                   parm                    LUNG
016000990304      *   Apro file stampa
016100990304     c                   OPEN      PRTF198
016200990304      *
016300990304     C                   ENDSR
016400981028      *****************************************************************
016500981028      * AGGIORNO FILES LEGGENDO SUBFILE
016600981028      *****************************************************************
016700981028     C     CA_multi      BEGSR
016800981028
016900981028      * Leggo  subfile
017000981028     c                   z-add     1             nrr2
017100981028     c     NRR2          CHAIN     FI00S02                            31
017200981028      *
017300981028      * Elaboro se C.A. non esclusa e scelta non bloccata
017400981028     c     *IN31         DOWEQ     *OFF
017500981028     c                   IF        V2Csce <> 'E'  and  V2Hi82 <> '1'
017600981028      *
017700981028      * Aggancio testata C.A. eimposto flag evento/non evento
017800981028     C     KDCT          CHAIN     FNDCT000                           30
017900981028      *
018000981028     c                   if        DCTnev > *zeros
018100981028     c                   movel     'E'           wfpe
018200981028     c                   else
018300981028     c                   movel     'N'           Wfpe
018400981028     c                   endif
018500981028      *
018600981028      * Ricavo gestore fase successiva
018700981028      *        ricerco in schiera se parametri già elaborati, altrimenti calcolo
018800981028     c                   z-add     1             XX
018900981028     c                   eval      WPTF = DCTptr + DCTtad + WFPE
019000981028     c     WPTF          LOOKUP    PTF(XX)                                30
019100981028      *
019200981028     c                   IF        *IN30 = *ON
019300981028     c                   movel     GFS(XX)       DFAgfs
019400981028     c                   ELSE
019500981028      *
019600981028      *  Calcolo la fase successiva
019700981028     c                   EXSR      FASEsucc
019800981028      *
019900981028      *  Memorizzo in schiera i parametri
020000981028     c                   Z-add     1             XX
020100981028     c     *blanks       LOOKUP    PTF(XX)                                30
020200981028     c   30              eval      PTF(XX) = DCTptr + DCTtad + WFPE
020300981028     c   30              movel     DFAgfs        GFS(XX)
020400981028     c                   ENDIF
020500981028      *
020600981028      * Imposto campi per trasmissione dati
020700981028     c                   EXSR      RIEfndct
020800981028      *
020900981028      * Aggiorno testata C.A.
021000981028     c                   EXCEPT    UPDDCT
021100981028      *
021200981028      * Scrivo fasi C.A.
021300981028     c                   EXSR      WRTDCF
021400981028
021500981028      * Stampo numero C.A.
021600981028     c                   EXSR      PRTnumca
021700981028      *
021800981028     c                   ENDIF
021900981028      *
022000981028     c                   add       1             nrr2
022100981028     c     NRR2          CHAIN     FI00S02                            31
022200981028     c                   ENDDO
022300981028      *
022400981028     c                   ENDSR
022500981028      *****************************************************************
022600981028      * RICAVO FASE SUCCESSIVA
022700981028      *****************************************************************
022800981028     C     FASEsucc      BEGSR
022900981028      *
023000981028     c                   CLEAR                   FIDN05DS
023100981028     c                   movel     'F'           I05MOD
023200981028     c                   z-add     I00fca        I05fca
023300981028     c                   movel     'P'           I05fpr
023400981028     c                   movel     dctptr        I05tpc
023500981028     c                   movel     'O'           I05ffs
023600981028     c                   movel     Wfpe          I05fpe
023700981028     c                   z-add     DATEU         i05dta
023800981028     c                   move      dcttad        i05tad
023900981125      * Valorizzo numero CA
024000981125     c                   z-add     DCTaac        i05aac
024100981125     c                   z-add     DCTfil        i05fil
024200981125     c                   z-add     DCTnca        i05nca
024300981028
024400981028     C                   CALL      'FIDN05R'
024500981028     c                   PARM                    KPJBA
024600981028     c                   PARM                    FIDN05DS
024700981028      *
024800981028     c                   movel     O05REC        DS_FNDFA
024900981028      *
025000981028     c                   ENDSR
025100981028      *****************************************************************
025200981028      * IMPOSTO CAMPI DI FNDCT
025300981028      *****************************************************************
025400981028     C     RIEfndct      BEGSR
025500981028      *
025600981229     c                   movel     DCTflo        DDCT01
025700981229      *
025800981028     c                   SELECT
025900981028     c                   WHEN      DFAgfs = 'P'
026000990309     c                   movel     §DCTlnpc      dctgfc
026100981028     c                   WHEN      DFAgfs = 'S'
026200981028     c                   z-add     46            dctgfc
026300981028     c                   WHEN      DFAgfs = 'A'
026400981028     c                   z-add     DCTlna        dctgfc
026500981028     c                   OTHER
026600981028     c                   z-add     DCTfil        dctgfc
026700981028     c                   ENDSL
026800981028      *
026900981028     c                   z-add     I00fca        DCTfca
027000981222     c                   clear                   DCTft1
027100981222     c                   clear                   DCTft2
027200981028      *
027300981028     c                   ENDSR
027400981028      *****************************************************************
027500981028      * AGGIORNO FNDCF FASI C.A.
027600981028      *****************************************************************
027700981028     C     WRTDCF        BEGSR
027800981028      *
027900981028     C                   z-add     DCTaaC        DCFaac
028000981028     C                   z-add     DCTfil        DCFfil
028100981028     C                   z-add     DCTnca        DCFnca
028200981028     c                   z-add     I00fca        DCFfca
028300981028     c                   z-add     dateu         DCFdfc
028400981028     c                   z-add     wora          DCFhfc
028500981028     c                   z-add     I00fgs        DCFfev
028600981028     c                   movel     knmus         DCFpru
028700981028      *
028800981028     c                   WRITE     FNDCF000
028900981028      *
029000981028     c                   ENDSR
029100981028      *****************************************************************
029200981028      * PREPARO E STAMPO NUMERO C.A.
029300981028      *****************************************************************
029400981028     C     PRTnumca      BEGSR
029500981028      *
029600981214     c                   if        JJ = *zeros
029700981214     c                   eval      jj = -20
029800981214     c                   endif
029900981214      *
030000981028     c                   IF        i00nce = 1
030100981028     c                   movel     I00aac        WAanno
030200981028     c                   movel     I00fil        WAfil
030300981028     c                   movel     I00nca        w007a
030400981028     c                   ELSE
030500981028     c                   movel     V2Haac        WAanno
030600981028     c                   movel     V2Cfil        WAfil
030700981028     c                   movel     V2Cnca        w007a
030800981028     c                   ENDIF
030900981028      *
031000981028     c     '0'           check     w007a         yy
031100981028     c                   eval      WAnum = %subst(W007a:yy)
031200981028      *
031300981028     C                   eval      Wkca = 'N. '  +  WAanno  +  ' '  +  WAfil
031400981028     c                                     +  ' '  +  %trim(WAnum)
031500981028      *
031600981214     c                   add       21            JJ
031700981214     c                   movea     Wkca          NCA(JJ)
031800981028      *
031900981214     c                   IF        jj = 85                                        5  C.A. per riga
032000981214     c                   except    PNUMCA
032100981214     c                   eval      jj = -20
032200981028     c                   clear                   NCA
032300981028     c                   ENDIF
032400981028      *
032500981028     c                   ENDSR
032600990304      *****************************************************************
032700990304      *   AGGANCIO TABELLA TIPO TESTO
032800990304      *****************************************************************
032900990304     C     DECTLD        BEGSR
033000990304      *
033100990304     c                   clear                   tibs02ds
033200990304     c                   clear                   dtld
033300990304      *
033400990304     C                   movel     'C'           t02mod
033500990304     C                   movel     knsif         t02sif
033600990304     C                   movel     'TLD'         t02cod
033700990304     C                   movel(P)  'A15'         t02ke1
033800990304      *
033900990304     C                   CALL      'TIBS02R'
034000990304     C                   PARM                    KPJBA
034100990304     C                   PARM                    TIBS02DS
034200990304      *
034300990304     C                   movel     T02uni        DTLD
034400990304      *
034500990304     C                   ENDSR
034600981028      *****************************************************************
034700981028      * ROUTINE INIZIALE
034800981028      *****************************************************************
034900981028     C     *INZSR        BEGSR
035000981028      *
035100981028     C     *ENTRY        PLIST
035200981028     C                   PARM                    KPJBA
035300981028      *
035400981028     C                   MOVEL     KPJBU         FIDN00DS
035500981028      *
035600981028      * reperisco data e ora
035700981028     C                   TIME                    W0140
035800981028      * UDATE IN GGMMAAAA
035900981028     C                   MOVE      W0140         WDTGIO
036000981028     C                   MOVEL     W0140         WORA
036100981028      * UDATE IN AAAAMMGG
036200981028     C                   Z-ADD     WDTGIO        G02DAT
036300981028     C                   MOVEL     *BLANK        G02ERR
036400981028     C                   CALL      'XSRDA8'
036500981028     C                   PARM                    WLBDAT
036600981028     C                   MOVEL     G02INV        DATEU
036700981028     C     *iso          MOVEL     DATEU         DATA_oggi
036800981028      *
036900981028     C     KDCT00        KLIST
037000981028     C                   KFLD                    I00AAC
037100981028     C                   KFLD                    I00FIL
037200981028     C                   KFLD                    I00NCA
037300981028      *
037400981028     C     KDCT          KLIST
037500981028     C                   KFLD                    V2HAAC
037600981028     C                   KFLD                    V2CFIL
037700981028     C                   KFLD                    V2CNCA
037800981028      *
037900981028     C                   ENDSR
038000981028      *---------------------------------------------------------------------------------------------
038100981028     OFNDCT000  E            UPDDCT
038200981028     O                       DCTgfc
038300981028     O                       DCTfca
038400981222     O                       DCTft1
038500981222     O                       DCTft2
038600981028      *---------------------------------------------------------------------------------------------
038700981028     OPRTF198   E            PINTES           15
038800981028     o                                           27 'Punto Operativo di'
038900981028     o          E            PINTES      1
039000981028     O                       ORGdes              29
039100981028     o          E            PINTES      1
039200981028     O                       ORGind              29
039300981028     o                                           82 'Spett.le'
039400981028     o          E            PINTES      1
039500981028     O                       ORGcpf              14
039600981028     O                       ORGloc              35
039700981028     O                       ORGpro              38
039800990208     o                                           93 'Ufficio DANNI MERCE'
039900981028     o          E            PINTES      1
040000981028     o                                           13 'TEL:'
040100981028     O                       ORGtel              27
040200981028     o                                           86 'BOLOGNA SEDE'
040300981028     o          E            PINTES      1
040400981028     o                                           13 'FAX:'
040500981028     O                       ORGfax              27
040600981028     o          E            PINTES      2
040700981028     o                       Pdalo               39
040800981028     o          E            PINTES      5
040900060713     o***                                     +   9 'Oggetto: Invio C.A. relati-
041000060713     o***
041100060713     o                                        +   9 'Oggetto: Invio Documentazi-
041200060713     o                                              one C.A.'
041300981028     o          E            PINTES      0
041400060713     o                                        +   9 'Oggetto: Invio Documentazi-
041500060713     o                                              one C.A.'
041600981028     o          E            PINTES      3
041700981028     o                                        +  28 'Con la presente Vi inviamo-
041800981028     o                                               documentazione delle segue-
041900981028     o                                              nti C.A.'
042000981028      *
042100981028     o          E            PNUMCA      2
042200981028     O                       NCA              +  13
042300981028      *
042400981028     o          E            PFIRMA      3
042500981028     O                       Wsocie           +  78
042600981028     o          E            PFIRMA      1
042700981028     O                                        +  78 'P.O. di'
042800981028     O                       ORGDES           +   1
042900981028      *---------------------------------------------------------------------------------------------
043000981028      *
043100981028N. aaa* fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnnnnn  N. aaaa fil nnnn
043200981028nnn   *
043300990304** CMD   (Lunga 80)                                                            *
043400990304OVRPRTF PRTF198 FORMTYPE(xxxxxxxxxx) SAVE(*YES)                                  1
043500990304OVRPRTF PRTF198 SAVE(*YES)                                                       2
