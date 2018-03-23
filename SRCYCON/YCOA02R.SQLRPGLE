000100030124     hDECEDIT('0,') DATEDIT(*DMY.)
000200120507     h DFTACTGRP(*NO) BNDDIR('QC2LE')
000300120507
000400030117      *****************************************************************
000500030221      *      controllo clienti contenzioso
000600030117      *****************************************************************
000700080128     Fcnaco00f  IF   E           K DISK    prefix(£)
000800030221     Fcnind02l  IF   E           K DISK
000900030221     Fcnind00f  IF   E           K DISK    rename(cnind000:cnind0)
001000030224     Fcnclp00f  IF   E           K DISK
001100030224     Ftabel00f  IF   E           K DISK
001200030224     Fprtf198   o    f  198        printer oflind(*inof)
001300050803
001400050803     D x20ScadDS     E DS
001500050803     D                                     INZ
001600050803     D  x20Societa   E
001700050803     D                                     INZ('201')
001800050803     D  x20Ctb       E
001900050803     D                                     INZ('CG')
002000050803     D  x20Def       E
002100050803     D                                     INZ(*ON)
002200050803     D  x20Prov      E
002300050803     D                                     INZ(*ON)
002400050803     D  x20Gest      E
002500050803     D                                     INZ(*OFF)
002600050803     D  x20SNatura   E
002700050803     D                                     INZ('C')
002800050803     D  x20DtRegMD   E
002900050803     D                                     INZ(*SYS)
003000050803     D  x20DtRegMP   E
003100050803     D                                     INZ(*HIVAL)
003200050803     D  x20DtSca     E
003300050803     D                                     INZ(*SYS)
003400050803     D  x20LRT       E
003500050803     D                                     INZ('R')
003600030220     D kpjba         e DS
003700030221     D ycoa01ds      e DS
003800030224     D ds4w          e DS
003900120801     D anrco00f      E DS                  TEMPLATE QUALIFIED
004000120801     D anrco           DS
004100120801     D  rcoKsc                        8A
004200120801     D  clnCliFatt                    8A
004300080128     D cnaco         e DS                  extname(cnaco00f)
004400080128     D cnaco0        e DS                  extname(cnaco00f) prefix(£)
004500030224     d savksc          s                   like(acoksc)
004600030224     d savkscs         s                   like(acoksc)
004700030224     d savrags         s                   like(acorag)
004800030224     d savpivs         s                   like(indiva)
004900120801     d dti             s                   like(anrco00f.rcodtimm)
005000120801     d dtf             s                   like(anrco00f.rcodtimm)
005100030221     d dataEUR         s               d   datfmt(*EUR)
005200030221     d dtieur          s               d   datfmt(*EUR)
005300030221     d dtfeur          s               d   datfmt(*EUR)
005400030221     d dataiso         s               d
005500050803     D Esposizione     S             11P 2
005600030221     D                SDS
005700030221     D PgmName           *PROC
005800120507
005900120508      /COPY GAITRASRC/SRCCONST,IFS
006000120508      /COPY GAITRASRC/SRCCONST,ERRNO
006100120508     D CRLF...
006200120508     D                 C                   X'0D25'
006300120508      /COPY GAITRASRC/SRCPROTOPI,ERRNO
006400120508      /COPY GAITRASRC/SRCPROTOPR,ERRNO
006500120508      /COPY GAITRASRC/SRCPROTOPR,IFS
006600120507     D bytes_write...
006700120507     D                 S             10I 0
006800120507     D fd...
006900120507     D                 S             10I 0
007000120507     D buf...
007100120508     D                 S          32766A
007200120507     D bufPtr...
007300120507     D                 S               *   INZ(%ADDR(buf))
007400120507
007500030117      *****************************************************************
007600120508     c                   seton                                        lr
007700080128     C* Leggo il file anrco con data immissione compresa nel range di
007800080128     c* date scelte a video.
007900030220     C/EXEC SQL
008000120801     C+ DECLARE A1 CURSOR FOR
008100120801     C+ SELECT anrco00f.rcoKsc, ancln00f.clnCliFatt
008200120801     C+ FROM anrco00f
008300120801     C+ JOIN ancln00f
008400120801     C+   ON anrco00f.rcoSocieta = ancln00f.clnSocieta
008500120801     C+  AND anrco00f.rcoKsc = ancln00f.clnKsc
008600120801     C+  AND 0 = ancln00f.clnFiliale
008700120801     C+  AND '' = ancln00f.clnLineaV
008800120801     C+ WHERE anrco00f.rcodtimm between :dti and :dtf
008900120801     C+   AND anrco00f.rcoSocieta = '201' AND anrco00f.rcoSNatura = 'C'
009000120801     C+   AND anrco00f.rcoKsc <> ''
009100030220     C/END-EXEC
009200030220      *          apertura cursore
009300030220     C/EXEC SQL
009400030220     C+ OPEN A1
009500030220     C/END-EXEC
009600030220     C                   DOU       SqlCod = 100 OR SqlCod < 0
009700030220      *          lettura cursore
009800030220     C/EXEC SQL
009900080128     C+ Fetch Next From A1 Into :anrco
010000030220     C/END-EXEC
010100030220      *
010200030220     C                   SELECT
010300030220     C                   WHEN      SqlCod = 100
010400030220     C                   WHEN      SqlCod >= 0
010500030221     c* reperisco la partita iva e controllo se esistoo altre anagrafiche
010600030221     c* in contenzioso
010700080128     c                   move      rcoksc        acoksc
010800030221     c                   exsr      srind
010900030220     C                   OTHER
011000030220     C                   ENDSL
011100030220      *
011200030220     C                   ENDDO
011300030220     C/EXEC SQL
011400030220     C+ Close A1
011500030220     C/END-EXEC
011600120508     C                   CALLP     IFS_CloseFile( fd )
011700120508
011800030220     c**********************************************************************
011900030221     c     srind         begsr
012000030220     c**********************************************************************
012100030221     c* reperisco p.i.
012200030221     c     kind          chain     cnind00f
012300030221     c                   if        %found(cnind00f)
012400080128     c* reperisco ragione sociale cliente contenzioso
012500080128     c     kaco          chain     cnaco00f
012600080128     c                   if        %found(cnaco00f)
012700080128     c                   eval      cnaco = cnaco0
012800080128     c                   end
012900030221     c* leggo tutti i ksc con quella p.i.
013000030221     c     kind2         setll     cnind02l
013100030221     c                   do        *hival
013200030221     c     kind2         reade     cnind02l
013300030221     c                   if        %eof(cnind02l)
013400030221     c                   leave
013500030221     c                   end
013600030221     c* controllo se ksc è in contenzioso
013700030221     c                   if        indksc <> acoksc
013800051018     C                             and %subst(indiva:1:2) <> '$$'
013900030224     c     kaco          chain     cnclp00f
014000030224     c                   if        %found(cnclp00f) and clpcon <> *blanks
014100120801     C* decodifico blocco contenzioso
014200120801     C                   MOVEL     '4W'          tblcod                         *
014300120801     C                   MOVEL     *BLANKS       COMO3             3            *
014400120801     C                   MOVE      CLPCON        COMO3
014500120801     C                   MOVEL(P)  COMO3         tblkey                         *
014600120801     C     ktab          CHAIN     TABEL00F                                     *
014700120801     c                   if        %found(tabel00f)
014800120801     C                   MOVEL     TBLUNI        DS4W
014900120801     C                   ELSE
015000120801     C                   CLEAR                   ds4w
015100120801     c                   end
015200120801     c                   IF        §4wTip <> *BLANK
015300050803     ** Reperisco l'esposizione del cliente.
015400050803     C                   CLEAR                   Esposizione
015500050803     C                   RESET                   X20ScadDS
015600050803     C                   EVAL      x20Kcc = '00' + %EDITC(indKcc:'X')
015700050803     C                   EVAL      x20Ksc = '0' + %EDITC(indKsc:'X')
015800050803     C                   CALL      'X20SCADR'
015900050803     C                   PARM                    x20ScadDS
016000050803     C                   EVAL      Esposizione = x20LScadu + x20LAScad
016100060117     C                   IF        Esposizione >= 0
016200030224     c* reperisco ragione sociale cliente contenzioso
016300030224     c     kaco          chain     cnaco00f
016400030221     c                   exsr      stampa
016500060117     c                   ENDIF
016600120801     c                   ENDIF
016700030220     c                   End
016800030221     c                   End
016900030221     c                   Enddo
017000030221     c                   End
017100030220     c*
017200030220     c                   endsr
017300030220     c**********************************************************************
017400030221     c     stampa        begsr
017500030220     c**********************************************************************
017600030224     C* stampa testata
017700030224     c* per non ripetere sempre il cliente variato
017800080128     C                   IF        ACOKSC <> SAVKSC
017900080128     C                   EVAL      SAVKSC = acoksc
018000080128     C                   EVAL      SAVKSCs = acoksc
018100080128     C                   EVAL      SAVrags = acorag
018200030224     C                   EVAL      SAVpivs = indiva
018300030224     c                   else
018400030224     c                   if        *inof
018500080128     C                   EVAL      SAVKSCs = acoksc
018600080128     C                   EVAL      SAVrags = acorag
018700030224     C                   EVAL      SAVpivs = indiva
018800030224     c                   else
018900120507     c*                  clear                   savkscs
019000120507     c*                  clear                   savrags
019100120507     c*                  clear                   savpivs
019200030224     c                   end
019300030224     c                   end
019400030224     c   of              except    testa
019500030224     c                   setoff                                       of
019600030224     c* stampa riga
019700030221     c                   except    riga
019800120508     **
019900120518EDPDCC                   EVAL      buf = '"0' + %EDITC(savkscs:'X') +
020000120801     C                             '";"' + clnCliFatt +
020100120801EDPDCC                             '";"' + %TRIMR(savrags)
020200120518EDPDCC                             + '";"' + %TRIMR(savpivs) + '";"0' +
020300120518EDPDCC                             %EDITC(£acoksc:'X') + '";"' +
020400120518EDPDCC                             %TRIMR(£acorag) + '";"' + clpcon +
020500120801     C                             '";' + %TRIML(%EDITC(esposizione : 'P'))
020600120508     C                             + CRLF
020700120508     C                   EVAL      bytes_write = IFS_WriteToDescriptor( fd
020800120508     C                             : bufPtr : %LEN(%TRIMR(buf)) )
020900120508     C
021000120508     C                   IF         bytes_write = -1
021100120508     C                   EVAL       errnoPtr = getErrno()
021200120508     C                   DUMP(A)
021300120508     C                   EVAL      *INLR = *ON
021400120508     C                   RETURN
021500120508     C                   ENDIF
021600120508     **
021700030221     c                   endsr
021800030221     c**********************************************************************
021900030221     c     *inzsr        begsr
022000030221     c**********************************************************************
022100030220     C     *ENTRY        PLIST
022200030220     C                   PARM                    KPJBA
022300030221     C                   movel     KPJBU         ycoa01ds
022400030221     c                   move      y01dti        dti
022500030221     c                   move      y01dtf        dtf
022600030221     c                   move      y01dti        dataiso
022700030221     c                   move      dataiso       dtieur
022800030221     c                   move      y01dtf        dataiso
022900030221     c                   move      dataiso       dtfeur
023000030220     c*
023100030221     c     kind          klist
023200030221     c                   kfld                    acokut
023300030221     c                   kfld                    acokcc
023400030221     c                   kfld                    acoksc
023500030221     c*
023600030221     c     kind2         klist
023700030221     c                   kfld                    indkut
023800030221     c                   kfld                    indkcc
023900030221     c                   kfld                    indiva
024000030221     c*
024100030221     c     kaco          klist
024200030221     c                   kfld                    indkut
024300030221     c                   kfld                    indkcc
024400030221     c                   kfld                    indksc
024500030224     c*
024600030224     c     ktab          klist
024700030224     c                   kfld                    acokut
024800030224     c                   kfld                    tblcod
024900030224     c                   kfld                    tblkey
025000030224     c*
025100080128     c                   eval      acokut = 1
025200080128     c                   eval      acokcc = 151
025300030221     c                   TIME                    dataeur
025400030221     c                   seton                                        of
025500120508     ** Creazione e apertura del file.csv nella directory corrente.
025600120508     C                   EVAL      fd = IFS_OpenFile( 'clientiInContenzioso_'
025700120508     C                             + %CHAR(y01dti) + '_' + %CHAR(y01dtf)
025800120508     C                             + '.csv'
025900120507     C                             : O_CREAT + O_WRONLY + O_CCSID + O_TEXTDATA
026000120507     C                             + O_TEXT_CREAT + O_EXCL
026100120507     C                             : S_IRUSR + S_IWUSR + S_IXUSR
026200120507     C                             : CP_WINDOWS : CP_ITALIAN)
026300120508     C
026400120507     C                   IF         fd = -1
026500120507     C                   EVAL       errnoPtr = getErrno()
026600120507     C                   DUMP(A)
026700120508     C                   EVAL      *INLR = *ON
026800120507     C                   RETURN
026900120507     C                   ENDIF
027000120508     C
027100120801     C                   EVAL      buf = '"IMMESSO";"CLIENTE_FATTURAZIONE"-
027200120801     C                             ;"RAGIONE_SOCIALE";-
027300120508     C                             "PARTITA_IVA";"CONTENZIOSO";-
027400120508     C                             "RAGIONE_SOCIALE";"ID_STATO_CREDITO";-
027500120801EDPDCC                             "ESPOSIZIONE"'
027600120508     C                             + CRLF
027700120508     C
027800120508     C                   EVAL      bytes_write = IFS_WriteToDescriptor( fd
027900120508     C                             : bufPtr : %LEN(%TRIMR(buf)) )
028000120508     C
028100120508     C                   IF         bytes_write = -1
028200120508     C                   EVAL       errnoPtr = getErrno()
028300120508     C                   DUMP(A)
028400120508     C                   EVAL      *INLR = *ON
028500120508     C                   RETURN
028600120508     C                   ENDIF
028700120507     **
028800030221     c                   endsr
028900120507
029000030224     oprtf198   e            testa          3  1
029100030221     O                       KNSif
029200030221     o                                           50 'CLIENTI IN CONTENZIOSO DAL'
029300030221     o                       dtieur              +1
029400030221     o                                           +1 'AL'
029500030221     o                       dtfeur              +1
029600030221     o                       PgmName            100
029700030221     o                       dataeur             +1
029800030221     o                                          127 'Pag.'
029900030221     o                       page               132
030000030221     o          e            testa          2
030100080128     o                                            7 'Immesso'
030200030221     o                                           +1 'Ragione sociale'
030300030221     o                                           62 'P.Iva'
030400030221     o                                           81 'Conten.'
030500030221     o                                           +1 'Ragione sociale'
030600030224     o                                          150 'Blocco contenzioso'
030700050803     o                                          178 'Esposizione'
030800030221     o          e            riga        2
030900030224     o                       SAVkscs       z      7
031000030224     o                       SAVrags             +1
031100030224     o                       SAVpivs             +1
031200030224     o                       £acoksc       z     +1
031300030221     o                       £acorag             +1
031400030224     o                       clpcon              +1
031500030224     o                       §4wdes              +1
031600050803     o                       Esposizione   2     +1
