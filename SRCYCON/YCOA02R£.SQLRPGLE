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
003900080128     D anrco         e DS                  extname(anrco00f)
004000080128     D cnaco         e DS                  extname(cnaco00f)
004100080128     D cnaco0        e DS                  extname(cnaco00f) prefix(£)
004200030224     d savksc          s                   like(acoksc)
004300030224     d savkscs         s                   like(acoksc)
004400030224     d savrags         s                   like(acorag)
004500030224     d savpivs         s                   like(indiva)
004600080128     d dti             s                   like(rcodtimm)
004700080128     d dtf             s                   like(rcodtimm)
004800030221     d dataEUR         s               d   datfmt(*EUR)
004900030221     d dtieur          s               d   datfmt(*EUR)
005000030221     d dtfeur          s               d   datfmt(*EUR)
005100030221     d dataiso         s               d
005200050803     D Esposizione     S             11P 2
005300030221     D                SDS
005400030221     D PgmName           *PROC
005500120507
005600120508      /COPY GAITRASRC/SRCCONST,IFS
005700120508      /COPY GAITRASRC/SRCCONST,ERRNO
005800120508     D CRLF...
005900120508     D                 C                   X'0D25'
006000120508      /COPY GAITRASRC/SRCPROTOPI,ERRNO
006100120508      /COPY GAITRASRC/SRCPROTOPR,ERRNO
006200120508      /COPY GAITRASRC/SRCPROTOPR,IFS
006300120507     D bytes_write...
006400120507     D                 S             10I 0
006500120507     D fd...
006600120507     D                 S             10I 0
006700120507     D buf...
006800120508     D                 S          32766A
006900120507     D bufPtr...
007000120507     D                 S               *   INZ(%ADDR(buf))
007100120507
007200030117      *****************************************************************
007300120508     c                   seton                                        lr
007400080128     C* Leggo il file anrco con data immissione compresa nel range di
007500080128     c* date scelte a video.
007600030220     C/EXEC SQL
007700080128     C+ DECLARE A1 CURSOR FOR SELECT * FROM anrco00f WHERE rcodtimm between
007800080128     C+ :dti and :dtf  AND rcoKcc = '000151'
007900030220     C/END-EXEC
008000030220      *          apertura cursore
008100030220     C/EXEC SQL
008200030220     C+ OPEN A1
008300030220     C/END-EXEC
008400030220     C                   DOU       SqlCod = 100 OR SqlCod < 0
008500030220      *          lettura cursore
008600030220     C/EXEC SQL
008700080128     C+ Fetch Next From A1 Into :anrco
008800030220     C/END-EXEC
008900030220      *
009000030220     C                   SELECT
009100030220     C                   WHEN      SqlCod = 100
009200030220     C                   WHEN      SqlCod >= 0
009300030221     c* reperisco la partita iva e controllo se esistoo altre anagrafiche
009400030221     c* in contenzioso
009500080128     c                   move      rcoksc        acoksc
009600030221     c                   exsr      srind
009700030220     C                   OTHER
009800030220     C                   ENDSL
009900030220      *
010000030220     C                   ENDDO
010100030220     C/EXEC SQL
010200030220     C+ Close A1
010300030220     C/END-EXEC
010400120508     C                   CALLP     IFS_CloseFile( fd )
010500120508
010600030220     c**********************************************************************
010700030221     c     srind         begsr
010800030220     c**********************************************************************
010900030221     c* reperisco p.i.
011000030221     c     kind          chain     cnind00f
011100030221     c                   if        %found(cnind00f)
011200080128     c* reperisco ragione sociale cliente contenzioso
011300080128     c     kaco          chain     cnaco00f
011400080128     c                   if        %found(cnaco00f)
011500080128     c                   eval      cnaco = cnaco0
011600080128     c                   end
011700030221     c* leggo tutti i ksc con quella p.i.
011800030221     c     kind2         setll     cnind02l
011900030221     c                   do        *hival
012000030221     c     kind2         reade     cnind02l
012100030221     c                   if        %eof(cnind02l)
012200030221     c                   leave
012300030221     c                   end
012400030221     c* controllo se ksc è in contenzioso
012500030221     c                   if        indksc <> acoksc
012600051018     C                             and %subst(indiva:1:2) <> '$$'
012700030224     c     kaco          chain     cnclp00f
012800030224     c                   if        %found(cnclp00f) and clpcon <> *blanks
012900040301     c                             and clpcon <> ' 30'
013000050803     ** Reperisco l'esposizione del cliente.
013100050803     C                   CLEAR                   Esposizione
013200050803     C                   RESET                   X20ScadDS
013300050803     C                   EVAL      x20Kcc = '00' + %EDITC(indKcc:'X')
013400050803     C                   EVAL      x20Ksc = '0' + %EDITC(indKsc:'X')
013500050803     C                   CALL      'X20SCADR'
013600050803     C                   PARM                    x20ScadDS
013700050803     C                   EVAL      Esposizione = x20LScadu + x20LAScad
013800060117     C                   IF        Esposizione >= 0
013900030224     c* reperisco ragione sociale cliente contenzioso
014000030224     c     kaco          chain     cnaco00f
014100030221     c                   exsr      stampa
014200060117     c                   ENDIF
014300030220     c                   End
014400030221     c                   End
014500030221     c                   Enddo
014600030221     c                   End
014700030220     c*
014800030220     c                   endsr
014900030220     c**********************************************************************
015000030221     c     stampa        begsr
015100030220     c**********************************************************************
015200030224     C* stampa testata
015300030224     c* per non ripetere sempre il cliente variato
015400080128     C                   IF        ACOKSC <> SAVKSC
015500080128     C                   EVAL      SAVKSC = acoksc
015600080128     C                   EVAL      SAVKSCs = acoksc
015700080128     C                   EVAL      SAVrags = acorag
015800030224     C                   EVAL      SAVpivs = indiva
015900030224     c                   else
016000030224     c                   if        *inof
016100080128     C                   EVAL      SAVKSCs = acoksc
016200080128     C                   EVAL      SAVrags = acorag
016300030224     C                   EVAL      SAVpivs = indiva
016400030224     c                   else
016500120507     c*                  clear                   savkscs
016600120507     c*                  clear                   savrags
016700120507     c*                  clear                   savpivs
016800030224     c                   end
016900030224     c                   end
017000030224     c   of              except    testa
017100030224     c                   setoff                                       of
017200030224     C* decodifico blocco contenzioso
017300030224     C                   MOVEL     '4W'          tblcod                         *
017400030224     C                   MOVEL     *BLANKS       COMO3             3            *
017500030224     C                   MOVE      CLPCON        COMO3
017600030224     C                   MOVEL(P)  COMO3         tblkey                         *
017700030224     C     ktab          CHAIN     TABEL00F                                     *
017800030224     c                   if        %found(tabel00f)
017900030224     C                   MOVEL     TBLUNI        DS4W
018000030224     c                   end
018100030224     c* stampa riga
018200030221     c                   except    riga
018300120508     **
018400120518EDPDCC                   EVAL      buf = '"0' + %EDITC(savkscs:'X') +
018500120518EDPDCC                             '";"' + %TRIMR(savrags)
018600120518EDPDCC                             + '";"' + %TRIMR(savpivs) + '";"0' +
018700120518EDPDCC                             %EDITC(£acoksc:'X') + '";"' +
018800120518EDPDCC                             %TRIMR(£acorag) + '";"' + clpcon +
018900120508     C                             '";' + %TRIML(%EDITC(esposizione : 'P'))
019000120508     C                             + CRLF
019100120508     C                   EVAL      bytes_write = IFS_WriteToDescriptor( fd
019200120508     C                             : bufPtr : %LEN(%TRIMR(buf)) )
019300120508     C
019400120508     C                   IF         bytes_write = -1
019500120508     C                   EVAL       errnoPtr = getErrno()
019600120508     C                   DUMP(A)
019700120508     C                   EVAL      *INLR = *ON
019800120508     C                   RETURN
019900120508     C                   ENDIF
020000120508     **
020100030221     c                   endsr
020200030221     c**********************************************************************
020300030221     c     *inzsr        begsr
020400030221     c**********************************************************************
020500030220     C     *ENTRY        PLIST
020600030220     C                   PARM                    KPJBA
020700030221     C                   movel     KPJBU         ycoa01ds
020800030221     c                   move      y01dti        dti
020900030221     c                   move      y01dtf        dtf
021000030221     c                   move      y01dti        dataiso
021100030221     c                   move      dataiso       dtieur
021200030221     c                   move      y01dtf        dataiso
021300030221     c                   move      dataiso       dtfeur
021400030220     c*
021500030221     c     kind          klist
021600030221     c                   kfld                    acokut
021700030221     c                   kfld                    acokcc
021800030221     c                   kfld                    acoksc
021900030221     c*
022000030221     c     kind2         klist
022100030221     c                   kfld                    indkut
022200030221     c                   kfld                    indkcc
022300030221     c                   kfld                    indiva
022400030221     c*
022500030221     c     kaco          klist
022600030221     c                   kfld                    indkut
022700030221     c                   kfld                    indkcc
022800030221     c                   kfld                    indksc
022900030224     c*
023000030224     c     ktab          klist
023100030224     c                   kfld                    acokut
023200030224     c                   kfld                    tblcod
023300030224     c                   kfld                    tblkey
023400030224     c*
023500080128     c                   eval      acokut = 1
023600080128     c                   eval      acokcc = 151
023700030221     c                   TIME                    dataeur
023800030221     c                   seton                                        of
023900120508     ** Creazione e apertura del file.csv nella directory corrente.
024000120508     C                   EVAL      fd = IFS_OpenFile( 'clientiInContenzioso_'
024100120508     C                             + %CHAR(y01dti) + '_' + %CHAR(y01dtf)
024200120508     C                             + '.csv'
024300120507     C                             : O_CREAT + O_WRONLY + O_CCSID + O_TEXTDATA
024400120507     C                             + O_TEXT_CREAT + O_EXCL
024500120507     C                             : S_IRUSR + S_IWUSR + S_IXUSR
024600120507     C                             : CP_WINDOWS : CP_ITALIAN)
024700120508     C
024800120507     C                   IF         fd = -1
024900120507     C                   EVAL       errnoPtr = getErrno()
025000120507     C                   DUMP(A)
025100120508     C                   EVAL      *INLR = *ON
025200120507     C                   RETURN
025300120507     C                   ENDIF
025400120508     C
025500120508     C                   EVAL      buf = '"IMMESSO";"RAGIONE_SOCIALE";-
025600120508     C                             "PARTITA_IVA";"CONTENZIOSO";-
025700120508     C                             "RAGIONE_SOCIALE";"ID_STATO_CREDITO";-
025800120518EDPDCC                             "ESPOSIZIONE"'
025900120508     C                             + CRLF
026000120508     C
026100120508     C                   EVAL      bytes_write = IFS_WriteToDescriptor( fd
026200120508     C                             : bufPtr : %LEN(%TRIMR(buf)) )
026300120508     C
026400120508     C                   IF         bytes_write = -1
026500120508     C                   EVAL       errnoPtr = getErrno()
026600120508     C                   DUMP(A)
026700120508     C                   EVAL      *INLR = *ON
026800120508     C                   RETURN
026900120508     C                   ENDIF
027000120507     **
027100030221     c                   endsr
027200120507
027300030224     oprtf198   e            testa          3  1
027400030221     O                       KNSif
027500030221     o                                           50 'CLIENTI IN CONTENZIOSO DAL'
027600030221     o                       dtieur              +1
027700030221     o                                           +1 'AL'
027800030221     o                       dtfeur              +1
027900030221     o                       PgmName            100
028000030221     o                       dataeur             +1
028100030221     o                                          127 'Pag.'
028200030221     o                       page               132
028300030221     o          e            testa          2
028400080128     o                                            7 'Immesso'
028500030221     o                                           +1 'Ragione sociale'
028600030221     o                                           62 'P.Iva'
028700030221     o                                           81 'Conten.'
028800030221     o                                           +1 'Ragione sociale'
028900030224     o                                          150 'Blocco contenzioso'
029000050803     o                                          178 'Esposizione'
029100030221     o          e            riga        2
029200030224     o                       SAVkscs       z      7
029300030224     o                       SAVrags             +1
029400030224     o                       SAVpivs             +1
029500030224     o                       £acoksc       z     +1
029600030221     o                       £acorag             +1
029700030224     o                       clpcon              +1
029800030224     o                       §4wdes              +1
029900050803     o                       Esposizione   2     +1
