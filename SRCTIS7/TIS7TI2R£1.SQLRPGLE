000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TRUL')
000300991027
000400071128     FTITAS30C  IF   E           K DISK
000500090323     FTIVGDTMP  uF   E             DISK
000600071128     Ftis7prgf  uf   E             DISK    rename(tis7prgf:tis7prg0)
000700071128     F                                     prefix(f_)
000800071128     F                                     usropn
000900071128     FTIVLT01L  UF A E           K DISK
001000071204
001100100114     D/COPY GAITRASRC/SRCPROTOPR,UBUTISQL
001200071204     D/COPY GAITRASRC/SRCPROTOPR,TRULAPICI
001300071127
001400071128     D DVAIMAC       E DS                  inz
001500071128     D DVAIBAN       E DS                  inz prefix(§)
001600071128     D TITAS00FDS    E DS                  extname(TITAS00F) inz
001700071205     D TIVAWWWT      E DS                  inz
001800991027
001900991027     D prmppt          s             50
002000991027     D prmesito        s              1
002100991027     D wrkesito        s                   like(prmesito)
002200090323     D fivaids       e ds                  extname(fivai00t)
002300071128
002400071205     D wMbrPrgMAC      s             10    inz
002500071205     D wMbrPrgBAN      s             10    inz
002600071128     D dwlprg          s             10    inz(*all'0')
002700071128     D wrkprg          s              8  0 inz(*zeros)
002800071128     D wrkoggiiso      s               d
002900071128     D wrkmese         s              2  0
003000071128     D prmisv          s              2
003100071128     D prmksc          s              7
003200080219     D prmout          s              1
003300071128     D mc              s              1    dim(12) ctdata perrcd(12)
003400071205     D cmd             s             80    dim(5)  ctdata perrcd(1)
003500071205     D depcmd          s            150    INZ(*blanks)
003600071205
003700071128     D                 ds                  INZ
003800071128     D wrkday                  1      2s 0
003900071128     D wrkdaychr                      2    overlay(wrkday)
004000071204
004100090327     D WWWdta          s          65535A   inz
004200071204     D wStringaSQl     s           1000A   varying
004300060720     D wDlmTxt         s              5    varying inz('')
004400071130
004500071128
004600071128     D*---------------------
004700071128     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800071128     D*---------------------
004900071128     D WLBDA8          DS
005000071128     D  G08DAT                 1      8  0
005100071128     D  G08INV                 9     16  0
005200071128     D  G08ERR                17     17
005300071128     D  G08TGI                18     22  0
005400071128
005500991027
005600071205     C*
005700071205     C                   setoff                                       70
005800071205     C*
005900071205     C  N70              EXSR      preOPE
006000080219     C*
006100080219     C  N70
006200080219     CAN 80              EXSR      opeFILEMAC
006300080219     C*
006400080219     C  N70
006500080219     CAN 81              EXSR      calPROG
006600080219     C  N70
006700080219     CAN 81              EXSR      opeFILEBAN
006800080219     C*
006900080219     C  N70
007000080219     CAN 80              EXSR      chkRECFILMAC
007100080219     C*
007200080219     C  N70
007300080219     CAN 81              EXSR      chkRECFILBAN
007400080219     C*
007500071205     C  N70              EXSR      traduci
007600080219     C*
007700080219     C  N70
007800080219     CAN 81              EXSR      creaLOG
007900080219     C*
008000071204     C  N70              EXSR      cloFILE
008100071128     C*
008200921023     C                   SETON                                        LR
008300071205
008400071205
008500071205
008600071205      /TITLE Operazioni preliminari
008700071205     C     preOPE        begsr
008800071205     C*
008900071205     C                   MOVEL(p)  CMD(1)        depcmd
009000071205     C*
009100071205     C                   Z-ADD     150           LENGH            15 5
009200071205     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
009300071205     C                   PARM                    depcmd
009400071205     C                   PARM                    LENGH
009500071205     C*
009600071205     C                   IF        %error
009700071205     C                   SETON                                        70
009800071205     C                   ENDIF
009900071205     C*
010000071205     C                   endsr
010100071204
010200071204
010300071204
010400071204      /TITLE Valorizzazione Progressivo Applicazione
010500071204     C     calprog       begsr
010600071204     C*
010700071204     C                   open      tis7prgf
010800071204     C*
010900071204     C                   read(e)   tis7prgf
011000071204     C                   if        not %error
011100071204     C                   eval      dwlprg = f_tis7prgf
011200071204     C*
011300071204     C                   move(p)   dwlprg        wrkprg
011400071204     C                   add       1             wrkprg
011500071204     C                   move(p)   wrkprg        dwlprg
011600071204     C                   movel     prmisv        dwlprg
011700071204     C*
011800071204     C                   eval      f_tis7prgf = dwlprg
011900071204     C                   update    tis7prg0
012000071204     C                   endif
012100071204     C*
012200071204     C                   close     tis7prgf
012300071204     C*
012400071204     C                   endsr
012500071204
012600071204
012700071128
012800071128
012900071205     C     opeFILEMAC    BEGSR
013000071128     C*
013100071205     C                   eval      wMbrPrgMAC = 'M0' + prmksc
013200071130     C*
013300071128     C                   call      'TIS7TIC1'
013400071204     C                   parm      *blanks       wNomeLib         10
013500071128     C                   parm      'TIVAWWWT'    wNomeFile        10
013600071205     C                   parm      wMbrPrgMAC    wNomeMembro      10
013700071128     C                   parm      'TIS7TIR1'    wTesto           30
013800071128     C                   parm      *blanks       wEsito            1
013900071130     C*
014000071130     C                   if        wEsito = '1'
014100071130     C*
014200071205     C                   if        SQL_CreaAlias(wMbrPrgMAC
014300071204     C                                          :'QTEMP'
014400071204     C                                          :'TIVAWWWT'
014500071204     C                                          :*blanks
014600071205     C                                          :wMbrPrgMAC)
014700071205     C                                          = 0
014800071204     C                   else
014900071203     C                   seton                                        70
015000071203     C                   endif
015100071130     C*
015200071204     C                   else
015300071204     C                   seton                                        70
015400071128     C                   endif
015500071128     C*
015600071128     C                   ENDSR
015700071205
015800071205
015900071205
016000071205
016100071205     C     opeFILEBAN    BEGSR
016200071205     C*
016300071205     C                   eval      wMbrPrgBAN = dwlprg
016400071205     C*
016500071205     C                   call      'TIS7TIC1'
016600071205     C                   parm      *blanks       wNomeLib         10
016700071205     C                   parm      'TIVAWWWT'    wNomeFile        10
016800071205     C                   parm      wMbrPrgBAN    wNomeMembro      10
016900071205     C                   parm      'TIS7TIR1'    wTesto           30
017000071205     C                   parm      *blanks       wEsito            1
017100071205     C*
017200071205     C                   if        wEsito = '1'
017300071205     C*
017400071205     C                   if        SQL_CreaAlias(wMbrPrgBAN
017500071205     C                                          :'QTEMP'
017600071205     C                                          :'TIVAWWWT'
017700071205     C                                          :*blanks
017800071205     C                                          :wMbrPrgBAN)
017900071205     C                                          = 0
018000071205     C                   else
018100071205     C                   seton                                        70
018200071205     C                   endif
018300071205     C*
018400071205     C                   else
018500071205     C                   seton                                        70
018600071205     C                   endif
018700071205     C*
018800071205     C                   ENDSR
018900071204
019000071204
019100071204
019200071204
019300071204     C     cloFILE       BEGSR
019400071204     C*
019500080219     C   80              callp     SQL_DropAlias(wMbrPrgMAC:'QTEMP')
019600080219     C   81              callp     SQL_DropAlias(wMbrPrgBAN:'QTEMP')
019700071204     C*
019800071204     C                   ENDSR
019900071205
020000071205
020100071205
020200071205
020300071205     C     chkRECFILMAC  BEGSR
020400071205     C*
020500071205     C                   call      'TRUL51C'
020600071205     C                   parm      '*LIBL'       wUL51_Lib        10
020700071205     C                   parm      'TIVAWWWT'    wUL51_Fil        10
020800071205     C                   parm      wMbrPrgMAC    wUL51_Mbr        10
020900071205     C                   parm      *zeros        wUL51_Nrr        10 0
021000071205     C*
021100071205     C                   if        wUL51_Nrr = *zeros
021200071205     C                   seton                                        55
021300071205     C                   endif
021400071205     C*
021500071205     C                   ENDSR
021600071204
021700071204
021800071203
021900071203
022000071204     C     chkRECFILBAN  BEGSR
022100071203     C*
022200071203     C                   call      'TRUL51C'
022300071204     C                   parm      '*LIBL'       wUL51_Lib        10
022400071204     C                   parm      'TIVAWWWT'    wUL51_Fil        10
022500071205     C                   parm      wMbrPrgBAN    wUL51_Mbr        10
022600071204     C                   parm      *zeros        wUL51_Nrr        10 0
022700071203     C*
022800071203     C                   if        wUL51_Nrr = *zeros
022900071203     C                   seton                                        56
023000071203     C                   endif
023100071203     C*
023200071203     C                   ENDSR
023300071204
023400071204
023500071204
023600071204
023700071205     C     insertMAC     BEGSR
023800071204     C*
023900071204     C* Costruisco la stringa SQL parametrica
024000090327     c                   eval      WWWdta = VAWdta
024100071205     C                   eval      wStringaSQL = 'insert into ' + wMbrPrgMAC +
024200071205     C                                           ' values('''                +
024300090327     C                              RaddoppiareApici(wwwDTA : %SIZE(VAWDTA)) +
024400071205     C                                           ''')'
024500071204     C                   eval      %len(wStringaSQL) = %len(%trim(wStringaSQL))
024600071204     C*
024700071204     C/EXEC SQL
024800071204     C+ EXECUTE IMMEDIATE :wStringaSQL
024900071204     C/END-EXEC
025000071204     C*
025100071204     C                   ENDSR
025200071205
025300071205
025400071205
025500071205
025600071205     C     insertBAN     BEGSR
025700071205     C*
025800071205     C* Costruisco la stringa SQL parametrica
025900090327     c                   eval      WWWdta = VAWdta
026000071205     C                   eval      wStringaSQL = 'insert into ' + wMbrPrgBAN +
026100071205     C                                           ' values('''                +
026200090327     C                              RaddoppiareApici(wwwDTA : %SIZE(VAWDTA)) +
026300071205     C                                           ''')'
026400071205     C                   eval      %len(wStringaSQL) = %len(%trim(wStringaSQL))
026500071205     C*
026600071205     C/EXEC SQL
026700071205     C+ EXECUTE IMMEDIATE :wStringaSQL
026800071205     C/END-EXEC
026900071205     C*
027000071205     C                   ENDSR
027100071128
027200071128
027300071128
027400071128
027500991027     C     traduci       BEGSR
027600040206     C*
027700040206     C* Verifico se richiesto un particolare carattere d separatore campi
027800071127     C                   IF        %subst(prmppt:2:1) <> *blanks
027900071128     C                   EVAL      wSepCampi = %subst(prmppt:2:1)
028000040206     C                   ELSE
028100071127     C                   MOVEL     ','           wSepCampi         1
028200040206     C                   ENDIF
028300060720     C*
028400060720     C* Verifico se richiesto nessun delimitatore testo
028500060720     C*   => *blanks = standard = "
028600060720     C*   => N       = nessun delimitatore testo
028700060720     C                   EVAL      wDlmTxt = '"'
028800071127     C                   IF        %subst(prmppt:3:1) = 'N'
028900060720     C                   EVAL      wDlmTxt = *blanks
029000060720     C                   EVAL      wDlmTxt = %trim(wDlmTxt)
029100060720     C                   ENDIF
029200030325     C*
029300030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
029400080219     C                   IF        *IN80
029500030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
029600071128     C                   EVAL      VAWDTA =
029700071128     C                             wDlmTxt+'VAIATB'+wDlmTxt+wSepCampi+
029800071128     C                             wDlmTxt+'VAICCM'+wDlmTxt+wSepCampi+
029900071128     C                             wDlmTxt+'VAIAAS'+wDlmTxt+wSepCampi+
030000071128     C                             wDlmTxt+'VAILNP'+wDlmTxt+wSepCampi+
030100071128     C                             wDlmTxt+'VAINRS'+wDlmTxt+wSepCampi+
030200071128     C                             wDlmTxt+'VAINSP'+wDlmTxt+wSepCampi+
030300071128     C                             wDlmTxt+'VAIRMN'+wDlmTxt+wSepCampi+
030400071128     C                             wDlmTxt+'VAIRMA'+wDlmTxt+wSepCampi+
030500071128     C                             wDlmTxt+'VAIRMX'+wDlmTxt+wSepCampi+
030600071128     C                             wDlmTxt+'VAIDAC'+wDlmTxt+wSepCampi+
030700071128     C                             wDlmTxt+'VAIORC'+wDlmTxt+wSepCampi+
030800071128     C                             wDlmTxt+'VAITDT'+wDlmTxt+wSepCampi+
030900071128     C                             wDlmTxt+'CAUS_VAR'+wDlmTxt+wSepCampi+
031000071128     C                             wDlmTxt+'FIL_VAR'+wDlmTxt+wSepCampi+
031100071128     C                             wDlmTxt+'TIPO_DATA'+wDlmTxt+wSepCampi+
031200071128     C                             wDlmTxt+'DATA_CONS_RICH'+wDlmTxt+wSepCampi+
031300071128     C                             wDlmTxt+'ORA_CONS_RICH'+wDlmTxt+wSepCampi+
031400071128     C                             wDlmTxt+'1_GIORNO_CHIUS'+wDlmTxt+wSepCampi+
031500071128     C                             wDlmTxt+'2_GIORNO_CHIUS'+wDlmTxt+wSepCampi+
031600071128     C                             wDlmTxt+'1_CONS_PART'+wDlmTxt+wSepCampi+
031700071128     C                             wDlmTxt+'2_CONS_PART'+wDlmTxt+wSepCampi+
031800071128     C                             wDlmTxt+'NOMINATIVO_VAR'+wDlmTxt+wSepCampi+
031900071128     C                             wDlmTxt+'TELEFONO_DEST'+wDlmTxt+wSepCampi+
032000071128     C                             wDlmTxt+'MOTIVO_VAR'+wDlmTxt+wSepCampi+
032100071128     C                             wDlmTxt+'RAG_SOC_DEST'+wDlmTxt+wSepCampi+
032200071128     C                             wDlmTxt+'INDIRIZZO_DEST'+wDlmTxt+wSepCampi+
032300071128     C                             wDlmTxt+'LOCALITA_DEST'+wDlmTxt+wSepCampi+
032400071128     C                             wDlmTxt+'CAP_DEST'+wDlmTxt+wSepCampi+
032500071128     C                             wDlmTxt+'PROVINCIA_DEST'+wDlmTxt+wSepCampi+
032600071128     C                             wDlmTxt+'NAZIONE_DEST'+wDlmTxt+wSepCampi
032700030325     C*
032800071205     C                   EXSR      insertMAC
032900030325     C                   ENDIF
033000080219     C                   ENDIF
033100071128     C*
033200071128     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
033300080219     C                   IF        *IN81
033400071128     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN56 = *ON
033500071128     C                   EVAL      VAWDTA =
033600071128     C                             wDlmTxt+'VAIATB'+wDlmTxt+wSepCampi+
033700071128     C                             wDlmTxt+'VAICCM'+wDlmTxt+wSepCampi+
033800071128     C                             wDlmTxt+'VAIAAS'+wDlmTxt+wSepCampi+
033900071128     C                             wDlmTxt+'VAILNP'+wDlmTxt+wSepCampi+
034000071128     C                             wDlmTxt+'VAINRS'+wDlmTxt+wSepCampi+
034100071128     C                             wDlmTxt+'VAINSP'+wDlmTxt+wSepCampi+
034200071128     C                             wDlmTxt+'VAIRMN'+wDlmTxt+wSepCampi+
034300071128     C                             wDlmTxt+'VAIRMA'+wDlmTxt+wSepCampi+
034400071128     C                             wDlmTxt+'VAIRMX'+wDlmTxt+wSepCampi+
034500071128     C                             wDlmTxt+'VAIDAC'+wDlmTxt+wSepCampi+
034600071128     C                             wDlmTxt+'VAIORC'+wDlmTxt+wSepCampi+
034700071128     C                             wDlmTxt+'VAITDT'+wDlmTxt+wSepCampi+
034800071128     C                             wDlmTxt+'1MO_TIPO_BANC'+wDlmTxt+wSepCampi+
034900071128     C                             wDlmTxt+'NUM_BANC_1MO'+wDlmTxt+wSepCampi+
035000071128     C                             wDlmTxt+'2ND_TIPO_BANC'+wDlmTxt+wSepCampi+
035100071128     C                             wDlmTxt+'NUM_BANC_2ND'+wDlmTxt+wSepCampi+
035200071128     C                             wDlmTxt+'BANC_1MO_NON_RIT'+wDlmTxt+wSepCampi+
035300071128     C                             wDlmTxt+'BANC_2ND_NON_RIT'+wDlmTxt+wSepCampi+
035400071128     C                             wDlmTxt+'RITIRO_NEGATO_DA'+wDlmTxt+wSepCampi+
035500071128     C                             wDlmTxt+'MOTIVO_RIFIUTO'+wDlmTxt+wSepCampi+
035600071128     C                             wDlmTxt+'RAG_SOC_DEST'+wDlmTxt+wSepCampi+
035700071128     C                             wDlmTxt+'INDIRIZZO_DEST'+wDlmTxt+wSepCampi+
035800071128     C                             wDlmTxt+'LOCALITA_DEST'+wDlmTxt+wSepCampi+
035900071128     C                             wDlmTxt+'CAP_DEST'+wDlmTxt+wSepCampi+
036000071128     C                             wDlmTxt+'PROVINCIA_DEST'+wDlmTxt+wSepCampi+
036100071128     C                             wDlmTxt+'NAZIONE_DEST'+wDlmTxt+wSepCampi
036200071128     C*
036300071205     C                   EXSR      insertBAN
036400071128     C                   ENDIF
036500080219     C                   ENDIF
036600991027     C*
036700071128     C* Quindi scorro tutto il file d input
036800090323     C                   READ      TIVGDTMP
036900090323     C                   DOW       not %eof(TIVGDTMP)
037000090323     C                   eval      fivaids = vgdDTA
037100071128     C*
037200071128     C* Aggancio subito la bolla
037300071128     C     KEYtas30_P    CHAIN     TITAS30C
037400071128     C                   IF        %found(TITAS30C)
037500071128     C                   ELSE
037600071128     C                   EVAL      TITAS00FDS = *all'*'
037700071128     C                   ENDIF
037800071128     C*
037900071128     C* Gestione x tipo record MAC
038000080219     C                   IF        VAITDT = 'MAC' AND *IN80
038100071127     C                   EVAL      DVAIMAC= VAIDTI
038200071127     C                   EVAL      VAWDTA=
038300071127     C                                  wDlmTxt+%trim(VAIATB)+wDlmTxt+wSepCampi+
038400071127     C                                  %editc(VAICCM:'Q')+wSepCampi+
038500071127     C                                  %editc(VAIAAS:'Q')+wSepCampi+
038600071127     C                                  %editc(VAILNP:'Q')+wSepCampi+
038700071127     C                                  %editc(VAINRS:'Q')+wSepCampi+
038800071127     C                                  %editc(VAINSP:'Q')+wSepCampi+
038900071127     C                                  %editc(VAIRMN:'Q')+wSepCampi+
039000071127     C                                  wDlmTxt+%trim(VAIRMA)+wDlmTxt+wSepCampi+
039100071127     C                                  wDlmTxt+%trim(VAIRMX)+wDlmTxt+wSepCampi+
039200071127     C                                  %editw(VAIDAC:'    /  /  ')+wSepCampi+
039300071127     C                                  %editw(VAIORC:'  :  :  ')+wSepCampi+
039400071127     C                                  wDlmTxt+%trim(VAITDT)+wDlmTxt+wSepCampi+
039500071127     C                             wDlmTxt+%trim(§VAICVB)+wDlmTxt+wSepCampi+
039600071127     C                                  %editc(§VAIPOU:'Q')+wSepCampi+
039700071127     C                             wDlmTxt+%trim(§VAITCR)+wDlmTxt+wSepCampi+
039800071127     C                             %editw(§VAIDCR:'    /  /  ')+wSepCampi+
039900071127     C                             %editw(§VAIHCR:'  :  ')+wSepCampi+
040000071127     C                             wDlmTxt+%trim(§VAIGC1)+wDlmTxt+wSepCampi+
040100071127     C                             wDlmTxt+%trim(§VAIGC2)+wDlmTxt+wSepCampi+
040200071127     C                             wDlmTxt+%trim(§VAITC1)+wDlmTxt+wSepCampi+
040300071127     C                             wDlmTxt+%trim(§VAITC2)+wDlmTxt+wSepCampi+
040400071127     C                             wDlmTxt+%trim(§VAINOM)+wDlmTxt+wSepCampi+
040500071127     C                             wDlmTxt+%trim(§VAITEL)+wDlmTxt+wSepCampi+
040600071128     C                             wDlmTxt+%trim(§VAIMOT)+wDlmTxt+wSepCampi+
040700071128     C                             wDlmTxt+%trim(TASRSD)+wDlmTxt+wSepCampi+
040800071128     C                             wDlmTxt+%trim(TASIND)+wDlmTxt+wSepCampi+
040900071128     C                             wDlmTxt+%trim(TASLOD)+wDlmTxt+wSepCampi+
041000071128     C                             wDlmTxt+%trim(TASCAD)+wDlmTxt+wSepCampi+
041100071128     C                             wDlmTxt+%trim(TASPRD)+wDlmTxt+wSepCampi+
041200071128     C                             wDlmTxt+%trim(TASNZD)+wDlmTxt+wSepCampi
041300930409     C*
041400071205     C                   EXSR      insertMAC
041500071128     C                   ENDIF
041600071128     C*
041700071128     C* Gestione x tipo record BAN
041800080219     C                   IF        VAITDT = 'BAN' AND *IN81
041900071128     C                   EVAL      DVAIBAN= VAIDTI
042000071128     C                   EVAL      VAWDTA=
042100071128     C                                  wDlmTxt+%trim(VAIATB)+wDlmTxt+wSepCampi+
042200071128     C                                  %editc(VAICCM:'Q')+wSepCampi+
042300071128     C                                  %editc(VAIAAS:'Q')+wSepCampi+
042400071128     C                                  %editc(VAILNP:'Q')+wSepCampi+
042500071128     C                                  %editc(VAINRS:'Q')+wSepCampi+
042600071128     C                                  %editc(VAINSP:'Q')+wSepCampi+
042700071128     C                                  %editc(VAIRMN:'Q')+wSepCampi+
042800071128     C                                  wDlmTxt+%trim(VAIRMA)+wDlmTxt+wSepCampi+
042900071128     C                                  wDlmTxt+%trim(VAIRMX)+wDlmTxt+wSepCampi+
043000071128     C                                  %editw(VAIDAC:'    /  /  ')+wSepCampi+
043100071128     C                                  %editw(VAIORC:'  :  :  ')+wSepCampi+
043200071128     C                                  wDlmTxt+%trim(VAITDT)+wDlmTxt+wSepCampi+
043300071128     C                                  %editc(§§VAITBA:'Q')+wSepCampi+
043400071128     C                                  %editc(§§VAINBA:'Q')+wSepCampi+
043500071128     C                                  %editc(§§VAITB2:'Q')+wSepCampi+
043600071128     C                                  %editc(§§VAINB2:'Q')+wSepCampi+
043700071128     C                                  %editc(§§VAIRB1:'Q')+wSepCampi+
043800071128     C                                  %editc(§§VAIRB2:'Q')+wSepCampi+
043900071128     C                             wDlmTxt+%trim(§§VAINOM)+wDlmTxt+wSepCampi+
044000071128     C                             wDlmTxt+%trim(§§VAIMOT)+wDlmTxt+wSepCampi+
044100071128     C                             wDlmTxt+%trim(TASRSD)+wDlmTxt+wSepCampi+
044200071128     C                             wDlmTxt+%trim(TASIND)+wDlmTxt+wSepCampi+
044300071128     C                             wDlmTxt+%trim(TASLOD)+wDlmTxt+wSepCampi+
044400071128     C                             wDlmTxt+%trim(TASCAD)+wDlmTxt+wSepCampi+
044500071128     C                             wDlmTxt+%trim(TASPRD)+wDlmTxt+wSepCampi+
044600071128     C                             wDlmTxt+%trim(TASNZD)+wDlmTxt+wSepCampi
044700071128     C*
044800071205     C                   EXSR      insertBAN
044900071128     C                   ENDIF
045000071128     C*
045100090323     C                   DELETE    TIVGD000
045200991027     C*
045300090323     C                   READ      TIVGDTMP
045400030325     C                   ENDDO
045500991027     C*
045600030325     C                   EVAL      wrkesito = '0'
045700991027     C*
045800910830     C                   ENDSR
045900071128
046000071128
046100071128
046200071128     C     CREALOG       BEGSR
046300071128     C*
046400071128     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
046500071128     C                   CLEAR                   TIVLT000
046600071128     C                   EVAL      vltISV = prmisv
046700071128     C                   EVAL      vltKSC = '0' + prmksc
046800071128     C                   EVAL      vltKSU = '0' + prmksc
046900071205     C                   EVAL      vltMBD = wMbrPrgBAN
047000071128     C                   EVAL      vltSND = 'D'
047100071128     C                   EVAL      vltFLS = 'FIVAI00S'
047200071128     C*
047300071128     C                   time                    wrkoggiiso
047400071128     C                   extrct    wrkoggiiso:*m wrkmese
047500071128     C                   extrct    wrkoggiiso:*d wrkday
047600071128     C                   eval      vltMBS =
047700071128     C                             mc(wrkmese)
047800071128     C                             + wrkdaychr
047900071128     C                             + %subst(vltKSU:2:7)
048000071128     C*
048100071128     C                   EVAL      vltDAT = DATCOR
048200071128     C                   EVAL      vltORA = ORACOR
048300071128     C                   EVAL      vltTIA = 'F'
048400071128     C                   EVAL      vltFL2 = '01'
048500071128     C                   EVAL      vltSTS = '0'
048600071205     C                   EVAL      vltPRG = wMbrPrgBAN
048700071128     C*
048800071128     C                   EVAL      vltTIP = 'VI'
048900071128     C                   EVAL      vltFLD = 'TIVAWWWT'
049000071128     C*
049100071128     C* Quindi scarico il buffer e creo il record d log x il download
049200071128     C                   WRITE     TIVLT000
049300071128     C*
049400071128     C                   ENDSR
049500071127
049600071127
049700991027
049800991027      /TITLE Operazioni iniziali.
049900991027     C     *inzsr        BEGSR
050000991027     C*
050100991027     C     *ENTRY        PLIST
050200991027     C                   parm                    prmppt
050300991027     C     wrkesito      parm      wrkesito      prmesito
050400071128     C*
050500071128     C* Scompongo subito i parametri ricevuti
050600071128     C                   EVAL      prmisv = %subst(prmppt:4:2)
050700071128     C                   EVAL      prmksc = %subst(prmppt:6:7)
050800080219     C                   EVAL      prmout = %subst(prmppt:13:1)
050900080219     C*
051000080219     C* Verifico i tipi output richiesti: M=MAC B=BAN E=Entrambi
051100080219     C                   SETOFF                                       8081
051200080219     C                   IF        prmout = 'M'
051300080219     C                   SETON                                        80
051400080219     C                   ENDIF
051500080219     C                   IF        prmout = 'B'
051600080219     C                   SETON                                        81
051700080219     C                   ENDIF
051800080219     C                   IF        prmout = 'E'
051900080219     C                   SETON                                        8081
052000080219     C                   ENDIF
052100071128     C*
052200071128     C* Definizioni chiavi
052300071128     C* - TITAS30C - Parziale
052400071128     C     KEYtas30_P    KLIST
052500071128     C                   KFLD                    vaiAAS
052600071128     C                   KFLD                    vaiLNP
052700071128     C                   KFLD                    vaiNRS
052800071128     C                   KFLD                    vaiNSP
052900071128     C*
053000071128     C* IMPOSTA LA DATA CORRENTE
053100071128     C                   TIME                    N14              14 0
053200071128     C                   MOVE      N14           N8                8 0
053300071128     C                   MOVEL     N14           ORACOR            6 0
053400071128     C                   Z-ADD     N8            G08DAT
053500071128     C                   Z-ADD     *ZEROS        G08INV
053600071128     C                   MOVEL     '0'           G08ERR
053700071128     C                   CALL      'XSRDA8'
053800071128     C                   PARM                    WLBDA8
053900071128     C                   Z-ADD     G08INV        DATCOR            8 0
054000991027     C*
054100991027     C                   ENDSR
054200071205** ctdata mc
054300071128ABCDEFGHILMN
054400071205** CMD - COMANDI CL
054500071205DLTOVR FILE(TIVAWWWT) LVL(*JOB)
054600071205
054700071205
054800071205
054900071205
