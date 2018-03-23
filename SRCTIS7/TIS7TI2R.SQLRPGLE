000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200140609     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TRUL':'UBBNDDIR')
000300991027
000400071128     FTITAS30C  IF   E           K DISK
000500090323     FTIVGDTMP  uF   E             DISK
000600071128     Ftis7prgf  uf   E             DISK    rename(tis7prgf:tis7prg0)
000700071128     F                                     prefix(f_)
000800071128     FTIVLT01L  UF A E           K DISK
000900071204
001000100114     D/COPY GAITRASRC/SRCPROTOPR,UBUTISQL
001100071204     D/COPY GAITRASRC/SRCPROTOPR,TRULAPICI
001200071127
001300071128     D DVAIMAC       E DS                  inz
001400071128     D DVAIBAN       E DS                  inz prefix(§)
001500071128     D TITAS00FDS    E DS                  extname(TITAS00F) inz
001600071205     D TIVAWWWT      E DS                  inz
001700991027
001800991027     D prmppt          s             50
001900991027     D prmesito        s              1
002000991027     D wrkesito        s                   like(prmesito)
002100090323     D fivaids       e ds                  extname(fivai00t)
002200071128
002300071205     D wMbrPrgMAC      s             10    inz
002400071205     D wMbrPrgBAN      s             10    inz
002500071128     D dwlprg          s             10    inz(*all'0')
002600071128     D wrkprg          s              8  0 inz(*zeros)
002700071128     D wrkoggiiso      s               d
002800071128     D wrkmese         s              2  0
002900071128     D prmisv          s              2
003000071128     D prmksc          s              7
003100080219     D prmout          s              1
003200071128     D mc              s              1    dim(12) ctdata perrcd(12)
003300071205     D cmd             s             80    dim(5)  ctdata perrcd(1)
003400071205     D depcmd          s            150    INZ(*blanks)
003500071205
003600071128     D                 ds                  INZ
003700071128     D wrkday                  1      2s 0
003800071128     D wrkdaychr                      2    overlay(wrkday)
003900071204
004000090327     D WWWdta          s          65535A   inz
004100071204     D wStringaSQl     s           1000A   varying
004200060720     D wDlmTxt         s              5    varying inz('')
004300071130
004400071128
004500071128     D*---------------------
004600071128     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700071128     D*---------------------
004800071128     D WLBDA8          DS
004900071128     D  G08DAT                 1      8  0
005000071128     D  G08INV                 9     16  0
005100071128     D  G08ERR                17     17
005200071128     D  G08TGI                18     22  0
005300071128
005400991027
005500071205     C*
005600071205     C                   setoff                                       70
005700071205     C*
005800071205     C  N70              EXSR      preOPE
005900080219     C*
006000080219     C  N70
006100080219     CAN 80              EXSR      opeFILEMAC
006200080219     C*
006300080219     C  N70
006400080219     CAN 81              EXSR      calPROG
006500080219     C  N70
006600080219     CAN 81              EXSR      opeFILEBAN
006700080219     C*
006800080219     C  N70
006900080219     CAN 80              EXSR      chkRECFILMAC
007000080219     C*
007100080219     C  N70
007200080219     CAN 81              EXSR      chkRECFILBAN
007300080219     C*
007400071205     C  N70              EXSR      traduci
007500080219     C*
007600080219     C  N70
007700080219     CAN 81              EXSR      creaLOG
007800080219     C*
007900071204     C  N70              EXSR      cloFILE
008000071128     C*
008100921023     C                   SETON                                        LR
008200071205
008300071205
008400071205
008500071205      /TITLE Operazioni preliminari
008600071205     C     preOPE        begsr
008700071205     C*
008800071205     C                   MOVEL(p)  CMD(1)        depcmd
008900071205     C*
009000071205     C                   Z-ADD     150           LENGH            15 5
009100071205     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
009200071205     C                   PARM                    depcmd
009300071205     C                   PARM                    LENGH
009400071205     C*
009500071205     C                   IF        %error
009600071205     C                   SETON                                        70
009700071205     C                   ENDIF
009800071205     C*
009900071205     C                   endsr
010000071204
010100071204
010200071204
010300071204      /TITLE Valorizzazione Progressivo Applicazione
010400071204     C     calprog       begsr
010500071204     C*
010600170324     C     *start        setll     tis7prgf
010700071204     C                   read(e)   tis7prgf
010800071204     C                   if        not %error
010900071204     C                   eval      dwlprg = f_tis7prgf
011000071204     C*
011100071204     C                   move(p)   dwlprg        wrkprg
011200071204     C                   add       1             wrkprg
011300071204     C                   move(p)   wrkprg        dwlprg
011400071204     C                   movel     prmisv        dwlprg
011500071204     C*
011600071204     C                   eval      f_tis7prgf = dwlprg
011700071204     C                   update    tis7prg0
011800071204     C                   endif
011900071204     C*
012000071204     C                   endsr
012100071204
012200071204
012300071128
012400071128
012500071205     C     opeFILEMAC    BEGSR
012600071128     C*
012700071205     C                   eval      wMbrPrgMAC = 'M0' + prmksc
012800071130     C*
012900071128     C                   call      'TIS7TIC1'
013000071204     C                   parm      *blanks       wNomeLib         10
013100071128     C                   parm      'TIVAWWWT'    wNomeFile        10
013200071205     C                   parm      wMbrPrgMAC    wNomeMembro      10
013300071128     C                   parm      'TIS7TIR1'    wTesto           30
013400071128     C                   parm      *blanks       wEsito            1
013500071130     C*
013600071130     C                   if        wEsito = '1'
013700071130     C*
013800071205     C                   if        SQL_CreaAlias(wMbrPrgMAC
013900071204     C                                          :'QTEMP'
014000071204     C                                          :'TIVAWWWT'
014100071204     C                                          :*blanks
014200071205     C                                          :wMbrPrgMAC)
014300071205     C                                          = 0
014400071204     C                   else
014500071203     C                   seton                                        70
014600071203     C                   endif
014700071130     C*
014800071204     C                   else
014900071204     C                   seton                                        70
015000071128     C                   endif
015100071128     C*
015200071128     C                   ENDSR
015300071205
015400071205
015500071205
015600071205
015700071205     C     opeFILEBAN    BEGSR
015800071205     C*
015900071205     C                   eval      wMbrPrgBAN = dwlprg
016000071205     C*
016100071205     C                   call      'TIS7TIC1'
016200071205     C                   parm      *blanks       wNomeLib         10
016300071205     C                   parm      'TIVAWWWT'    wNomeFile        10
016400071205     C                   parm      wMbrPrgBAN    wNomeMembro      10
016500071205     C                   parm      'TIS7TIR1'    wTesto           30
016600071205     C                   parm      *blanks       wEsito            1
016700071205     C*
016800071205     C                   if        wEsito = '1'
016900071205     C*
017000071205     C                   if        SQL_CreaAlias(wMbrPrgBAN
017100071205     C                                          :'QTEMP'
017200071205     C                                          :'TIVAWWWT'
017300071205     C                                          :*blanks
017400071205     C                                          :wMbrPrgBAN)
017500071205     C                                          = 0
017600071205     C                   else
017700071205     C                   seton                                        70
017800071205     C                   endif
017900071205     C*
018000071205     C                   else
018100071205     C                   seton                                        70
018200071205     C                   endif
018300071205     C*
018400071205     C                   ENDSR
018500071204
018600071204
018700071204
018800071204
018900071204     C     cloFILE       BEGSR
019000071204     C*
019100080219     C   80              callp     SQL_DropAlias(wMbrPrgMAC:'QTEMP')
019200080219     C   81              callp     SQL_DropAlias(wMbrPrgBAN:'QTEMP')
019300071204     C*
019400071204     C                   ENDSR
019500071205
019600071205
019700071205
019800071205
019900071205     C     chkRECFILMAC  BEGSR
020000071205     C*
020100071205     C                   call      'TRUL51C'
020200071205     C                   parm      '*LIBL'       wUL51_Lib        10
020300071205     C                   parm      'TIVAWWWT'    wUL51_Fil        10
020400071205     C                   parm      wMbrPrgMAC    wUL51_Mbr        10
020500071205     C                   parm      *zeros        wUL51_Nrr        10 0
020600071205     C*
020700071205     C                   if        wUL51_Nrr = *zeros
020800071205     C                   seton                                        55
020900071205     C                   endif
021000071205     C*
021100071205     C                   ENDSR
021200071204
021300071204
021400071203
021500071203
021600071204     C     chkRECFILBAN  BEGSR
021700071203     C*
021800071203     C                   call      'TRUL51C'
021900071204     C                   parm      '*LIBL'       wUL51_Lib        10
022000071204     C                   parm      'TIVAWWWT'    wUL51_Fil        10
022100071205     C                   parm      wMbrPrgBAN    wUL51_Mbr        10
022200071204     C                   parm      *zeros        wUL51_Nrr        10 0
022300071203     C*
022400071203     C                   if        wUL51_Nrr = *zeros
022500071203     C                   seton                                        56
022600071203     C                   endif
022700071203     C*
022800071203     C                   ENDSR
022900071204
023000071204
023100071204
023200071204
023300071205     C     insertMAC     BEGSR
023400071204     C*
023500071204     C* Costruisco la stringa SQL parametrica
023600090327     c                   eval      WWWdta = VAWdta
023700071205     C                   eval      wStringaSQL = 'insert into ' + wMbrPrgMAC +
023800071205     C                                           ' values('''                +
023900090327     C                              RaddoppiareApici(wwwDTA : %SIZE(VAWDTA)) +
024000071205     C                                           ''')'
024100071204     C                   eval      %len(wStringaSQL) = %len(%trim(wStringaSQL))
024200071204     C*
024300071204     C/EXEC SQL
024400071204     C+ EXECUTE IMMEDIATE :wStringaSQL
024500071204     C/END-EXEC
024600071204     C*
024700071204     C                   ENDSR
024800071205
024900071205
025000071205
025100071205
025200071205     C     insertBAN     BEGSR
025300071205     C*
025400071205     C* Costruisco la stringa SQL parametrica
025500090327     c                   eval      WWWdta = VAWdta
025600071205     C                   eval      wStringaSQL = 'insert into ' + wMbrPrgBAN +
025700071205     C                                           ' values('''                +
025800090327     C                              RaddoppiareApici(wwwDTA : %SIZE(VAWDTA)) +
025900071205     C                                           ''')'
026000071205     C                   eval      %len(wStringaSQL) = %len(%trim(wStringaSQL))
026100071205     C*
026200071205     C/EXEC SQL
026300071205     C+ EXECUTE IMMEDIATE :wStringaSQL
026400071205     C/END-EXEC
026500071205     C*
026600071205     C                   ENDSR
026700071128
026800071128
026900071128
027000071128
027100991027     C     traduci       BEGSR
027200040206     C*
027300040206     C* Verifico se richiesto un particolare carattere d separatore campi
027400071127     C                   IF        %subst(prmppt:2:1) <> *blanks
027500071128     C                   EVAL      wSepCampi = %subst(prmppt:2:1)
027600040206     C                   ELSE
027700071127     C                   MOVEL     ','           wSepCampi         1
027800040206     C                   ENDIF
027900060720     C*
028000060720     C* Verifico se richiesto nessun delimitatore testo
028100060720     C*   => *blanks = standard = "
028200060720     C*   => N       = nessun delimitatore testo
028300060720     C                   EVAL      wDlmTxt = '"'
028400071127     C                   IF        %subst(prmppt:3:1) = 'N'
028500060720     C                   EVAL      wDlmTxt = *blanks
028600060720     C                   EVAL      wDlmTxt = %trim(wDlmTxt)
028700060720     C                   ENDIF
028800030325     C*
028900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
029000080219     C                   IF        *IN80
029100030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
029200071128     C                   EVAL      VAWDTA =
029300071128     C                             wDlmTxt+'VAIATB'+wDlmTxt+wSepCampi+
029400071128     C                             wDlmTxt+'VAICCM'+wDlmTxt+wSepCampi+
029500071128     C                             wDlmTxt+'VAIAAS'+wDlmTxt+wSepCampi+
029600071128     C                             wDlmTxt+'VAILNP'+wDlmTxt+wSepCampi+
029700071128     C                             wDlmTxt+'VAINRS'+wDlmTxt+wSepCampi+
029800071128     C                             wDlmTxt+'VAINSP'+wDlmTxt+wSepCampi+
029900071128     C                             wDlmTxt+'VAIRMN'+wDlmTxt+wSepCampi+
030000071128     C                             wDlmTxt+'VAIRMA'+wDlmTxt+wSepCampi+
030100071128     C                             wDlmTxt+'VAIRMX'+wDlmTxt+wSepCampi+
030200071128     C                             wDlmTxt+'VAIDAC'+wDlmTxt+wSepCampi+
030300071128     C                             wDlmTxt+'VAIORC'+wDlmTxt+wSepCampi+
030400071128     C                             wDlmTxt+'VAITDT'+wDlmTxt+wSepCampi+
030500071128     C                             wDlmTxt+'CAUS_VAR'+wDlmTxt+wSepCampi+
030600071128     C                             wDlmTxt+'FIL_VAR'+wDlmTxt+wSepCampi+
030700071128     C                             wDlmTxt+'TIPO_DATA'+wDlmTxt+wSepCampi+
030800071128     C                             wDlmTxt+'DATA_CONS_RICH'+wDlmTxt+wSepCampi+
030900071128     C                             wDlmTxt+'ORA_CONS_RICH'+wDlmTxt+wSepCampi+
031000071128     C                             wDlmTxt+'1_GIORNO_CHIUS'+wDlmTxt+wSepCampi+
031100071128     C                             wDlmTxt+'2_GIORNO_CHIUS'+wDlmTxt+wSepCampi+
031200071128     C                             wDlmTxt+'1_CONS_PART'+wDlmTxt+wSepCampi+
031300071128     C                             wDlmTxt+'2_CONS_PART'+wDlmTxt+wSepCampi+
031400071128     C                             wDlmTxt+'NOMINATIVO_VAR'+wDlmTxt+wSepCampi+
031500071128     C                             wDlmTxt+'TELEFONO_DEST'+wDlmTxt+wSepCampi+
031600071128     C                             wDlmTxt+'MOTIVO_VAR'+wDlmTxt+wSepCampi+
031700071128     C                             wDlmTxt+'RAG_SOC_DEST'+wDlmTxt+wSepCampi+
031800071128     C                             wDlmTxt+'INDIRIZZO_DEST'+wDlmTxt+wSepCampi+
031900071128     C                             wDlmTxt+'LOCALITA_DEST'+wDlmTxt+wSepCampi+
032000071128     C                             wDlmTxt+'CAP_DEST'+wDlmTxt+wSepCampi+
032100071128     C                             wDlmTxt+'PROVINCIA_DEST'+wDlmTxt+wSepCampi+
032200071128     C                             wDlmTxt+'NAZIONE_DEST'+wDlmTxt+wSepCampi
032300030325     C*
032400071205     C                   EXSR      insertMAC
032500030325     C                   ENDIF
032600080219     C                   ENDIF
032700071128     C*
032800071128     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
032900080219     C                   IF        *IN81
033000071128     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN56 = *ON
033100071128     C                   EVAL      VAWDTA =
033200071128     C                             wDlmTxt+'VAIATB'+wDlmTxt+wSepCampi+
033300071128     C                             wDlmTxt+'VAICCM'+wDlmTxt+wSepCampi+
033400071128     C                             wDlmTxt+'VAIAAS'+wDlmTxt+wSepCampi+
033500071128     C                             wDlmTxt+'VAILNP'+wDlmTxt+wSepCampi+
033600071128     C                             wDlmTxt+'VAINRS'+wDlmTxt+wSepCampi+
033700071128     C                             wDlmTxt+'VAINSP'+wDlmTxt+wSepCampi+
033800071128     C                             wDlmTxt+'VAIRMN'+wDlmTxt+wSepCampi+
033900071128     C                             wDlmTxt+'VAIRMA'+wDlmTxt+wSepCampi+
034000071128     C                             wDlmTxt+'VAIRMX'+wDlmTxt+wSepCampi+
034100071128     C                             wDlmTxt+'VAIDAC'+wDlmTxt+wSepCampi+
034200071128     C                             wDlmTxt+'VAIORC'+wDlmTxt+wSepCampi+
034300071128     C                             wDlmTxt+'VAITDT'+wDlmTxt+wSepCampi+
034400071128     C                             wDlmTxt+'1MO_TIPO_BANC'+wDlmTxt+wSepCampi+
034500071128     C                             wDlmTxt+'NUM_BANC_1MO'+wDlmTxt+wSepCampi+
034600071128     C                             wDlmTxt+'2ND_TIPO_BANC'+wDlmTxt+wSepCampi+
034700071128     C                             wDlmTxt+'NUM_BANC_2ND'+wDlmTxt+wSepCampi+
034800071128     C                             wDlmTxt+'BANC_1MO_NON_RIT'+wDlmTxt+wSepCampi+
034900071128     C                             wDlmTxt+'BANC_2ND_NON_RIT'+wDlmTxt+wSepCampi+
035000071128     C                             wDlmTxt+'RITIRO_NEGATO_DA'+wDlmTxt+wSepCampi+
035100071128     C                             wDlmTxt+'MOTIVO_RIFIUTO'+wDlmTxt+wSepCampi+
035200071128     C                             wDlmTxt+'RAG_SOC_DEST'+wDlmTxt+wSepCampi+
035300071128     C                             wDlmTxt+'INDIRIZZO_DEST'+wDlmTxt+wSepCampi+
035400071128     C                             wDlmTxt+'LOCALITA_DEST'+wDlmTxt+wSepCampi+
035500071128     C                             wDlmTxt+'CAP_DEST'+wDlmTxt+wSepCampi+
035600071128     C                             wDlmTxt+'PROVINCIA_DEST'+wDlmTxt+wSepCampi+
035700071128     C                             wDlmTxt+'NAZIONE_DEST'+wDlmTxt+wSepCampi
035800071128     C*
035900071205     C                   EXSR      insertBAN
036000071128     C                   ENDIF
036100080219     C                   ENDIF
036200991027     C*
036300071128     C* Quindi scorro tutto il file d input
036400090323     C                   READ      TIVGDTMP
036500090323     C                   DOW       not %eof(TIVGDTMP)
036600090323     C                   eval      fivaids = vgdDTA
036700071128     C*
036800071128     C* Aggancio subito la bolla
036900071128     C     KEYtas30_P    CHAIN     TITAS30C
037000071128     C                   IF        %found(TITAS30C)
037100071128     C                   ELSE
037200071128     C                   EVAL      TITAS00FDS = *all'*'
037300071128     C                   ENDIF
037400071128     C*
037500071128     C* Gestione x tipo record MAC
037600080219     C                   IF        VAITDT = 'MAC' AND *IN80
037700071127     C                   EVAL      DVAIMAC= VAIDTI
037800071127     C                   EVAL      VAWDTA=
037900071127     C                                  wDlmTxt+%trim(VAIATB)+wDlmTxt+wSepCampi+
038000071127     C                                  %editc(VAICCM:'Q')+wSepCampi+
038100071127     C                                  %editc(VAIAAS:'Q')+wSepCampi+
038200071127     C                                  %editc(VAILNP:'Q')+wSepCampi+
038300071127     C                                  %editc(VAINRS:'Q')+wSepCampi+
038400071127     C                                  %editc(VAINSP:'Q')+wSepCampi+
038500071127     C                                  %editc(VAIRMN:'Q')+wSepCampi+
038600071127     C                                  wDlmTxt+%trim(VAIRMA)+wDlmTxt+wSepCampi+
038700071127     C                                  wDlmTxt+%trim(VAIRMX)+wDlmTxt+wSepCampi+
038800071127     C                                  %editw(VAIDAC:'    /  /  ')+wSepCampi+
038900071127     C                                  %editw(VAIORC:'  :  :  ')+wSepCampi+
039000071127     C                                  wDlmTxt+%trim(VAITDT)+wDlmTxt+wSepCampi+
039100071127     C                             wDlmTxt+%trim(§VAICVB)+wDlmTxt+wSepCampi+
039200071127     C                                  %editc(§VAIPOU:'Q')+wSepCampi+
039300071127     C                             wDlmTxt+%trim(§VAITCR)+wDlmTxt+wSepCampi+
039400071127     C                             %editw(§VAIDCR:'    /  /  ')+wSepCampi+
039500071127     C                             %editw(§VAIHCR:'  :  ')+wSepCampi+
039600071127     C                             wDlmTxt+%trim(§VAIGC1)+wDlmTxt+wSepCampi+
039700071127     C                             wDlmTxt+%trim(§VAIGC2)+wDlmTxt+wSepCampi+
039800071127     C                             wDlmTxt+%trim(§VAITC1)+wDlmTxt+wSepCampi+
039900071127     C                             wDlmTxt+%trim(§VAITC2)+wDlmTxt+wSepCampi+
040000071127     C                             wDlmTxt+%trim(§VAINOM)+wDlmTxt+wSepCampi+
040100071127     C                             wDlmTxt+%trim(§VAITEL)+wDlmTxt+wSepCampi+
040200071128     C                             wDlmTxt+%trim(§VAIMOT)+wDlmTxt+wSepCampi+
040300071128     C                             wDlmTxt+%trim(TASRSD)+wDlmTxt+wSepCampi+
040400071128     C                             wDlmTxt+%trim(TASIND)+wDlmTxt+wSepCampi+
040500071128     C                             wDlmTxt+%trim(TASLOD)+wDlmTxt+wSepCampi+
040600071128     C                             wDlmTxt+%trim(TASCAD)+wDlmTxt+wSepCampi+
040700071128     C                             wDlmTxt+%trim(TASPRD)+wDlmTxt+wSepCampi+
040800071128     C                             wDlmTxt+%trim(TASNZD)+wDlmTxt+wSepCampi
040900930409     C*
041000071205     C                   EXSR      insertMAC
041100071128     C                   ENDIF
041200071128     C*
041300071128     C* Gestione x tipo record BAN
041400080219     C                   IF        VAITDT = 'BAN' AND *IN81
041500071128     C                   EVAL      DVAIBAN= VAIDTI
041600071128     C                   EVAL      VAWDTA=
041700071128     C                                  wDlmTxt+%trim(VAIATB)+wDlmTxt+wSepCampi+
041800071128     C                                  %editc(VAICCM:'Q')+wSepCampi+
041900071128     C                                  %editc(VAIAAS:'Q')+wSepCampi+
042000071128     C                                  %editc(VAILNP:'Q')+wSepCampi+
042100071128     C                                  %editc(VAINRS:'Q')+wSepCampi+
042200071128     C                                  %editc(VAINSP:'Q')+wSepCampi+
042300071128     C                                  %editc(VAIRMN:'Q')+wSepCampi+
042400071128     C                                  wDlmTxt+%trim(VAIRMA)+wDlmTxt+wSepCampi+
042500071128     C                                  wDlmTxt+%trim(VAIRMX)+wDlmTxt+wSepCampi+
042600071128     C                                  %editw(VAIDAC:'    /  /  ')+wSepCampi+
042700071128     C                                  %editw(VAIORC:'  :  :  ')+wSepCampi+
042800071128     C                                  wDlmTxt+%trim(VAITDT)+wDlmTxt+wSepCampi+
042900071128     C                                  %editc(§§VAITBA:'Q')+wSepCampi+
043000071128     C                                  %editc(§§VAINBA:'Q')+wSepCampi+
043100071128     C                                  %editc(§§VAITB2:'Q')+wSepCampi+
043200071128     C                                  %editc(§§VAINB2:'Q')+wSepCampi+
043300071128     C                                  %editc(§§VAIRB1:'Q')+wSepCampi+
043400071128     C                                  %editc(§§VAIRB2:'Q')+wSepCampi+
043500071128     C                             wDlmTxt+%trim(§§VAINOM)+wDlmTxt+wSepCampi+
043600071128     C                             wDlmTxt+%trim(§§VAIMOT)+wDlmTxt+wSepCampi+
043700071128     C                             wDlmTxt+%trim(TASRSD)+wDlmTxt+wSepCampi+
043800071128     C                             wDlmTxt+%trim(TASIND)+wDlmTxt+wSepCampi+
043900071128     C                             wDlmTxt+%trim(TASLOD)+wDlmTxt+wSepCampi+
044000071128     C                             wDlmTxt+%trim(TASCAD)+wDlmTxt+wSepCampi+
044100071128     C                             wDlmTxt+%trim(TASPRD)+wDlmTxt+wSepCampi+
044200071128     C                             wDlmTxt+%trim(TASNZD)+wDlmTxt+wSepCampi
044300071128     C*
044400071205     C                   EXSR      insertBAN
044500071128     C                   ENDIF
044600071128     C*
044700090323     C                   DELETE    TIVGD000
044800991027     C*
044900090323     C                   READ      TIVGDTMP
045000030325     C                   ENDDO
045100991027     C*
045200030325     C                   EVAL      wrkesito = '0'
045300991027     C*
045400910830     C                   ENDSR
045500071128
045600071128
045700071128
045800071128     C     CREALOG       BEGSR
045900071128     C*
046000071128     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
046100071128     C                   CLEAR                   TIVLT000
046200071128     C                   EVAL      vltISV = prmisv
046300071128     C                   EVAL      vltKSC = '0' + prmksc
046400071128     C                   EVAL      vltKSU = '0' + prmksc
046500071205     C                   EVAL      vltMBD = wMbrPrgBAN
046600071128     C                   EVAL      vltSND = 'D'
046700071128     C                   EVAL      vltFLS = 'FIVAI00S'
046800071128     C*
046900071128     C                   time                    wrkoggiiso
047000071128     C                   extrct    wrkoggiiso:*m wrkmese
047100071128     C                   extrct    wrkoggiiso:*d wrkday
047200071128     C                   eval      vltMBS =
047300071128     C                             mc(wrkmese)
047400071128     C                             + wrkdaychr
047500071128     C                             + %subst(vltKSU:2:7)
047600071128     C*
047700071128     C                   EVAL      vltDAT = DATCOR
047800071128     C                   EVAL      vltORA = ORACOR
047900071128     C                   EVAL      vltTIA = 'F'
048000071128     C                   EVAL      vltFL2 = '01'
048100071128     C                   EVAL      vltSTS = '0'
048200071205     C                   EVAL      vltPRG = wMbrPrgBAN
048300071128     C*
048400071128     C                   EVAL      vltTIP = 'VI'
048500071128     C                   EVAL      vltFLD = 'TIVAWWWT'
048600071128     C*
048700071128     C* Quindi scarico il buffer e creo il record d log x il download
048800071128     C                   WRITE     TIVLT000
048900071128     C*
049000071128     C                   ENDSR
049100071127
049200071127
049300991027
049400991027      /TITLE Operazioni iniziali.
049500991027     C     *inzsr        BEGSR
049600991027     C*
049700991027     C     *ENTRY        PLIST
049800991027     C                   parm                    prmppt
049900991027     C     wrkesito      parm      wrkesito      prmesito
050000071128     C*
050100071128     C* Scompongo subito i parametri ricevuti
050200071128     C                   EVAL      prmisv = %subst(prmppt:4:2)
050300071128     C                   EVAL      prmksc = %subst(prmppt:6:7)
050400080219     C                   EVAL      prmout = %subst(prmppt:13:1)
050500080219     C*
050600080219     C* Verifico i tipi output richiesti: M=MAC B=BAN E=Entrambi
050700080219     C                   SETOFF                                       8081
050800080219     C                   IF        prmout = 'M'
050900080219     C                   SETON                                        80
051000080219     C                   ENDIF
051100080219     C                   IF        prmout = 'B'
051200080219     C                   SETON                                        81
051300080219     C                   ENDIF
051400080219     C                   IF        prmout = 'E'
051500080219     C                   SETON                                        8081
051600080219     C                   ENDIF
051700071128     C*
051800071128     C* Definizioni chiavi
051900071128     C* - TITAS30C - Parziale
052000071128     C     KEYtas30_P    KLIST
052100071128     C                   KFLD                    vaiAAS
052200071128     C                   KFLD                    vaiLNP
052300071128     C                   KFLD                    vaiNRS
052400071128     C                   KFLD                    vaiNSP
052500071128     C*
052600071128     C* IMPOSTA LA DATA CORRENTE
052700071128     C                   TIME                    N14              14 0
052800071128     C                   MOVE      N14           N8                8 0
052900071128     C                   MOVEL     N14           ORACOR            6 0
053000071128     C                   Z-ADD     N8            G08DAT
053100071128     C                   Z-ADD     *ZEROS        G08INV
053200071128     C                   MOVEL     '0'           G08ERR
053300071128     C                   CALL      'XSRDA8'
053400071128     C                   PARM                    WLBDA8
053500071128     C                   Z-ADD     G08INV        DATCOR            8 0
053600991027     C*
053700991027     C                   ENDSR
053800071205** ctdata mc
053900071128ABCDEFGHILMN
054000071205** CMD - COMANDI CL
054100071205DLTOVR FILE(TIVAWWWT) LVL(*JOB)
054200071205
054300071205
054400071205
054500071205
