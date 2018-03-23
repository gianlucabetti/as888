000100161205     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000
000400170119     Ftivgd09l  uf   e           k disk    commit
000500161212     F                                     infds(infds_VGD)
000600161213     Ftivgdtmp  o    e             disk    rename(tivgd000:tivgdT) prefix(t_)
000700000000     F                                     usropn
000800000000     Ftivtf01l  if   e           k disk
000900000000     Ftivir02l  if   e           k disk
001000161212
001100161212
001200161212     D*****
001300161212     D* INFDS
001400161212     D*****
001500161212     D  infds_VGD      ds                  qualified
001600161212     D   db_rrn              397    400i 0
001700000000
001800000000
001900160906     D*****
002000160906     D* Variabili di work
002100160906     D*****
002200170323     D  wSTO           s              1    inz('R')
002300170317     D  wID            s              1    inz('6')
002400161212     D  DVGD001      e ds                  inz
002500000000     D  dsVGD        e ds                  extname(tivgd00f)
002600000000     D  dsVGDtmp     e ds                  extname(tivgd00f) prefix(t_)
002700161212     D  wFileInput     s                   like(vtffid) inz(*blanks)
002800161212     D  wFileStor      s                   like(vtffis) inz(*blanks)
002900161212     D  wTipoServizio  s                   like(virfi1) inz(*blanks)
003000161212     D  wEsito         s              1    inz(*blanks)
003100170119     D  wMessErrore    s            500    inz(*blanks)
003200000000
003300160906
003400160906     D* DS di work per lettura recordset SQL (C1)
003500000000     D  dsSQLVGD       ds
003600170119     D   SQL_STO                           like(vgdsto)
003700170119     D   SQL_PRG                           like(vgdprg)
003800000000     D   SQL_TIP                           like(vgdtip)
003900000000     D   SQL_KSU                           like(vgdksu)
004000000000     D   SQL_TSC                           like(vgdtsc)
004100170315
004200170315
004300170315     D* Schiera di work per momorizzazione dati recordset SQL (C1)
004400170327     D  skVGD          ds                  likeds(dsSQLVGD) dim(500)
004500170327     D  jVGD           s              3S 0 inz
004600170327     D  sVGD           s              3S 0 inz
004700000000
004800000000
004900000000     C*-----------------------------------------------------------------------------
005000000000     C*
005100000000     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005200000000     C
005300000000     C/EXEC SQL
005400000000     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
005500000000     C/END-EXEC
005600000000     C*
005700160906     C* Monitoro l'intero flusso del *pgm
005800160906     C                   monitor
005900000000     C*
006000000000     C/EXEC SQL
006100000000     C+ DECLARE C1 CURSOR FOR
006200170119     C+ SELECT DISTINCT VGDSTO, VGDPRG, VGDTIP, VGDKSU, VGDTSC FROM TIVGD00F
006300170323     C+ WHERE VGDSTO = :wSTO AND
006400170119     C+       VGDPRG <> ' ' AND
006500170405     C+       VGDTSC <> '  '
006600000000     C+ FOR READ ONLY
006700000000     C/END-EXEC
006800000000     C
006900000000     C/EXEC SQL
007000000000     C+ OPEN C1
007100000000     C/END-EXEC
007200000000     C
007300000000     C/EXEC SQL
007400000000     C+ FETCH C1 INTO :dsSQLVGD
007500000000     C/END-EXEC
007600170315     C
007700170315     C*
007800170315     C* Inizializzo schiera di work per momorizzazione dati recordset SQL (C1)
007900170315     C                   clear                   skVGD
008000170315     C                   clear                   jVGD
008100170315     C*
008200170315     C* Schiera di work per momorizzazione dati recordset SQL (C1)
008300170315     C                   dow       sqlcod = *zeros AND
008400170329     C                             jVGD <  %elem(skVGD)
008500170327     C                   if        %subst(SQL_PRG:1:2) <> 'XX'
008600170315     C                   add       1             jVGD
008700170315     C                   eval      skVGD(jVGD) = dsSQLVGD
008800170327     C                   endif
008900170315     C*
009000170315     C/EXEC SQL
009100170315     C+ FETCH C1 INTO :dsSQLVGD
009200170315     C/END-EXEC
009300170315     C                   enddo
009400170315     C*
009500170315     C/EXEC SQL
009600170315     C+ CLOSE C1
009700170315     C/END-EXEC
009800170315     C
009900170315     C*
010000170315     C* Scorro tutti gli elementi della schiera di work
010100170315     C                   z-add     1             sVGD
010200170315     C                   dow       sVGD <= jVGD
010300170315     C                   eval      dsSQLVGD = skVGD(sVGD)
010400161213     C*
010500161213     C* Elimino il contenuto del file d wrk in QTEMP
010600161213     C
010700161213     C/EXEC SQL
010800161213     C+ DELETE FROM QTEMP/TIVGDTMP
010900170119     C+ WITH NC
011000161213     C/END-EXEC
011100161213     C
011200161213     C*
011300161213     C* Se errore esco e segnalo
011400161213     C                   if        sqlcod < *zeros
011500161213     C                   eval      prmesito = '2'
011600161213     C                   leave
011700161213     C                   else
011800000000     C*
011900000000     C* Apro il file d wrk
012000000000     C                   if        not %open(tivgdtmp)
012100000000     C                   open      tivgdtmp
012200000000     C                   endif
012300000000     C*
012400000000     C* Leggo la vista logica del file generico download x ogni chiave d rottura traduzione
012500090518     C* ovvero TIP/KSU/TSC/PRG
012600170119     C     KEYvgd09_c    setll     tivgd09l
012700170119     C                   if        %equal(tivgd09l)
012800000000     C*
012900000000     C* 1° lettura file fuori ciclo
013000170119     C     KEYvgd09_c    reade     tivgd09l
013100000000     C*
013200160906     C* Leggo tutta la vista logica per medesima chiave
013300170119     C                   dow       not %eof(tivgd09l)
013400000000     C*
013500000000     C* Gestione ENDJOB.
013600000000     C                   shtdn                                        60
013700160906     C                   if        not *in60
013800161212     C*
013900161212     C* Memorizzo il relative record number "originale"
014000161212     C                   eval      DVGD001 = vgdATTRIB
014100161212     C                   eval      §VGD001RRN = infds_VGD.db_rrn
014200170317     C                   eval      §VGD001FL1 = wID
014300161212     C                   eval      vgdATTRIB = DVGD001
014400161212     C*
014500000000     C* Passo i dati dal file "buono" al file d wrk in QTEMP
014600160906     C                   eval      dsVGDtmp = dsVGD
014700160906     C                   write     tivgdT
014800160906     C                   eval      vgdsto = '-'
014900000000     C                   update    tivgd000
015000000000     C*
015100000000     C* Gestisco lo shutdown
015200000000     C                   else
015300000000     C                   eval      prmesito = '2'
015400000000     C                   leave
015500000000     C                   endif
015600000000     C*
015700170119     C* Proseguo con la lettura del file VAS Generico Download (TIVGD)
015800170119     C     KEYvgd09_c    reade     tivgd09l
015900000000     C                   enddo
016000000000     C*
016100000000     C* Effettuo la traduzione
016200161213     C  N60              exsr      traduzione
016300000000     C                   endif
016400161213     C*
016500161213     C                   endif
016600000000     C*
016700000000     C* Verifico eventuale shutdown
016800161212     C   60              eval      prmesito = '2'
016900000000     C   60              leave
017000000000     C*
017100170315     C                   add       1             sVGD
017200000000     C                   enddo
017300160906     C*
017400160906     C* Gestisco eventuale errore
017500160906     C                   on-error
017600160906     C                   eval      prmesito = '2'
017700171003     C                   dump(A)
017800160906     C*
017900160906     C* Interrompo monitoraggio errori
018000160906     C                   endmon
018100000000     C*
018200000000     C                   seton                                        lr
018300000000     C*-----------------------------------------------------------------------------
018400000000
018500000000
018600000000
018700000000      /TITLE Traduzione/copy file
018800000000     C     traduzione    begsr
018900000000     C*
019000000000     C* Inizializzo variabili d wrk
019100000000     C                   eval      wEsito      = *blanks
019200000000     C                   eval      wMessErrore = *blanks
019300000000     C*
019400000000     C* Reperisco il file "da tradurre" legato al tipo file corrente
019500000000     C                   exsr      repvtf
019600161212     C*
019700161212     C* Se dati "anagrafici" reperiti => procedo
019800161212     C                   if        wEsito = *blanks
019900000000     C*
020000160906     C* Se fin qui tutto ok => proseguo con elaborazione
020100170405     C                   exsr      exeTsc
020200000000     C*
020300000000     C* In uscita traduzione/copy file effettuo la storicizzazione
020400000000     C                   eval      wEsito = Esito
020500161212     C*
020600161212     C                   endif
020700161212     C*
020800161212     C* Al termine storicizzo
020900000000     C                   exsr      storicizza
021000000000     C*
021100000000     C                   endsr
021200170405
021300170405
021400170405
021500170405      /TITLE Eseguo operazioni di traduzione rispetto al Tipo Scarico Dati
021600170405     C     exeTsc        begsr
021700170405     C*
021800170405     C* Verifico il tipo scarico dati...
021900170405     C                   select
022000170405     C* WW
022100170405     C                   when      vgdtsc = 'WW'
022200170405     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
022300170405     C  N77              eval      p_ope = 'WWI'
022400170405     C   77              eval      p_ope = 'WWN'
022500170405     C                   call      'TIS781C3'
022600170405     C                   parm      vgdksu        p_ksu             8
022700170405     C                   parm      vgdtip        p_tip             2
022800170405     C                   parm      vgdtsc        p_tsc             2
022900170405     C                   parm      vgdprg        p_prog           10
023000170405     C                   parm      wFileInput    p_filein         10
023100170405     C                   parm      wFileStor     p_filesto        10
023200170405     C                   parm                    p_mbrin          10
023300170405     C                   parm                    p_ope             3
023400170405     C                   parm      *blanks       p_out            10
023500170405     C                   parm      *blanks       p_nfp             2
023600170405     C                   parm      *blanks       p_attrib        256
023700170405     C                   parm      *blanks       Esito             1
023800170405     C* W2
023900170405     C                   when      vgdtsc = 'W2'
024000170405     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
024100170405     C* W2 CPYF
024200170405     C                   call      'TIS781C3'
024300170405     C                   parm                    p_ksu
024400170405     C                   parm                    p_tip
024500170405     C                   parm                    p_tsc
024600170405     C                   parm                    p_prog
024700170405     C                   parm      wFileInput    p_filein
024800170405     C                   parm      *blanks       p_filesto
024900170405     C                   parm                    p_mbrin
025000170405     C                   parm      'CFI'         p_ope
025100170405     C                   parm      *blanks       p_out
025200170405     C                   parm      *blanks       p_nfp
025300170405     C                   parm      *blanks       p_attrib
025400170405     C                   parm      *blanks       Esito
025500170405     C* W2 WW
025600170405     C  N77              eval      p_ope = 'WWI'
025700170405     C   77              eval      p_ope = 'WWN'
025800170405     C                   call      'TIS781C3'
025900170405     C                   parm      vgdksu        p_ksu
026000170405     C                   parm      vgdtip        p_tip
026100170405     C                   parm      vgdtsc        p_tsc
026200170405     C                   parm      vgdprg        p_prog
026300170405     C                   parm      wFileInput    p_filein
026400170405     C                   parm      wFileStor     p_filesto
026500170405     C                   parm                    p_mbrin
026600170405     C                   parm                    p_ope
026700170405     C                   parm      *blanks       p_out
026800170405     C                   parm      *blanks       p_nfp
026900170405     C                   parm      *blanks       p_attrib
027000170405     C                   parm      *blanks       Esito
027100170405     C*
027200170405     C                   other
027300170405     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
027400170405     C                   call      'TIS781C3'
027500170405     C                   parm                    p_ksu
027600170405     C                   parm                    p_tip
027700170405     C                   parm                    p_tsc
027800170405     C                   parm                    p_prog
027900170405     C                   parm      wFileInput    p_filein
028000170405     C                   parm      *blanks       p_filesto
028100170405     C                   parm                    p_mbrin
028200170405     C                   parm      'CFI'         p_ope
028300170405     C                   parm      *blanks       p_out
028400170405     C                   parm      *blanks       p_nfp
028500170405     C                   parm      *blanks       p_attrib
028600170405     C                   parm      *blanks       Esito
028700170405     C*
028800170405     C                   endsl
028900170405     C*
029000170405     C                   endsr
029100000000
029200000000
029300000000
029400000000      /TITLE Routine di reperimento file "da tradiurre" legata al tipo file corrente
029500000000     C*
029600000000     C     repvtf        begsr
029700000000     C*
029800000000     C                   eval      wFileInput    = *blanks
029900000000     C                   eval      wFileStor     = *blanks
030000170119     C                   eval      wTipoServizio = 'DG'
030100000000     C*
030200161212     C* Anagrafica Tipo-File
030300000000     C     vgdtip        chain     tivtf01l
030400000000     C                   if        %found(tivtf01l)
030500000000     C                   eval      wFileInput = VTFFID
030600000000     C                   eval      wFileStor  = VTFFIS
030700000000     C*
030800161212     C* Anagrafica Cliente/Tipo-File
030900000000     C     KEYvir02_p    chain     tivir02l
031000000000     C                   if        %found(tivir02l)
031100000000     C                   eval      wTipoServizio = VIRFI1
031200000000     C                   if        VIRFID <> *blanks
031300000000     C                   eval      wFileInput = VIRFID
031400000000     C                   endif
031500000000     C                   if        VIRFIS <> *blanks
031600000000     C                   eval      wFileStor  = VTFFIS
031700000000     C                   endif
031800000000     C                   endif
031900161212     C*
032000000000     C                   else
032100000000     C*
032200161212     C* Se tipo file corrente NON trovato in anagrafica => mando e-mail a cedalert@brt.it
032300000000     C* e fleggo tali record a stato storicizzazione (STO) = 2
032400161212     C                   eval      wEsito = '2'
032500000000     C                   eval      wMessErrore = 'VERIFICARE IL FILE DOWNLOAD '+
032600170119     C                             ' (TIVGD00F)'                       +':/N'+
032700000000     C                             ' '                                 +':/N'+
032800000000     C                             'RIFERIMENTI'                       +':/N'+
032900000000     C                             'Cliente: ' + vgdksu                +':/N'+
033000000000     C                             'Tipo file: ' + vgdtip              +':/N'+
033100161212     C                             'Progressivo: ' + vgdprg            +':/N'+
033200000000     C                             ' '                                 +':/N'+
033300000000     C                             'MESSAGGIO'                         +':/N'+
033400170119     C                             'ERRORE: Tipo File ERRATO'          +':/N'+
033500170119     C                             'SEVERITA: GRAVE'
033600000000     C                   exsr      sndeml
033700000000     C                   endif
033800000000     C*
033900000000     C                   setoff                                       77
034000000000     C                   if        %trim(wFileInput) = '*'
034100000000     C                   eval      wFileInput = 'TIVGDTMP'
034200000000     C                   seton                                        77
034300000000     C                   endif
034400000000     C                   if        %trim(wFileStor)  = '*'
034500000000     C                   eval      wFileStor  = 'TIVGDTMP'
034600000000     C                   seton                                        77
034700000000     C                   endif
034800000000     C*
034900000000     C                   endsr
035000000000
035100170119
035200000000
035300000000      /TITLE Storicizzazione post traduzione/copy file
035400000000     C     storicizza    begsr
035500161213     C*
035600161213     C* A questo punto la singola transazione è logicamente conclusa x cui sancisco il commit
035700161213     C                   commit(e)
035800000000     C*
035900000000     C* Effettuo alcune considerazioni sull'esito
036000000000     C                   if        wEsito = ' ' OR wEsito = '0'
036100000000     C                   eval      Esito  = 'S'
036200000000     C                   else
036300000000     C                   if        wEsito = 'Y'
036400000000     C                   eval      Esito  = *blanks
036500000000     C                   else
036600000000     C                   eval      Esito  = wEsito
036700000000     C                   endif
036800000000     C                   endif
036900000000     C
037000000000     C/EXEC SQL
037100161212     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
037200000000     C/END-EXEC
037300000000     C
037400000000     C/EXEC SQL
037500000000     C+ UPDATE TIVGD00F SET VGDSTO = :ESITO,
037600170119     C+ VGDCNT = VGDCNT + 1
037700170119     C+ WHERE VGDSTO = '-'     AND VGDPRG = :VGDPRG AND
037800170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
037900170119     C+       VGDTSC = :VGDTSC
038000000000     C/END-EXEC
038100161212     C
038200161212     C/EXEC SQL
038300161212     C+ COMMIT
038400161212     C/END-EXEC
038500000000     C
038600000000     C*
038700000000     C* Chiudo il file d wrk
038800000000     C                   if        %open(tivgdtmp)
038900000000     C                   close     tivgdtmp
039000000000     C                   endif
039100000000     C*
039200000000     C                   endsr
039300000000
039400000000
039500000000      /TITLE Compongo il testo e spedisco una e-m@ail
039600161212     C     sndeml        begsr
039700000000     C*
039800000000     C* Inizializzo variabili
039900000000     C                   movel     *blanks       wrkEml          253
040000000000     C                   movel     *blanks       wrkEmlMsg      5000
040100000000     C                   movel     *blanks       wrkEmlOgg        44
040200000000     C* Valorizzo i campi della e-m@ail
040300120312     C                   eval      wrkEml='cedalert@brt.it'
040400170119     C                   eval      wrkEmlOgg='VAS - DOWNLOAD - ERROR'
040500000000     C                   eval      wrkEmlMsg=wMessErrore
040600000000     C*
040700000000     C                   call(e)   'TIS701C'
040800000000     C                   parm                    wrkEml
040900000000     C                   parm                    wrkEmlOgg
041000000000     C                   parm                    wrkEmlMsg
041100000000     C*
041200000000     C                   endsr
041300000000
041400000000
041500000000
041600000000      /TITLE Routine di *init PGM
041700000000     C*
041800000000     C     *inzsr        begsr
041900000000     C*
042000000000     C* Passaggio parametri
042100000000     C     *entry        plist
042200000000     C                   parm                    prmesito          1
042300000000     C*
042400000000     C* Definizione chiavi
042500170119     C     KEYvgd09_c    klist
042600000000     C                   kfld                    SQL_TIP
042700000000     C                   kfld                    SQL_KSU
042800000000     C                   kfld                    SQL_TSC
042900160906     C                   kfld                    SQL_PRG
043000000000     C*
043100000000     C     KEYvir02_p    klist
043200000000     C                   kfld                    vgdksu
043300000000     C                   kfld                    vgdtip
043400160906     C*
043500000000     C                   endsr
043600160906     C*
