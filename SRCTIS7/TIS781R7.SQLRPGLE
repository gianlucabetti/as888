000100161205     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000
000400170227     Ftivgd10l  uf   e           k disk    commit
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
002200170323     D  wSTO           s              1    inz('L')
002300170317     D  wID            s              1    inz('7')
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
006500170405     C+       VGDTSC <> '  ' AND
006600170222     C+       VGDKSU NOT IN
006700170222     C+          (SELECT VGSKSU FROM TIVGS10F WHERE VGSTIP=' ') AND
006800170227     C+       VGDTIP NOT IN
006900170227     C+          (SELECT VGSTIP FROM TIVGS10F WHERE VGSKSU=' ') AND
007000170222     C+       (VGDKSU, VGDTIP) NOT IN
007100170222     C+          (SELECT VGSKSU, VGSTIP FROM TIVGS10F)
007200000000     C+ FOR READ ONLY
007300000000     C/END-EXEC
007400000000     C
007500000000     C/EXEC SQL
007600000000     C+ OPEN C1
007700000000     C/END-EXEC
007800000000     C
007900000000     C/EXEC SQL
008000000000     C+ FETCH C1 INTO :dsSQLVGD
008100000000     C/END-EXEC
008200170315     C
008300170315     C*
008400170315     C* Inizializzo schiera di work per momorizzazione dati recordset SQL (C1)
008500170315     C                   clear                   skVGD
008600170315     C                   clear                   jVGD
008700170315     C*
008800170315     C* Schiera di work per momorizzazione dati recordset SQL (C1)
008900170315     C                   dow       sqlcod = *zeros AND
009000170329     C                             jVGD <  %elem(skVGD)
009100170327     C                   if        %subst(SQL_PRG:1:2) <> 'XX'
009200170315     C                   add       1             jVGD
009300170315     C                   eval      skVGD(jVGD) = dsSQLVGD
009400170327     C                   endif
009500170315     C*
009600170315     C/EXEC SQL
009700170315     C+ FETCH C1 INTO :dsSQLVGD
009800170315     C/END-EXEC
009900170315     C                   enddo
010000170315     C*
010100170315     C/EXEC SQL
010200170315     C+ CLOSE C1
010300170315     C/END-EXEC
010400170315     C
010500170315     C*
010600170315     C* Scorro tutti gli elementi della schiera di work
010700170315     C                   z-add     1             sVGD
010800170315     C                   dow       sVGD <= jVGD
010900170315     C                   eval      dsSQLVGD = skVGD(sVGD)
011000161213     C*
011100161213     C* Elimino il contenuto del file d wrk in QTEMP
011200161213     C
011300161213     C/EXEC SQL
011400161213     C+ DELETE FROM QTEMP/TIVGDTMP
011500170119     C+ WITH NC
011600161213     C/END-EXEC
011700161213     C
011800161213     C*
011900161213     C* Se errore esco e segnalo
012000161213     C                   if        sqlcod < *zeros
012100161213     C                   eval      prmesito = '2'
012200161213     C                   leave
012300161213     C                   else
012400000000     C*
012500000000     C* Apro il file d wrk
012600000000     C                   if        not %open(tivgdtmp)
012700000000     C                   open      tivgdtmp
012800000000     C                   endif
012900000000     C*
013000000000     C* Leggo la vista logica del file generico download x ogni chiave d rottura traduzione
013100090518     C* ovvero TIP/KSU/TSC/PRG
013200170227     C     KEYvgd10_c    setll     tivgd10l
013300170227     C                   if        %equal(tivgd10l)
013400000000     C*
013500000000     C* 1° lettura file fuori ciclo
013600170227     C     KEYvgd10_c    reade     tivgd10l
013700000000     C*
013800160906     C* Leggo tutta la vista logica per medesima chiave
013900170227     C                   dow       not %eof(tivgd10l)
014000000000     C*
014100000000     C* Gestione ENDJOB.
014200000000     C                   shtdn                                        60
014300160906     C                   if        not *in60
014400161212     C*
014500161212     C* Memorizzo il relative record number "originale"
014600161212     C                   eval      DVGD001 = vgdATTRIB
014700161212     C                   eval      §VGD001RRN = infds_VGD.db_rrn
014800170317     C                   eval      §VGD001FL1 = wID
014900161212     C                   eval      vgdATTRIB = DVGD001
015000161212     C*
015100000000     C* Passo i dati dal file "buono" al file d wrk in QTEMP
015200160906     C                   eval      dsVGDtmp = dsVGD
015300160906     C                   write     tivgdT
015400160906     C                   eval      vgdsto = '-'
015500000000     C                   update    tivgd000
015600000000     C*
015700000000     C* Gestisco lo shutdown
015800000000     C                   else
015900000000     C                   eval      prmesito = '2'
016000000000     C                   leave
016100000000     C                   endif
016200000000     C*
016300170119     C* Proseguo con la lettura del file VAS Generico Download (TIVGD)
016400170227     C     KEYvgd10_c    reade     tivgd10l
016500000000     C                   enddo
016600000000     C*
016700000000     C* Effettuo la traduzione
016800161213     C  N60              exsr      traduzione
016900000000     C                   endif
017000161213     C*
017100161213     C                   endif
017200000000     C*
017300000000     C* Verifico eventuale shutdown
017400161212     C   60              eval      prmesito = '2'
017500000000     C   60              leave
017600000000     C*
017700170315     C                   add       1             sVGD
017800000000     C                   enddo
017900160906     C*
018000160906     C* Gestisco eventuale errore
018100160906     C                   on-error
018200160906     C                   eval      prmesito = '2'
018300171003     C                   dump(A)
018400160906     C*
018500160906     C* Interrompo monitoraggio errori
018600160906     C                   endmon
018700000000     C*
018800000000     C                   seton                                        lr
018900000000     C*-----------------------------------------------------------------------------
019000000000
019100000000
019200000000
019300000000      /TITLE Traduzione/copy file
019400000000     C     traduzione    begsr
019500000000     C*
019600000000     C* Inizializzo variabili d wrk
019700000000     C                   eval      wEsito      = *blanks
019800000000     C                   eval      wMessErrore = *blanks
019900000000     C*
020000000000     C* Reperisco il file "da tradurre" legato al tipo file corrente
020100000000     C                   exsr      repvtf
020200161212     C*
020300161212     C* Se dati "anagrafici" reperiti => procedo
020400161212     C                   if        wEsito = *blanks
020500000000     C*
020600160906     C* Se fin qui tutto ok => proseguo con elaborazione
020700170405     C                   exsr      exeTsc
020800000000     C*
020900000000     C* In uscita traduzione/copy file effettuo la storicizzazione
021000000000     C                   eval      wEsito = Esito
021100161212     C*
021200161212     C                   endif
021300161212     C*
021400161212     C* Al termine storicizzo
021500000000     C                   exsr      storicizza
021600000000     C*
021700000000     C                   endsr
021800170405
021900170405
022000170405
022100170405      /TITLE Eseguo operazioni di traduzione rispetto al Tipo Scarico Dati
022200170405     C     exeTsc        begsr
022300170405     C*
022400170405     C* Verifico il tipo scarico dati...
022500170405     C                   select
022600170405     C* WW
022700170405     C                   when      vgdtsc = 'WW'
022800170405     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
022900170405     C  N77              eval      p_ope = 'WWI'
023000170405     C   77              eval      p_ope = 'WWN'
023100170405     C                   call      'TIS781C3'
023200170405     C                   parm      vgdksu        p_ksu             8
023300170405     C                   parm      vgdtip        p_tip             2
023400170405     C                   parm      vgdtsc        p_tsc             2
023500170405     C                   parm      vgdprg        p_prog           10
023600170405     C                   parm      wFileInput    p_filein         10
023700170405     C                   parm      wFileStor     p_filesto        10
023800170405     C                   parm                    p_mbrin          10
023900170405     C                   parm                    p_ope             3
024000170405     C                   parm      *blanks       p_out            10
024100170405     C                   parm      *blanks       p_nfp             2
024200170405     C                   parm      *blanks       p_attrib        256
024300170405     C                   parm      *blanks       Esito             1
024400170405     C* W2
024500170405     C                   when      vgdtsc = 'W2'
024600170405     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
024700170405     C* W2 CPYF
024800170405     C                   call      'TIS781C3'
024900170405     C                   parm                    p_ksu
025000170405     C                   parm                    p_tip
025100170405     C                   parm                    p_tsc
025200170405     C                   parm                    p_prog
025300170405     C                   parm      wFileInput    p_filein
025400170405     C                   parm      *blanks       p_filesto
025500170405     C                   parm                    p_mbrin
025600170405     C                   parm      'CFI'         p_ope
025700170405     C                   parm      *blanks       p_out
025800170405     C                   parm      *blanks       p_nfp
025900170405     C                   parm      *blanks       p_attrib
026000170405     C                   parm      *blanks       Esito
026100170405     C* W2 WW
026200170405     C  N77              eval      p_ope = 'WWI'
026300170405     C   77              eval      p_ope = 'WWN'
026400170405     C                   call      'TIS781C3'
026500170405     C                   parm      vgdksu        p_ksu
026600170405     C                   parm      vgdtip        p_tip
026700170405     C                   parm      vgdtsc        p_tsc
026800170405     C                   parm      vgdprg        p_prog
026900170405     C                   parm      wFileInput    p_filein
027000170405     C                   parm      wFileStor     p_filesto
027100170405     C                   parm                    p_mbrin
027200170405     C                   parm                    p_ope
027300170405     C                   parm      *blanks       p_out
027400170405     C                   parm      *blanks       p_nfp
027500170405     C                   parm      *blanks       p_attrib
027600170405     C                   parm      *blanks       Esito
027700170405     C*
027800170405     C                   other
027900170405     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
028000170405     C                   call      'TIS781C3'
028100170405     C                   parm                    p_ksu
028200170405     C                   parm                    p_tip
028300170405     C                   parm                    p_tsc
028400170405     C                   parm                    p_prog
028500170405     C                   parm      wFileInput    p_filein
028600170405     C                   parm      *blanks       p_filesto
028700170405     C                   parm                    p_mbrin
028800170405     C                   parm      'CFI'         p_ope
028900170405     C                   parm      *blanks       p_out
029000170405     C                   parm      *blanks       p_nfp
029100170405     C                   parm      *blanks       p_attrib
029200170405     C                   parm      *blanks       Esito
029300170405     C*
029400170405     C                   endsl
029500170405     C*
029600170405     C                   endsr
029700000000
029800000000
029900000000
030000000000      /TITLE Routine di reperimento file "da tradiurre" legata al tipo file corrente
030100000000     C*
030200000000     C     repvtf        begsr
030300000000     C*
030400000000     C                   eval      wFileInput    = *blanks
030500000000     C                   eval      wFileStor     = *blanks
030600170119     C                   eval      wTipoServizio = 'DG'
030700000000     C*
030800161212     C* Anagrafica Tipo-File
030900000000     C     vgdtip        chain     tivtf01l
031000000000     C                   if        %found(tivtf01l)
031100000000     C                   eval      wFileInput = VTFFID
031200000000     C                   eval      wFileStor  = VTFFIS
031300000000     C*
031400161212     C* Anagrafica Cliente/Tipo-File
031500000000     C     KEYvir02_p    chain     tivir02l
031600000000     C                   if        %found(tivir02l)
031700000000     C                   eval      wTipoServizio = VIRFI1
031800000000     C                   if        VIRFID <> *blanks
031900000000     C                   eval      wFileInput = VIRFID
032000000000     C                   endif
032100000000     C                   if        VIRFIS <> *blanks
032200000000     C                   eval      wFileStor  = VTFFIS
032300000000     C                   endif
032400000000     C                   endif
032500161212     C*
032600000000     C                   else
032700000000     C*
032800161212     C* Se tipo file corrente NON trovato in anagrafica => mando e-mail a cedalert@brt.it
032900000000     C* e fleggo tali record a stato storicizzazione (STO) = 2
033000161212     C                   eval      wEsito = '2'
033100000000     C                   eval      wMessErrore = 'VERIFICARE IL FILE DOWNLOAD '+
033200170119     C                             ' (TIVGD00F)'                       +':/N'+
033300000000     C                             ' '                                 +':/N'+
033400000000     C                             'RIFERIMENTI'                       +':/N'+
033500000000     C                             'Cliente: ' + vgdksu                +':/N'+
033600000000     C                             'Tipo file: ' + vgdtip              +':/N'+
033700161212     C                             'Progressivo: ' + vgdprg            +':/N'+
033800000000     C                             ' '                                 +':/N'+
033900000000     C                             'MESSAGGIO'                         +':/N'+
034000170119     C                             'ERRORE: Tipo File ERRATO'          +':/N'+
034100170119     C                             'SEVERITA: GRAVE'
034200000000     C                   exsr      sndeml
034300000000     C                   endif
034400000000     C*
034500000000     C                   setoff                                       77
034600000000     C                   if        %trim(wFileInput) = '*'
034700000000     C                   eval      wFileInput = 'TIVGDTMP'
034800000000     C                   seton                                        77
034900000000     C                   endif
035000000000     C                   if        %trim(wFileStor)  = '*'
035100000000     C                   eval      wFileStor  = 'TIVGDTMP'
035200000000     C                   seton                                        77
035300000000     C                   endif
035400000000     C*
035500000000     C                   endsr
035600000000
035700170119
035800000000
035900000000      /TITLE Storicizzazione post traduzione/copy file
036000000000     C     storicizza    begsr
036100161213     C*
036200161213     C* A questo punto la singola transazione è logicamente conclusa x cui sancisco il commit
036300161213     C                   commit(e)
036400000000     C*
036500000000     C* Effettuo alcune considerazioni sull'esito
036600000000     C                   if        wEsito = ' ' OR wEsito = '0'
036700000000     C                   eval      Esito  = 'S'
036800000000     C                   else
036900000000     C                   if        wEsito = 'Y'
037000000000     C                   eval      Esito  = *blanks
037100000000     C                   else
037200000000     C                   eval      Esito  = wEsito
037300000000     C                   endif
037400000000     C                   endif
037500000000     C
037600000000     C/EXEC SQL
037700161212     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
037800000000     C/END-EXEC
037900000000     C
038000000000     C/EXEC SQL
038100000000     C+ UPDATE TIVGD00F SET VGDSTO = :ESITO,
038200170119     C+ VGDCNT = VGDCNT + 1
038300170119     C+ WHERE VGDSTO = '-'     AND VGDPRG = :VGDPRG AND
038400170119     C+       VGDTIP = :VGDTIP AND VGDKSU = :VGDKSU AND
038500170119     C+       VGDTSC = :VGDTSC
038600000000     C/END-EXEC
038700161212     C
038800161212     C/EXEC SQL
038900161212     C+ COMMIT
039000161212     C/END-EXEC
039100000000     C
039200000000     C*
039300000000     C* Chiudo il file d wrk
039400000000     C                   if        %open(tivgdtmp)
039500000000     C                   close     tivgdtmp
039600000000     C                   endif
039700000000     C*
039800000000     C                   endsr
039900000000
040000000000
040100000000      /TITLE Compongo il testo e spedisco una e-m@ail
040200161212     C     sndeml        begsr
040300000000     C*
040400000000     C* Inizializzo variabili
040500000000     C                   movel     *blanks       wrkEml          253
040600000000     C                   movel     *blanks       wrkEmlMsg      5000
040700000000     C                   movel     *blanks       wrkEmlOgg        44
040800000000     C* Valorizzo i campi della e-m@ail
040900120312     C                   eval      wrkEml='cedalert@brt.it'
041000170119     C                   eval      wrkEmlOgg='VAS - DOWNLOAD - ERROR'
041100000000     C                   eval      wrkEmlMsg=wMessErrore
041200000000     C*
041300000000     C                   call(e)   'TIS701C'
041400000000     C                   parm                    wrkEml
041500000000     C                   parm                    wrkEmlOgg
041600000000     C                   parm                    wrkEmlMsg
041700000000     C*
041800000000     C                   endsr
041900000000
042000000000
042100000000
042200000000      /TITLE Routine di *init PGM
042300000000     C*
042400000000     C     *inzsr        begsr
042500000000     C*
042600000000     C* Passaggio parametri
042700000000     C     *entry        plist
042800000000     C                   parm                    prmesito          1
042900000000     C*
043000000000     C* Definizione chiavi
043100170227     C     KEYvgd10_c    klist
043200000000     C                   kfld                    SQL_TIP
043300000000     C                   kfld                    SQL_KSU
043400000000     C                   kfld                    SQL_TSC
043500160906     C                   kfld                    SQL_PRG
043600000000     C*
043700000000     C     KEYvir02_p    klist
043800000000     C                   kfld                    vgdksu
043900000000     C                   kfld                    vgdtip
044000160906     C*
044100000000     C                   endsr
044200160906     C*
