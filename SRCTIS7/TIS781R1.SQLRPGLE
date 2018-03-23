000100000000     H* xxxxxxxxxxxxxxxxxxxx
000200000000     H dftactgrp(*no) actgrp(*caller) Decedit('0,') Datedit(*dmy.)
000300000000
000400000000     Ftivgd01l  uf a e           k disk    commit
000500000000     Ftivgdtmp  uf a e             disk    rename(tivgd000:tivgdT) prefix(t_)
000600000000     F                                     usropn
000700000000     Ftivtf01l  if   e           k disk
000800000000     Ftivir02l  if   e           k disk
000900000000     Ftis7prgf  uf   e             disk    rename(tis7prgf:tis7prg0)
001000000000     F                                     prefix(f_)
001100000000     F                                     usropn
001200000000
001300000000
001400000000     D* variabili di work
001500000000     D  dsVGD        e ds                  extname(tivgd00f)
001600000000     D  dsVGDtmp     e ds                  extname(tivgd00f) prefix(t_)
001700000000     D  trul47ds     e ds
001800000000     D  tis781ds     e ds
001900000000     D  tis781dflo   e ds
002000000000     D  dvgdflo      e ds
002100000000     D  dvgd001      e ds
002200000000     D  wFileInput     s                   like(vtffid) inz(*blanks)
002300000000     D  wFileStor      s                   like(vtffis) inz(*blanks)
002400000000     D  wTipoServizio  s                   like(virfi1) inz(*blanks)
002500000000     D  dwlprg         s             10    inz(*all'0')
002600000000     D  wrkprg         s              8  0 inz(*zeros)
002700000000     D  wEsito         s              1    inz(*blanks)
002800000000     D  wMessErrore    s           2000    inz(*blanks)
002900000000     D  savTipF        s                   like(vgdtip) inz(*blanks)
003000000000     d  wOGGI          s              8  0
003100000000     d  Cli_Abil       s              1    inz(*off)
003200000000     d  wRecOk         s              1    inz(*blanks)
003300000000     d  parvgdprg      s                   like(vgdprg)
003400000000     d  parvgdflotfsF  s                   like(§781flotfs)
003500000000     d  parvgdflotfsT  s                   like(§781flotfs)
003600000000     d  parvgdflotfn   s                   like(§781flotfn)
003700090521     d  p§781OUT       s                   like(§781out)
003800090521     d  p§781NFP       s                   like(§781nfp)
003900090521     d  p§781ATTRIB    s                   like(§781attrib)
004000000000
004100000000     D* DS d wrk
004200000000     D  dsSQLVGD       ds
004300000000     D   SQL_TIP                           like(vgdtip)
004400000000     D   SQL_KSU                           like(vgdksu)
004500000000     D   SQL_TSC                           like(vgdtsc)
004600090522 xxx D   SQL_PRG                           like(vgdprg)
004700000000
004800000000     D wlbda8          ds                  inz
004900000000     D  g08dat                 1      8  0
005000000000     D  g08inv                 9     16  0
005100000000     D  g08err                17     17
005200000000     D  g08tgi                18     22  0
005300000000
005400000000
005500000000
005600000000     C*-----------------------------------------------------------------------------
005700000000     C*
005800000000     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005900000000     C
006000000000     C/EXEC SQL
006100000000     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
006200000000     C/END-EXEC
006300000000     C*
006400000000     C*
006500000000     C* In ingresso elimino subito il contenuto del file d wrk in QTEMP
006600000000     C                   z-add     50            LENGH            15 5
006700000000     C                   movel     *blanks       CMD              50
006800000000     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
006900000000     C                   call      'QCMDEXC'                                    *LANCIA CMD
007000000000     C                   parm                    CMD
007100000000     C                   parm                    LENGH
007200000000     C*
007300000000     C* Reperisco i flag operativi
007400000000     C                   eval      tis781dflo = §781FLO
007500000000     C*
007600000000     C* Gestione flag operativo "CODICE REGOLA"
007700000000     C                   if        §781FLOTFS = *blanks
007800000000     C                   eval      parvgdflotfsF = *loval
007900000000     C                   eval      parvgdflotfsT = *hival
008000000000     C                   else
008100000000     C                   eval      parvgdflotfsF = §781FLOTFS
008200000000     C                   eval      parvgdflotfsT = §781FLOTFS
008300000000     C                   endif
008400000000     C                   eval      parvgdflotfn  = §781FLOTFN
008500000000     C*
008600000000     C/EXEC SQL
008700000000     C+ DECLARE C1 CURSOR FOR
008800090519 xxx C+ SELECT DISTINCT VGDTIP, VGDKSU, VGDTSC, VGDPRG FROM TIVGD00F
008900090519 xxx C***+ SELECT DISTINCT VGDTIP, VGDKSU, VGDTSC FROM TIVGD00F
009000000000     C+ WHERE VGDSTO = ' '
009100000000     C+ AND VGDTIP >= :parvgdflotfsF AND VGDTIP <= :parvgdflotfsT
009200000000     C+ AND VGDTIP <> :parvgdflotfn
009300090519 xxx C+ ORDER BY VGDTIP, VGDKSU, VGDTSC, VGDPRG
009400090519 xxx C***+ ORDER BY VGDTIP, VGDKSU, VGDTSC
009500000000     C+ FOR READ ONLY
009600000000     C/END-EXEC
009700000000     C
009800000000     C/EXEC SQL
009900000000     C+ OPEN C1
010000000000     C/END-EXEC
010100000000     C
010200000000     C/EXEC SQL
010300000000     C+ FETCH C1 INTO :dsSQLVGD
010400000000     C/END-EXEC
010500000000     C*
010600000000     C                   dow       sqlcod = *zeros
010700000000     C*
010800000000     C* Verifico innanzitutto se il tipo file corrente è in stato elaborabile
010900000000     C                   if        SQL_TIP <> savTipF
011000000000     C*
011100000000     C* Inizializzo indicatore che subordina l'elaborazione d ogni tipo file/cliente/tipo scarico
011200000000     C                   setoff                                       55
011300000000     C*
011400000000     C* Salvo il tipo file corrente
011500000000     C                   eval      savTipF = SQL_TIP
011600000000     C*
011700000000     C* Verifico se presente il blocco elaborazione TIVGD x tipo file corrente
011800000000     C                   clear                   trul47ds
011900000000     C                   eval      d47opz  = 'C'
012000000000     C                   eval      d47tip  = SQL_TIP
012100000000     C                   call      'TRUL47R'
012200000000     C                   parm                    trul47ds
012300000000     C*
012400000000     C* Testo il respondo ottenuto
012500000000     C                   if        d47sts = *blanks
012600000000     C                   seton                                        55
012700000000     C                   endif
012800000000     C                   endif
012900000000     C*
013000000000     C* Se elaborazione consentita => proseguo
013100000000  IF1C                   if        *in55 = *on
013200000000     C*
013300000000     C* Apro il file d wrk
013400000000     C                   if        not %open(tivgdtmp)
013500000000     C                   open      tivgdtmp
013600000000     C                   endif
013700000000     C*
013800000000     C* Inizializzo flag x verifica lettura record
013900000000     C                   movel     'N'           wRecordLetti      1
014000000000     C*
014100000000     C* Leggo la vista logica del file generico download x ogni chiave d rottura traduzione
014200090518     C* ovvero TIP/KSU/TSC/PRG
014300000000     C     KEYvgd01_c    setll     tivgd01l
014400000000     C                   if        %equal(tivgd01l)
014500000000     C*
014600000000     C* 1° lettura file fuori ciclo
014700000000     C     KEYvgd01_c    reade     tivgd01l
014800000000     C*
014900000000     C* Leggo tutta la vista logica
015000000000     C                   dow       not %eof(tivgd01l)
015100000000     C*
015200000000     C* Gestione ENDJOB.
015300000000     C                   shtdn                                        60
015400000000     C                   if        not *in60
015500000000     C*
015600000000     C* Verifico innanzitutto se richieste parzializzazionei in ingresso
015700000000     C                   movel     'S'           wRecOk
015800000000     C                   exsr      chkrec
015900000000     C*
016000000000     C* Se record valido proseguo l'elaborazione
016100000000     C                   if        wRecOk = 'S'
016200000000     C*
016300000000     C* Gestisco attributi specifici a livello d record corrente
016400000000     C                   exsr      chkattrib
016500000000     C*
016600000000     C* Valorizzo flag x verifica lettura record
016700000000     C                   movel     'S'           wRecordLetti
016800000000     C*
016900000000     C* Passo i dati dal file "buono" al file d wrk in QTEMP
017000000000     C                   eval      dsVGDtmp = dsVGD
017100000000     C                   write     tivgdT
017200000000     C                   eval      vgdsto = '*'
017300000000     C                   update    tivgd000
017400000000     C                   endif
017500000000     C*
017600000000     C* Gestisco lo shutdown
017700000000     C                   else
017800000000     C                   eval      prmesito = '2'
017900000000     C                   leave
018000000000     C                   endif
018100000000     C*
018200000000     C* Proseguo con la lettura del file Vas Generico Download (TIVGD)
018300000000     C     KEYvgd01_c    reade     tivgd01l
018400000000     C                   enddo
018500000000     C*
018600000000     C* Effettuo test particolare x tipo file 'VP' x verifica abilitazione cliente
018700000000     C  N60              if        vgdtip = 'VP'
018800000000     C                   exsr      CtrlAbil
018900000000     C                   else
019000000000     C                   eval      Cli_Abil = *on
019100000000     C                   endif
019200000000     C*
019300000000     C* Effettuo la traduzione
019400000000     C  N60              if        Cli_Abil = *on
019500000000     C                   exsr      traduzione
019600000000     C                   else
019700000000     C                   exsr      elimina
019800000000     C***                exsr      storicizzaNA
019900000000     C                   endif
020000000000     C*
020100000000     C                   endif
020200000000     C*
020300000000  E 1C                   endif
020400000000     C*
020500000000     C* Verifico eventuale shutdown
020600000000     C   60              leave
020700000000     C*
020800000000     C/EXEC SQL
020900000000     C+ FETCH C1 INTO :dsSQLVGD
021000000000     C/END-EXEC
021100000000     C                   enddo
021200000000     C*
021300000000     C/EXEC SQL
021400000000     C+ CLOSE C1
021500000000     C/END-EXEC
021600000000     C
021700000000     C*
021800000000     C                   seton                                        lr
021900000000     C*-----------------------------------------------------------------------------
022000000000
022100000000
022200000000
022300000000      /TITLE Verifica validità record
022400000000     C     chkrec        begsr
022500000000     C*
022600000000     C* Scompongo i flag operativi del record
022700000000     C                   eval      dvgdflo = vgdFLO
022800000000     C*
022900000000     C* Se ricevuti in input parametri d selezione effettuo considerazioni
023000000000     C                   if        tis781ds <> *blanks
023100000000     C*
023200000000     C* Verifico se richiesto specifico tipo file
023300000000     C                   if        §781TIP <> *blanks
023400000000     C                   if        vgdTIP  <> §781TIP
023500000000     C                   eval      wRecOk = 'N'
023600000000     C                   endif
023700000000     C                   endif
023800000000     C*
023900000000     C* Verifico se richiesto specifico cliente unificante
024000000000     C                   if        §781KSU <> *blanks
024100000000     C                   if        vgdKSU  <> §781KSU
024200000000     C                   eval      wRecOk = 'N'
024300000000     C                   endif
024400000000     C                   endif
024500000000     C*
024600000000     C* Verifico se richiesto specifico tipo scarico dati
024700000000     C                   if        §781TSC <> *blanks
024800000000     C                   if        vgdTSC  <> §781TSC
024900000000     C                   eval      wRecOk = 'N'
025000000000     C                   endif
025100000000     C                   endif
025200000000     C*
025300000000     C* Verifico se richiesto specifico data inserimento dati
025400000000     C                   if        §781DAT <> *zeros
025500000000     C                   if        vgdDAT  <> §781DAT
025600000000     C                   eval      wRecOk = 'N'
025700000000     C                   endif
025800000000     C                   endif
025900000000     C*
026000000000     C* Verifico se richiesto specifico programma scrivente
026100000000     C                   if        §781PGM <> *blanks
026200000000     C                   if        vgdPGM  <> §781PGM
026300000000     C                   eval      wRecOk = 'N'
026400000000     C                   endif
026500000000     C                   endif
026600000000     C*
026700000000     C* Verifico se richiesto specifico progressivo transazione
026800000000     C                   if        §781PRG <> *blanks
026900000000     C                   if        vgdPRG  <> §781PRG
027000000000     C                   eval      wRecOk = 'N'
027100000000     C                   endif
027200000000     C                   endif
027300000000     C*
027400000000     C* Verifico il tipo elaborazione richiesta
027500000000     C                   if        §vgdfela <> §781FLOELA
027600000000     C                   eval      wRecOk = 'N'
027700000000     C                   endif
027800000000     C*
027900090521     C                   else
028000000000     C*
028100000000     C* Se nn ricevuti in input parametri d selezione devo elaborare solo i record standard
028200000000     C                   if        §vgdfela <> *blanks
028300000000     C                   eval      wRecOk = 'N'
028400000000     C                   endif
028500000000     C                   endif
028600000000     C*
028700000000     C                   endsr
028800000000
028900000000
029000000000
029100000000      /TITLE Verifica validità record
029200000000     C     chkattrib     begsr
029300090521     C*
029400090521     C* Se ricevuti in input parametri d selezione => vincono quelli, altrimenti
029500090521     C* verifico gli attributi sul file
029600090910     C                   if        §781OUT <> *blanks OR §781NFP <> *blanks
029700090521     C                   eval      p§781NFP = §781NFP
029800090521     C                   eval      p§781OUT = §781OUT
029900090521     C                   else
030000090518     C*
030100090518     C* Inizializzazioni
030200090521     C                   clear                   dvgd001
030300090521     C                   eval      p§781NFP = *blanks
030400090521     C                   eval      p§781OUT = *blanks
030500090521     C                   endif
030600000000     C*
030700000000     C                   endsr
030800000000
030900000000
031000000000
031100000000      /TITLE Traduzione/copy file
031200000000     C     traduzione    begsr
031300000000     C*
031400000000     C* Elaboro solo se effettivamente c'erano record
031500000000     C                   if        wRecordLetti = 'S'
031600000000     C*
031700000000     C* Inizializzo variabili d wrk
031800000000     C                   eval      wEsito      = *blanks
031900000000     C                   eval      wMessErrore = *blanks
032000000000     C*
032100000000     C* Reperisco il file "da tradurre" legato al tipo file corrente
032200000000     C                   exsr      repvtf
032300000000     C*
032400000000     C* Se NN già presente 1 ID transazione stacco 1 progressivo univoco x la transazione corrente
032500000000     C                   if        vgdprg = *blanks
032600000000     C                   exsr      calprog
032700000000     C                   else
032800000000     C                   eval      dwlprg = vgdprg
032900000000     C                   endif
033000000000     C*
033100000000     C* Ritorno l'ID univoco transazione
033200000000     C                   eval      parvgdprg = dwlprg
033300000000     C*
033400000000     C* Se errore NN elaboro e flaggo record letti ad errore (VGDSTO = '2')
033500000000     C                   if        wEsito = '2'
033600000000     C                   exsr      storicizza
033700000000     C*
033800000000     C* Se invece fin qui tutto ok => proseguo con elaborazione
033900000000     C                   else
034000000000     C*
034100000000     C* Verifico il tipo scarico dati...
034200000000     C*
034300000000     C* ...se scarico dati è WW => innesco iter traduzione "standard"
034400000000     C                   if        vgdtsc = 'WW'
034500000000     C                   eval      p_mbrin  = 'M'+%subst(vgdksu:2:7)
034600000000     C  N77              eval      p_ope = 'WWI'
034700000000     C   77              eval      p_ope = 'WWN'
034800000000     C                   call      'TIS781C2'
034900000000     C                   parm      vgdksu        p_ksu             8
035000000000     C                   parm      vgdtip        p_tip             2
035100000000     C                   parm      vgdtsc        p_tsc             2
035200000000     C                   parm      dwlprg        p_prog           10
035300000000     C                   parm      wFileInput    p_filein         10
035400000000     C                   parm      wFileStor     p_filesto        10
035500000000     C                   parm                    p_mbrin          10
035600000000     C                   parm                    p_ope             3
035700090521     C                   parm      p§781OUT      p_out            10
035800090521     C                   parm      p§781NFP      p_nfp             2
035900090521     C                   parm      p§781ATTRIB   p_attrib        256
036000000000     C                   parm      *blanks       Esito             1
036100000000     C*
036200000000     C* In uscita traduzione/copy file effettuo la storicizzazione
036300000000     C                   eval      wEsito = Esito
036400000000     C                   exsr      storicizza
036500000000     C*
036600000000     C                   else
036700000000     C*
036800000000     C* ...se scarico dati è W2 => innesco iter traduzione "standard" + copyfile sul relativo 00T
036900000000     C                   if        vgdtsc = 'W2'
037000000000     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
037100000000     C* CPYF
037200000000     C                   call      'TIS781C2'
037300000000     C                   parm                    p_ksu
037400000000     C                   parm                    p_tip
037500000000     C                   parm                    p_tsc
037600000000     C                   parm                    p_prog
037700000000     C                   parm      wFileInput    p_filein
037800000000     C                   parm      *blanks       p_filesto
037900000000     C                   parm                    p_mbrin
038000000000     C                   parm      'CFI'         p_ope
038100090521     C                   parm      p§781OUT      p_out
038200090521     C                   parm      p§781NFP      p_nfp
038300090521     C                   parm      p§781ATTRIB   p_attrib
038400000000     C                   parm      *blanks       Esito
038500000000     C* TRADUZIONE WW
038600000000     C  N77              eval      p_ope = 'WWI'
038700000000     C   77              eval      p_ope = 'WWN'
038800000000     C                   call      'TIS781C2'
038900000000     C                   parm      vgdksu        p_ksu
039000000000     C                   parm      vgdtip        p_tip
039100000000     C                   parm      vgdtsc        p_tsc
039200000000     C                   parm      dwlprg        p_prog
039300000000     C                   parm      wFileInput    p_filein
039400000000     C                   parm      wFileStor     p_filesto
039500000000     C                   parm                    p_mbrin
039600000000     C                   parm                    p_ope
039700090521     C                   parm      p§781OUT      p_out
039800090521     C                   parm      p§781NFP      p_nfp
039900090521 xxx C                   parm      p§781ATTRIB   p_attrib
040000000000     C                   parm      *blanks       Esito
040100000000     C*
040200000000     C* In uscita traduzione/copy file effettuo la storicizzazione x tipo file/cliente/tipo scarico
040300000000     C                   eval      wEsito = Esito
040400000000     C                   exsr      storicizza
040500000000     C*
040600000000     C                   else
040700000000     C*
040800000000     C* ...se scarico dati NON è ne WW ne W2 => effettuo semplicemente il copy file sul relativo 00T
040900000000     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
041000000000     C                   call      'TIS781C2'
041100000000     C                   parm                    p_ksu
041200000000     C                   parm                    p_tip
041300000000     C                   parm                    p_tsc
041400000000     C                   parm                    p_prog
041500000000     C                   parm      wFileInput    p_filein
041600000000     C                   parm      *blanks       p_filesto
041700000000     C                   parm                    p_mbrin
041800000000     C                   parm      'CFI'         p_ope
041900090521     C                   parm      p§781OUT      p_out
042000090521     C                   parm      p§781NFP      p_nfp
042100090521     C                   parm      p§781ATTRIB   p_attrib
042200000000     C                   parm      *blanks       Esito
042300000000     C*
042400000000     C* In uscita traduzione/copy file effettuo la storicizzazione x tipo file/cliente/tipo scarico
042500000000     C                   eval      wEsito = Esito
042600000000     C                   exsr      storicizza
042700000000     C*
042800000000     C                   endif
042900000000     C                   endif
043000000000     C                   endif
043100000000     C                   endif
043200000000     C*
043300000000     C                   endsr
043400000000
043500000000
043600000000
043700000000      /TITLE Routine di reperimento file "da tradiurre" legata al tipo file corrente
043800000000     C*
043900000000     C     repvtf        begsr
044000000000     C*
044100000000     C                   eval      wFileInput    = *blanks
044200000000     C                   eval      wFileStor     = *blanks
044300000000     C                   eval      wTipoServizio = 'DG'
044400000000     C*
044500000000     C     vgdtip        chain     tivtf01l
044600000000     C                   if        %found(tivtf01l)
044700000000     C                   eval      wFileInput = VTFFID
044800000000     C                   eval      wFileStor  = VTFFIS
044900000000     C*
045000000000     C     KEYvir02_p    chain     tivir02l
045100000000     C                   if        %found(tivir02l)
045200000000     C                   eval      wTipoServizio = VIRFI1
045300000000     C                   if        VIRFID <> *blanks
045400000000     C                   eval      wFileInput = VIRFID
045500000000     C                   endif
045600000000     C                   if        VIRFIS <> *blanks
045700000000     C                   eval      wFileStor  = VTFFIS
045800000000     C                   endif
045900000000     C                   endif
046000000000     C                   else
046100000000     C*
046200120312     C* Se tipo file corrente NN trovato in anagrafica => mando e-mail a ced@brt.it
046300000000     C* e fleggo tali record a stato storicizzazione (STO) = 2
046400000000     C                   eval      wMessErrore = 'VERIFICARE IL FILE DOWNLOAD '+
046500000000     C                             'GENERICO TIVGD00F'                 +':/N'+
046600000000     C                             ' '                                 +':/N'+
046700000000     C                             'RIFERIMENTI'                       +':/N'+
046800000000     C                             'Cliente: ' + vgdksu                +':/N'+
046900000000     C                             'Tipo file: ' + vgdtip              +':/N'+
047000000000     C                             'Progressivo: ' + dwlprg            +':/N'+
047100000000     C                             ' '                                 +':/N'+
047200000000     C                             'MESSAGGIO'                         +':/N'+
047300000000     C                             'ERRORE GRAVE: tipo file ERRATO!!!'
047400000000     C                   exsr      sndeml
047500000000     C                   eval      wEsito = '2'
047600000000     C                   endif
047700000000     C*
047800000000     C                   setoff                                       77
047900000000     C                   if        %trim(wFileInput) = '*'
048000000000     C                   eval      wFileInput = 'TIVGDTMP'
048100000000     C                   seton                                        77
048200000000     C                   endif
048300000000     C                   if        %trim(wFileStor)  = '*'
048400000000     C                   eval      wFileStor  = 'TIVGDTMP'
048500000000     C                   seton                                        77
048600000000     C                   endif
048700000000     C*
048800000000     C                   endsr
048900000000
049000000000
049100000000
049200000000      /TITLE Valorizzazione Progressivo Applicazione
049300000000     C     calprog       begsr
049400000000     C*
049500000000     C* Inizializzo contatore x tentativi reperimento progressivo transazione
049600000000     C                   z-add     1             wCnt              1 0
049700000000     C*
049800000000     C                   open      tis7prgf
049900000000     C                   dow       wCnt <= 3
050000000000     C*
050100000000     C                   read(e)   tis7prgf
050200000000     C                   if        not %error
050300000000     C                   eval      dwlprg = f_tis7prgf
050400000000     C*
050500000000     C                   move(p)   dwlprg        wrkprg
050600000000     C                   add       1             wrkprg
050700000000     C                   move(p)   wrkprg        dwlprg
050800000000     C                   movel     wTipoServizio dwlprg
050900000000     C*
051000000000     C                   eval      f_tis7prgf = dwlprg
051100000000     C                   update(e) tis7prg0
051200000000     C                   if        not %error
051300000000     C                   eval      wCnt = *hival
051400000000     C                   endif
051500000000     C                   endif
051600000000     C*
051700000000     C                   unlock(e) tis7prgf
051800000000     C                   enddo
051900000000     C                   close     tis7prgf
052000000000     C*
052100000000     C                   if        dwlprg = *blanks OR
052200000000     C                             dwlprg = *zeros
052300000000     C                   eval      prmesito = '2'
052400000000     C                   endif
052500000000     C*
052600000000     C                   endsr
052700000000
052800000000
052900000000
053000000000      /TITLE Storicizzazione post traduzione/copy file
053100000000     C     storicizza    begsr
053200000000     C*
053300000000     C* Effettuo alcune considerazioni sull'esito
053400000000     C                   if        wEsito = ' ' OR wEsito = '0'
053500000000     C                   eval      Esito  = 'S'
053600000000     C                   else
053700000000     C                   if        wEsito = 'Y'
053800000000     C                   eval      Esito  = *blanks
053900090522 xxx C                   eval      dwlprg = *blanks
054000000000     C                   else
054100000000     C                   eval      Esito  = wEsito
054200000000     C                   endif
054300000000     C                   endif
054400000000     C
054500000000     C/EXEC SQL
054600000000     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
054700000000     C/END-EXEC
054800000000     C
054900000000     C/EXEC SQL
055000000000     C+ UPDATE TIVGD00F SET VGDSTO = :ESITO,
055100000000     C+ VGDPRG = :DWLPRG, VGDCNT = VGDCNT + 1 WHERE
055200000000     C+ VGDKSU = :VGDKSU AND VGDTIP = :VGDTIP AND
055300000000     C+ VGDTSC = :VGDTSC AND VGDSTO = '*'
055400000000     C/END-EXEC
055500000000     C
055600000000     C*
055700000000     C* A questo punto la singola transazione è logicamente conclusa x cui sancisco il commit
055800000000     C                   commit
055900000000     C*
056000000000     C* Chiudo il file d wrk
056100000000     C                   if        %open(tivgdtmp)
056200000000     C                   close     tivgdtmp
056300000000     C                   endif
056400000000     C*
056500000000     C* Elimino il contenuto del file d wrk in QTEMP
056600000000     C                   z-add     50            LENGH            15 5
056700000000     C                   movel     *blanks       CMD              50
056800000000     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
056900000000     C                   call      'QCMDEXC'                                    *LANCIA CMD
057000000000     C                   parm                    CMD
057100000000     C                   parm                    LENGH
057200000000     C*
057300000000     C                   endsr
057400000000
057500000000
057600000000
057700000000      /TITLE Storicizzazione x clienti/tipi file NON abilitati
057800000000     C     storicizzaNA  begsr
057900000000     C*
058000000000     C/EXEC SQL
058100000000     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
058200000000     C/END-EXEC
058300000000     C
058400000000     C
058500000000     C/EXEC SQL
058600000000     C+ UPDATE TIVGD00F SET VGDSTO = 'A',
058700000000     C+ VGDPRG = :DWLPRG, VGDCNT = VGDCNT + 1 WHERE
058800000000     C+ VGDKSU = :VGDKSU AND VGDTIP = :VGDTIP AND
058900000000     C+ VGDTSC = :VGDTSC AND VGDSTO = '*'
059000000000     C/END-EXEC
059100000000     C
059200000000     C*
059300000000     C* A questo punto la singola transazione è logicamente conclusa x cui sancisco il commit
059400000000     C                   commit
059500000000     C*
059600000000     C* Chiudo il file d wrk
059700000000     C                   if        %open(tivgdtmp)
059800000000     C                   close     tivgdtmp
059900000000     C                   endif
060000000000     C*
060100000000     C* Elimino il contenuto del file d wrk in QTEMP
060200000000     C                   z-add     50            LENGH            15 5
060300000000     C                   movel     *blanks       CMD              50
060400000000     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
060500000000     C                   call      'QCMDEXC'                                    *LANCIA CMD
060600000000     C                   parm                    CMD
060700000000     C                   parm                    LENGH
060800000000     C*
060900000000     C                   endsr
061000000000
061100000000
061200000000
061300000000
061400000000      /TITLE Eliminazione record nn utili
061500000000     C     elimina       begsr
061600000000     C*
061700000000     C/EXEC SQL
061800000000     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
061900000000     C/END-EXEC
062000000000     C
062100000000     C
062200000000     C/EXEC SQL
062300000000     C+ DELETE FROM TIVGD00F WHERE
062400000000     C+ VGDKSU = :VGDKSU AND VGDTIP = :VGDTIP AND
062500000000     C+ VGDTSC = :VGDTSC AND VGDSTO = '*'
062600000000     C/END-EXEC
062700000000     C
062800000000     C*
062900000000     C* A questo punto la singola transazione è logicamente conclusa x cui sancisco il commit
063000000000     C                   commit
063100000000     C*
063200000000     C* Chiudo il file d wrk
063300000000     C                   if        %open(tivgdtmp)
063400000000     C                   close     tivgdtmp
063500000000     C                   endif
063600000000     C*
063700000000     C* Elimino il contenuto del file d wrk in QTEMP
063800000000     C                   z-add     50            LENGH            15 5
063900000000     C                   movel     *blanks       CMD              50
064000000000     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
064100000000     C                   call      'QCMDEXC'                                    *LANCIA CMD
064200000000     C                   parm                    CMD
064300000000     C                   parm                    LENGH
064400000000     C*
064500000000     C                   endsr
064600000000
064700000000
064800000000
064900000000
065000000000      /TITLE Compongo il testo e spedisco una e-m@ail
065100000000     C     sndeml        begsr
065200000000     C*
065300000000     C* Inizializzo variabili
065400000000     C                   movel     *blanks       wrkEml          253
065500000000     C                   movel     *blanks       wrkEmlMsg      5000
065600000000     C                   movel     *blanks       wrkEmlOgg        44
065700000000     C* Valorizzo i campi della e-m@ail
065800120312     C                   eval      wrkEml='cedalert@brt.it'
065900000000     C                   eval      wrkEmlOgg='VAS - DOWNLOAD - ERROR!!!'
066000000000     C                   eval      wrkEmlMsg=wMessErrore
066100000000     C*
066200000000     C                   call(e)   'TIS701C'
066300000000     C                   parm                    wrkEml
066400000000     C                   parm                    wrkEmlOgg
066500000000     C                   parm                    wrkEmlMsg
066600000000     C*
066700000000     C                   endsr
066800000000
066900000000
067000000000
067100000000      /TITLE Verifico abilitazione cliente/tipo file
067200000000     C     CtrlAbil      begsr
067300000000     C*
067400000000     C* Controllo se il cliente è abilitato al tipo file corrente
067500000000     C     KEYvir02_c    setgt     tivir02l
067600000000     C     KEYvir02_p    readpe    tivir02l
067700000000     C                   if        %eof(tivir02l)
067800000000     C                             or VIRdtf < wOGGI
067900000000     C                   eval      Cli_Abil = *off
068000000000     C                   else
068100000000     C                   eval      Cli_Abil = *on
068200000000     C                   endif
068300000000     C*
068400000000     C                   endsr
068500000000     C***
068600000000
068700000000
068800000000
068900000000     C     *pssr         begsr
069000000000     C*
069100000000     C                   eval      prmesito = '2'
069200000000     C*
069300000000     C                   endsr     '*CANCL'
069400000000     C***
069500000000
069600000000
069700000000
069800000000      /TITLE Routine di *init PGM
069900000000     C*
070000000000     C     *inzsr        begsr
070100000000     C*
070200000000     C* Passaggio parametri
070300000000     C     *entry        plist
070400000000     C                   parm                    prmesito          1
070500000000     C                   parm                    tis781ds
070600000000     C                   parm                    parvgdprg
070700000000     C*
070800000000     C* Calcolo la data corrente
070900000000     C                   move      *date         g08dat
071000000000     C                   movel     *blanks       g08err
071100000000     C                   call      'XSRDA8'
071200000000     C                   parm                    wlbda8
071300000000     C                   if        g08err = '1'
071400000000     C                   z-add     0             wOGGI
071500000000     C                   else
071600000000     C                   z-add     g08inv        wOGGI
071700000000     C                   endif
071800000000     C*
071900000000     C* Definizione chiavi
072000000000     C     KEYvgd01_c    klist
072100000000     C                   kfld                    SQL_TIP
072200000000     C                   kfld                    SQL_KSU
072300000000     C                   kfld                    SQL_TSC
072400090519 xxx C                   kfld                    SQL_PRG
072500000000     C*
072600000000     C     KEYvir02_p    klist
072700000000     C                   kfld                    vgdksu
072800000000     C                   kfld                    vgdtip
072900000000     C*
073000000000     C     KEYvir02_c    klist
073100000000     C                   kfld                    vgdksu
073200000000     C                   kfld                    vgdtip
073300000000     C                   kfld                    wOGGI
073400000000     C                   endsr
073500000000
