000100090123     H dftactgrp(*no) actgrp(*caller) Decedit('0,') Datedit(*dmy.)
000200990907
000300050609     Ftivgd01l  uf a e           k disk    commit
000400050607     Ftivgdtmp  uf a e             disk    rename(tivgd000:tivgdT) prefix(t_)
000500050627     F                                     usropn
000600050606     Ftivtf01l  if   e           k disk
000700050902     Ftivir02l  if   e           k disk
000800050531     Ftis7prgf  uf   e             disk    rename(tis7prgf:tis7prg0)
000900010604     F                                     prefix(f_)
001000010604     F                                     usropn
001100990907
001200010524
001300000710     D* variabili di work
001400050607     D  dsVGD        e ds                  extname(tivgd00f)
001500050607     D  dsVGDtmp     e ds                  extname(tivgd00f) prefix(t_)
001600050907     D  trul47ds     e ds
001700060804     D  tis781ds     e ds
001800080903     D  tis781dflo   e ds
001900060804     D  dvgdflo      e ds
002000050531     D  wFileInput     s                   like(vtffid) inz(*blanks)
002100050531     D  wFileStor      s                   like(vtffis) inz(*blanks)
002200050902     D  wTipoServizio  s                   like(virfi1) inz(*blanks)
002300050531     D  dwlprg         s             10    inz(*all'0')
002400050531     D  wrkprg         s              8  0 inz(*zeros)
002500050531     D  wEsito         s              1    inz(*blanks)
002600050531     D  wMessErrore    s           2000    inz(*blanks)
002700050907     D  savTipF        s                   like(vgdtip) inz(*blanks)
002800050921     d  wOGGI          s              8  0
002900050921     d  Cli_Abil       s              1    inz(*off)
003000060804     d  wRecOk         s              1    inz(*blanks)
003100060804     d  parvgdprg      s                   like(vgdprg)
003200080903     d  parvgdflotfsF  s                   like(§781flotfs)
003300080903     d  parvgdflotfsT  s                   like(§781flotfs)
003400080903     d  parvgdflotfn   s                   like(§781flotfn)
003500050609
003600050609     D* DS d wrk
003700050609     D  dsSQLVGD       ds
003800050609     D   SQL_TIP                           like(vgdtip)
003900050609     D   SQL_KSU                           like(vgdksu)
004000050609     D   SQL_TSC                           like(vgdtsc)
004100050609
004200050921     D wlbda8          ds                  inz
004300050921     D  g08dat                 1      8  0
004400050921     D  g08inv                 9     16  0
004500050921     D  g08err                17     17
004600050921     D  g08tgi                18     22  0
004700010525
004800010525
004900010525
005000010525     C*-----------------------------------------------------------------------------
005100080115     C*
005200080115     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005300080115     C
005400080115     C/EXEC SQL
005500080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
005600080115     C/END-EXEC
005700080115     C*
005800050628     C*
005900050628     C* In ingresso elimino subito il contenuto del file d wrk in QTEMP
006000050628     C                   z-add     50            LENGH            15 5
006100050628     C                   movel     *blanks       CMD              50
006200050628     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
006300050628     C                   call      'QCMDEXC'                                    *LANCIA CMD
006400050628     C                   parm                    CMD
006500050628     C                   parm                    LENGH
006600080903     C*
006700080903     C* Reperisco i flag operativi
006800080903     C                   eval      tis781dflo = §781FLO
006900080903     C*
007000080903     C* Gestione flag operativo "CODICE REGOLA"
007100080903     C                   if        §781FLOTFS = *blanks
007200080903     C                   eval      parvgdflotfsF = *loval
007300080903     C                   eval      parvgdflotfsT = *hival
007400080903     C                   else
007500080903     C                   eval      parvgdflotfsF = §781FLOTFS
007600080903     C                   eval      parvgdflotfsT = §781FLOTFS
007700080903     C                   endif
007800080903     C                   eval      parvgdflotfn  = §781FLOTFN
007900050609     C*
008000050609     C/EXEC SQL
008100050609     C+ DECLARE C1 CURSOR FOR
008200050609     C+ SELECT DISTINCT VGDTIP, VGDKSU, VGDTSC FROM TIVGD00F
008300080903     C+ WHERE VGDSTO = ' '
008400080903     C+ AND VGDTIP >= :parvgdflotfsF AND VGDTIP <= :parvgdflotfsT
008500080903     C+ AND VGDTIP <> :parvgdflotfn
008600050907     C+ ORDER BY VGDTIP
008700050609     C+ FOR READ ONLY
008800050609     C/END-EXEC
008900050609     C
009000050609     C/EXEC SQL
009100050609     C+ OPEN C1
009200050609     C/END-EXEC
009300050609     C
009400050609     C/EXEC SQL
009500050609     C+ FETCH C1 INTO :dsSQLVGD
009600050609     C/END-EXEC
009700050609     C*
009800050609     C                   dow       sqlcod = *zeros
009900050907     C*
010000060804     C* Verifico innanzitutto se il tipo file corrente è in stato elaborabile
010100050907     C                   if        SQL_TIP <> savTipF
010200050907     C*
010300050907     C* Inizializzo indicatore che subordina l'elaborazione d ogni tipo file/cliente/tipo scarico
010400050907     C                   setoff                                       55
010500050907     C*
010600050907     C* Salvo il tipo file corrente
010700050907     C                   eval      savTipF = SQL_TIP
010800050907     C*
010900050907     C* Verifico se presente il blocco elaborazione TIVGD x tipo file corrente
011000050907     C                   clear                   trul47ds
011100050907     C                   eval      d47opz  = 'C'
011200050907     C                   eval      d47tip  = SQL_TIP
011300050907     C                   call      'TRUL47R'
011400050907     C                   parm                    trul47ds
011500050907     C*
011600050907     C* Testo il respondo ottenuto
011700050907     C                   if        d47sts = *blanks
011800050907     C                   seton                                        55
011900050907     C                   endif
012000050907     C                   endif
012100050907     C*
012200050907     C* Se elaborazione consentita => proseguo
012300050907  IF1C                   if        *in55 = *on
012400050627     C*
012500050627     C* Apro il file d wrk
012600050628     C                   if        not %open(tivgdtmp)
012700050627     C                   open      tivgdtmp
012800050628     C                   endif
012900050609     C*
013000050609     C* Inizializzo flag x verifica lettura record
013100050609     C                   movel     'N'           wRecordLetti      1
013200050609     C*
013300050609     C* Leggo la vista logica del file generico download x ogni chiave d rottura traduzione
013400050609     C* ovvero TIP/KSU/TSC
013500050609     C     KEYvgd01_c    setll     tivgd01l
013600050609     C                   if        %equal(tivgd01l)
013700010525     C*
013800050531     C* 1° lettura file fuori ciclo
013900050609     C     KEYvgd01_c    reade     tivgd01l
014000050531     C*
014100050531     C* Leggo tutta la vista logica
014200050609     C                   dow       not %eof(tivgd01l)
014300060804     C*
014400060804     C* Gestione ENDJOB.
014500060804     C                   shtdn                                        60
014600060804     C                   if        not *in60
014700060804     C*
014800060804     C* Verifico innanzitutto se richieste parzializzazionei in ingresso
014900060804     C                   movel     'S'           wRecOk
015000060804     C                   exsr      chkrec
015100060804     C*
015200060804     C* Se record valido proseguo l'elaborazione
015300060804     C                   if        wRecOk = 'S'
015400050607     C*
015500050607     C* Valorizzo flag x verifica lettura record
015600050607     C                   movel     'S'           wRecordLetti
015700050609     C*
015800050609     C* Passo i dati dal file "buono" al file d wrk in QTEMP
015900050609     C                   eval      dsVGDtmp = dsVGD
016000050609     C                   write     tivgdT
016100050609     C                   eval      vgdsto = '*'
016200050609     C                   update    tivgd000
016300060804     C                   endif
016400050608     C*
016500050608     C* Gestisco lo shutdown
016600050608     C                   else
016700050609     C                   eval      prmesito = '2'
016800060804     C                   leave
016900050607     C                   endif
017000050531     C*
017100050531     C* Proseguo con la lettura del file Vas Generico Download (TIVGD)
017200050609     C     KEYvgd01_c    reade     tivgd01l
017300050609     C                   enddo
017400050921     C*
017500050921     C* Effettuo test particolare x tipo file 'VP' x verifica abilitazione cliente
017600060804     C  N60              if        vgdtip = 'VP'
017700060804     C                   exsr      CtrlAbil
017800050921     C                   else
017900050921     C                   eval      Cli_Abil = *on
018000050921     C                   endif
018100050609     C*
018200050609     C* Effettuo la traduzione
018300060804     C  N60              if        Cli_Abil = *on
018400060804     C                   exsr      traduzione
018500050921     C                   else
018600060804     C                   exsr      elimina
018700060804     C***                exsr      storicizzaNA
018800050921     C                   endif
018900050609     C*
019000050609     C                   endif
019100050907     C*
019200050907  E 1C                   endif
019300060804     C*
019400060804     C* Verifico eventuale shutdown
019500060804     C   60              leave
019600050609     C*
019700050609     C/EXEC SQL
019800050609     C+ FETCH C1 INTO :dsSQLVGD
019900050609     C/END-EXEC
020000050609     C                   enddo
020100050609     C*
020200050609     C/EXEC SQL
020300050609     C+ CLOSE C1
020400050609     C/END-EXEC
020500050609     C
020600050607     C*
020700050607     C                   seton                                        lr
020800010525     C*-----------------------------------------------------------------------------
020900060804
021000060804
021100060804
021200060804      /TITLE Verifica validità record
021300060804     C     chkrec        begsr
021400080905     C*
021500080905     C* Scompongo i flag operativi del record
021600080905     C                   eval      dvgdflo = vgdFLO
021700060804     C*
021800060804     C* Se ricevuti in input parametri d selezione effettuo considerazioni
021900060804     C                   if        tis781ds <> *blanks
022000060804     C*
022100060804     C* Verifico se richiesto specifico tipo file
022200060804     C                   if        §781TIP <> *blanks
022300060804     C                   if        vgdTIP  <> §781TIP
022400060804     C                   eval      wRecOk = 'N'
022500060804     C                   endif
022600060804     C                   endif
022700060804     C*
022800060804     C* Verifico se richiesto specifico cliente unificante
022900060804     C                   if        §781KSU <> *blanks
023000060804     C                   if        vgdKSU  <> §781KSU
023100060804     C                   eval      wRecOk = 'N'
023200060804     C                   endif
023300060804     C                   endif
023400060804     C*
023500060804     C* Verifico se richiesto specifico tipo scarico dati
023600060804     C                   if        §781TSC <> *blanks
023700060804     C                   if        vgdTSC  <> §781TSC
023800060804     C                   eval      wRecOk = 'N'
023900060804     C                   endif
024000060804     C                   endif
024100060804     C*
024200060804     C* Verifico se richiesto specifico data inserimento dati
024300060804     C                   if        §781DAT <> *zeros
024400060804     C                   if        vgdDAT  <> §781DAT
024500060804     C                   eval      wRecOk = 'N'
024600060804     C                   endif
024700060804     C                   endif
024800060804     C*
024900060804     C* Verifico se richiesto specifico programma scrivente
025000060804     C                   if        §781PGM <> *blanks
025100060804     C                   if        vgdPGM  <> §781PGM
025200060804     C                   eval      wRecOk = 'N'
025300060804     C                   endif
025400060804     C                   endif
025500060804     C*
025600060804     C* Verifico se richiesto specifico progressivo transazione
025700060804     C                   if        §781PRG <> *blanks
025800060804     C                   if        vgdPRG  <> §781PRG
025900060804     C                   eval      wRecOk = 'N'
026000060804     C                   endif
026100060804     C                   endif
026200080905     C*
026300080905     C* Verifico il tipo elaborazione richiesta
026400080905     C                   if        §vgdfela <> §781FLOELA
026500080905     C                   eval      wRecOk = 'N'
026600080905     C                   endif
026700060804     C*
026800060804     C                   else
026900060804     C*
027000060804     C* Se nn ricevuti in input parametri d selezione devo elaborare solo i record standard
027100060804     C                   if        §vgdfela <> *blanks
027200060804     C                   eval      wRecOk = 'N'
027300060804     C                   endif
027400060804     C                   endif
027500060804     C*
027600060804     C                   endsr
027700050609
027800050609
027900050609
028000050609      /TITLE Traduzione/copy file
028100050609     C     traduzione    begsr
028200050609     C*
028300050609     C* Elaboro solo se effettivamente c'erano record
028400050609     C                   if        wRecordLetti = 'S'
028500050609     C*
028600050609     C* Inizializzo variabili d wrk
028700050609     C                   eval      wEsito      = *blanks
028800050609     C                   eval      wMessErrore = *blanks
028900050609     C*
029000050609     C* Reperisco il file "da tradurre" legato al tipo file corrente
029100050609     C                   exsr      repvtf
029200050902     C*
029300050902     C* Se NN già presente 1 ID transazione stacco 1 progressivo univoco x la transazione corrente
029400050902     C                   if        vgdprg = *blanks
029500050902     C                   exsr      calprog
029600050902     C                   else
029700050902     C                   eval      dwlprg = vgdprg
029800050902     C                   endif
029900060804     C*
030000060804     C* Ritorno l'ID univoco transazione
030100060804     C                   eval      parvgdprg = dwlprg
030200050609     C*
030300050609     C* Se errore NN elaboro e flaggo record letti ad errore (VGDSTO = '2')
030400050609     C                   if        wEsito = '2'
030500050609     C                   exsr      storicizza
030600050609     C*
030700050609     C* Se invece fin qui tutto ok => proseguo con elaborazione
030800050609     C                   else
030900050609     C*
031000050609     C* Verifico il tipo scarico dati...
031100050609     C*
031200050609     C* ...se scarico dati è WW => innesco iter traduzione "standard"
031300050609     C                   if        vgdtsc = 'WW'
031400050609     C                   eval      p_mbrin  = 'M'+%subst(vgdksu:2:7)
031500060616     C  N77              eval      p_ope = 'WWI'
031600060616     C   77              eval      p_ope = 'WWN'
031700050609     C                   call      'TIS781C2'
031800050609     C                   parm      vgdksu        p_ksu             8
031900050609     C                   parm      vgdtip        p_tip             2
032000050609     C                   parm      vgdtsc        p_tsc             2
032100050609     C                   parm      dwlprg        p_prog           10
032200050609     C                   parm      wFileInput    p_filein         10
032300050609     C                   parm      wFileStor     p_filesto        10
032400050609     C                   parm                    p_mbrin          10
032500060616     C                   parm                    p_ope             3
032600070207     C                   parm      §781OUT       p_out            10
032700050609     C                   parm      *blanks       Esito             1
032800050609     C*
032900050609     C* In uscita traduzione/copy file effettuo la storicizzazione
033000060320     C                   eval      wEsito = Esito
033100050609     C                   exsr      storicizza
033200050609     C*
033300050609     C                   else
033400060118     C*
033500060118     C* ...se scarico dati è W2 => innesco iter traduzione "standard" + copyfile sul relativo 00T
033600060118     C                   if        vgdtsc = 'W2'
033700060118     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
033800060118     C* CPYF
033900060118     C                   call      'TIS781C2'
034000060118     C                   parm                    p_ksu
034100060118     C                   parm                    p_tip
034200060118     C                   parm                    p_tsc
034300060118     C                   parm                    p_prog
034400060118     C                   parm      wFileInput    p_filein
034500060118     C                   parm      *blanks       p_filesto
034600060118     C                   parm                    p_mbrin
034700060118     C                   parm      'CFI'         p_ope
034800070207     C                   parm      §781OUT       p_out
034900060118     C                   parm      *blanks       Esito
035000060118     C* TRADUZIONE WW
035100060616     C  N77              eval      p_ope = 'WWI'
035200060616     C   77              eval      p_ope = 'WWN'
035300060118     C                   call      'TIS781C2'
035400070207     C                   parm      vgdksu        p_ksu
035500070207     C                   parm      vgdtip        p_tip
035600070207     C                   parm      vgdtsc        p_tsc
035700070207     C                   parm      dwlprg        p_prog
035800070207     C                   parm      wFileInput    p_filein
035900070207     C                   parm      wFileStor     p_filesto
036000070207     C                   parm                    p_mbrin
036100070207     C                   parm                    p_ope
036200070207     C                   parm      §781OUT       p_out
036300070207     C                   parm      *blanks       Esito
036400060118     C*
036500060118     C* In uscita traduzione/copy file effettuo la storicizzazione x tipo file/cliente/tipo scarico
036600060320     C                   eval      wEsito = Esito
036700060118     C                   exsr      storicizza
036800060118     C*
036900060118     C                   else
037000060118     C*
037100060118     C* ...se scarico dati NON è ne WW ne W2 => effettuo semplicemente il copy file sul relativo 00T
037200050609     C                   eval      p_mbrin = 'M'+%subst(vgdksu:2:7)
037300050609     C                   call      'TIS781C2'
037400050609     C                   parm                    p_ksu
037500050609     C                   parm                    p_tip
037600050609     C                   parm                    p_tsc
037700050609     C                   parm                    p_prog
037800050609     C                   parm      wFileInput    p_filein
037900050609     C                   parm      *blanks       p_filesto
038000050609     C                   parm                    p_mbrin
038100050609     C                   parm      'CFI'         p_ope
038200070207     C                   parm      §781OUT       p_out
038300050609     C                   parm      *blanks       Esito
038400050609     C*
038500050609     C* In uscita traduzione/copy file effettuo la storicizzazione x tipo file/cliente/tipo scarico
038600060320     C                   eval      wEsito = Esito
038700050609     C                   exsr      storicizza
038800060118     C*
038900050609     C                   endif
039000060118     C                   endif
039100050609     C                   endif
039200050609     C                   endif
039300050609     C*
039400050609     C                   endsr
039500010525
039600010525
039700010525
039800050531      /TITLE Routine di reperimento file "da tradiurre" legata al tipo file corrente
039900010525     C*
040000050531     C     repvtf        begsr
040100050531     C*
040200050902     C                   eval      wFileInput    = *blanks
040300050902     C                   eval      wFileStor     = *blanks
040400050902     C                   eval      wTipoServizio = 'DG'
040500050531     C*
040600050609     C     vgdtip        chain     tivtf01l
040700050531     C                   if        %found(tivtf01l)
040800050531     C                   eval      wFileInput = VTFFID
040900050531     C                   eval      wFileStor  = VTFFIS
041000050902     C*
041100050921     C     KEYvir02_p    chain     tivir02l
041200050902     C                   if        %found(tivir02l)
041300050902     C                   eval      wTipoServizio = VIRFI1
041400050902     C                   if        VIRFID <> *blanks
041500050902     C                   eval      wFileInput = VIRFID
041600050902     C                   endif
041700050902     C                   if        VIRFIS <> *blanks
041800050902     C                   eval      wFileStor  = VTFFIS
041900050902     C                   endif
042000050902     C                   endif
042100050531     C                   else
042200050531     C*
042300120306     C* Se tipo file corrente NN trovato in anagrafica => mando e-mail a ced@brt.it
042400050902     C* e fleggo tali record a stato storicizzazione (STO) = 2
042500050531     C                   eval      wMessErrore = 'VERIFICARE IL FILE DOWNLOAD '+
042600050531     C                             'GENERICO TIVGD00F'                 +':/N'+
042700050531     C                             ' '                                 +':/N'+
042800050531     C                             'RIFERIMENTI'                       +':/N'+
042900050609     C                             'Cliente: ' + vgdksu                +':/N'+
043000050609     C                             'Tipo file: ' + vgdtip              +':/N'+
043100050531     C                             'Progressivo: ' + dwlprg            +':/N'+
043200050531     C                             ' '                                 +':/N'+
043300050531     C                             'MESSAGGIO'                         +':/N'+
043400050531     C                             'ERRORE GRAVE: tipo file ERRATO!!!'
043500050531     C                   exsr      sndeml
043600050531     C                   eval      wEsito = '2'
043700050531     C                   endif
043800060616     C*
043900060616     C                   setoff                                       77
044000060616     C                   if        %trim(wFileInput) = '*'
044100060616     C                   eval      wFileInput = 'TIVGDTMP'
044200060616     C                   seton                                        77
044300060616     C                   endif
044400060616     C                   if        %trim(wFileStor)  = '*'
044500060616     C                   eval      wFileStor  = 'TIVGDTMP'
044600060616     C                   seton                                        77
044700060616     C                   endif
044800010525     C*
044900010525     C                   endsr
045000010525
045100010525
045200010525
045300010525      /TITLE Valorizzazione Progressivo Applicazione
045400050531     C     calprog       begsr
045500080115     C*
045600080115     C* Inizializzo contatore x tentativi reperimento progressivo transazione
045700080115     C                   z-add     1             wCnt              1 0
045800080115     C*
045900080115     C                   open      tis7prgf
046000080115     C                   dow       wCnt <= 3
046100010604     C*
046200010525     C                   read(e)   tis7prgf
046300010525     C                   if        not %error
046400050531     C                   eval      dwlprg = f_tis7prgf
046500010525     C*
046600050531     C                   move(p)   dwlprg        wrkprg
046700010525     C                   add       1             wrkprg
046800050531     C                   move(p)   wrkprg        dwlprg
046900050902     C                   movel     wTipoServizio dwlprg
047000010525     C*
047100050531     C                   eval      f_tis7prgf = dwlprg
047200080115     C                   update(e) tis7prg0
047300080115     C                   if        not %error
047400080115     C                   eval      wCnt = *hival
047500080115     C                   endif
047600010525     C                   endif
047700010604     C*
047800080115     C                   unlock(e) tis7prgf
047900080115     C                   enddo
048000010604     C                   close     tis7prgf
048100080115     C*
048200080115     C                   if        dwlprg = *blanks OR
048300080115     C                             dwlprg = *zeros
048400080115     C                   eval      prmesito = '2'
048500080115     C                   endif
048600010525     C*
048700010525     C                   endsr
048800050606
048900050606
049000050606
049100050607      /TITLE Storicizzazione post traduzione/copy file
049200050609     C     storicizza    begsr
049300050609     C*
049400050609     C* Effettuo alcune considerazioni sull'esito
049500050609     C                   if        wEsito = ' ' OR wEsito = '0'
049600050609     C                   eval      Esito  = 'S'
049700050609     C                   else
049800060320     C                   if        wEsito = 'Y'
049900060320     C                   eval      Esito  = *blanks
050000060320     C                   else
050100050609     C                   eval      Esito  = wEsito
050200060320     C                   endif
050300050609     C                   endif
050400050609     C
050500050609     C/EXEC SQL
050600080116     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
050700050609     C/END-EXEC
050800050609     C
050900050609     C/EXEC SQL
051000050609     C+ UPDATE TIVGD00F SET VGDSTO = :ESITO,
051100050609     C+ VGDPRG = :DWLPRG, VGDCNT = VGDCNT + 1 WHERE
051200050609     C+ VGDKSU = :VGDKSU AND VGDTIP = :VGDTIP AND
051300050609     C+ VGDTSC = :VGDTSC AND VGDSTO = '*'
051400050609     C/END-EXEC
051500080116     C
051600050609     C*
051700050609     C* A questo punto la singola transazione è logicamente conclusa x cui sancisco il commit
051800080116     C                   commit
051900050627     C*
052000050627     C* Chiudo il file d wrk
052100050628     C                   if        %open(tivgdtmp)
052200050627     C                   close     tivgdtmp
052300050628     C                   endif
052400050627     C*
052500050628     C* Elimino il contenuto del file d wrk in QTEMP
052600050627     C                   z-add     50            LENGH            15 5
052700050627     C                   movel     *blanks       CMD              50
052800050627     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
052900050627     C                   call      'QCMDEXC'                                    *LANCIA CMD
053000050627     C                   parm                    CMD
053100050627     C                   parm                    LENGH
053200050606     C*
053300050606     C                   endsr
053400050921
053500050921
053600050921
053700050921      /TITLE Storicizzazione x clienti/tipi file NON abilitati
053800050921     C     storicizzaNA  begsr
053900050921     C*
054000080116     C/EXEC SQL
054100080116     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
054200080116     C/END-EXEC
054300050921     C
054400080116     C
054500050921     C/EXEC SQL
054600050921     C+ UPDATE TIVGD00F SET VGDSTO = 'A',
054700050921     C+ VGDPRG = :DWLPRG, VGDCNT = VGDCNT + 1 WHERE
054800050921     C+ VGDKSU = :VGDKSU AND VGDTIP = :VGDTIP AND
054900050921     C+ VGDTSC = :VGDTSC AND VGDSTO = '*'
055000050921     C/END-EXEC
055100080116     C
055200050921     C*
055300050921     C* A questo punto la singola transazione è logicamente conclusa x cui sancisco il commit
055400080116     C                   commit
055500050921     C*
055600050921     C* Chiudo il file d wrk
055700050921     C                   if        %open(tivgdtmp)
055800050921     C                   close     tivgdtmp
055900050921     C                   endif
056000050921     C*
056100050921     C* Elimino il contenuto del file d wrk in QTEMP
056200050921     C                   z-add     50            LENGH            15 5
056300050921     C                   movel     *blanks       CMD              50
056400050921     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
056500050921     C                   call      'QCMDEXC'                                    *LANCIA CMD
056600050921     C                   parm                    CMD
056700050921     C                   parm                    LENGH
056800050921     C*
056900050921     C                   endsr
057000050921
057100050921
057200050921
057300050921
057400050921      /TITLE Eliminazione record nn utili
057500050921     C     elimina       begsr
057600050921     C*
057700080116     C/EXEC SQL
057800080116     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
057900080116     C/END-EXEC
058000080116     C
058100050921     C
058200050921     C/EXEC SQL
058300050921     C+ DELETE FROM TIVGD00F WHERE
058400050921     C+ VGDKSU = :VGDKSU AND VGDTIP = :VGDTIP AND
058500050921     C+ VGDTSC = :VGDTSC AND VGDSTO = '*'
058600050921     C/END-EXEC
058700080116     C
058800050921     C*
058900050921     C* A questo punto la singola transazione è logicamente conclusa x cui sancisco il commit
059000050921     C                   commit
059100050921     C*
059200050921     C* Chiudo il file d wrk
059300050921     C                   if        %open(tivgdtmp)
059400050921     C                   close     tivgdtmp
059500050921     C                   endif
059600050921     C*
059700050921     C* Elimino il contenuto del file d wrk in QTEMP
059800050921     C                   z-add     50            LENGH            15 5
059900050921     C                   movel     *blanks       CMD              50
060000050921     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
060100050921     C                   call      'QCMDEXC'                                    *LANCIA CMD
060200050921     C                   parm                    CMD
060300050921     C                   parm                    LENGH
060400050921     C*
060500050921     C                   endsr
060600010525
060700010525
060800050531
060900050531
061000050531      /TITLE Compongo il testo e spedisco una e-m@ail
061100050531     C     sndeml        begsr
061200050531     C*
061300050531     C* Inizializzo variabili
061400050531     C                   movel     *blanks       wrkEml          253
061500050531     C                   movel     *blanks       wrkEmlMsg      5000
061600050531     C                   movel     *blanks       wrkEmlOgg        44
061700050531     C* Valorizzo i campi della e-m@ail
061800120306     C                   eval      wrkEml='cedalert@brt.it'
061900050531     C                   eval      wrkEmlOgg='VAS - DOWNLOAD - ERROR!!!'
062000050531     C                   eval      wrkEmlMsg=wMessErrore
062100050531     C*
062200050531     C                   call(e)   'TIS701C'
062300050531     C                   parm                    wrkEml
062400050531     C                   parm                    wrkEmlOgg
062500050531     C                   parm                    wrkEmlMsg
062600050531     C*
062700050531     C                   endsr
062800050921
062900050921
063000050921
063100050921      /TITLE Verifico abilitazione cliente/tipo file
063200050921     C     CtrlAbil      begsr
063300050921     C*
063400050921     C* Controllo se il cliente è abilitato al tipo file corrente
063500050921     C     KEYvir02_c    setgt     tivir02l
063600050921     C     KEYvir02_p    readpe    tivir02l
063700050921     C                   if        %eof(tivir02l)
063800050921     C                             or VIRdtf < wOGGI
063900050921     C                   eval      Cli_Abil = *off
064000050921     C                   else
064100050921     C                   eval      Cli_Abil = *on
064200050921     C                   endif
064300050921     C*
064400050921     C                   endsr
064500050921     C***
064600050531
064700050531
064800050607
064900050607     C     *pssr         begsr
065000050607     C*
065100050607     C                   eval      prmesito = '2'
065200050607     C*
065300050607     C                   endsr     '*CANCL'
065400050607     C***
065500010525
065600010525
065700010525
065800000714      /TITLE Routine di *init PGM
065900000714     C*
066000050531     C     *inzsr        begsr
066100050607     C*
066200050609     C* Passaggio parametri
066300050607     C     *entry        plist
066400050607     C                   parm                    prmesito          1
066500060804     C                   parm                    tis781ds
066600060804     C                   parm                    parvgdprg
066700050921     C*
066800050921     C* Calcolo la data corrente
066900050921     C                   move      *date         g08dat
067000050921     C                   movel     *blanks       g08err
067100050921     C                   call      'XSRDA8'
067200050921     C                   parm                    wlbda8
067300050921     C                   if        g08err = '1'
067400050921     C                   z-add     0             wOGGI
067500050921     C                   else
067600050921     C                   z-add     g08inv        wOGGI
067700050921     C                   endif
067800050609     C*
067900050609     C* Definizione chiavi
068000050609     C     KEYvgd01_c    klist
068100050609     C                   kfld                    SQL_TIP
068200050609     C                   kfld                    SQL_KSU
068300050609     C                   kfld                    SQL_TSC
068400050902     C*
068500050902     C     KEYvir02_p    klist
068600050902     C                   kfld                    vgdksu
068700050902     C                   kfld                    vgdtip
068800050921     C*
068900050921     C     KEYvir02_c    klist
069000050921     C                   kfld                    vgdksu
069100050921     C                   kfld                    vgdtip
069200050921     C                   kfld                    wOGGI
069300000714     C                   endsr
069400060804
