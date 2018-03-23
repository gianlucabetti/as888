000100050310      /TITLE Upload ins. manuale da Internet: lettura TIVPI00F
000200170321     H DFTACTGRP(*NO) ACTGRP('VASENTRY')
000300990907
000400170118     Ftivpi02i  uf   e           k disk    commit
000500051108     Ftivin00r  o    e             disk    usropn commit
000600010618     Ftianv02l  if   e           k disk
000700090319     Ftivlr00f  o    e             disk    commit
000800010604     Ftis7prgf  uf   e             disk    rename(tis7prgf:tis7prg0)
000900010604     F                                     prefix(f_)
001000170321     Ftigcp52l  uf   e           k disk    commit
001100990907
001200010524
001300170118     D* VARIABILI DI WRK
001400000714     D  wrkoggiiso     s               d
001500000714     D  wrkoggi        s              8  0
001600000714     D  wrkora         s              6  0
001700000714     D  datime         s             14
001800010613     D  depisv         s                   like(vpiisv) inz(*blanks)
001900010525     D  deptip         s                   like(vpitip) inz(*blanks)
002000010525     D  depksu         s                   like(vpiksu) inz(*blanks)
002100010613     D  depsun         s                   like(vpisun) inz(*blanks)
002200170118     D  wISV           s                   like(vpiisv) inz(*blanks)
002300010525     D  ope            s              1
002400010525     D  uplprg         s             10    inz(*all'0')
002500010525     D  wrkprg         s              8  0 inz(*zeros)
002600010525     D  wrkmbr         s             10    inz(*blanks)
002700010525     D  flgOK          s              1
002800010710     D  tipFile        s              3
002900010921     D  esito          s              1    inz('0')
003000010525
003100030211
003200170118     D* DS ESTERNE
003300170404     D  DVPIXXX1     e ds                  qualified
003400170321     D  fnvareiir    e ds
003500170321     D  DGNPDMCxxx   e ds                  extname(DGNPDMC900)
003600010525
003700010525
003800010525
003900010525     C*-----------------------------------------------------------------------------
004000051108     C                   setoff                                       lr
004100170118     C*
004200170118     C* Avvio il monitoring del intero flusso
004300170118     C                   monitor
004400010525     C*
004500051108     C* Leggo tutta la vista logica
004600010525     C                   dow       not *inlr
004700051108     C*
004800170118     C                   read      tivpi02i                               lr
004900051108     C*
005000051108     C* Se presenti record da leggere
005100051108     C                   if        not *inlr
005200991105     C*
005300991111     C* Gestione ENDJOB.
005400991111     C                   shtdn                                        lr
005500010525     C                   if        not *inlr
005600061106     C*
005700061106     C* Verifica: se trattasi d record relativi al cliente unificante 1020001 => delete fissa del
005800061106     C* record in quanto trattasi d servizio d test ad uso POC
005900170118     C                   if        vpiISV = 'TT' AND
006000170118     C                             (vpiKSU = '01020001'  OR
006100170118     C                              vpiKSU = '01020004'  OR
006200170118     C                              vpiSUN = '000000004')
006300100507     C                   delete    tivpi000
006400061106     C                   else
006500061106     C*
006600010525     C* ... calcolo un progressivo (lungo 7 per differenziarlo dagli altri metodi di scambio dati)
006700010525     C                   exsr      calprog
006800010525     C* ... eseguo le operazioni iniziali di aggiuta/allocazione/sostituzione mebro nel TIVIN
006900010525     C                   eval      ope = 'I'                                    * I = Inizio
007000010525     C                   call(e)   'TIS780C1'
007100010525     C                   parm                    wrkmbr
007200010525     C                   parm                    vpiksu
007300010525     C                   parm                    ope
007400010921     C                   parm      '0'           esito
007500010921     C                   if        not %error and esito = '0'
007600010525     C                   open(e)   tivin00r
007700010525     C                   if        not %error
007800010525     C                   eval      flgOK = '1'
007900170118     C                   eval      depisv = vpiisv
008000010525     C                   eval      deptip = vpitip
008100010525     C                   eval      depksu = vpiksu
008200010613     C                   eval      depsun = vpisun
008300010525     C*
008400170519     C                   setoff                                       55
008500170118     C                   dow       depisv = vpiisv and
008600170118     C                             deptip = vpitip and
008700010525     C                             depksu = vpiksu and
008800100511     C                             depsun = vpisun and
008900010525     C                             not *inlr
009000010525     C* Procedo elaborando il file TIVPI e scrivendo nel TIVIN fino a rottura Tipo File/Cliente
009100010525     C                   exsr      exevin
009200170118     C                   read      tivpi02i                               lr
009300010525     C                   enddo
009400010525     C*
009500010525     C                   endif
009600010525     C                   endif
009700010525     C*
009800010525     C* Se tutto OK a rottura scrivo il record di log dell'UPLOAD (TIVLR),
009900010525     C* chiudo comunque il TIVIN ed elimino le sostituzioni/allocazioni sui membri
010000170519     C                   if        flgOK = '1' AND *in55
010100010525     C                   exsr      wrilog
010200010525     C                   endif
010300010525     C                   close(e)  tivin00r
010400010525     C                   eval      ope = 'F'                                    * F = Fine
010500010525     C                   call(e)   'TIS780C1'
010600010525     C                   parm                    wrkmbr
010700010525     C                   parm                    vpiksu
010800010525     C                   parm                    ope
010900010525     C                   eval      flgOK = '0'
011000061106     C*
011100061106     C                   endif
011200051108     C*
011300051108     C* Ad ogni transazione x Tipo File/Cliente => emetto operazione d COMMIT
011400170321     C                   COMMIT(e)
011500051108     C*
011600051108     C* Quindi mi riposiziono sull'ultima rottura d codice appena occorsa
011700170519     C   55keyVPI02_P    setll     tivpi02i
011800051108     C*
011900051108     C                   endif
012000051108     C                   endif
012100010525     C*
012200010525     C                   enddo
012300170118     C*
012400170118     C* Gestisco eventuale errore
012500170118     C                   on-error
012600170118     C*
012700170118     C* Se errore => emetto operazione d ROLLBACK
012800170321     C                   ROLBK
012900170118     C*
013000170118     C* Arresto il monitoring
013100170118     C                   endmon
013200010525     C*-----------------------------------------------------------------------------
013300010525
013400010525
013500010525
013600010525      /TITLE Routine di scrittura TIVIN e cancellaziono TIVPI
013700010525     C*
013800010525     C     exevin        begsr
013900170321     C*
014000170404     C                   eval      DVPIXXX1 = vpiXXX
014100010525     C*
014200010525     C                   clear                   tivin000
014300170328     C                   setoff                                       33
014400170519     C*
014500170519     C* Elaboro il record unicamente se STS è *blanks
014600170519     C                   if        DVPIXXX1.DVPISTS = *blanks
014700170321     C*
014800170321     C* Testo se l'immissione da internet è relativa ad una disposizione giacenza
014900170321     C                   if        vpitip = 'TR'
015000170321     C*
015100170321     C* Gestione particolare per flag indicante Tipo Accesso internet
015200170321     C                   eval      fnvareiir = vpidta
015300170321     C                   if        vhrDMCTA <> *blanks
015400170321     C                   if        vhrNPR = ' 900' or
015500170321     C                             vhrNPR = ' 910' or
015600170321     C                             vhrNPR = ' 911' or
015700170321     C                             vhrNPR = ' 914' or
015800170321     C                             vhrNPR = ' 916' or
015900170321     C                             vhrNPR = ' 920'
016000170328     C*
016100170328     C* Gestione valori particolari per "Tipo Accesso Internet"
016200170328     C                   select
016300170328     C                   when      vhrDMCTA  = 'B'
016400170328     C                   eval      UTENTEWEB = 'D'
016500170328     C                   when      vhrDMCTA  = 'C' or vhrDMCTA  = 'K'
016600170328     C                   eval      UTENTEWEB = 'M'
016700170328     C                   other
016800170328     C                   seton                                        33
016900170404     C                   eval      DVPIXXX1.DVPISTS = 'E'
017000170328     C                   eval      vpiXXX = DVPIXXX1
017100170328     C                   endsl
017200170328     C*
017300170328     C  N33              eval      COSTANTE  = vhrDMC
017400170328     C  N33              eval      vhrDMC = DGNPDMCxxx
017500170328     C  N33              eval      vpidta = fnvareiir
017600170321     C                   endif
017700170321     C                   endif
017800170321     C                   endif
017900100510     C*
018000100510     C* Forzatura personalizzazione x LIMONI (utente 0013580R)
018100100510     C                   if        vpisun = '000090987' AND
018200100510     C                             vpitip = 'TO'
018300100510     C                   eval      %subst(vpidta:585:1) = 'O'                   * sempre paga ORDIN.
018400100510     C                   endif
018500100510     C*
018600170328     C* Verifico se record corrente da aggiornare (errore) oppure da processare
018700170328     C                   if        *in33
018800170328     C                   update(e) tivpi000
018900170328     C                   else
019000010525     C                   eval      vindta = vpidta
019100010525     C                   write(e)  tivin000
019200010525     C                   if        not %error
019300170519     C                   seton                                        55
019400010525     C                   delete    tivpi000
019500010525     C                   endif
019600170328     C                   endif
019700170118     C*
019800030211     C* Testo se l'immissione da internet è relativa ad una disposizione giacenza
019900030211     C* per la quale "chaino" la giacenza in sede e ne aggiorno la fase.
020000050410     C                   if        vpitip = 'TR'
020100170321     C                   eval      fnvareiir = vpidta
020200050410     C                   move(p)   vhragc        gcpagc
020300050410     C                   move(p)   vhrfgc        gcpfgc
020400050410     C                   move(p)   vhrngc        gcpngc
020500050410     C                   movel     *zeros        gcpfrg
020600170328     C     keygcp2       chain(e)  tigcp52l
020700170328     C                   if        not %error
020800050410     C                   if        %found(tigcp52l)
020900050410     C                   if        gcpfas = 020
021000050410     C                   eval      gcpfas = 025
021100050410     C                   update(e) tigcp000
021200050410     C                   endif
021300050410     C                   endif
021400170328     C                   endif
021500050410     C                   endif
021600170321     C*
021700170321     C                   endif
021800010525     C*
021900010525     C                   endsr
022000010525
022100010525
022200010525
022300010525      /TITLE Valorizzazione Progressivo Applicazione
022400010525     C     calprog       begsr
022500170118     C*
022600170118     C                   movel     vpiISV        wISV
022700170118     C                   if        wISV = *blanks
022800170118     C                   eval      wISV = 'TT'
022900170118     C                   endif
023000010604     C*
023100170324     C     *start        setll     tis7prgf
023200010525     C                   read(e)   tis7prgf
023300010525     C                   if        not %error
023400010525     C                   eval      uplprg = f_tis7prgf
023500010525     C*
023600010525     C                   move(p)   uplprg        wrkprg
023700010525     C                   add       1             wrkprg
023800010525     C                   move(p)   wrkprg        uplprg
023900170118     C                   movel     wISV          uplprg
024000010525     C*
024100010525     C* Costruisco anche il nome del membro del TIVIN
024200170118     C                   if        wISV = 'TT'
024300170118     C                   eval      wrkmbr = 'M' + %subst(uplprg:4:7)
024400170118     C                   else
024500170118     C                   eval      wrkmbr = 'M' + wISV + %subst(uplprg:4:7)
024600170118     C                   endif
024700010525     C*
024800010525     C                   eval      f_tis7prgf = uplprg
024900010525     C                   update    tis7prg0
025000170324     C                   else
025100170324     C                   eval      f_tis7prgf = 'XX00000000'
025200010525     C                   endif
025300010525     C*
025400010525     C                   endsr
025500010525
025600010525
025700010525
025800010525      /TITLE Routine che aggiunge il record nel log dell'UPLOAD (TIVLR)
025900010525     C*
026000010626     C     wrilog        begsr
026100010525     C*
026200010525     C                   clear                   tivlr000
026300010613     C                   eval      vlrksc = depksu
026400010613     C                   eval      vlrtip = deptip
026500170721     C                   eval      vlrpth = *blanks
026600010525     C                   eval      vlrdat = wrkoggi
026700010525     C                   eval      vlrora = wrkora
026800010525     C                   eval      vlrhdl = wrkmbr
026900010613     C                   eval      vlrisv = depisv
027000170324     C                   eval      vlrprg = f_tis7prgf
027100021126     C                   eval      vlrfis = 'TIVIN00S'
027200010525     C                   eval      vlrflg = '0'
027300010528     C                   eval      vlrtia = 'I'
027400010626     C                   eval      vlrfl2 = '01'
027500040309     C                   eval      %subst(vlrfl3:4:2) = 'II'
027600170217     C*
027700170728     C* Considerazioni particolari sui tipi file/servizio
027800170217     C                   select
027900170731     C                   when      vlrisv = 'IW' AND %subst(vlrtip:1:1) <> 'T'
028000170217     C                   eval      vlrtia = 'F'
028100170217     C                   endsl
028200170118     C*
028300010713     C* Considerazioni particolari sui tipi file
028400170118     C                   select
028500170118     C                   when      vlrtip = 'TO'
028600010713     C                   eval      tipFile = 'ORM'
028700010713     C                   eval      vlrfl1 = 'G'
028800010820     C                   eval      vlrpoi = 999
028900170118     C*
029000170118     C                   when      vlrtip = 'TR'
029100010713     C                   eval      tipFile = 'VAR'
029200130114     C                   eval      %subst(vlrfl3:4:2) = '30'
029300010820     C                   eval      vlrpoi = 999
029400170118     C*
029500170118     C                   when      vlrtip = 'TI'
029600140819     C                   eval      tipFile = 'IDC'
029700140819     C                   eval      vlrfl1 = 'G'
029800140819     C                   eval      vlrpoi = 999
029900170118     C*
030000170118     C                   endsl
030100170118     C*
030200010525     C* Considerazioni particolari x i clienti non codificati
030300010618     C                   if        depsun <> *blanks and
030400010618     C                             depsun <> *all'0'
030500010613     C                   eval      vlrsun = depsun
030600010525     C                   else
030700010710     C* Rispetto al tipo file => reperisco il SUN dal file delle anagrafiche varie (TIANV00F)
030800010710     C     tipFile       chain     tianv02l
030900010618     C                   if        %found(tianv02l)
031000010618     C                   eval      vlrsun = anvcod
031100010618     C                   eval      vlrksc = anvcli
031200010525     C                   endif
031300010618     C                   endif
031400010525     C*
031500010525     C                   write     tivlr000
031600010525     C*
031700010525     C                   endsr
031800000714
031900000714
032000000714
032100000714      /TITLE Routine di *init PGM
032200000714     C*
032300000714     C     *inzsr        begsr
032400030211     C*
032500030211     C* CHIAVI DI LETTURA
032600050410     C     keygcp2       klist                                                  tigcp52l - completa
032700050410     C                   kfld                    gcpagc                         -anno apertura giac.
032800050410     C                   kfld                    gcpfgc                         -linea apertura giac
032900050410     C                   kfld                    gcpngc                         -n° giacienza
033000050410     C                   kfld                    gcpfrg                         -progr. riap. giac.
033100051108     C*
033200170118     C     keyVPI02_P    klist                                                  tivpi02i - Parziale
033300170118     C                   kfld                    vpiisv                         -tipo servizio
033400051108     C                   kfld                    vpitip                         -tipo file
033500051108     C                   kfld                    vpiksu                         -codice cliente
033600010525     C*
033700000714     C                   time                    wrkoggiiso
033800000714     C                   time                    wrkora
033900000714     C     *iso          move      wrkoggiiso    wrkoggi
034000000714     C                   move(p)   wrkora        datime
034100000714     C                   movel     wrkoggi       datime
034200000714     C*
034300000714     C                   endsr
034400000714
