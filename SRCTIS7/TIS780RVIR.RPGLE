000100050310      /TITLE Upload ins. manuale da Internet: lettura TIVPI00F
000200051108     H dftactgrp(*no)
000300051108     H actgrp('VASENTRY')
000400990907
000500051108     Ftivpi01l  uf   e           k disk    commit
000600051108     Ftivin00r  o    e             disk    usropn commit
000700010618     Ftianv02l  if   e           k disk
000800080521     Ftivir02l  if   e           k disk
000900010525     Ftivlr00f  o    e             disk
001000010604     Ftis7prgf  uf   e             disk    rename(tis7prgf:tis7prg0)
001100010604     F                                     prefix(f_)
001200010604     F                                     usropn
001300050410     Ftigcp52l  uf   e           k disk
001400990907
001500010524
001600000710     D* variabili di work
001700000714     D  wrkoggiiso     s               d
001800000714     D  wrkoggi        s              8  0
001900000714     D  wrkora         s              6  0
002000000714     D  datime         s             14
002100010613     D  depisv         s                   like(vpiisv) inz(*blanks)
002200010525     D  deptip         s                   like(vpitip) inz(*blanks)
002300010525     D  depksu         s                   like(vpiksu) inz(*blanks)
002400010613     D  depsun         s                   like(vpisun) inz(*blanks)
002500010525     D  ope            s              1
002600010525     D  uplprg         s             10    inz(*all'0')
002700010525     D  wrkprg         s              8  0 inz(*zeros)
002800010525     D  wrkmbr         s             10    inz(*blanks)
002900010525     D  flgOK          s              1
003000010710     D  tipFile        s              3
003100010921     D  esito          s              1    inz('0')
003200080521     D dataiso         s               d
003300080521     D dataiso1        s               d
003400080521     D data8           s              8  0
003500010525
003600030211
003700030211     D* ds di work
003800030211     D  fnvaraar     e ds
003900010525
004000010525
004100010525
004200010525     C*-----------------------------------------------------------------------------
004300051108     C                   setoff                                       lr
004400010525     C*
004500051108     C* Leggo tutta la vista logica
004600010525     C                   dow       not *inlr
004700051108     C*
004800051108     C                   read      tivpi01l                               lr
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
005900061106     C                   if        vpiKSU = '01020001'
006000061106     C                   delete    tivpi000
006100061106     C                   else
006200061106     C*
006300010525     C* ... calcolo un progressivo (lungo 7 per differenziarlo dagli altri metodi di scambio dati)
006400010525     C                   exsr      calprog
006500010525     C* ... eseguo le operazioni iniziali di aggiuta/allocazione/sostituzione mebro nel TIVIN
006600010525     C                   eval      ope = 'I'                                    * I = Inizio
006700010525     C                   call(e)   'TIS780C1'
006800010525     C                   parm                    wrkmbr
006900010525     C                   parm                    vpiksu
007000010525     C                   parm                    ope
007100010921     C                   parm      '0'           esito
007200010921     C                   if        not %error and esito = '0'
007300010525     C                   open(e)   tivin00r
007400010525     C                   if        not %error
007500010525     C                   eval      flgOK = '1'
007600010525     C                   eval      deptip = vpitip
007700010525     C                   eval      depksu = vpiksu
007800010613     C                   eval      depsun = vpisun
007900010613     C                   eval      depisv = vpiisv
008000010525     C*
008100010525     C                   dow       deptip = vpitip and
008200010525     C                             depksu = vpiksu and
008300010525     C                             not *inlr
008400010525     C* Procedo elaborando il file TIVPI e scrivendo nel TIVIN fino a rottura Tipo File/Cliente
008500010525     C                   exsr      exevin
008600010525     C                   read      tivpi01l                               lr
008700010525     C                   enddo
008800010525     C*
008900010525     C                   endif
009000010525     C                   endif
009100010525     C*
009200010525     C* Se tutto OK a rottura scrivo il record di log dell'UPLOAD (TIVLR),
009300010525     C* chiudo comunque il TIVIN ed elimino le sostituzioni/allocazioni sui membri
009400010525     C                   if        flgOK = '1'
009500010525     C                   exsr      wrilog
009600010525     C                   endif
009700010525     C                   close(e)  tivin00r
009800010525     C                   eval      ope = 'F'                                    * F = Fine
009900010525     C                   call(e)   'TIS780C1'
010000010525     C                   parm                    wrkmbr
010100010525     C                   parm                    vpiksu
010200010525     C                   parm                    ope
010300010525     C                   eval      flgOK = '0'
010400061106     C*
010500061106     C                   endif
010600051108     C*
010700051108     C* Ad ogni transazione x Tipo File/Cliente => emetto operazione d COMMIT
010800051108     C                   COMMIT
010900051108     C*
011000051108     C* Quindi mi riposiziono sull'ultima rottura d codice appena occorsa
011100051108     C     keyvpi        setll     tivpi01l
011200051108     C*
011300051108     C                   endif
011400051108     C                   endif
011500010525     C*
011600010525     C                   enddo
011700010525     C*-----------------------------------------------------------------------------
011800010525
011900010525
012000010525
012100010525      /TITLE Routine di scrittura TIVIN e cancellaziono TIVPI
012200010525     C*
012300010525     C     exevin        begsr
012400010525     C*
012500010525     C                   clear                   tivin000
012600010525     C                   eval      vindta = vpidta
012700010525     C                   write(e)  tivin000
012800010525     C                   if        not %error
012900010525     C                   delete    tivpi000
013000010525     C                   endif
013100030211     C* Testo se l'immissione da internet è relativa ad una disposizione giacenza
013200030211     C* per la quale "chaino" la giacenza in sede e ne aggiorno la fase.
013300050410     C                   if        vpitip = 'TR'
013400050410     C                   eval      fnvaraar = vpidta
013500050410     C                   move(p)   vhragc        gcpagc
013600050410     C                   move(p)   vhrfgc        gcpfgc
013700050410     C                   move(p)   vhrngc        gcpngc
013800050410     C                   movel     *zeros        gcpfrg
013900050410     C     keygcp2       chain     tigcp52l
014000050410     C                   if        %found(tigcp52l)
014100050410     C                   if        gcpfas = 020
014200050410     C                   eval      gcpfas = 025
014300050410     C                   update(e) tigcp000
014400050410     C                   endif
014500050410     C                   endif
014600050410     C                   endif
014700010525     C*
014800010525     C                   endsr
014900010525
015000010525
015100010525
015200010525
015300010525
015400010525      /TITLE Valorizzazione Progressivo Applicazione
015500010525     C     calprog       begsr
015600010525     C*
015700010604     C                   open      tis7prgf
015800010604     C*
015900010525     C                   read(e)   tis7prgf
016000010525     C                   if        not %error
016100010525     C                   eval      uplprg = f_tis7prgf
016200010525     C*
016300010525     C                   move(p)   uplprg        wrkprg
016400010525     C                   add       1             wrkprg
016500010525     C                   move(p)   wrkprg        uplprg
016600010525     C                   movel     'TT'          uplprg
016700010525     C*
016800010525     C* Costruisco anche il nome del membro del TIVIN
016900010525     C                   eval      wrkmbr = 'M' + %subst(uplprg:4:7)
017000010525     C*
017100010525     C                   eval      f_tis7prgf = uplprg
017200010525     C                   update    tis7prg0
017300010525     C                   endif
017400010604     C*
017500010604     C                   close     tis7prgf
017600010525     C*
017700010525     C                   endsr
017800010525
017900010525
018000010525
018100010525
018200010525
018300010525      /TITLE Routine che aggiunge il record nel log dell'UPLOAD (TIVLR)
018400010525     C*
018500010626     C     wrilog        begsr
018600010525     C*
018700010525     C                   clear                   tivlr000
018800010613     C                   eval      vlrksc = depksu
018900010613     C                   eval      vlrtip = deptip
019000010525     C                   eval      vlrpth = 'Immissione manuale da Internet'
019100010525     C                   eval      vlrdat = wrkoggi
019200010525     C                   eval      vlrora = wrkora
019300010525     C                   eval      vlrhdl = wrkmbr
019400010613     C                   eval      vlrisv = depisv
019500010525     C                   eval      vlrprg = uplprg
019600021126     C                   eval      vlrfis = 'TIVIN00S'
019700010525     C                   eval      vlrflg = '0'
019800010528     C                   eval      vlrtia = 'I'
019900010626     C                   eval      vlrfl2 = '01'
020000040309     C                   eval      %subst(vlrfl3:4:2) = 'II'
020100010713     C* Considerazioni particolari sui tipi file
020200010713     C                   if        vlrtip = 'TO'
020300010713     C                   eval      tipFile = 'ORM'
020400010713     C                   eval      vlrfl1 = 'G'
020500010820     C                   eval      vlrpoi = 999
020600010713     C                   endif
020700010713     C                   if        vlrtip = 'TR'
020800010713     C                   eval      tipFile = 'VAR'
020900010820     C                   eval      vlrpoi = 999
021000010713     C                   endif
021100010525     C* Considerazioni particolari x i clienti non codificati
021200010618     C                   if        depsun <> *blanks and
021300010618     C                             depsun <> *all'0'
021400010613     C                   eval      vlrsun = depsun
021500010525     C                   else
021600010710     C* Rispetto al tipo file => reperisco il SUN dal file delle anagrafiche varie (TIANV00F)
021700010710     C     tipFile       chain     tianv02l
021800010618     C                   if        %found(tianv02l)
021900010618     C                   eval      vlrsun = anvcod
022000010618     C                   eval      vlrksc = anvcli
022100010525     C                   endif
022200010618     C                   endif
022300080521     C*
022400080521     C* Verifico eventuali particolarità x il cliente/tipo-file corrente
022500080521     C                   time                    dataiso
022600080521     C     *iso          move      dataiso       data8
022700080522     C                   move      dataiso       dataiso1
022800080522     C                   adddur    1:*d          dataiso1
022900080521     C     *iso          move      dataiso1      virdti
023000080521     C                   eval      virksc = vlrksc
023100080521     C                   eval      virtip = vlrtip
023200080521     C     k03vir02      setll     tivir02l
023300080521     C     k02vir02      readpe    tivir02l
023400080521     C*
023500080521     C* Se ho letto il record ed è valido segnalo che l'ho trovato.
023600080521     C                   if        not %eof
023700080521     C                             and data8 <= virdtf
023800080521     C                   eval      vlrpgt = virpgt
023900080521     C                   eval      vlrppt = virppt
024000080521     C                   endif
024100010525     C*
024200010525     C                   write     tivlr000
024300010525     C*
024400010525     C                   endsr
024500051108
024600051108
024700051108
024800051108      /TITLE Gestione errore generico
024900051108     C     *pssr         BEGSR
025000051108     C*
025100051108     C* Se errore => emetto operazione d ROLLBACK
025200051108     C                   ROLBK
025300051108     C*
025400051108     C                   ENDSR     '*CANCL'
025500051108     C***
025600010525
025700010525
025800000714
025900000714
026000000714
026100000714      /TITLE Routine di *init PGM
026200000714     C*
026300000714     C     *inzsr        begsr
026400030211     C*
026500030211     C* CHIAVI DI LETTURA
026600050410     C     keygcp2       klist                                                  tigcp52l - completa
026700050410     C                   kfld                    gcpagc                         -anno apertura giac.
026800050410     C                   kfld                    gcpfgc                         -linea apertura giac
026900050410     C                   kfld                    gcpngc                         -n° giacienza
027000050410     C                   kfld                    gcpfrg                         -progr. riap. giac.
027100051108     C*
027200051108     C     keyvpi        klist                                                  tivpi01l - completa
027300051108     C                   kfld                    vpitip                         -tipo file
027400051108     C                   kfld                    vpiksu                         -codice cliente
027500080521     C*
027600080521     C     k03vir02      klist
027700080521     C                   kfld                    virksc
027800080521     C                   kfld                    virtip
027900080521     C                   kfld                    virdti
028000080521     C*
028100080521     C     k02vir02      klist
028200080521     C                   kfld                    virksc
028300080521     C                   kfld                    virtip
028400010525     C*
028500000714     C                   time                    wrkoggiiso
028600000714     C                   time                    wrkora
028700000714     C     *iso          move      wrkoggiiso    wrkoggi
028800000714     C                   move(p)   wrkora        datime
028900000714     C                   movel     wrkoggi       datime
029000000714     C*
029100000714     C                   endsr
029200000714
