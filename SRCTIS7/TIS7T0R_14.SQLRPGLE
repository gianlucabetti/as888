000100010925      *PARMS DYNUSRPRF(*OWNER)
000200991026      /TITLE Download via Internet: decisore traduzione ASCII.
000300111230     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400991026
000500991026      **********************************************************************
000600991026      * Legenda PRMESITO
000700991026      * ' ' = Elaborazione non eseguita.
000800991026      * '0' = Elaborazione eseguita senza errori.
000900991104      * 'A' = Parametro PRMKSU vuoto.
001000991026      * 'B' = Parametro PRMTIP vuoto o sconosciuto.
001100991026      * 'C' = Tipo file non previsto per il cliente.
001200991104      * 'D' = Cliente non abilitato al servizio.
001300031008      * 'E' = File/membro già allocato dalla procedura FTP/e-mail sul server
001400031008      * 'F' = Progressivo NON univoco  => ATTENZIONE, possibili errori gravi ed imprevedibili!!!
001500991026      **********************************************************************
001600991026
001700050909 TESTF*tivltnew  o    e             disk    RENAME(tivlt000:tivltn00) USROPN
001800991026     Ftivlt00f  o    e             disk
001900000712     Ftivlt07l  uf   e           k disk    RENAME(tivlt000:tivlt700)
002000031008     Ftivlt08l  if   e           k disk    RENAME(tivlt000:tivlt800)
002100031008     F                                     PREFIX(w_)
002200991105     Ftivss02l  if   e           k disk
002300990920     Ftivtf01l  if   e           k disk
002400991026     Ftivir02l  if   e           k disk
002500000707     Ftivre01l  if   e           k disk
002600000622     Ftntbe01l  if   e           k disk
002700000627     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002800000627     F                                     PREFIX(f_)
002900000627
003000020212     D disv          e ds
003100030526     D dvir01        e ds
003200991027     D psds           sds
003300991027     D  psjobname            244    253
003400991027     D  psuser               254    263
003500991027     D  psjobnbr             264    269s 0
003600991026     D prmksc          s                   like(virksc)
003700991026     D prmksu          s                   like(virksc)
003800991026     D prmtip          s                   like(virtip)
003900991026     D prmesito        s              1
004000000623     D prmprog         s                   like(vltprg)
004100000623     D prmsrv          s                   like(vltisv)
004200000628     D prmope          s              1
004300000713     D prmnfp          s                   like(vltfl2)
004400070207     D prmout          s             10
004500120105     D prmsrc          s              1
004600991026     D wrkksc          s                   like(prmksc)
004700991026     D wrkksu          s                   like(prmksu)
004800991026     D wrktip          s                   like(prmtip)
004900991026     D wrkesito        s                   like(prmesito)
005000991026     D wrkoggiiso      s               d
005100991026     D wrkoggi         s              8  0
005200991026     D wrkora          s              6  0
005300991026     D wrkmese         s              2  0
005400000623     D wrkprg          s              8  0 INZ(*zeros)
005500000711     D wrkvarie        s              1    INZ('0')
005600000712     D wrkmsg          s             80    INZ(*blanks)
005700000914     D vssok           s               n   INZ(*off)
005800051011
005900051011     D dwlprg          ds
006000051011     D  dwl_isv                       2    INZ(*all'0')
006100051207     D**dwl_tip                       2    INZ(*all'0')
006200051207     D**dwl_prg                       6    INZ(*all'0')
006300051207     D  dwl_prg                       8    INZ(*all'0')
006400000711
006500991026     D                 ds                  inz
006600991026     D wrkday                  1      2s 0
006700991026     D wrkdaychr                      2    overlay(wrkday)
006800991026     D wrkpgt          s                   like(vtfpgt)
006900991026     D wrkppt          s                   like(vtfppt)
007000000711     D wrkpgi          s                   like(vtfpgi)
007100000711     D wrkppi          s                   like(vtfppi)
007200991026     D wrkfid          s                   like(vtffid)
007300991026     D wrkmbd          s                   like(vltmbd)
007400991026     D wrkfis          s                   like(vtffis)
007500991026     D wrkmbs          s                   like(vltmbs)
007600991026     D wrkfit          s                   like(vtffit)
007700991026     D wrkmbt          s                   like(vltmbd)
007800991026     D prmfid          s                   like(wrkfid)
007900991026     D prmmbd          s                   like(wrkmbd)
008000991026     D prmfis          s                   like(wrkfis)
008100991026     D prmmbs          s                   like(wrkmbs)
008200991026     D prmfit          s                   like(wrkfit)
008300991027     D prmmbt          s                   like(wrkmbt)
008400991026     D prmpgt          s                   like(wrkpgt)
008500991026     D prmppt          s                   like(wrkppt)
008600991026     D mc              s              1    dim(12) ctdata perrcd(12)
008700111230
008800111230     D wKSC            s                   like(virKSC) inz
008900111230     D wTIP            s                   like(virTIP) inz
009000111230     D wDTI            s              8S 0 inz
009100111230
009200991104
009300991111     D mbrd01          s            266
009400991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
009500991111     D mbrd03          s              8    inz('MBRD0200')
009600991111     D mbrd04          ds
009700991111     D mbrd04a                       10                                         File
009800991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
009900991111     D mbrd05          s             10                                         Membro
010000991125     D mbrd06          s              1    inz('0')
010100991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
010200991104     D rneta01         ds
010300991104     D  rneta01a                      8b 0
010400991104     D  rneta01b                      8b 0
010500991104     D  rneta01c                     24
010600991104     D rneta02         s              8b 0 inz(%size(rneta01))
010700991104     D rneta03         s              8b 0 inz(1)
010800991104     D rneta04         s             10    inz('SYSNAME')
010900991104     D nait            ds
011000991104     D  nait01                       10
011100991104     D  nait02                        1
011200991104     D  nait03                        1
011300991104     D  nait04                        8b 0
011400991104     D  sysname                       8
011500991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
011600991104
011700000622
011800000905     D*-------------------
011900000905     D* DS di ridefinizione file di log x passaggio recordset SQL
012000000905     D*-------------------
012100000905     D  dsvlt        E DS                  extname(tivlt00f)
012200000905     D                                     prefix(s_)
012300000905
012400040817
012500040817     D*-------------------
012600050909 TESTD* Variabili d wrk
012700040817     D*-------------------
012800050909     D*  COMMAND        S            300    inz(*blanks)
012900040817
013000000905
013100000905
013200000622      /TITLE *** MAIN ***
013300090414     C*
013400090414     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013500090414     C
013600090414     C/EXEC SQL
013700090414     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013800090414     C/END-EXEC
013900000622     C*
014000000906     C                   if        prmnfp  > '01' and
014100000906     C                             prmprog > *blanks
014200000905     C                   exsr      rtvdatora
014300000905     C                   else
014400000905     C                   time                    wrkoggiiso
014500000905     C                   time                    wrkora
014600000905     C     *iso          move      wrkoggiiso    wrkoggi
014700000905     C                   endif
014800030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
014900000328     C* Inizializzo variabili "wrk".
015000000328     C                   exsr      inzwrk
015100991026     C* Controllo parametri ricevuti.
015200991026     C                   exsr      chkprm
015300000626     C* Reperisco informazioni file.
015400000706     C                   exsr      rtvleg
015500000706     C* Alla fine...
015600000706     C                   exsr      uscita
015700000626
015800000626
015900000905
016000000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
016100000905     C*
016200000905     C     rtvdatora     begsr
016300000905     C*
016400000905     C/EXEC SQL
016500000905     C+ declare C1 cursor for select *
016600000905     C+ from tivlt00f where
016700000905     C+ vltprg = :prmprog
016800000905     C+ for fetch only
016900000905     C/END-EXEC
017000000905     C
017100000905     C/EXEC SQL
017200000905     C+ open C1
017300000905     C/END-EXEC
017400000905     C
017500000905     C/EXEC SQL
017600000905     C+ Fetch C1 into :dsvlt
017700000905     C/END-EXEC
017800000905     C*
017900000906     C                   if        sqlcod = *zeros     and
018000000906     C                             s_vltprg  = prmprog and
018100000906     C                             s_vltdat  > *zeros  and
018200000905     C                             s_vltora  > *zeros
018300000905     C                   z-add     s_vltdat      wrkoggi
018400000905     C                   z-add     s_vltora      wrkora
018500000905     C                   else
018600000905     C                   time                    wrkoggiiso
018700000905     C                   time                    wrkora
018800000905     C     *iso          move      wrkoggiiso    wrkoggi
018900000905     C                   endif
019000000905     C
019100000905     C/EXEC SQL
019200000905     C+ close C1
019300000905     C/END-EXEC
019400000905     C
019500000905     C*
019600000905     C                   endsr
019700000905
019800000905
019900000706
020000000706      /TITLE Determino eventuali legami cliente/tipi file
020100000706     C     rtvleg        begsr
020200000706     C*
020300000706     C                   eval      virksc = wrkksu
020400000706     C                   eval      virtip = wrktip
020500000706     C                   eval      virdti = wrkoggi
020600000707     C                   eval      vtftip = wrktip
020700000706     C* Reperisco informazioni file.
020800000706     C                   exsr      rtvtip
020900000706     C*
021000000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
021100000713     C                   if        wrkesito = '0'     and
021200000713     C                             prmprog  = *blanks and
021300000713     C                             prmope   = *blanks
021400000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
021500000711     C                   movel     '1'           wrkvarie
021600000706     C*
021700000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
021800000707     C     k03vir02      setll     tivre01l
021900000707     C                   if        %found(tivre01l)
022000000707     C     k03vir02      reade     tivre01l
022100000706     C                   dow       not %eof
022200000706     C                   eval      virksc = wrkksu
022300000706     C                   eval      virtip = vretfl
022400000706     C                   eval      virdti = wrkoggi
022500000707     C                   eval      vtftip = vretfl
022600000712     C                   eval      wrktip = vretfl
022700000706     C* Reperisco informazioni file.
022800000706     C                   exsr      rtvtip
022900000712     C                   eval      virtip = vretip
023000000707     C     k03vir02      reade     tivre01l
023100000706     C                   enddo
023200000706     C                   endif
023300000706     C*
023400000706     C                   endif
023500031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
023600031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
023700031008     C                   if        chkPRG <> '1'
023800030512     C                   if        chkFTP <> '1'
023900000711     C                   exsr      exepgm
024000030512     C                   endif
024100031008     C                   endif
024200000706     C*
024300000706     C                   endsr
024400000706
024500000706
024600000706
024700000706      /TITLE Reperisco tipo file.
024800000706     C     rtvtip        begsr
024900120105     C* Inizializzazioni
025000120105     C                   clear                   wKSC
025100120105     C                   clear                   wTIP
025200120105     C                   clear                   wDTI
025300000706     C* Reperisco il tipo file.
025400000706     C     k01vtf01      chain     tivtf01l
025500000706     C* Se il tipo file non esiste
025600000706     C* oppure non è valido per il download.
025700000706     C                   if        not %found
025800000706     C                             or vtfud <> 'D'
025900031008     C*
026000031008     C                   exsr      inztivlt
026100031008     C                   eval      vltsts = '2'
026200031008     C                   eval      vltmsg =
026300031008     C                               %trim(sysname)
026400031008     C                             + '/'
026500031008     C                             + %trim(psjobname)
026600031008     C                             + '/'
026700031008     C                             + %trim(psuser)
026800031008     C                             + '/'
026900031008     C                             + %trim(%editc(psjobnbr:'Z'))
027000031008     C                             + ' Tipo file inesistente.'
027100031008     C                   exsr      wrttivlt
027200031008     C                   eval      wrkesito = 'B'
027300031008     C*
027400000721     C                   else
027500000706     C* Reperisco il tipo file del cliente.
027600000706     C     k03vir02      setgt     tivir02l
027700000706     C     k02vir02      readpe    tivir02l
027800000706     C* Tipo file non previsto per il cliente
027900000706     C* oppure scaduto.
028000000731     C                   if        %eof
028100000706     C                             or virdtf < wrkoggi
028200000706     C                             or virdti > wrkoggi
028300000706     C*
028400000731     C                   exsr      inztivltp
028500000706     C*
028600000706     C                   eval      vltsts = '2'
028700000706     C                   eval      vltmsg =
028800000706     C                               %trim(sysname)
028900000706     C                             + '/'
029000000706     C                             + %trim(psjobname)
029100000706     C                             + '/'
029200000706     C                             + %trim(psuser)
029300000706     C                             + '/'
029400000706     C                             + %trim(%editc(psjobnbr:'Z'))
029500000706     C                             + ' Tipo file non previsto per il cliente.'
029600000706     C                   exsr      wrttivlt
029700000706     C                   eval      wrkesito = 'C'
029800000706     C*
029900000706     C                   else
030000111230     C*
030100111230     C* Salvo subito la chiave univoca della regola corrente
030200111230     C                   eval      wKSC = virKSC
030300111230     C                   eval      wTIP = virTIP
030400111230     C                   eval      wDTI = virDTI
030500000706     C*
030600000706     C* Imposto i default.
030700000706     C                   if        virpgt <> *blanks
030800000706     C                   eval      wrkpgt = virpgt
030900000706     C                   eval      wrkppt = virppt
031000000706     C                   else
031100000706     C                   eval      wrkpgt = vtfpgt
031200000706     C                   eval      wrkppt = vtfppt
031300000706     C                   endif
031400000712     C*
031500000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
031600000711     C                   if        wrkvarie = '0'                               <===================
031700000711     C                   if        virpgi <> *blanks                            <===================
031800000711     C                   eval      wrkpgi = virpgi                              <===================
031900000711     C                   eval      wrkppi = virppi                              <===================
032000000711     C                   else                                                   <===================
032100000711     C                   eval      wrkpgi = vtfpgi                              <===================
032200101013     C***                eval      wrkppi = vtfppi                              <===================
032300000711     C                   endif                                                  <===================
032400000711     C                   endif                                                  <===================
032500000706     C* File da tradurre.
032600000706     C                   if        virfid <> *blanks
032700000706     C                   eval      wrkfid = virfid
032800000706     C                   else
032900000706     C                   eval      wrkfid = vtffid
033000000706     C                   endif
033100000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
033200000706     C* File storico.
033300000706     C                   if        virfis <> *blanks
033400000706     C                   eval      wrkfis = virfis
033500000706     C                   else
033600000706     C                   eval      wrkfis = vtffis
033700000706     C                   endif
033800000706     C                   extrct    wrkoggiiso:*m wrkmese
033900000706     C                   extrct    wrkoggiiso:*d wrkday
034000000706     C                   eval      wrkmbs =
034100000706     C                             mc(wrkmese)
034200000706     C                             + wrkdaychr
034300000706     C                             + %subst(wrkksu:2:7)
034400030519     C*
034500030519     C                   eval      vssisv = virfi1
034600030519     C* Valorizzo subito il progressivo applicazione.
034700030519     C                   exsr      calprog
034800000706     C* File tradotto.
034900000706     C                   if        virfit <> *blanks
035000000706     C                   eval      wrkfit = virfit
035100000706     C                   else
035200000706     C                   eval      wrkfit = vtffit
035300000706     C                   endif
035400030526     C* Tipo nome membro tradotto
035500030526     C                   eval      dvir01 = virpth
035600070207     C* Se forzatura in ingresso adotto quella, altrimenti applico considerazioni standard
035700070207     C                   if        prmout <> *blanks
035800070207     C                   eval      wrkmbt = prmout                              membro = forzatura
035900070207     C                   else
036000030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
036100100311     C                   select
036200100311     C                   when      §VIR01TM = 'P'
036300030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
036400030526 xxx C***                eval      wrkmbs = dwlprg
036500100311     C                   when      §VIR01TM = 'T'
036600070504     C                   eval      wrkmbt = dwlprg                              membro = progressivo
036700070504     C                   eval      %subst(wrkmbt:3:2) = wrktip                  membro = prog + tipf
036800100311     C                   when      §VIR01TM = 'M'
036900100311     C                   eval      wrkmbt = wrktip + wrkksu                     membro = tipf + KSC
037000100311     C                   other
037100030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
037200100311     C                   endsl
037300030519     C                   endif
037400000706     C* Controllo abilitazione al servizio.
037500000706     C                   exsr      chkvss
037600000706     C                   endif
037700000706     C                   endif
037800000706     C*
037900000706     C                   endsr
038000000706
038100000711
038200000711
038300000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
038400000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
038500000711     C*
038600000711     C     exepgm        begsr
038700000711     C* Lancio il pgm in variabile con relativi parametri
038800000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
038900000711    C                   if        wrkpgi <> *blanks
039000000712     C* Chiamo il pgm post traduzione.
039100000712     C                   call(e)   'TIS7T9C1'
039200000712     C                   parm      wrkfid        prmfid
039300000712     C                   parm      wrkmbd        prmmbd
039400000712     C                   parm      wrkfis        prmfis
039500000712     C                   parm      wrkmbs        prmmbs
039600031106     C                   parm      wrkfit        prmfit
039700031106     C                   parm      wrkmbt        prmmbt
039800000712     C                   parm                    wrkpgi
039900000712     C                   parm                    wrkppi
040000000712     C                   parm      *blanks       prmesito
040100000712     C* Se la call è finita in errore
040200000712     C* o il lancio del PGM ha avuto degli errori.
040300000712     C                   if        %error
040400000712     C                             or prmesito <> '0'
040500000712     C     k01vlt        setll     tivlt07l
040600000712     C                   if        %found(tivlt07l)
040700000712     C     k01vlt        reade     tivlt07l
040800000712     C                   dow       not %eof
040900000712     C                   if        vltsqz = *zeros and
041000000712     C                             (vltsts = '0' or vltsts = '1')
041100000712     C                   eval      vltsts = '2'
041200000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
041300000712     C                   eval      vltmsg =
041400000712     C                               %trim(sysname)
041500000712     C                             + '/'
041600000712     C                             + %trim(psjobname)
041700000712     C                             + '/'
041800000712     C                             + %trim(psuser)
041900000712     C                             + '/'
042000000712     C                             + %trim(%editc(psjobnbr:'Z'))
042100000712     C                             + %trim(wrkmsg)
042200000712     C                   update    tivlt700
042300000712     C                   endif
042400000712     C     k01vlt        reade     tivlt07l
042500000712     C                   enddo
042600000712     C                   endif
042700000712     C                   endif
042800000712     C*
042900000711     C                   endif
043000000711     C*
043100000711     C                   endsr
043200000711
043300000711
043400000626
043500000626      /TITLE Valorizzazione Progressivo Applicazione
043600000626     C     calprog       begsr
043700000626     C*
043800000706     C                   if        dwlprg = *all'0'
043900000627     C                   read(e)   tis7prgf
044000000627     C                   if        not %error
044100000627     C                   eval      dwlprg = f_tis7prgf
044200000627     C*
044300000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
044400000719     C                   if        prmsrv <> *blanks
044500000719     C                   eval      vssisv = prmsrv
044600000719     C                   endif
044700000626     C*
044800000719     C                   if        prmprog = *blanks
044900051011     C                   move(p)   dwl_prg       wrkprg
045000000626     C                   add       1             wrkprg
045100051011     C                   move(p)   wrkprg        dwl_prg
045200051011     C                   movel     vssisv        dwl_isv
045300051207     C*                  if        wrktip <> *blanks
045400051207     C*                  movel     wrktip        dwl_tip
045500051207     C*                  else
045600051207     C*                  movel     'XX'          dwl_tip
045700051207     C*                  endif
045800040123     C                   eval      f_tis7prgf = dwlprg
045900040123     C                   update    tis7prg0
046000000626     C                   else
046100040123     C                   unlock(e) tis7prgf
046200000626     C                   eval      dwlprg = prmprog
046300000626     C                   endif
046400000627     C*
046500000627     C                   endif
046600000706     C                   endif
046700000626     C*
046800000626     C                   endsr
046900991104
047000000706
047100000706
047200000706
047300991104      /TITLE Controllo abilitazione servizio.
047400991104     C     chkvss        begsr
047500991104     C*
047600000914     C                   eval      vssok = *off
047700000914     C*
047800991105     C                   eval      vssksu = wrkksu
047900000914     C     k02vss02      setll     tivss02l
048000000914     C     k02vss02      reade     tivss02l
048100000914     C                   dow       not %eof
048200000914     C                   if        vssdde <= wrkoggi and
048300000914     C                             vssdsc >= wrkoggi and
048400000914     C                             vssvat <> 'A'     and
048500000914     C                             vssvat <> 'S'
048600000914     C                   eval      vssok = *on
048700000914     C                   leave
048800000914     C                   endif
048900000914     C     k02vss02      reade     tivss02l
049000000914     C                   enddo
049100000914     C*
049200000914     C                   if        vssok = *off
049300991104     C*
049400000622     C                   exsr      inztivlt
049500000329     C                   eval      vltsts = '2'
049600991104     C                   eval      vltmsg =
049700991104     C                               %trim(sysname)
049800991104     C                             + '/'
049900991104     C                             + %trim(psjobname)
050000991104     C                             + '/'
050100991104     C                             + %trim(psuser)
050200991104     C                             + '/'
050300991104     C                             + %trim(%editc(psjobnbr:'Z'))
050400991104     C                             + ' Cliente non abilitato al servizio.'
050500000622     C                   exsr      wrttivlt
050600000622     C                   eval      wrkesito = 'D'
050700031008     C*
050800000622     C                   else
050900031008     C*
051000031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
051100031008     C* del progressivo rispetto a file/membro e cliente
051200031008     C                   movel     '0'           chkPRG            1
051300031008     C     k08vlt        setll     tivlt08l
051400031008     C                   if        %equal(tivlt08l)
051500031008     C     k08vlt        reade     tivlt08l
051600031008     C                   dow       not %eof(tivlt08l)
051700031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
051800090414     C* (considero solamente i record in stato "da elaborare")
051900090414     C                   if        w_vltksc <> wrkksc AND
052000090414     C                             w_vltsts  = '0'
052100031008     C                   movel     '1'           chkPRG
052200031008     C                   endif
052300031008     C     k08vlt        reade     tivlt08l
052400031008     C                   enddo
052500031008     C                   endif
052600031008     C*
052700000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
052800031008     C                   if        chkPRG = '0'
052900000706     C                   exsr      traduci
053000031008     C                   else
053100031008     C*
053200031008     C                   exsr      inztivlt
053300031008     C                   eval      vltsts = '2'
053400031008     C                   eval      vltmsg =
053500031008     C                               %trim(sysname)
053600031008     C                             + '/'
053700031008     C                             + %trim(psjobname)
053800031008     C                             + '/'
053900031008     C                             + %trim(psuser)
054000031008     C                             + '/'
054100031008     C                             + %trim(%editc(psjobnbr:'Z'))
054200031008     C                             + ' Progressivo/membro NON univoco!!!'
054300031008     C                   exsr      wrttivlt
054400031008     C                   eval      wrkesito = 'F'
054500031008     C                   exsr      sndeml
054600031008     C                   exsr      uscita
054700031008     C                   endif
054800991104     C*
054900991104     C                   endif
055000991104     C*
055100991104     C                   endsr
055200000622
055300000622
055400000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
055500000622     C     rtvtia        begsr
055600000622     C*
055700000622     C                   eval      tbecod = 'ISV'
055800000731     C                   eval      tbeke1 = virfi1
055900000622     C     keytbe        chain     tntbe01l
056000000622     C                   if        %found(tntbe01l)
056100020212     C                   movel     tbeuni        disv
056200000622     C                   endif
056300000622     C*
056400000622     C                   endsr
056500000706
056600000706
056700000706
056800000706      /TITLE Traduzione.
056900000706     C     traduci       begsr
057000031105     C*
057100031105     C* Reperisco informazioni membro da tradurre.
057200031105     C                   eval      mbrd04a = wrkfid
057300031105     C                   eval      mbrd05  = wrkmbd
057400031105     C                   exsr      rtvmbrd
057500031105     C* Controllo se il membro da tradurre ha dei record.
057600031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
057700031105     C                   if        (qusei=*blanks and
057800060616     C                             qusnbrcr > 0) or prmnfp > '01'
057900060616     C                             or %trim(wrkfid) = '*'
058000030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
058100030512     C                   call      'TIS7T9R2'
058200031008     C                   parm                    wrkfit
058300031008     C                   parm                    wrkmbt
058400030512     C                   parm      '0'           chkFTP            1
058500031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
058600031105     C                   if        chkFTP = '0'
058700000706     C*
058800000706     C                   exsr      inztivlt
058900000706     C* Chiamo il pgm traduttore.
059000060616     C                   if        %trim(wrkfid) = '*' AND
059100060616     C                             %trim(wrkfis) = '*'
059200060616     C                   call(e)   'TIS7T9C3'
059300120105     C                   parm                    prmsrc
059400111230     C                   parm                    wKSC
059500111230     C                   parm                    wTIP
059600111230     C                   parm                    wDTI
059700060616     C                   parm      wrkmbd        prmmbd
059800060616     C                   parm      wrkfit        prmfit
059900060616     C                   parm      wrkmbt        prmmbt
060000060616     C                   parm      wrkpgt        prmpgt
060100060616     C                   parm      wrkppt        prmppt
060200060616     C                   parm      *blanks       prmesito
060300060616     C                   else
060400000706     C                   call(e)   'TIS7T9C'
060500120105     C                   parm                    prmsrc
060600111230     C                   parm                    wKSC
060700111230     C                   parm                    wTIP
060800111230     C                   parm                    wDTI
060900000706     C                   parm      wrkfid        prmfid
061000000706     C                   parm      wrkmbd        prmmbd
061100000706     C                   parm      wrkfis        prmfis
061200000706     C                   parm      wrkmbs        prmmbs
061300000706     C                   parm      wrkfit        prmfit
061400000706     C                   parm      wrkmbt        prmmbt
061500000706     C                   parm      wrkpgt        prmpgt
061600000706     C                   parm      wrkppt        prmppt
061700000706     C                   parm      *blanks       prmesito
061800060616     C                   endif
061900000706     C* Se la call è finita in errore
062000000706     C* o la traduzione ha avuto degli errori.
062100060320     C                   if        %error
062200060320     C                             or (prmesito <> '0' and
062300060320     C                                 prmesito <> 'Y')
062400000706     C                   eval      vltsts = '2'
062500000706     C                   eval      vltmsg =
062600000706     C                               %trim(sysname)
062700000706     C                             + '/'
062800000706     C                             + %trim(psjobname)
062900000706     C                             + '/'
063000000706     C                             + %trim(psuser)
063100000706     C                             + '/'
063200000706     C                             + %trim(%editc(psjobnbr:'Z'))
063300000706     C                             + ' Errori nella traduzione.'
063400000706     C                   else
063500060320     C                   if        prmesito = 'Y'
063600060320     C                   eval      wrkesito = prmesito
063700060320     C                   else
063800000706     C                   eval      wrkesito = '0'
063900060320     C                   endif
064000000706     C                   endif
064100000706     C*
064200000706     C                   exsr      wrttivlt
064300031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
064400031105     C                   else
064500030512     C                   exsr      inztivlt
064600030512     C                   eval      vltsts = '2'
064700030512     C                   eval      vltmsg =
064800030512     C                               %trim(sysname)
064900030512     C                             + '/'
065000030512     C                             + %trim(psjobname)
065100030512     C                             + '/'
065200030512     C                             + %trim(psuser)
065300030512     C                             + '/'
065400030512     C                             + %trim(%editc(psjobnbr:'Z'))
065500030512     C                             + ' Cliente/TipoFile in uso da server FTP'
065600030512     C                   exsr      wrttivlt
065700030512     C                   eval      wrkesito = 'E'
065800030512     C                   endif
065900031105     C* NO record da tradurre
066000031105     C                   endif
066100000706     C*
066200000706     C                   endsr
066300000328
066400000706
066500000706
066600000328      /TITLE Inizializzo variabili "wrk".
066700000328     C     inzwrk        begsr
066800000328     C*
066900000328     C                   clear                   wrkfit
067000000328     C                   clear                   wrkmbt
067100000328     C                   clear                   wrkpgi
067200000328     C                   clear                   wrkppi
067300000328     C                   clear                   wrkfis
067400000328     C                   clear                   wrkmbs
067500000328     C*
067600000328     C                   endsr
067700991026
067800000706
067900000706
068000991026      /TITLE Inizializzo TIVLT00F.
068100991026     C     inztivlt      begsr
068200000804     C*
068300000804     C                   exsr      rtvtia
068400991026     C*
068500000328     C                   clear                   tivlt000
068600991026     C                   eval      vltksc = wrkksc
068700991026     C                   eval      vltksu = wrkksu
068800000721     C                   eval      vlttip = wrktip
068900991026     C                   eval      vltfld = wrkfit
069000991026     C                   eval      vltmbd = wrkmbt
069100100311     C                   eval      vltpgi = wrkpgt                              * forzatura PGI->PGT
069200100311     C***                eval      vltppi = wrkppi
069300020212     C                   if        §isvproc = 'F'
069400000804     C                   eval      vltsnd = 'D'
069500000804     C                   else
069600991026     C                   eval      vltsnd = vtfsnd
069700000804     C                   endif
069800991026     C                   eval      vltfls = wrkfis
069900991026     C                   eval      vltmbs = wrkmbs
070000991026     C                   eval      vltdat = wrkoggi
070100991115     C                   eval      vltora = wrkora
070200991026     C                   eval      vltsts = '0'
070300991027     C                   eval      vltmsg =
070400991104     C                               %trim(sysname)
070500991104     C                             + '/'
070600991104     C                             + %trim(psjobname)
070700991027     C                             + '/'
070800991027     C                             + %trim(psuser)
070900991027     C                             + '/'
071000991027     C                             + %trim(%editc(psjobnbr:'Z'))
071100000328     C                   z-add     *zeros        vltsqz
071200000731     C*
071300991026     C                   endsr
071400991026
071500000706
071600000731
071700000731      /TITLE Inizializzo parzialmente TIVLT00F.
071800000731     C     inztivltp     begsr
071900000731     C*
072000000731     C                   clear                   tivlt000
072100000731     C                   eval      vltksc = wrkksc
072200000731     C                   eval      vltksu = wrkksu
072300000731     C                   eval      vlttip = wrktip
072400100311     C                   eval      vltpgi = wrkpgt                              * forzatura PGI->PGT
072500100311     C***                eval      vltppi = wrkppi
072600000731     C                   eval      vltsnd = vtfsnd
072700000731     C                   eval      vltdat = wrkoggi
072800000731     C                   eval      vltora = wrkora
072900000731     C                   eval      vltsts = '0'
073000000731     C                   eval      vltmsg =
073100000731     C                               %trim(sysname)
073200000731     C                             + '/'
073300000731     C                             + %trim(psjobname)
073400000731     C                             + '/'
073500000731     C                             + %trim(psuser)
073600000731     C                             + '/'
073700000731     C                             + %trim(%editc(psjobnbr:'Z'))
073800000731     C                   z-add     *zeros        vltsqz
073900000731     C*
074000000731     C                   exsr      rtvtia
074100000731     C*
074200000731     C                   endsr
074300000731
074400000731
074500000706
074600991026      /TITLE Scrittura TIVLT00F.
074700000731     C     wrttivlt      begsr
074800991026     C*
074900000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
075000000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
075100000731     C                   eval      vltisv = virfi1
075200020212     C                   eval      vlttia = §isvproc
075300000622     C                   eval      vltprg = dwlprg
075400000713     C                   eval      vltfl2 = prmnfp
075500991026     C                   write     tivlt000
075600040817     C*
075700050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
075800040817     C* procedura scambio dati
075900040817     C* ...quindi eseguo prima d tutto l'override
076000050909  "  C*
076100050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
076200050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
076300050909     C*                  eval      LUNG = %len(%trim(COMMAND))
076400050909  "  C*                  call(e)   'QCMDEXC'
076500050909     C*                  parm                    COMMAND
076600050909     C*                  parm                    LUNG             15 5
076700040817     C*
076800050909  "  C*                  if        not %error
076900050909     C*                  if        not %open(tivltnew)
077000050909     C*                  open(e)   tivltnew
077100050909     C*                  endif
077200050909  "  C*                  if        not %error
077300050909     C*                  write     tivltn00
077400050909     C*                  endif
077500050909     C*                  if        %open(tivltnew)
077600050909  "  C*                  close(e)  tivltnew
077700050909     C*                  endif
077800050909 TESTC*                  endif
077900991026     C*
078000991026     C                   endsr
078100991026
078200000706
078300000706
078400991026      /TITLE Operazioni finali.
078500991026     C     uscita        begsr
078600991026     C*
078700000627     C                   seton                                        lr
078800991026     C*
078900991026     C                   endsr
079000991026
079100000706
079200000706
079300991026      /TITLE Controllo formale parametri ricevuti.
079400991026     C     chkprm        begsr
079500991026     C*
079600991104     C                   if        wrkksu = *blanks
079700031008     C*
079800031008     C                   exsr      inztivlt
079900031008     C                   eval      vltsts = '2'
080000031008     C                   eval      vltmsg =
080100031008     C                               %trim(sysname)
080200031008     C                             + '/'
080300031008     C                             + %trim(psjobname)
080400031008     C                             + '/'
080500031008     C                             + %trim(psuser)
080600031008     C                             + '/'
080700031008     C                             + %trim(%editc(psjobnbr:'Z'))
080800031008     C                             + ' Parametro codice cliente non passato.'
080900031008     C                   exsr      wrttivlt
081000120105     C                   eval      wrkesito = 'K'
081100031008     C*
081200991026     C                   exsr      uscita
081300991026     C                   endif
081400991026     C*
081500991026     C                   if        wrktip = *blanks
081600031008     C*
081700031008     C                   exsr      inztivlt
081800031008     C                   eval      vltsts = '2'
081900031008     C                   eval      vltmsg =
082000031008     C                               %trim(sysname)
082100031008     C                             + '/'
082200031008     C                             + %trim(psjobname)
082300031008     C                             + '/'
082400031008     C                             + %trim(psuser)
082500031008     C                             + '/'
082600031008     C                             + %trim(%editc(psjobnbr:'Z'))
082700031008     C                             + ' Parametro tipo file non passato.'
082800031008     C                   exsr      wrttivlt
082900120105     C                   eval      wrkesito = 'T'
083000031008     C*
083100991026     C                   exsr      uscita
083200991026     C                   endif
083300991026     C*
083400991026     C                   clear                   wrkesito
083500991026     C*
083600991026     C                   endsr
083700991111
083800000706
083900000706
084000991111      /TITLE Reperisco informazioni membro.
084100991111     C     rtvmbrd       begsr
084200991111     C* Pulisco e imposto la DS degli errori.
084300000124     C                   clear                   qusec
084400991111     C                   eval      qusbprv = %size(qusec)
084500991111     C*
084600991111     C                   call      'QUSRMBRD'
084700991111     C     qusm0200      parm                    mbrd01
084800991111     C                   parm                    mbrd02
084900991111     C                   parm                    mbrd03
085000991111     C                   parm                    mbrd04
085100991111     C                   parm                    mbrd05
085200991111     C                   parm                    mbrd06
085300991111     C                   parm                    qusec
085400991111     C*
085500991111     C                   endsr
085600031008
085700031008
085800031008
085900031008
086000031008      /TITLE Compongo il testo e spedisco una e-m@ail
086100031008     C     sndeml        begsr
086200031008     C*
086300031008     C* Inizializzo variabili
086400031008     C                   movel     *blanks       wrkEml          253
086500031008     C                   movel     *blanks       wrkEmlMsg      5000
086600031008     C                   movel     *blanks       wrkEmlOgg        44
086700031008     C* Valorizzo i campi della e-m@ail
086800120305     C                   eval      wrkEml='cedalert@brt.it'
086900031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
087000031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
087100031008     C                             'SU AS/400'                         +':/N'+
087200031008     C                             ' '                                 +':/N'+
087300031008     C                             'RIFERIMENTI'                       +':/N'+
087400031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
087500031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
087600031008     C                             'Cliente:'+vltksc                   +':/N'+
087700031008     C                             'TipoFile:'+vlttip                  +':/N'+
087800031008     C                             ' '                                 +':/N'+
087900031008     C                             'MESSAGGIO'                         +':/N'+
088000031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
088100031008     C*
088200031008     C                   call(e)   'TIS701C'
088300031008     C                   parm                    wrkEml
088400031008     C                   parm                    wrkEmlOgg
088500031008     C                   parm                    wrkEmlMsg
088600031008     C*
088700031008     C                   endsr
088800990910
088900000706
089000000706
089100990908      /TITLE Operazioni iniziali.
089200990908     C     *inzsr        begsr
089300990908     C*
089400990908     C     *entry        plist
089500991026     C     wrkksc        parm                    prmksc
089600991026     C     wrkksu        parm      wrkksu        prmksu
089700991026     C     wrktip        parm                    prmtip
089800991026     C                   parm      wrkesito      prmesito
089900000623     C                   parm                    prmprog
090000000623     C                   parm                    prmsrv
090100000628     C                   parm                    prmope
090200000713     C                   parm                    prmnfp
090300070301     C                   parm                    wprmout          10
090400120105     C                   parm                    wprmsrc           1
090500070301     C*
090600070301     C* Gestisco il ricevimento parametri
090700070301     C                   eval      prmout = *blanks
090800120105     C                   eval      prmsrc = *blanks
090900120105     C                   select
091000120105     C                   when      %parms = 9
091100070301     C                   eval      prmout = wprmout
091200120105     C                   when      %parms = 10
091300120105     C                   eval      prmout = wprmout
091400120105     C                   eval      prmsrc = wprmsrc
091500120105     C                   endsl
091600991104     C*
091700991105     C     k02vss02      klist
091800991105     C                   kfld                    vssksu
091900991104     C                   kfld                    vssisv
092000991026     C*
092100991026     C     k03vir02      klist
092200991026     C                   kfld                    virksc
092300991026     C                   kfld                    virtip
092400991026     C                   kfld                    virdti
092500991026     C*
092600991026     C     k02vir02      klist
092700991026     C                   kfld                    virksc
092800991026     C                   kfld                    virtip
092900000706     C*
093000000706     C     k04vir02      klist
093100000706     C                   kfld                    vreksc
093200000706     C                   kfld                    vretfl
093300000706     C                   kfld                    vredti
093400990920     C*
093500990920     C     k01vtf01      klist
093600990920     C                   kfld                    vtftip
093700000622     C*
093800000622     C     keytbe        klist
093900000622     C                   kfld                    tbecod
094000000622     C                   kfld                    tbeke1
094100000712     C*
094200000712     C     k01vlt        klist
094300000712     C                   kfld                    vltprg
094400000712     C                   kfld                    vltksc
094500031008     C*
094600031008     C     k08vlt        klist
094700031008     C                   kfld                    wrkfit
094800031008     C                   kfld                    wrkmbt
094900000706     C*
095000991104     C* Reperisco nome sistema.
095100991104     C                   eval      qusbprv = %size(qusec)
095200991104     C                   call      'QWCRNETA'
095300991104     C                   parm                    rneta01
095400991104     C                   parm                    rneta02
095500991104     C                   parm                    rneta03
095600991104     C                   parm                    rneta04
095700991104     C                   parm                    qusec
095800991104     C                   eval      nait = rneta01c
095900990908     C*
096000990908     C                   endsr
096100991026** ctdata mc
096200991026ABCDEFGHILMN
