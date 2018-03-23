000100111230     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200991026
000300991026      **********************************************************************
000400991026      * Legenda PRMESITO
000500991026      * ' ' = Elaborazione non eseguita.
000600991026      * '0' = Elaborazione eseguita senza errori.
000700991104      * 'A' = Parametro PRMKSU vuoto.
000800991026      * 'B' = Parametro PRMTIP vuoto o sconosciuto.
000900991026      * 'C' = Tipo file non previsto per il cliente.
001000991104      * 'D' = Cliente non abilitato al servizio.
001100031008      * 'E' = File/membro già allocato dalla procedura FTP/e-mail sul server
001200031008      * 'F' = Progressivo NON univoco  => ATTENZIONE, possibili errori gravi ed imprevedibili!!!
001300991026      **********************************************************************
001400991026
001500050909 TESTF*tivltnew  o    e             disk    RENAME(tivlt000:tivltn00) USROPN
001600991026     Ftivlt00f  o    e             disk
001700000712     Ftivlt07l  uf   e           k disk    RENAME(tivlt000:tivlt700)
001800031008     Ftivlt08l  if   e           k disk    RENAME(tivlt000:tivlt800)
001900031008     F                                     PREFIX(w_)
002000991105     Ftivss02l  if   e           k disk
002100990920     Ftivtf01l  if   e           k disk
002200991026     Ftivir02l  if   e           k disk
002300000707     Ftivre01l  if   e           k disk
002400000622     Ftntbe01l  if   e           k disk
002500000627     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002600000627     F                                     PREFIX(f_)
002700000627
002800020212     D disv          e ds
002900030526     D dvir01        e ds
003000991027     D psds           sds
003100991027     D  psjobname            244    253
003200991027     D  psuser               254    263
003300991027     D  psjobnbr             264    269s 0
003400991026     D prmksc          s                   like(virksc)
003500991026     D prmksu          s                   like(virksc)
003600991026     D prmtip          s                   like(virtip)
003700991026     D prmesito        s              1
003800000623     D prmprog         s                   like(vltprg)
003900000623     D prmsrv          s                   like(vltisv)
004000000628     D prmope          s              1
004100000713     D prmnfp          s                   like(vltfl2)
004200070207     D prmout          s             10
004300120105     D prmsrc          s              1
004400991026     D wrkksc          s                   like(prmksc)
004500991026     D wrkksu          s                   like(prmksu)
004600991026     D wrktip          s                   like(prmtip)
004700991026     D wrkesito        s                   like(prmesito)
004800991026     D wrkoggiiso      s               d
004900991026     D wrkoggi         s              8  0
005000991026     D wrkora          s              6  0
005100991026     D wrkmese         s              2  0
005200000623     D wrkprg          s              8  0 INZ(*zeros)
005300000711     D wrkvarie        s              1    INZ('0')
005400170913     D wrksts          s              1    INZ(*blanks)
005500000712     D wrkmsg          s             80    INZ(*blanks)
005600000914     D vssok           s               n   INZ(*off)
005700051011
005800051011     D dwlprg          ds
005900051011     D  dwl_isv                       2    INZ(*all'0')
006000051207     D**dwl_tip                       2    INZ(*all'0')
006100051207     D**dwl_prg                       6    INZ(*all'0')
006200051207     D  dwl_prg                       8    INZ(*all'0')
006300000711
006400991026     D                 ds                  inz
006500991026     D wrkday                  1      2s 0
006600991026     D wrkdaychr                      2    overlay(wrkday)
006700991026     D wrkpgt          s                   like(vtfpgt)
006800991026     D wrkppt          s                   like(vtfppt)
006900000711     D wrkpgi          s                   like(vtfpgi)
007000000711     D wrkppi          s                   like(vtfppi)
007100991026     D wrkfid          s                   like(vtffid)
007200991026     D wrkmbd          s                   like(vltmbd)
007300991026     D wrkfis          s                   like(vtffis)
007400991026     D wrkmbs          s                   like(vltmbs)
007500991026     D wrkfit          s                   like(vtffit)
007600991026     D wrkmbt          s                   like(vltmbd)
007700991026     D prmfid          s                   like(wrkfid)
007800991026     D prmmbd          s                   like(wrkmbd)
007900991026     D prmfis          s                   like(wrkfis)
008000991026     D prmmbs          s                   like(wrkmbs)
008100991026     D prmfit          s                   like(wrkfit)
008200991027     D prmmbt          s                   like(wrkmbt)
008300991026     D prmpgt          s                   like(wrkpgt)
008400991026     D prmppt          s                   like(wrkppt)
008500991026     D mc              s              1    dim(12) ctdata perrcd(12)
008600111230
008700111230     D wKSC            s                   like(virKSC) inz
008800111230     D wTIP            s                   like(virTIP) inz
008900111230     D wDTI            s              8S 0 inz
009000111230
009100991104
009200991111     D mbrd01          s            266
009300991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
009400991111     D mbrd03          s              8    inz('MBRD0200')
009500991111     D mbrd04          ds
009600991111     D mbrd04a                       10                                         File
009700991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
009800991111     D mbrd05          s             10                                         Membro
009900991125     D mbrd06          s              1    inz('0')
010000991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
010100991104     D rneta01         ds
010200991104     D  rneta01a                      8b 0
010300991104     D  rneta01b                      8b 0
010400991104     D  rneta01c                     24
010500991104     D rneta02         s              8b 0 inz(%size(rneta01))
010600991104     D rneta03         s              8b 0 inz(1)
010700991104     D rneta04         s             10    inz('SYSNAME')
010800991104     D nait            ds
010900991104     D  nait01                       10
011000991104     D  nait02                        1
011100991104     D  nait03                        1
011200991104     D  nait04                        8b 0
011300991104     D  sysname                       8
011400991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
011500991104
011600000622
011700000905     D*-------------------
011800000905     D* DS di ridefinizione file di log x passaggio recordset SQL
011900000905     D*-------------------
012000000905     D  dsvlt        E DS                  extname(tivlt00f)
012100000905     D                                     prefix(s_)
012200000905
012300040817
012400040817     D*-------------------
012500050909 TESTD* Variabili d wrk
012600040817     D*-------------------
012700050909     D*  COMMAND        S            300    inz(*blanks)
012800040817
012900000905
013000000905
013100000622      /TITLE *** MAIN ***
013200090414     C*
013300090414     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013400090414     C
013500090414     C/EXEC SQL
013600090414     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013700090414     C/END-EXEC
013800000622     C*
013900000906     C                   if        prmnfp  > '01' and
014000000906     C                             prmprog > *blanks
014100000905     C                   exsr      rtvdatora
014200000905     C                   else
014300000905     C                   time                    wrkoggiiso
014400000905     C                   time                    wrkora
014500000905     C     *iso          move      wrkoggiiso    wrkoggi
014600000905     C                   endif
014700030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
014800000328     C* Inizializzo variabili "wrk".
014900000328     C                   exsr      inzwrk
015000991026     C* Controllo parametri ricevuti.
015100991026     C                   exsr      chkprm
015200000626     C* Reperisco informazioni file.
015300000706     C                   exsr      rtvleg
015400000706     C* Alla fine...
015500000706     C                   exsr      uscita
015600000626
015700000626
015800000905
015900000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
016000000905     C*
016100000905     C     rtvdatora     begsr
016200000905     C*
016300000905     C/EXEC SQL
016400000905     C+ declare C1 cursor for select *
016500000905     C+ from tivlt00f where
016600000905     C+ vltprg = :prmprog
016700000905     C+ for fetch only
016800000905     C/END-EXEC
016900000905     C
017000000905     C/EXEC SQL
017100000905     C+ open C1
017200000905     C/END-EXEC
017300000905     C
017400000905     C/EXEC SQL
017500000905     C+ Fetch C1 into :dsvlt
017600000905     C/END-EXEC
017700000905     C*
017800000906     C                   if        sqlcod = *zeros     and
017900000906     C                             s_vltprg  = prmprog and
018000000906     C                             s_vltdat  > *zeros  and
018100000905     C                             s_vltora  > *zeros
018200000905     C                   z-add     s_vltdat      wrkoggi
018300000905     C                   z-add     s_vltora      wrkora
018400000905     C                   else
018500000905     C                   time                    wrkoggiiso
018600000905     C                   time                    wrkora
018700000905     C     *iso          move      wrkoggiiso    wrkoggi
018800000905     C                   endif
018900000905     C
019000000905     C/EXEC SQL
019100000905     C+ close C1
019200000905     C/END-EXEC
019300000905     C
019400000905     C*
019500000905     C                   endsr
019600000905
019700000905
019800000706
019900000706      /TITLE Determino eventuali legami cliente/tipi file
020000000706     C     rtvleg        begsr
020100000706     C*
020200000706     C                   eval      virksc = wrkksu
020300000706     C                   eval      virtip = wrktip
020400000706     C                   eval      virdti = wrkoggi
020500000707     C                   eval      vtftip = wrktip
020600000706     C* Reperisco informazioni file.
020700000706     C                   exsr      rtvtip
020800000706     C*
020900000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
021000000713     C                   if        wrkesito = '0'     and
021100000713     C                             prmprog  = *blanks and
021200000713     C                             prmope   = *blanks
021300000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
021400000711     C                   movel     '1'           wrkvarie
021500000706     C*
021600000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
021700000707     C     k03vir02      setll     tivre01l
021800000707     C                   if        %found(tivre01l)
021900000707     C     k03vir02      reade     tivre01l
022000000706     C                   dow       not %eof
022100000706     C                   eval      virksc = wrkksu
022200000706     C                   eval      virtip = vretfl
022300000706     C                   eval      virdti = wrkoggi
022400000707     C                   eval      vtftip = vretfl
022500000712     C                   eval      wrktip = vretfl
022600000706     C* Reperisco informazioni file.
022700000706     C                   exsr      rtvtip
022800000712     C                   eval      virtip = vretip
022900000707     C     k03vir02      reade     tivre01l
023000000706     C                   enddo
023100000706     C                   endif
023200000706     C*
023300000706     C                   endif
023400031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
023500031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
023600031008     C                   if        chkPRG <> '1'
023700030512     C                   if        chkFTP <> '1'
023800000711     C                   exsr      exepgm
023900030512     C                   endif
024000031008     C                   endif
024100000706     C*
024200000706     C                   endsr
024300000706
024400000706
024500000706
024600000706      /TITLE Reperisco tipo file.
024700000706     C     rtvtip        begsr
024800120105     C* Inizializzazioni
024900120105     C                   clear                   wKSC
025000120105     C                   clear                   wTIP
025100120105     C                   clear                   wDTI
025200000706     C* Reperisco il tipo file.
025300000706     C     k01vtf01      chain     tivtf01l
025400000706     C* Se il tipo file non esiste
025500000706     C* oppure non è valido per il download.
025600000706     C                   if        not %found
025700000706     C                             or vtfud <> 'D'
025800031008     C*
025900031008     C                   exsr      inztivlt
026000031008     C                   eval      vltsts = '2'
026100031008     C                   eval      vltmsg =
026200031008     C                               %trim(sysname)
026300031008     C                             + '/'
026400031008     C                             + %trim(psjobname)
026500031008     C                             + '/'
026600031008     C                             + %trim(psuser)
026700031008     C                             + '/'
026800031008     C                             + %trim(%editc(psjobnbr:'Z'))
026900031008     C                             + ' Tipo file inesistente.'
027000031008     C                   exsr      wrttivlt
027100031008     C                   eval      wrkesito = 'B'
027200031008     C*
027300000721     C                   else
027400000706     C* Reperisco il tipo file del cliente.
027500000706     C     k03vir02      setgt     tivir02l
027600000706     C     k02vir02      readpe    tivir02l
027700000706     C* Tipo file non previsto per il cliente
027800000706     C* oppure scaduto.
027900000731     C                   if        %eof
028000000706     C                             or virdtf < wrkoggi
028100000706     C                             or virdti > wrkoggi
028200000706     C*
028300000731     C                   exsr      inztivltp
028400000706     C*
028500000706     C                   eval      vltsts = '2'
028600000706     C                   eval      vltmsg =
028700000706     C                               %trim(sysname)
028800000706     C                             + '/'
028900000706     C                             + %trim(psjobname)
029000000706     C                             + '/'
029100000706     C                             + %trim(psuser)
029200000706     C                             + '/'
029300000706     C                             + %trim(%editc(psjobnbr:'Z'))
029400000706     C                             + ' Tipo file non previsto per il cliente.'
029500000706     C                   exsr      wrttivlt
029600000706     C                   eval      wrkesito = 'C'
029700000706     C*
029800000706     C                   else
029900111230     C*
030000111230     C* Salvo subito la chiave univoca della regola corrente
030100111230     C                   eval      wKSC = virKSC
030200111230     C                   eval      wTIP = virTIP
030300111230     C                   eval      wDTI = virDTI
030400000706     C*
030500000706     C* Imposto i default.
030600000706     C                   if        virpgt <> *blanks
030700000706     C                   eval      wrkpgt = virpgt
030800000706     C                   else
030900000706     C                   eval      wrkpgt = vtfpgt
031000000706     C                   endif
031100170119     C                   if        virppt <> *blanks
031200170119     C                   eval      wrkppt = virppt
031300170119     C                   else
031400170119     C                   eval      wrkppt = vtfppt
031500170119     C                   endif
031600000712     C*
031700000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
031800000711     C                   if        wrkvarie = '0'                               <===================
031900000711     C                   if        virpgi <> *blanks                            <===================
032000000711     C                   eval      wrkpgi = virpgi                              <===================
032100000711     C                   eval      wrkppi = virppi                              <===================
032200000711     C                   else                                                   <===================
032300000711     C                   eval      wrkpgi = vtfpgi                              <===================
032400101013     C***                eval      wrkppi = vtfppi                              <===================
032500000711     C                   endif                                                  <===================
032600000711     C                   endif                                                  <===================
032700000706     C* File da tradurre.
032800000706     C                   if        virfid <> *blanks
032900000706     C                   eval      wrkfid = virfid
033000000706     C                   else
033100000706     C                   eval      wrkfid = vtffid
033200000706     C                   endif
033300000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
033400000706     C* File storico.
033500000706     C                   if        virfis <> *blanks
033600000706     C                   eval      wrkfis = virfis
033700000706     C                   else
033800000706     C                   eval      wrkfis = vtffis
033900000706     C                   endif
034000000706     C                   extrct    wrkoggiiso:*m wrkmese
034100000706     C                   extrct    wrkoggiiso:*d wrkday
034200000706     C                   eval      wrkmbs =
034300000706     C                             mc(wrkmese)
034400000706     C                             + wrkdaychr
034500000706     C                             + %subst(wrkksu:2:7)
034600030519     C*
034700030519     C                   eval      vssisv = virfi1
034800030519     C* Valorizzo subito il progressivo applicazione.
034900030519     C                   exsr      calprog
035000000706     C* File tradotto.
035100000706     C                   if        virfit <> *blanks
035200000706     C                   eval      wrkfit = virfit
035300000706     C                   else
035400000706     C                   eval      wrkfit = vtffit
035500000706     C                   endif
035600030526     C* Tipo nome membro tradotto
035700030526     C                   eval      dvir01 = virpth
035800070207     C* Se forzatura in ingresso adotto quella, altrimenti applico considerazioni standard
035900070207     C                   if        prmout <> *blanks
036000070207     C                   eval      wrkmbt = prmout                              membro = forzatura
036100070207     C                   else
036200030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
036300100311     C                   select
036400100311     C                   when      §VIR01TM = 'P'
036500030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
036600030526 xxx C***                eval      wrkmbs = dwlprg
036700100311     C                   when      §VIR01TM = 'T'
036800070504     C                   eval      wrkmbt = dwlprg                              membro = progressivo
036900070504     C                   eval      %subst(wrkmbt:3:2) = wrktip                  membro = prog + tipf
037000100311     C                   when      §VIR01TM = 'M'
037100100311     C                   eval      wrkmbt = wrktip + wrkksu                     membro = tipf + KSC
037200100311     C                   other
037300030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
037400100311     C                   endsl
037500030519     C                   endif
037600000706     C* Controllo abilitazione al servizio.
037700000706     C                   exsr      chkvss
037800000706     C                   endif
037900000706     C                   endif
038000000706     C*
038100000706     C                   endsr
038200000706
038300000711
038400000711
038500000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
038600000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
038700000711     C*
038800000711     C     exepgm        begsr
038900000711     C* Lancio il pgm in variabile con relativi parametri
039000000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
039100000711    C                   if        wrkpgi <> *blanks
039200000712     C* Chiamo il pgm post traduzione.
039300000712     C                   call(e)   'TIS7T9C1'
039400000712     C                   parm      wrkfid        prmfid
039500000712     C                   parm      wrkmbd        prmmbd
039600000712     C                   parm      wrkfis        prmfis
039700000712     C                   parm      wrkmbs        prmmbs
039800031106     C                   parm      wrkfit        prmfit
039900031106     C                   parm      wrkmbt        prmmbt
040000000712     C                   parm                    wrkpgi
040100000712     C                   parm                    wrkppi
040200000712     C                   parm      *blanks       prmesito
040300000712     C* Se la call è finita in errore
040400000712     C* o il lancio del PGM ha avuto degli errori.
040500170914     C                   setoff                                       55
040600170913     C                   select
040700170913     C                   when      prmesito = '1'
040800170913     C                   eval      wrksts = '1'
040900170913     C                   eval      wrkmsg = 'Traduzione effettuata.'
041000170914     C                   seton                                        55
041100170913     C                   when      %error or prmesito <> '0'
041200170913     C                   eval      wrksts = '2'
041300170913     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
041400170914     C                   seton                                        55
041500170913     C                   endsl
041600170913     C*
041700170914     C                   if        *in55
041800000712     C     k01vlt        setll     tivlt07l
041900000712     C                   if        %found(tivlt07l)
042000000712     C     k01vlt        reade     tivlt07l
042100000712     C                   dow       not %eof
042200000712     C                   if        vltsqz = *zeros and
042300000712     C                             (vltsts = '0' or vltsts = '1')
042400170913     C                   eval      vltsts = wrksts
042500000712     C                   eval      vltmsg =
042600000712     C                               %trim(sysname)
042700000712     C                             + '/'
042800000712     C                             + %trim(psjobname)
042900000712     C                             + '/'
043000000712     C                             + %trim(psuser)
043100000712     C                             + '/'
043200000712     C                             + %trim(%editc(psjobnbr:'Z'))
043300000712     C                             + %trim(wrkmsg)
043400000712     C                   update    tivlt700
043500000712     C                   endif
043600000712     C     k01vlt        reade     tivlt07l
043700000712     C                   enddo
043800000712     C                   endif
043900170914     C                   endif
044000000712     C*
044100000711     C                   endif
044200000711     C*
044300000711     C                   endsr
044400000711
044500000711
044600000626
044700000626      /TITLE Valorizzazione Progressivo Applicazione
044800000626     C     calprog       begsr
044900000626     C*
045000000706     C                   if        dwlprg = *all'0'
045100000627     C                   read(e)   tis7prgf
045200000627     C                   if        not %error
045300000627     C                   eval      dwlprg = f_tis7prgf
045400000627     C*
045500000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
045600000719     C                   if        prmsrv <> *blanks
045700000719     C                   eval      vssisv = prmsrv
045800000719     C                   endif
045900000626     C*
046000000719     C                   if        prmprog = *blanks
046100051011     C                   move(p)   dwl_prg       wrkprg
046200000626     C                   add       1             wrkprg
046300051011     C                   move(p)   wrkprg        dwl_prg
046400051011     C                   movel     vssisv        dwl_isv
046500051207     C*                  if        wrktip <> *blanks
046600051207     C*                  movel     wrktip        dwl_tip
046700051207     C*                  else
046800051207     C*                  movel     'XX'          dwl_tip
046900051207     C*                  endif
047000040123     C                   eval      f_tis7prgf = dwlprg
047100040123     C                   update    tis7prg0
047200000626     C                   else
047300040123     C                   unlock(e) tis7prgf
047400000626     C                   eval      dwlprg = prmprog
047500000626     C                   endif
047600000627     C*
047700000627     C                   endif
047800000706     C                   endif
047900000626     C*
048000000626     C                   endsr
048100991104
048200000706
048300000706
048400000706
048500991104      /TITLE Controllo abilitazione servizio.
048600991104     C     chkvss        begsr
048700991104     C*
048800000914     C                   eval      vssok = *off
048900000914     C*
049000991105     C                   eval      vssksu = wrkksu
049100000914     C     k02vss02      setll     tivss02l
049200000914     C     k02vss02      reade     tivss02l
049300000914     C                   dow       not %eof
049400000914     C                   if        vssdde <= wrkoggi and
049500000914     C                             vssdsc >= wrkoggi and
049600000914     C                             vssvat <> 'A'     and
049700000914     C                             vssvat <> 'S'
049800000914     C                   eval      vssok = *on
049900000914     C                   leave
050000000914     C                   endif
050100000914     C     k02vss02      reade     tivss02l
050200000914     C                   enddo
050300000914     C*
050400000914     C                   if        vssok = *off
050500991104     C*
050600000622     C                   exsr      inztivlt
050700000329     C                   eval      vltsts = '2'
050800991104     C                   eval      vltmsg =
050900991104     C                               %trim(sysname)
051000991104     C                             + '/'
051100991104     C                             + %trim(psjobname)
051200991104     C                             + '/'
051300991104     C                             + %trim(psuser)
051400991104     C                             + '/'
051500991104     C                             + %trim(%editc(psjobnbr:'Z'))
051600991104     C                             + ' Cliente non abilitato al servizio.'
051700000622     C                   exsr      wrttivlt
051800000622     C                   eval      wrkesito = 'D'
051900031008     C*
052000000622     C                   else
052100031008     C*
052200031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
052300031008     C* del progressivo rispetto a file/membro e cliente
052400031008     C                   movel     '0'           chkPRG            1
052500031008     C     k08vlt        setll     tivlt08l
052600031008     C                   if        %equal(tivlt08l)
052700031008     C     k08vlt        reade     tivlt08l
052800031008     C                   dow       not %eof(tivlt08l)
052900031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
053000090414     C* (considero solamente i record in stato "da elaborare")
053100090414     C                   if        w_vltksc <> wrkksc AND
053200090414     C                             w_vltsts  = '0'
053300031008     C                   movel     '1'           chkPRG
053400031008     C                   endif
053500031008     C     k08vlt        reade     tivlt08l
053600031008     C                   enddo
053700031008     C                   endif
053800031008     C*
053900000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
054000031008     C                   if        chkPRG = '0'
054100000706     C                   exsr      traduci
054200031008     C                   else
054300031008     C*
054400031008     C                   exsr      inztivlt
054500031008     C                   eval      vltsts = '2'
054600031008     C                   eval      vltmsg =
054700031008     C                               %trim(sysname)
054800031008     C                             + '/'
054900031008     C                             + %trim(psjobname)
055000031008     C                             + '/'
055100031008     C                             + %trim(psuser)
055200031008     C                             + '/'
055300031008     C                             + %trim(%editc(psjobnbr:'Z'))
055400031008     C                             + ' Progressivo/membro NON univoco!!!'
055500031008     C                   exsr      wrttivlt
055600031008     C                   eval      wrkesito = 'F'
055700031008     C                   exsr      sndeml
055800031008     C                   exsr      uscita
055900031008     C                   endif
056000991104     C*
056100991104     C                   endif
056200991104     C*
056300991104     C                   endsr
056400000622
056500000622
056600000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
056700000622     C     rtvtia        begsr
056800000622     C*
056900000622     C                   eval      tbecod = 'ISV'
057000000731     C                   eval      tbeke1 = virfi1
057100000622     C     keytbe        chain     tntbe01l
057200000622     C                   if        %found(tntbe01l)
057300020212     C                   movel     tbeuni        disv
057400000622     C                   endif
057500000622     C*
057600000622     C                   endsr
057700000706
057800000706
057900000706
058000000706      /TITLE Traduzione.
058100000706     C     traduci       begsr
058200031105     C*
058300031105     C* Reperisco informazioni membro da tradurre.
058400031105     C                   eval      mbrd04a = wrkfid
058500031105     C                   eval      mbrd05  = wrkmbd
058600031105     C                   exsr      rtvmbrd
058700031105     C* Controllo se il membro da tradurre ha dei record.
058800031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
058900031105     C                   if        (qusei=*blanks and
059000060616     C                             qusnbrcr > 0) or prmnfp > '01'
059100060616     C                             or %trim(wrkfid) = '*'
059200030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
059300030512     C                   call      'TIS7T9R2'
059400031008     C                   parm                    wrkfit
059500031008     C                   parm                    wrkmbt
059600030512     C                   parm      '0'           chkFTP            1
059700031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
059800031105     C                   if        chkFTP = '0'
059900000706     C*
060000000706     C                   exsr      inztivlt
060100000706     C* Chiamo il pgm traduttore.
060200060616     C                   if        %trim(wrkfid) = '*' AND
060300060616     C                             %trim(wrkfis) = '*'
060400060616     C                   call(e)   'TIS7T9C3'
060500120105     C                   parm                    prmsrc
060600111230     C                   parm                    wKSC
060700111230     C                   parm                    wTIP
060800111230     C                   parm                    wDTI
060900060616     C                   parm      wrkmbd        prmmbd
061000060616     C                   parm      wrkfit        prmfit
061100060616     C                   parm      wrkmbt        prmmbt
061200060616     C                   parm      wrkpgt        prmpgt
061300060616     C                   parm      wrkppt        prmppt
061400060616     C                   parm      *blanks       prmesito
061500060616     C                   else
061600000706     C                   call(e)   'TIS7T9C'
061700120105     C                   parm                    prmsrc
061800111230     C                   parm                    wKSC
061900111230     C                   parm                    wTIP
062000111230     C                   parm                    wDTI
062100000706     C                   parm      wrkfid        prmfid
062200000706     C                   parm      wrkmbd        prmmbd
062300000706     C                   parm      wrkfis        prmfis
062400000706     C                   parm      wrkmbs        prmmbs
062500000706     C                   parm      wrkfit        prmfit
062600000706     C                   parm      wrkmbt        prmmbt
062700000706     C                   parm      wrkpgt        prmpgt
062800000706     C                   parm      wrkppt        prmppt
062900000706     C                   parm      *blanks       prmesito
063000060616     C                   endif
063100000706     C* Se la call è finita in errore
063200000706     C* o la traduzione ha avuto degli errori.
063300060320     C                   if        %error
063400060320     C                             or (prmesito <> '0' and
063500060320     C                                 prmesito <> 'Y')
063600000706     C                   eval      vltsts = '2'
063700000706     C                   eval      vltmsg =
063800000706     C                               %trim(sysname)
063900000706     C                             + '/'
064000000706     C                             + %trim(psjobname)
064100000706     C                             + '/'
064200000706     C                             + %trim(psuser)
064300000706     C                             + '/'
064400000706     C                             + %trim(%editc(psjobnbr:'Z'))
064500000706     C                             + ' Errori nella traduzione.'
064600000706     C                   else
064700060320     C                   if        prmesito = 'Y'
064800060320     C                   eval      wrkesito = prmesito
064900060320     C                   else
065000000706     C                   eval      wrkesito = '0'
065100060320     C                   endif
065200000706     C                   endif
065300000706     C*
065400000706     C                   exsr      wrttivlt
065500031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
065600031105     C                   else
065700030512     C                   exsr      inztivlt
065800030512     C                   eval      vltsts = '2'
065900030512     C                   eval      vltmsg =
066000030512     C                               %trim(sysname)
066100030512     C                             + '/'
066200030512     C                             + %trim(psjobname)
066300030512     C                             + '/'
066400030512     C                             + %trim(psuser)
066500030512     C                             + '/'
066600030512     C                             + %trim(%editc(psjobnbr:'Z'))
066700030512     C                             + ' Cliente/TipoFile in uso da server FTP'
066800030512     C                   exsr      wrttivlt
066900030512     C                   eval      wrkesito = 'E'
067000030512     C                   endif
067100031105     C* NO record da tradurre
067200031105     C                   endif
067300000706     C*
067400000706     C                   endsr
067500000328
067600000706
067700000706
067800000328      /TITLE Inizializzo variabili "wrk".
067900000328     C     inzwrk        begsr
068000000328     C*
068100000328     C                   clear                   wrkfit
068200000328     C                   clear                   wrkmbt
068300000328     C                   clear                   wrkpgi
068400000328     C                   clear                   wrkppi
068500000328     C                   clear                   wrkfis
068600000328     C                   clear                   wrkmbs
068700000328     C*
068800000328     C                   endsr
068900991026
069000000706
069100000706
069200991026      /TITLE Inizializzo TIVLT00F.
069300991026     C     inztivlt      begsr
069400000804     C*
069500000804     C                   exsr      rtvtia
069600991026     C*
069700000328     C                   clear                   tivlt000
069800991026     C                   eval      vltksc = wrkksc
069900991026     C                   eval      vltksu = wrkksu
070000000721     C                   eval      vlttip = wrktip
070100991026     C                   eval      vltfld = wrkfit
070200991026     C                   eval      vltmbd = wrkmbt
070300100311     C                   eval      vltpgi = wrkpgt                              * forzatura PGI->PGT
070400100311     C***                eval      vltppi = wrkppi
070500020212     C                   if        §isvproc = 'F'
070600000804     C                   eval      vltsnd = 'D'
070700000804     C                   else
070800991026     C                   eval      vltsnd = vtfsnd
070900000804     C                   endif
071000991026     C                   eval      vltfls = wrkfis
071100991026     C                   eval      vltmbs = wrkmbs
071200991026     C                   eval      vltdat = wrkoggi
071300991115     C                   eval      vltora = wrkora
071400991026     C                   eval      vltsts = '0'
071500991027     C                   eval      vltmsg =
071600991104     C                               %trim(sysname)
071700991104     C                             + '/'
071800991104     C                             + %trim(psjobname)
071900991027     C                             + '/'
072000991027     C                             + %trim(psuser)
072100991027     C                             + '/'
072200991027     C                             + %trim(%editc(psjobnbr:'Z'))
072300000328     C                   z-add     *zeros        vltsqz
072400000731     C*
072500991026     C                   endsr
072600991026
072700000706
072800000731
072900000731      /TITLE Inizializzo parzialmente TIVLT00F.
073000000731     C     inztivltp     begsr
073100000731     C*
073200000731     C                   clear                   tivlt000
073300000731     C                   eval      vltksc = wrkksc
073400000731     C                   eval      vltksu = wrkksu
073500000731     C                   eval      vlttip = wrktip
073600100311     C                   eval      vltpgi = wrkpgt                              * forzatura PGI->PGT
073700100311     C***                eval      vltppi = wrkppi
073800000731     C                   eval      vltsnd = vtfsnd
073900000731     C                   eval      vltdat = wrkoggi
074000000731     C                   eval      vltora = wrkora
074100000731     C                   eval      vltsts = '0'
074200000731     C                   eval      vltmsg =
074300000731     C                               %trim(sysname)
074400000731     C                             + '/'
074500000731     C                             + %trim(psjobname)
074600000731     C                             + '/'
074700000731     C                             + %trim(psuser)
074800000731     C                             + '/'
074900000731     C                             + %trim(%editc(psjobnbr:'Z'))
075000000731     C                   z-add     *zeros        vltsqz
075100000731     C*
075200000731     C                   exsr      rtvtia
075300000731     C*
075400000731     C                   endsr
075500000731
075600000731
075700000706
075800991026      /TITLE Scrittura TIVLT00F.
075900000731     C     wrttivlt      begsr
076000991026     C*
076100000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
076200000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
076300000731     C                   eval      vltisv = virfi1
076400020212     C                   eval      vlttia = §isvproc
076500000622     C                   eval      vltprg = dwlprg
076600000713     C                   eval      vltfl2 = prmnfp
076700991026     C                   write     tivlt000
076800040817     C*
076900050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
077000040817     C* procedura scambio dati
077100040817     C* ...quindi eseguo prima d tutto l'override
077200050909  "  C*
077300050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
077400050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
077500050909     C*                  eval      LUNG = %len(%trim(COMMAND))
077600050909  "  C*                  call(e)   'QCMDEXC'
077700050909     C*                  parm                    COMMAND
077800050909     C*                  parm                    LUNG             15 5
077900040817     C*
078000050909  "  C*                  if        not %error
078100050909     C*                  if        not %open(tivltnew)
078200050909     C*                  open(e)   tivltnew
078300050909     C*                  endif
078400050909  "  C*                  if        not %error
078500050909     C*                  write     tivltn00
078600050909     C*                  endif
078700050909     C*                  if        %open(tivltnew)
078800050909  "  C*                  close(e)  tivltnew
078900050909     C*                  endif
079000050909 TESTC*                  endif
079100991026     C*
079200991026     C                   endsr
079300991026
079400000706
079500000706
079600991026      /TITLE Operazioni finali.
079700991026     C     uscita        begsr
079800991026     C*
079900000627     C                   seton                                        lr
080000991026     C*
080100991026     C                   endsr
080200991026
080300000706
080400000706
080500991026      /TITLE Controllo formale parametri ricevuti.
080600991026     C     chkprm        begsr
080700991026     C*
080800991104     C                   if        wrkksu = *blanks
080900031008     C*
081000031008     C                   exsr      inztivlt
081100031008     C                   eval      vltsts = '2'
081200031008     C                   eval      vltmsg =
081300031008     C                               %trim(sysname)
081400031008     C                             + '/'
081500031008     C                             + %trim(psjobname)
081600031008     C                             + '/'
081700031008     C                             + %trim(psuser)
081800031008     C                             + '/'
081900031008     C                             + %trim(%editc(psjobnbr:'Z'))
082000031008     C                             + ' Parametro codice cliente non passato.'
082100031008     C                   exsr      wrttivlt
082200120105     C                   eval      wrkesito = 'K'
082300031008     C*
082400991026     C                   exsr      uscita
082500991026     C                   endif
082600991026     C*
082700991026     C                   if        wrktip = *blanks
082800031008     C*
082900031008     C                   exsr      inztivlt
083000031008     C                   eval      vltsts = '2'
083100031008     C                   eval      vltmsg =
083200031008     C                               %trim(sysname)
083300031008     C                             + '/'
083400031008     C                             + %trim(psjobname)
083500031008     C                             + '/'
083600031008     C                             + %trim(psuser)
083700031008     C                             + '/'
083800031008     C                             + %trim(%editc(psjobnbr:'Z'))
083900031008     C                             + ' Parametro tipo file non passato.'
084000031008     C                   exsr      wrttivlt
084100120105     C                   eval      wrkesito = 'T'
084200031008     C*
084300991026     C                   exsr      uscita
084400991026     C                   endif
084500991026     C*
084600991026     C                   clear                   wrkesito
084700991026     C*
084800991026     C                   endsr
084900991111
085000000706
085100000706
085200991111      /TITLE Reperisco informazioni membro.
085300991111     C     rtvmbrd       begsr
085400991111     C* Pulisco e imposto la DS degli errori.
085500000124     C                   clear                   qusec
085600991111     C                   eval      qusbprv = %size(qusec)
085700991111     C*
085800991111     C                   call      'QUSRMBRD'
085900991111     C     qusm0200      parm                    mbrd01
086000991111     C                   parm                    mbrd02
086100991111     C                   parm                    mbrd03
086200991111     C                   parm                    mbrd04
086300991111     C                   parm                    mbrd05
086400991111     C                   parm                    mbrd06
086500991111     C                   parm                    qusec
086600991111     C*
086700991111     C                   endsr
086800031008
086900031008
087000031008
087100031008
087200031008      /TITLE Compongo il testo e spedisco una e-m@ail
087300031008     C     sndeml        begsr
087400031008     C*
087500031008     C* Inizializzo variabili
087600031008     C                   movel     *blanks       wrkEml          253
087700031008     C                   movel     *blanks       wrkEmlMsg      5000
087800031008     C                   movel     *blanks       wrkEmlOgg        44
087900031008     C* Valorizzo i campi della e-m@ail
088000120305     C                   eval      wrkEml='cedalert@brt.it'
088100031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
088200031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
088300031008     C                             'SU AS/400'                         +':/N'+
088400031008     C                             ' '                                 +':/N'+
088500031008     C                             'RIFERIMENTI'                       +':/N'+
088600031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
088700031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
088800031008     C                             'Cliente:'+vltksc                   +':/N'+
088900031008     C                             'TipoFile:'+vlttip                  +':/N'+
089000031008     C                             ' '                                 +':/N'+
089100031008     C                             'MESSAGGIO'                         +':/N'+
089200031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
089300031008     C*
089400031008     C                   call(e)   'TIS701C'
089500031008     C                   parm                    wrkEml
089600031008     C                   parm                    wrkEmlOgg
089700031008     C                   parm                    wrkEmlMsg
089800031008     C*
089900031008     C                   endsr
090000990910
090100000706
090200000706
090300990908      /TITLE Operazioni iniziali.
090400990908     C     *inzsr        begsr
090500990908     C*
090600990908     C     *entry        plist
090700991026     C     wrkksc        parm                    prmksc
090800991026     C     wrkksu        parm      wrkksu        prmksu
090900991026     C     wrktip        parm                    prmtip
091000991026     C                   parm      wrkesito      prmesito
091100000623     C                   parm                    prmprog
091200000623     C                   parm                    prmsrv
091300000628     C                   parm                    prmope
091400000713     C                   parm                    prmnfp
091500070301     C                   parm                    wprmout          10
091600120105     C                   parm                    wprmsrc           1
091700070301     C*
091800070301     C* Gestisco il ricevimento parametri
091900070301     C                   eval      prmout = *blanks
092000120105     C                   eval      prmsrc = *blanks
092100120105     C                   select
092200120105     C                   when      %parms = 9
092300070301     C                   eval      prmout = wprmout
092400120105     C                   when      %parms = 10
092500120105     C                   eval      prmout = wprmout
092600120105     C                   eval      prmsrc = wprmsrc
092700120105     C                   endsl
092800991104     C*
092900991105     C     k02vss02      klist
093000991105     C                   kfld                    vssksu
093100991104     C                   kfld                    vssisv
093200991026     C*
093300991026     C     k03vir02      klist
093400991026     C                   kfld                    virksc
093500991026     C                   kfld                    virtip
093600991026     C                   kfld                    virdti
093700991026     C*
093800991026     C     k02vir02      klist
093900991026     C                   kfld                    virksc
094000991026     C                   kfld                    virtip
094100000706     C*
094200000706     C     k04vir02      klist
094300000706     C                   kfld                    vreksc
094400000706     C                   kfld                    vretfl
094500000706     C                   kfld                    vredti
094600990920     C*
094700990920     C     k01vtf01      klist
094800990920     C                   kfld                    vtftip
094900000622     C*
095000000622     C     keytbe        klist
095100000622     C                   kfld                    tbecod
095200000622     C                   kfld                    tbeke1
095300000712     C*
095400000712     C     k01vlt        klist
095500000712     C                   kfld                    vltprg
095600000712     C                   kfld                    vltksc
095700031008     C*
095800031008     C     k08vlt        klist
095900031008     C                   kfld                    wrkfit
096000031008     C                   kfld                    wrkmbt
096100000706     C*
096200991104     C* Reperisco nome sistema.
096300991104     C                   eval      qusbprv = %size(qusec)
096400991104     C                   call      'QWCRNETA'
096500991104     C                   parm                    rneta01
096600991104     C                   parm                    rneta02
096700991104     C                   parm                    rneta03
096800991104     C                   parm                    rneta04
096900991104     C                   parm                    qusec
097000991104     C                   eval      nait = rneta01c
097100990908     C*
097200990908     C                   endsr
097300991026** ctdata mc
097400991026ABCDEFGHILMN
