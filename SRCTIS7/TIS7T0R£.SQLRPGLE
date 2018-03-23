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
040500170913     C                   select
040600170913     C                   when      prmesito = '1'
040700170913     C                   eval      wrksts = '1'
040800170913     C                   eval      wrkmsg = 'Traduzione effettuata.'
040900170913     C                   when      %error or prmesito <> '0'
041000170913     C                   eval      wrksts = '2'
041100170913     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
041200170913     C                   endsl
041300170913     C*
041400000712     C     k01vlt        setll     tivlt07l
041500000712     C                   if        %found(tivlt07l)
041600000712     C     k01vlt        reade     tivlt07l
041700000712     C                   dow       not %eof
041800000712     C                   if        vltsqz = *zeros and
041900000712     C                             (vltsts = '0' or vltsts = '1')
042000170913     C                   eval      vltsts = wrksts
042100000712     C                   eval      vltmsg =
042200000712     C                               %trim(sysname)
042300000712     C                             + '/'
042400000712     C                             + %trim(psjobname)
042500000712     C                             + '/'
042600000712     C                             + %trim(psuser)
042700000712     C                             + '/'
042800000712     C                             + %trim(%editc(psjobnbr:'Z'))
042900000712     C                             + %trim(wrkmsg)
043000000712     C                   update    tivlt700
043100000712     C                   endif
043200000712     C     k01vlt        reade     tivlt07l
043300000712     C                   enddo
043400000712     C                   endif
043500000712     C*
043600000711     C                   endif
043700000711     C*
043800000711     C                   endsr
043900000711
044000000711
044100000626
044200000626      /TITLE Valorizzazione Progressivo Applicazione
044300000626     C     calprog       begsr
044400000626     C*
044500000706     C                   if        dwlprg = *all'0'
044600000627     C                   read(e)   tis7prgf
044700000627     C                   if        not %error
044800000627     C                   eval      dwlprg = f_tis7prgf
044900000627     C*
045000000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
045100000719     C                   if        prmsrv <> *blanks
045200000719     C                   eval      vssisv = prmsrv
045300000719     C                   endif
045400000626     C*
045500000719     C                   if        prmprog = *blanks
045600051011     C                   move(p)   dwl_prg       wrkprg
045700000626     C                   add       1             wrkprg
045800051011     C                   move(p)   wrkprg        dwl_prg
045900051011     C                   movel     vssisv        dwl_isv
046000051207     C*                  if        wrktip <> *blanks
046100051207     C*                  movel     wrktip        dwl_tip
046200051207     C*                  else
046300051207     C*                  movel     'XX'          dwl_tip
046400051207     C*                  endif
046500040123     C                   eval      f_tis7prgf = dwlprg
046600040123     C                   update    tis7prg0
046700000626     C                   else
046800040123     C                   unlock(e) tis7prgf
046900000626     C                   eval      dwlprg = prmprog
047000000626     C                   endif
047100000627     C*
047200000627     C                   endif
047300000706     C                   endif
047400000626     C*
047500000626     C                   endsr
047600991104
047700000706
047800000706
047900000706
048000991104      /TITLE Controllo abilitazione servizio.
048100991104     C     chkvss        begsr
048200991104     C*
048300000914     C                   eval      vssok = *off
048400000914     C*
048500991105     C                   eval      vssksu = wrkksu
048600000914     C     k02vss02      setll     tivss02l
048700000914     C     k02vss02      reade     tivss02l
048800000914     C                   dow       not %eof
048900000914     C                   if        vssdde <= wrkoggi and
049000000914     C                             vssdsc >= wrkoggi and
049100000914     C                             vssvat <> 'A'     and
049200000914     C                             vssvat <> 'S'
049300000914     C                   eval      vssok = *on
049400000914     C                   leave
049500000914     C                   endif
049600000914     C     k02vss02      reade     tivss02l
049700000914     C                   enddo
049800000914     C*
049900000914     C                   if        vssok = *off
050000991104     C*
050100000622     C                   exsr      inztivlt
050200000329     C                   eval      vltsts = '2'
050300991104     C                   eval      vltmsg =
050400991104     C                               %trim(sysname)
050500991104     C                             + '/'
050600991104     C                             + %trim(psjobname)
050700991104     C                             + '/'
050800991104     C                             + %trim(psuser)
050900991104     C                             + '/'
051000991104     C                             + %trim(%editc(psjobnbr:'Z'))
051100991104     C                             + ' Cliente non abilitato al servizio.'
051200000622     C                   exsr      wrttivlt
051300000622     C                   eval      wrkesito = 'D'
051400031008     C*
051500000622     C                   else
051600031008     C*
051700031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
051800031008     C* del progressivo rispetto a file/membro e cliente
051900031008     C                   movel     '0'           chkPRG            1
052000031008     C     k08vlt        setll     tivlt08l
052100031008     C                   if        %equal(tivlt08l)
052200031008     C     k08vlt        reade     tivlt08l
052300031008     C                   dow       not %eof(tivlt08l)
052400031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
052500090414     C* (considero solamente i record in stato "da elaborare")
052600090414     C                   if        w_vltksc <> wrkksc AND
052700090414     C                             w_vltsts  = '0'
052800031008     C                   movel     '1'           chkPRG
052900031008     C                   endif
053000031008     C     k08vlt        reade     tivlt08l
053100031008     C                   enddo
053200031008     C                   endif
053300031008     C*
053400000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
053500031008     C                   if        chkPRG = '0'
053600000706     C                   exsr      traduci
053700031008     C                   else
053800031008     C*
053900031008     C                   exsr      inztivlt
054000031008     C                   eval      vltsts = '2'
054100031008     C                   eval      vltmsg =
054200031008     C                               %trim(sysname)
054300031008     C                             + '/'
054400031008     C                             + %trim(psjobname)
054500031008     C                             + '/'
054600031008     C                             + %trim(psuser)
054700031008     C                             + '/'
054800031008     C                             + %trim(%editc(psjobnbr:'Z'))
054900031008     C                             + ' Progressivo/membro NON univoco!!!'
055000031008     C                   exsr      wrttivlt
055100031008     C                   eval      wrkesito = 'F'
055200031008     C                   exsr      sndeml
055300031008     C                   exsr      uscita
055400031008     C                   endif
055500991104     C*
055600991104     C                   endif
055700991104     C*
055800991104     C                   endsr
055900000622
056000000622
056100000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
056200000622     C     rtvtia        begsr
056300000622     C*
056400000622     C                   eval      tbecod = 'ISV'
056500000731     C                   eval      tbeke1 = virfi1
056600000622     C     keytbe        chain     tntbe01l
056700000622     C                   if        %found(tntbe01l)
056800020212     C                   movel     tbeuni        disv
056900000622     C                   endif
057000000622     C*
057100000622     C                   endsr
057200000706
057300000706
057400000706
057500000706      /TITLE Traduzione.
057600000706     C     traduci       begsr
057700031105     C*
057800031105     C* Reperisco informazioni membro da tradurre.
057900031105     C                   eval      mbrd04a = wrkfid
058000031105     C                   eval      mbrd05  = wrkmbd
058100031105     C                   exsr      rtvmbrd
058200031105     C* Controllo se il membro da tradurre ha dei record.
058300031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
058400031105     C                   if        (qusei=*blanks and
058500060616     C                             qusnbrcr > 0) or prmnfp > '01'
058600060616     C                             or %trim(wrkfid) = '*'
058700030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
058800030512     C                   call      'TIS7T9R2'
058900031008     C                   parm                    wrkfit
059000031008     C                   parm                    wrkmbt
059100030512     C                   parm      '0'           chkFTP            1
059200031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
059300031105     C                   if        chkFTP = '0'
059400000706     C*
059500000706     C                   exsr      inztivlt
059600000706     C* Chiamo il pgm traduttore.
059700060616     C                   if        %trim(wrkfid) = '*' AND
059800060616     C                             %trim(wrkfis) = '*'
059900060616     C                   call(e)   'TIS7T9C3'
060000120105     C                   parm                    prmsrc
060100111230     C                   parm                    wKSC
060200111230     C                   parm                    wTIP
060300111230     C                   parm                    wDTI
060400060616     C                   parm      wrkmbd        prmmbd
060500060616     C                   parm      wrkfit        prmfit
060600060616     C                   parm      wrkmbt        prmmbt
060700060616     C                   parm      wrkpgt        prmpgt
060800060616     C                   parm      wrkppt        prmppt
060900060616     C                   parm      *blanks       prmesito
061000060616     C                   else
061100000706     C                   call(e)   'TIS7T9C'
061200120105     C                   parm                    prmsrc
061300111230     C                   parm                    wKSC
061400111230     C                   parm                    wTIP
061500111230     C                   parm                    wDTI
061600000706     C                   parm      wrkfid        prmfid
061700000706     C                   parm      wrkmbd        prmmbd
061800000706     C                   parm      wrkfis        prmfis
061900000706     C                   parm      wrkmbs        prmmbs
062000000706     C                   parm      wrkfit        prmfit
062100000706     C                   parm      wrkmbt        prmmbt
062200000706     C                   parm      wrkpgt        prmpgt
062300000706     C                   parm      wrkppt        prmppt
062400000706     C                   parm      *blanks       prmesito
062500060616     C                   endif
062600000706     C* Se la call è finita in errore
062700000706     C* o la traduzione ha avuto degli errori.
062800060320     C                   if        %error
062900060320     C                             or (prmesito <> '0' and
063000060320     C                                 prmesito <> 'Y')
063100000706     C                   eval      vltsts = '2'
063200000706     C                   eval      vltmsg =
063300000706     C                               %trim(sysname)
063400000706     C                             + '/'
063500000706     C                             + %trim(psjobname)
063600000706     C                             + '/'
063700000706     C                             + %trim(psuser)
063800000706     C                             + '/'
063900000706     C                             + %trim(%editc(psjobnbr:'Z'))
064000000706     C                             + ' Errori nella traduzione.'
064100000706     C                   else
064200060320     C                   if        prmesito = 'Y'
064300060320     C                   eval      wrkesito = prmesito
064400060320     C                   else
064500000706     C                   eval      wrkesito = '0'
064600060320     C                   endif
064700000706     C                   endif
064800000706     C*
064900000706     C                   exsr      wrttivlt
065000031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
065100031105     C                   else
065200030512     C                   exsr      inztivlt
065300030512     C                   eval      vltsts = '2'
065400030512     C                   eval      vltmsg =
065500030512     C                               %trim(sysname)
065600030512     C                             + '/'
065700030512     C                             + %trim(psjobname)
065800030512     C                             + '/'
065900030512     C                             + %trim(psuser)
066000030512     C                             + '/'
066100030512     C                             + %trim(%editc(psjobnbr:'Z'))
066200030512     C                             + ' Cliente/TipoFile in uso da server FTP'
066300030512     C                   exsr      wrttivlt
066400030512     C                   eval      wrkesito = 'E'
066500030512     C                   endif
066600031105     C* NO record da tradurre
066700031105     C                   endif
066800000706     C*
066900000706     C                   endsr
067000000328
067100000706
067200000706
067300000328      /TITLE Inizializzo variabili "wrk".
067400000328     C     inzwrk        begsr
067500000328     C*
067600000328     C                   clear                   wrkfit
067700000328     C                   clear                   wrkmbt
067800000328     C                   clear                   wrkpgi
067900000328     C                   clear                   wrkppi
068000000328     C                   clear                   wrkfis
068100000328     C                   clear                   wrkmbs
068200000328     C*
068300000328     C                   endsr
068400991026
068500000706
068600000706
068700991026      /TITLE Inizializzo TIVLT00F.
068800991026     C     inztivlt      begsr
068900000804     C*
069000000804     C                   exsr      rtvtia
069100991026     C*
069200000328     C                   clear                   tivlt000
069300991026     C                   eval      vltksc = wrkksc
069400991026     C                   eval      vltksu = wrkksu
069500000721     C                   eval      vlttip = wrktip
069600991026     C                   eval      vltfld = wrkfit
069700991026     C                   eval      vltmbd = wrkmbt
069800100311     C                   eval      vltpgi = wrkpgt                              * forzatura PGI->PGT
069900100311     C***                eval      vltppi = wrkppi
070000020212     C                   if        §isvproc = 'F'
070100000804     C                   eval      vltsnd = 'D'
070200000804     C                   else
070300991026     C                   eval      vltsnd = vtfsnd
070400000804     C                   endif
070500991026     C                   eval      vltfls = wrkfis
070600991026     C                   eval      vltmbs = wrkmbs
070700991026     C                   eval      vltdat = wrkoggi
070800991115     C                   eval      vltora = wrkora
070900991026     C                   eval      vltsts = '0'
071000991027     C                   eval      vltmsg =
071100991104     C                               %trim(sysname)
071200991104     C                             + '/'
071300991104     C                             + %trim(psjobname)
071400991027     C                             + '/'
071500991027     C                             + %trim(psuser)
071600991027     C                             + '/'
071700991027     C                             + %trim(%editc(psjobnbr:'Z'))
071800000328     C                   z-add     *zeros        vltsqz
071900000731     C*
072000991026     C                   endsr
072100991026
072200000706
072300000731
072400000731      /TITLE Inizializzo parzialmente TIVLT00F.
072500000731     C     inztivltp     begsr
072600000731     C*
072700000731     C                   clear                   tivlt000
072800000731     C                   eval      vltksc = wrkksc
072900000731     C                   eval      vltksu = wrkksu
073000000731     C                   eval      vlttip = wrktip
073100100311     C                   eval      vltpgi = wrkpgt                              * forzatura PGI->PGT
073200100311     C***                eval      vltppi = wrkppi
073300000731     C                   eval      vltsnd = vtfsnd
073400000731     C                   eval      vltdat = wrkoggi
073500000731     C                   eval      vltora = wrkora
073600000731     C                   eval      vltsts = '0'
073700000731     C                   eval      vltmsg =
073800000731     C                               %trim(sysname)
073900000731     C                             + '/'
074000000731     C                             + %trim(psjobname)
074100000731     C                             + '/'
074200000731     C                             + %trim(psuser)
074300000731     C                             + '/'
074400000731     C                             + %trim(%editc(psjobnbr:'Z'))
074500000731     C                   z-add     *zeros        vltsqz
074600000731     C*
074700000731     C                   exsr      rtvtia
074800000731     C*
074900000731     C                   endsr
075000000731
075100000731
075200000706
075300991026      /TITLE Scrittura TIVLT00F.
075400000731     C     wrttivlt      begsr
075500991026     C*
075600000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
075700000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
075800000731     C                   eval      vltisv = virfi1
075900020212     C                   eval      vlttia = §isvproc
076000000622     C                   eval      vltprg = dwlprg
076100000713     C                   eval      vltfl2 = prmnfp
076200991026     C                   write     tivlt000
076300040817     C*
076400050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
076500040817     C* procedura scambio dati
076600040817     C* ...quindi eseguo prima d tutto l'override
076700050909  "  C*
076800050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
076900050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
077000050909     C*                  eval      LUNG = %len(%trim(COMMAND))
077100050909  "  C*                  call(e)   'QCMDEXC'
077200050909     C*                  parm                    COMMAND
077300050909     C*                  parm                    LUNG             15 5
077400040817     C*
077500050909  "  C*                  if        not %error
077600050909     C*                  if        not %open(tivltnew)
077700050909     C*                  open(e)   tivltnew
077800050909     C*                  endif
077900050909  "  C*                  if        not %error
078000050909     C*                  write     tivltn00
078100050909     C*                  endif
078200050909     C*                  if        %open(tivltnew)
078300050909  "  C*                  close(e)  tivltnew
078400050909     C*                  endif
078500050909 TESTC*                  endif
078600991026     C*
078700991026     C                   endsr
078800991026
078900000706
079000000706
079100991026      /TITLE Operazioni finali.
079200991026     C     uscita        begsr
079300991026     C*
079400000627     C                   seton                                        lr
079500991026     C*
079600991026     C                   endsr
079700991026
079800000706
079900000706
080000991026      /TITLE Controllo formale parametri ricevuti.
080100991026     C     chkprm        begsr
080200991026     C*
080300991104     C                   if        wrkksu = *blanks
080400031008     C*
080500031008     C                   exsr      inztivlt
080600031008     C                   eval      vltsts = '2'
080700031008     C                   eval      vltmsg =
080800031008     C                               %trim(sysname)
080900031008     C                             + '/'
081000031008     C                             + %trim(psjobname)
081100031008     C                             + '/'
081200031008     C                             + %trim(psuser)
081300031008     C                             + '/'
081400031008     C                             + %trim(%editc(psjobnbr:'Z'))
081500031008     C                             + ' Parametro codice cliente non passato.'
081600031008     C                   exsr      wrttivlt
081700120105     C                   eval      wrkesito = 'K'
081800031008     C*
081900991026     C                   exsr      uscita
082000991026     C                   endif
082100991026     C*
082200991026     C                   if        wrktip = *blanks
082300031008     C*
082400031008     C                   exsr      inztivlt
082500031008     C                   eval      vltsts = '2'
082600031008     C                   eval      vltmsg =
082700031008     C                               %trim(sysname)
082800031008     C                             + '/'
082900031008     C                             + %trim(psjobname)
083000031008     C                             + '/'
083100031008     C                             + %trim(psuser)
083200031008     C                             + '/'
083300031008     C                             + %trim(%editc(psjobnbr:'Z'))
083400031008     C                             + ' Parametro tipo file non passato.'
083500031008     C                   exsr      wrttivlt
083600120105     C                   eval      wrkesito = 'T'
083700031008     C*
083800991026     C                   exsr      uscita
083900991026     C                   endif
084000991026     C*
084100991026     C                   clear                   wrkesito
084200991026     C*
084300991026     C                   endsr
084400991111
084500000706
084600000706
084700991111      /TITLE Reperisco informazioni membro.
084800991111     C     rtvmbrd       begsr
084900991111     C* Pulisco e imposto la DS degli errori.
085000000124     C                   clear                   qusec
085100991111     C                   eval      qusbprv = %size(qusec)
085200991111     C*
085300991111     C                   call      'QUSRMBRD'
085400991111     C     qusm0200      parm                    mbrd01
085500991111     C                   parm                    mbrd02
085600991111     C                   parm                    mbrd03
085700991111     C                   parm                    mbrd04
085800991111     C                   parm                    mbrd05
085900991111     C                   parm                    mbrd06
086000991111     C                   parm                    qusec
086100991111     C*
086200991111     C                   endsr
086300031008
086400031008
086500031008
086600031008
086700031008      /TITLE Compongo il testo e spedisco una e-m@ail
086800031008     C     sndeml        begsr
086900031008     C*
087000031008     C* Inizializzo variabili
087100031008     C                   movel     *blanks       wrkEml          253
087200031008     C                   movel     *blanks       wrkEmlMsg      5000
087300031008     C                   movel     *blanks       wrkEmlOgg        44
087400031008     C* Valorizzo i campi della e-m@ail
087500120305     C                   eval      wrkEml='cedalert@brt.it'
087600031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
087700031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
087800031008     C                             'SU AS/400'                         +':/N'+
087900031008     C                             ' '                                 +':/N'+
088000031008     C                             'RIFERIMENTI'                       +':/N'+
088100031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
088200031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
088300031008     C                             'Cliente:'+vltksc                   +':/N'+
088400031008     C                             'TipoFile:'+vlttip                  +':/N'+
088500031008     C                             ' '                                 +':/N'+
088600031008     C                             'MESSAGGIO'                         +':/N'+
088700031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
088800031008     C*
088900031008     C                   call(e)   'TIS701C'
089000031008     C                   parm                    wrkEml
089100031008     C                   parm                    wrkEmlOgg
089200031008     C                   parm                    wrkEmlMsg
089300031008     C*
089400031008     C                   endsr
089500990910
089600000706
089700000706
089800990908      /TITLE Operazioni iniziali.
089900990908     C     *inzsr        begsr
090000990908     C*
090100990908     C     *entry        plist
090200991026     C     wrkksc        parm                    prmksc
090300991026     C     wrkksu        parm      wrkksu        prmksu
090400991026     C     wrktip        parm                    prmtip
090500991026     C                   parm      wrkesito      prmesito
090600000623     C                   parm                    prmprog
090700000623     C                   parm                    prmsrv
090800000628     C                   parm                    prmope
090900000713     C                   parm                    prmnfp
091000070301     C                   parm                    wprmout          10
091100120105     C                   parm                    wprmsrc           1
091200070301     C*
091300070301     C* Gestisco il ricevimento parametri
091400070301     C                   eval      prmout = *blanks
091500120105     C                   eval      prmsrc = *blanks
091600120105     C                   select
091700120105     C                   when      %parms = 9
091800070301     C                   eval      prmout = wprmout
091900120105     C                   when      %parms = 10
092000120105     C                   eval      prmout = wprmout
092100120105     C                   eval      prmsrc = wprmsrc
092200120105     C                   endsl
092300991104     C*
092400991105     C     k02vss02      klist
092500991105     C                   kfld                    vssksu
092600991104     C                   kfld                    vssisv
092700991026     C*
092800991026     C     k03vir02      klist
092900991026     C                   kfld                    virksc
093000991026     C                   kfld                    virtip
093100991026     C                   kfld                    virdti
093200991026     C*
093300991026     C     k02vir02      klist
093400991026     C                   kfld                    virksc
093500991026     C                   kfld                    virtip
093600000706     C*
093700000706     C     k04vir02      klist
093800000706     C                   kfld                    vreksc
093900000706     C                   kfld                    vretfl
094000000706     C                   kfld                    vredti
094100990920     C*
094200990920     C     k01vtf01      klist
094300990920     C                   kfld                    vtftip
094400000622     C*
094500000622     C     keytbe        klist
094600000622     C                   kfld                    tbecod
094700000622     C                   kfld                    tbeke1
094800000712     C*
094900000712     C     k01vlt        klist
095000000712     C                   kfld                    vltprg
095100000712     C                   kfld                    vltksc
095200031008     C*
095300031008     C     k08vlt        klist
095400031008     C                   kfld                    wrkfit
095500031008     C                   kfld                    wrkmbt
095600000706     C*
095700991104     C* Reperisco nome sistema.
095800991104     C                   eval      qusbprv = %size(qusec)
095900991104     C                   call      'QWCRNETA'
096000991104     C                   parm                    rneta01
096100991104     C                   parm                    rneta02
096200991104     C                   parm                    rneta03
096300991104     C                   parm                    rneta04
096400991104     C                   parm                    qusec
096500991104     C                   eval      nait = rneta01c
096600990908     C*
096700990908     C                   endsr
096800991026** ctdata mc
096900991026ABCDEFGHILMN
