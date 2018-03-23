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
005400000712     D wrkmsg          s             80    INZ(*blanks)
005500000914     D vssok           s               n   INZ(*off)
005600051011
005700051011     D dwlprg          ds
005800051011     D  dwl_isv                       2    INZ(*all'0')
005900051207     D**dwl_tip                       2    INZ(*all'0')
006000051207     D**dwl_prg                       6    INZ(*all'0')
006100051207     D  dwl_prg                       8    INZ(*all'0')
006200000711
006300991026     D                 ds                  inz
006400991026     D wrkday                  1      2s 0
006500991026     D wrkdaychr                      2    overlay(wrkday)
006600991026     D wrkpgt          s                   like(vtfpgt)
006700991026     D wrkppt          s                   like(vtfppt)
006800000711     D wrkpgi          s                   like(vtfpgi)
006900000711     D wrkppi          s                   like(vtfppi)
007000991026     D wrkfid          s                   like(vtffid)
007100991026     D wrkmbd          s                   like(vltmbd)
007200991026     D wrkfis          s                   like(vtffis)
007300991026     D wrkmbs          s                   like(vltmbs)
007400991026     D wrkfit          s                   like(vtffit)
007500991026     D wrkmbt          s                   like(vltmbd)
007600991026     D prmfid          s                   like(wrkfid)
007700991026     D prmmbd          s                   like(wrkmbd)
007800991026     D prmfis          s                   like(wrkfis)
007900991026     D prmmbs          s                   like(wrkmbs)
008000991026     D prmfit          s                   like(wrkfit)
008100991027     D prmmbt          s                   like(wrkmbt)
008200991026     D prmpgt          s                   like(wrkpgt)
008300991026     D prmppt          s                   like(wrkppt)
008400991026     D mc              s              1    dim(12) ctdata perrcd(12)
008500111230
008600111230     D wKSC            s                   like(virKSC) inz
008700111230     D wTIP            s                   like(virTIP) inz
008800111230     D wDTI            s              8S 0 inz
008900111230
009000991104
009100991111     D mbrd01          s            266
009200991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
009300991111     D mbrd03          s              8    inz('MBRD0200')
009400991111     D mbrd04          ds
009500991111     D mbrd04a                       10                                         File
009600991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
009700991111     D mbrd05          s             10                                         Membro
009800991125     D mbrd06          s              1    inz('0')
009900991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
010000991104     D rneta01         ds
010100991104     D  rneta01a                      8b 0
010200991104     D  rneta01b                      8b 0
010300991104     D  rneta01c                     24
010400991104     D rneta02         s              8b 0 inz(%size(rneta01))
010500991104     D rneta03         s              8b 0 inz(1)
010600991104     D rneta04         s             10    inz('SYSNAME')
010700991104     D nait            ds
010800991104     D  nait01                       10
010900991104     D  nait02                        1
011000991104     D  nait03                        1
011100991104     D  nait04                        8b 0
011200991104     D  sysname                       8
011300991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
011400991104
011500000622
011600000905     D*-------------------
011700000905     D* DS di ridefinizione file di log x passaggio recordset SQL
011800000905     D*-------------------
011900000905     D  dsvlt        E DS                  extname(tivlt00f)
012000000905     D                                     prefix(s_)
012100000905
012200040817
012300040817     D*-------------------
012400050909 TESTD* Variabili d wrk
012500040817     D*-------------------
012600050909     D*  COMMAND        S            300    inz(*blanks)
012700040817
012800000905
012900000905
013000000622      /TITLE *** MAIN ***
013100090414     C*
013200090414     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013300090414     C
013400090414     C/EXEC SQL
013500090414     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013600090414     C/END-EXEC
013700000622     C*
013800000906     C                   if        prmnfp  > '01' and
013900000906     C                             prmprog > *blanks
014000000905     C                   exsr      rtvdatora
014100000905     C                   else
014200000905     C                   time                    wrkoggiiso
014300000905     C                   time                    wrkora
014400000905     C     *iso          move      wrkoggiiso    wrkoggi
014500000905     C                   endif
014600030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
014700000328     C* Inizializzo variabili "wrk".
014800000328     C                   exsr      inzwrk
014900991026     C* Controllo parametri ricevuti.
015000991026     C                   exsr      chkprm
015100000626     C* Reperisco informazioni file.
015200000706     C                   exsr      rtvleg
015300000706     C* Alla fine...
015400000706     C                   exsr      uscita
015500000626
015600000626
015700000905
015800000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
015900000905     C*
016000000905     C     rtvdatora     begsr
016100000905     C*
016200000905     C/EXEC SQL
016300000905     C+ declare C1 cursor for select *
016400000905     C+ from tivlt00f where
016500000905     C+ vltprg = :prmprog
016600000905     C+ for fetch only
016700000905     C/END-EXEC
016800000905     C
016900000905     C/EXEC SQL
017000000905     C+ open C1
017100000905     C/END-EXEC
017200000905     C
017300000905     C/EXEC SQL
017400000905     C+ Fetch C1 into :dsvlt
017500000905     C/END-EXEC
017600000905     C*
017700000906     C                   if        sqlcod = *zeros     and
017800000906     C                             s_vltprg  = prmprog and
017900000906     C                             s_vltdat  > *zeros  and
018000000905     C                             s_vltora  > *zeros
018100000905     C                   z-add     s_vltdat      wrkoggi
018200000905     C                   z-add     s_vltora      wrkora
018300000905     C                   else
018400000905     C                   time                    wrkoggiiso
018500000905     C                   time                    wrkora
018600000905     C     *iso          move      wrkoggiiso    wrkoggi
018700000905     C                   endif
018800000905     C
018900000905     C/EXEC SQL
019000000905     C+ close C1
019100000905     C/END-EXEC
019200000905     C
019300000905     C*
019400000905     C                   endsr
019500000905
019600000905
019700000706
019800000706      /TITLE Determino eventuali legami cliente/tipi file
019900000706     C     rtvleg        begsr
020000000706     C*
020100000706     C                   eval      virksc = wrkksu
020200000706     C                   eval      virtip = wrktip
020300000706     C                   eval      virdti = wrkoggi
020400000707     C                   eval      vtftip = wrktip
020500000706     C* Reperisco informazioni file.
020600000706     C                   exsr      rtvtip
020700000706     C*
020800000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
020900000713     C                   if        wrkesito = '0'     and
021000000713     C                             prmprog  = *blanks and
021100000713     C                             prmope   = *blanks
021200000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
021300000711     C                   movel     '1'           wrkvarie
021400000706     C*
021500000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
021600000707     C     k03vir02      setll     tivre01l
021700000707     C                   if        %found(tivre01l)
021800000707     C     k03vir02      reade     tivre01l
021900000706     C                   dow       not %eof
022000000706     C                   eval      virksc = wrkksu
022100000706     C                   eval      virtip = vretfl
022200000706     C                   eval      virdti = wrkoggi
022300000707     C                   eval      vtftip = vretfl
022400000712     C                   eval      wrktip = vretfl
022500000706     C* Reperisco informazioni file.
022600000706     C                   exsr      rtvtip
022700000712     C                   eval      virtip = vretip
022800000707     C     k03vir02      reade     tivre01l
022900000706     C                   enddo
023000000706     C                   endif
023100000706     C*
023200000706     C                   endif
023300031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
023400031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
023500031008     C                   if        chkPRG <> '1'
023600030512     C                   if        chkFTP <> '1'
023700000711     C                   exsr      exepgm
023800030512     C                   endif
023900031008     C                   endif
024000000706     C*
024100000706     C                   endsr
024200000706
024300000706
024400000706
024500000706      /TITLE Reperisco tipo file.
024600000706     C     rtvtip        begsr
024700120105     C* Inizializzazioni
024800120105     C                   clear                   wKSC
024900120105     C                   clear                   wTIP
025000120105     C                   clear                   wDTI
025100000706     C* Reperisco il tipo file.
025200000706     C     k01vtf01      chain     tivtf01l
025300000706     C* Se il tipo file non esiste
025400000706     C* oppure non è valido per il download.
025500000706     C                   if        not %found
025600000706     C                             or vtfud <> 'D'
025700031008     C*
025800031008     C                   exsr      inztivlt
025900031008     C                   eval      vltsts = '2'
026000031008     C                   eval      vltmsg =
026100031008     C                               %trim(sysname)
026200031008     C                             + '/'
026300031008     C                             + %trim(psjobname)
026400031008     C                             + '/'
026500031008     C                             + %trim(psuser)
026600031008     C                             + '/'
026700031008     C                             + %trim(%editc(psjobnbr:'Z'))
026800031008     C                             + ' Tipo file inesistente.'
026900031008     C                   exsr      wrttivlt
027000031008     C                   eval      wrkesito = 'B'
027100031008     C*
027200000721     C                   else
027300000706     C* Reperisco il tipo file del cliente.
027400000706     C     k03vir02      setgt     tivir02l
027500000706     C     k02vir02      readpe    tivir02l
027600000706     C* Tipo file non previsto per il cliente
027700000706     C* oppure scaduto.
027800000731     C                   if        %eof
027900000706     C                             or virdtf < wrkoggi
028000000706     C                             or virdti > wrkoggi
028100000706     C*
028200000731     C                   exsr      inztivltp
028300000706     C*
028400000706     C                   eval      vltsts = '2'
028500000706     C                   eval      vltmsg =
028600000706     C                               %trim(sysname)
028700000706     C                             + '/'
028800000706     C                             + %trim(psjobname)
028900000706     C                             + '/'
029000000706     C                             + %trim(psuser)
029100000706     C                             + '/'
029200000706     C                             + %trim(%editc(psjobnbr:'Z'))
029300000706     C                             + ' Tipo file non previsto per il cliente.'
029400000706     C                   exsr      wrttivlt
029500000706     C                   eval      wrkesito = 'C'
029600000706     C*
029700000706     C                   else
029800111230     C*
029900111230     C* Salvo subito la chiave univoca della regola corrente
030000111230     C                   eval      wKSC = virKSC
030100111230     C                   eval      wTIP = virTIP
030200111230     C                   eval      wDTI = virDTI
030300000706     C*
030400000706     C* Imposto i default.
030500000706     C                   if        virpgt <> *blanks
030600000706     C                   eval      wrkpgt = virpgt
030700000706     C                   else
030800000706     C                   eval      wrkpgt = vtfpgt
030900000706     C                   endif
031000170119     C                   if        virppt <> *blanks
031100170119     C                   eval      wrkppt = virppt
031200170119     C                   else
031300170119     C                   eval      wrkppt = vtfppt
031400170119     C                   endif
031500000712     C*
031600000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
031700000711     C                   if        wrkvarie = '0'                               <===================
031800000711     C                   if        virpgi <> *blanks                            <===================
031900000711     C                   eval      wrkpgi = virpgi                              <===================
032000000711     C                   eval      wrkppi = virppi                              <===================
032100000711     C                   else                                                   <===================
032200000711     C                   eval      wrkpgi = vtfpgi                              <===================
032300101013     C***                eval      wrkppi = vtfppi                              <===================
032400000711     C                   endif                                                  <===================
032500000711     C                   endif                                                  <===================
032600000706     C* File da tradurre.
032700000706     C                   if        virfid <> *blanks
032800000706     C                   eval      wrkfid = virfid
032900000706     C                   else
033000000706     C                   eval      wrkfid = vtffid
033100000706     C                   endif
033200000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
033300000706     C* File storico.
033400000706     C                   if        virfis <> *blanks
033500000706     C                   eval      wrkfis = virfis
033600000706     C                   else
033700000706     C                   eval      wrkfis = vtffis
033800000706     C                   endif
033900000706     C                   extrct    wrkoggiiso:*m wrkmese
034000000706     C                   extrct    wrkoggiiso:*d wrkday
034100000706     C                   eval      wrkmbs =
034200000706     C                             mc(wrkmese)
034300000706     C                             + wrkdaychr
034400000706     C                             + %subst(wrkksu:2:7)
034500030519     C*
034600030519     C                   eval      vssisv = virfi1
034700030519     C* Valorizzo subito il progressivo applicazione.
034800030519     C                   exsr      calprog
034900000706     C* File tradotto.
035000000706     C                   if        virfit <> *blanks
035100000706     C                   eval      wrkfit = virfit
035200000706     C                   else
035300000706     C                   eval      wrkfit = vtffit
035400000706     C                   endif
035500030526     C* Tipo nome membro tradotto
035600030526     C                   eval      dvir01 = virpth
035700070207     C* Se forzatura in ingresso adotto quella, altrimenti applico considerazioni standard
035800070207     C                   if        prmout <> *blanks
035900070207     C                   eval      wrkmbt = prmout                              membro = forzatura
036000070207     C                   else
036100030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
036200100311     C                   select
036300100311     C                   when      §VIR01TM = 'P'
036400030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
036500030526 xxx C***                eval      wrkmbs = dwlprg
036600100311     C                   when      §VIR01TM = 'T'
036700070504     C                   eval      wrkmbt = dwlprg                              membro = progressivo
036800070504     C                   eval      %subst(wrkmbt:3:2) = wrktip                  membro = prog + tipf
036900100311     C                   when      §VIR01TM = 'M'
037000100311     C                   eval      wrkmbt = wrktip + wrkksu                     membro = tipf + KSC
037100100311     C                   other
037200030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
037300100311     C                   endsl
037400030519     C                   endif
037500000706     C* Controllo abilitazione al servizio.
037600000706     C                   exsr      chkvss
037700000706     C                   endif
037800000706     C                   endif
037900000706     C*
038000000706     C                   endsr
038100000706
038200000711
038300000711
038400000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
038500000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
038600000711     C*
038700000711     C     exepgm        begsr
038800000711     C* Lancio il pgm in variabile con relativi parametri
038900000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
039000000711    C                   if        wrkpgi <> *blanks
039100000712     C* Chiamo il pgm post traduzione.
039200000712     C                   call(e)   'TIS7T9C1'
039300000712     C                   parm      wrkfid        prmfid
039400000712     C                   parm      wrkmbd        prmmbd
039500000712     C                   parm      wrkfis        prmfis
039600000712     C                   parm      wrkmbs        prmmbs
039700031106     C                   parm      wrkfit        prmfit
039800031106     C                   parm      wrkmbt        prmmbt
039900000712     C                   parm                    wrkpgi
040000000712     C                   parm                    wrkppi
040100000712     C                   parm      *blanks       prmesito
040200000712     C* Se la call è finita in errore
040300000712     C* o il lancio del PGM ha avuto degli errori.
040400000712     C                   if        %error
040500000712     C                             or prmesito <> '0'
040600000712     C     k01vlt        setll     tivlt07l
040700000712     C                   if        %found(tivlt07l)
040800000712     C     k01vlt        reade     tivlt07l
040900000712     C                   dow       not %eof
041000000712     C                   if        vltsqz = *zeros and
041100000712     C                             (vltsts = '0' or vltsts = '1')
041200000712     C                   eval      vltsts = '2'
041300000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
041400000712     C                   eval      vltmsg =
041500000712     C                               %trim(sysname)
041600000712     C                             + '/'
041700000712     C                             + %trim(psjobname)
041800000712     C                             + '/'
041900000712     C                             + %trim(psuser)
042000000712     C                             + '/'
042100000712     C                             + %trim(%editc(psjobnbr:'Z'))
042200000712     C                             + %trim(wrkmsg)
042300000712     C                   update    tivlt700
042400000712     C                   endif
042500000712     C     k01vlt        reade     tivlt07l
042600000712     C                   enddo
042700000712     C                   endif
042800000712     C                   endif
042900000712     C*
043000000711     C                   endif
043100000711     C*
043200000711     C                   endsr
043300000711
043400000711
043500000626
043600000626      /TITLE Valorizzazione Progressivo Applicazione
043700000626     C     calprog       begsr
043800000626     C*
043900000706     C                   if        dwlprg = *all'0'
044000000627     C                   read(e)   tis7prgf
044100000627     C                   if        not %error
044200000627     C                   eval      dwlprg = f_tis7prgf
044300000627     C*
044400000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
044500000719     C                   if        prmsrv <> *blanks
044600000719     C                   eval      vssisv = prmsrv
044700000719     C                   endif
044800000626     C*
044900000719     C                   if        prmprog = *blanks
045000051011     C                   move(p)   dwl_prg       wrkprg
045100000626     C                   add       1             wrkprg
045200051011     C                   move(p)   wrkprg        dwl_prg
045300051011     C                   movel     vssisv        dwl_isv
045400051207     C*                  if        wrktip <> *blanks
045500051207     C*                  movel     wrktip        dwl_tip
045600051207     C*                  else
045700051207     C*                  movel     'XX'          dwl_tip
045800051207     C*                  endif
045900040123     C                   eval      f_tis7prgf = dwlprg
046000040123     C                   update    tis7prg0
046100000626     C                   else
046200040123     C                   unlock(e) tis7prgf
046300000626     C                   eval      dwlprg = prmprog
046400000626     C                   endif
046500000627     C*
046600000627     C                   endif
046700000706     C                   endif
046800000626     C*
046900000626     C                   endsr
047000991104
047100000706
047200000706
047300000706
047400991104      /TITLE Controllo abilitazione servizio.
047500991104     C     chkvss        begsr
047600991104     C*
047700000914     C                   eval      vssok = *off
047800000914     C*
047900991105     C                   eval      vssksu = wrkksu
048000000914     C     k02vss02      setll     tivss02l
048100000914     C     k02vss02      reade     tivss02l
048200000914     C                   dow       not %eof
048300000914     C                   if        vssdde <= wrkoggi and
048400000914     C                             vssdsc >= wrkoggi and
048500000914     C                             vssvat <> 'A'     and
048600000914     C                             vssvat <> 'S'
048700000914     C                   eval      vssok = *on
048800000914     C                   leave
048900000914     C                   endif
049000000914     C     k02vss02      reade     tivss02l
049100000914     C                   enddo
049200000914     C*
049300000914     C                   if        vssok = *off
049400991104     C*
049500000622     C                   exsr      inztivlt
049600000329     C                   eval      vltsts = '2'
049700991104     C                   eval      vltmsg =
049800991104     C                               %trim(sysname)
049900991104     C                             + '/'
050000991104     C                             + %trim(psjobname)
050100991104     C                             + '/'
050200991104     C                             + %trim(psuser)
050300991104     C                             + '/'
050400991104     C                             + %trim(%editc(psjobnbr:'Z'))
050500991104     C                             + ' Cliente non abilitato al servizio.'
050600000622     C                   exsr      wrttivlt
050700000622     C                   eval      wrkesito = 'D'
050800031008     C*
050900000622     C                   else
051000031008     C*
051100031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
051200031008     C* del progressivo rispetto a file/membro e cliente
051300031008     C                   movel     '0'           chkPRG            1
051400031008     C     k08vlt        setll     tivlt08l
051500031008     C                   if        %equal(tivlt08l)
051600031008     C     k08vlt        reade     tivlt08l
051700031008     C                   dow       not %eof(tivlt08l)
051800031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
051900090414     C* (considero solamente i record in stato "da elaborare")
052000090414     C                   if        w_vltksc <> wrkksc AND
052100090414     C                             w_vltsts  = '0'
052200031008     C                   movel     '1'           chkPRG
052300031008     C                   endif
052400031008     C     k08vlt        reade     tivlt08l
052500031008     C                   enddo
052600031008     C                   endif
052700031008     C*
052800000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
052900031008     C                   if        chkPRG = '0'
053000000706     C                   exsr      traduci
053100031008     C                   else
053200031008     C*
053300031008     C                   exsr      inztivlt
053400031008     C                   eval      vltsts = '2'
053500031008     C                   eval      vltmsg =
053600031008     C                               %trim(sysname)
053700031008     C                             + '/'
053800031008     C                             + %trim(psjobname)
053900031008     C                             + '/'
054000031008     C                             + %trim(psuser)
054100031008     C                             + '/'
054200031008     C                             + %trim(%editc(psjobnbr:'Z'))
054300031008     C                             + ' Progressivo/membro NON univoco!!!'
054400031008     C                   exsr      wrttivlt
054500031008     C                   eval      wrkesito = 'F'
054600031008     C                   exsr      sndeml
054700031008     C                   exsr      uscita
054800031008     C                   endif
054900991104     C*
055000991104     C                   endif
055100991104     C*
055200991104     C                   endsr
055300000622
055400000622
055500000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
055600000622     C     rtvtia        begsr
055700000622     C*
055800000622     C                   eval      tbecod = 'ISV'
055900000731     C                   eval      tbeke1 = virfi1
056000000622     C     keytbe        chain     tntbe01l
056100000622     C                   if        %found(tntbe01l)
056200020212     C                   movel     tbeuni        disv
056300000622     C                   endif
056400000622     C*
056500000622     C                   endsr
056600000706
056700000706
056800000706
056900000706      /TITLE Traduzione.
057000000706     C     traduci       begsr
057100031105     C*
057200031105     C* Reperisco informazioni membro da tradurre.
057300031105     C                   eval      mbrd04a = wrkfid
057400031105     C                   eval      mbrd05  = wrkmbd
057500031105     C                   exsr      rtvmbrd
057600031105     C* Controllo se il membro da tradurre ha dei record.
057700031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
057800031105     C                   if        (qusei=*blanks and
057900060616     C                             qusnbrcr > 0) or prmnfp > '01'
058000060616     C                             or %trim(wrkfid) = '*'
058100030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
058200030512     C                   call      'TIS7T9R2'
058300031008     C                   parm                    wrkfit
058400031008     C                   parm                    wrkmbt
058500030512     C                   parm      '0'           chkFTP            1
058600031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
058700031105     C                   if        chkFTP = '0'
058800000706     C*
058900000706     C                   exsr      inztivlt
059000000706     C* Chiamo il pgm traduttore.
059100060616     C                   if        %trim(wrkfid) = '*' AND
059200060616     C                             %trim(wrkfis) = '*'
059300060616     C                   call(e)   'TIS7T9C3'
059400120105     C                   parm                    prmsrc
059500111230     C                   parm                    wKSC
059600111230     C                   parm                    wTIP
059700111230     C                   parm                    wDTI
059800060616     C                   parm      wrkmbd        prmmbd
059900060616     C                   parm      wrkfit        prmfit
060000060616     C                   parm      wrkmbt        prmmbt
060100060616     C                   parm      wrkpgt        prmpgt
060200060616     C                   parm      wrkppt        prmppt
060300060616     C                   parm      *blanks       prmesito
060400060616     C                   else
060500000706     C                   call(e)   'TIS7T9C'
060600120105     C                   parm                    prmsrc
060700111230     C                   parm                    wKSC
060800111230     C                   parm                    wTIP
060900111230     C                   parm                    wDTI
061000000706     C                   parm      wrkfid        prmfid
061100000706     C                   parm      wrkmbd        prmmbd
061200000706     C                   parm      wrkfis        prmfis
061300000706     C                   parm      wrkmbs        prmmbs
061400000706     C                   parm      wrkfit        prmfit
061500000706     C                   parm      wrkmbt        prmmbt
061600000706     C                   parm      wrkpgt        prmpgt
061700000706     C                   parm      wrkppt        prmppt
061800000706     C                   parm      *blanks       prmesito
061900060616     C                   endif
062000000706     C* Se la call è finita in errore
062100000706     C* o la traduzione ha avuto degli errori.
062200060320     C                   if        %error
062300060320     C                             or (prmesito <> '0' and
062400060320     C                                 prmesito <> 'Y')
062500000706     C                   eval      vltsts = '2'
062600000706     C                   eval      vltmsg =
062700000706     C                               %trim(sysname)
062800000706     C                             + '/'
062900000706     C                             + %trim(psjobname)
063000000706     C                             + '/'
063100000706     C                             + %trim(psuser)
063200000706     C                             + '/'
063300000706     C                             + %trim(%editc(psjobnbr:'Z'))
063400000706     C                             + ' Errori nella traduzione.'
063500000706     C                   else
063600060320     C                   if        prmesito = 'Y'
063700060320     C                   eval      wrkesito = prmesito
063800060320     C                   else
063900000706     C                   eval      wrkesito = '0'
064000060320     C                   endif
064100000706     C                   endif
064200000706     C*
064300000706     C                   exsr      wrttivlt
064400031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
064500031105     C                   else
064600030512     C                   exsr      inztivlt
064700030512     C                   eval      vltsts = '2'
064800030512     C                   eval      vltmsg =
064900030512     C                               %trim(sysname)
065000030512     C                             + '/'
065100030512     C                             + %trim(psjobname)
065200030512     C                             + '/'
065300030512     C                             + %trim(psuser)
065400030512     C                             + '/'
065500030512     C                             + %trim(%editc(psjobnbr:'Z'))
065600030512     C                             + ' Cliente/TipoFile in uso da server FTP'
065700030512     C                   exsr      wrttivlt
065800030512     C                   eval      wrkesito = 'E'
065900030512     C                   endif
066000031105     C* NO record da tradurre
066100031105     C                   endif
066200000706     C*
066300000706     C                   endsr
066400000328
066500000706
066600000706
066700000328      /TITLE Inizializzo variabili "wrk".
066800000328     C     inzwrk        begsr
066900000328     C*
067000000328     C                   clear                   wrkfit
067100000328     C                   clear                   wrkmbt
067200000328     C                   clear                   wrkpgi
067300000328     C                   clear                   wrkppi
067400000328     C                   clear                   wrkfis
067500000328     C                   clear                   wrkmbs
067600000328     C*
067700000328     C                   endsr
067800991026
067900000706
068000000706
068100991026      /TITLE Inizializzo TIVLT00F.
068200991026     C     inztivlt      begsr
068300000804     C*
068400000804     C                   exsr      rtvtia
068500991026     C*
068600000328     C                   clear                   tivlt000
068700991026     C                   eval      vltksc = wrkksc
068800991026     C                   eval      vltksu = wrkksu
068900000721     C                   eval      vlttip = wrktip
069000991026     C                   eval      vltfld = wrkfit
069100991026     C                   eval      vltmbd = wrkmbt
069200100311     C                   eval      vltpgi = wrkpgt                              * forzatura PGI->PGT
069300100311     C***                eval      vltppi = wrkppi
069400020212     C                   if        §isvproc = 'F'
069500000804     C                   eval      vltsnd = 'D'
069600000804     C                   else
069700991026     C                   eval      vltsnd = vtfsnd
069800000804     C                   endif
069900991026     C                   eval      vltfls = wrkfis
070000991026     C                   eval      vltmbs = wrkmbs
070100991026     C                   eval      vltdat = wrkoggi
070200991115     C                   eval      vltora = wrkora
070300991026     C                   eval      vltsts = '0'
070400991027     C                   eval      vltmsg =
070500991104     C                               %trim(sysname)
070600991104     C                             + '/'
070700991104     C                             + %trim(psjobname)
070800991027     C                             + '/'
070900991027     C                             + %trim(psuser)
071000991027     C                             + '/'
071100991027     C                             + %trim(%editc(psjobnbr:'Z'))
071200000328     C                   z-add     *zeros        vltsqz
071300000731     C*
071400991026     C                   endsr
071500991026
071600000706
071700000731
071800000731      /TITLE Inizializzo parzialmente TIVLT00F.
071900000731     C     inztivltp     begsr
072000000731     C*
072100000731     C                   clear                   tivlt000
072200000731     C                   eval      vltksc = wrkksc
072300000731     C                   eval      vltksu = wrkksu
072400000731     C                   eval      vlttip = wrktip
072500100311     C                   eval      vltpgi = wrkpgt                              * forzatura PGI->PGT
072600100311     C***                eval      vltppi = wrkppi
072700000731     C                   eval      vltsnd = vtfsnd
072800000731     C                   eval      vltdat = wrkoggi
072900000731     C                   eval      vltora = wrkora
073000000731     C                   eval      vltsts = '0'
073100000731     C                   eval      vltmsg =
073200000731     C                               %trim(sysname)
073300000731     C                             + '/'
073400000731     C                             + %trim(psjobname)
073500000731     C                             + '/'
073600000731     C                             + %trim(psuser)
073700000731     C                             + '/'
073800000731     C                             + %trim(%editc(psjobnbr:'Z'))
073900000731     C                   z-add     *zeros        vltsqz
074000000731     C*
074100000731     C                   exsr      rtvtia
074200000731     C*
074300000731     C                   endsr
074400000731
074500000731
074600000706
074700991026      /TITLE Scrittura TIVLT00F.
074800000731     C     wrttivlt      begsr
074900991026     C*
075000000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
075100000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
075200000731     C                   eval      vltisv = virfi1
075300020212     C                   eval      vlttia = §isvproc
075400000622     C                   eval      vltprg = dwlprg
075500000713     C                   eval      vltfl2 = prmnfp
075600991026     C                   write     tivlt000
075700040817     C*
075800050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
075900040817     C* procedura scambio dati
076000040817     C* ...quindi eseguo prima d tutto l'override
076100050909  "  C*
076200050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
076300050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
076400050909     C*                  eval      LUNG = %len(%trim(COMMAND))
076500050909  "  C*                  call(e)   'QCMDEXC'
076600050909     C*                  parm                    COMMAND
076700050909     C*                  parm                    LUNG             15 5
076800040817     C*
076900050909  "  C*                  if        not %error
077000050909     C*                  if        not %open(tivltnew)
077100050909     C*                  open(e)   tivltnew
077200050909     C*                  endif
077300050909  "  C*                  if        not %error
077400050909     C*                  write     tivltn00
077500050909     C*                  endif
077600050909     C*                  if        %open(tivltnew)
077700050909  "  C*                  close(e)  tivltnew
077800050909     C*                  endif
077900050909 TESTC*                  endif
078000991026     C*
078100991026     C                   endsr
078200991026
078300000706
078400000706
078500991026      /TITLE Operazioni finali.
078600991026     C     uscita        begsr
078700991026     C*
078800000627     C                   seton                                        lr
078900991026     C*
079000991026     C                   endsr
079100991026
079200000706
079300000706
079400991026      /TITLE Controllo formale parametri ricevuti.
079500991026     C     chkprm        begsr
079600991026     C*
079700991104     C                   if        wrkksu = *blanks
079800031008     C*
079900031008     C                   exsr      inztivlt
080000031008     C                   eval      vltsts = '2'
080100031008     C                   eval      vltmsg =
080200031008     C                               %trim(sysname)
080300031008     C                             + '/'
080400031008     C                             + %trim(psjobname)
080500031008     C                             + '/'
080600031008     C                             + %trim(psuser)
080700031008     C                             + '/'
080800031008     C                             + %trim(%editc(psjobnbr:'Z'))
080900031008     C                             + ' Parametro codice cliente non passato.'
081000031008     C                   exsr      wrttivlt
081100120105     C                   eval      wrkesito = 'K'
081200031008     C*
081300991026     C                   exsr      uscita
081400991026     C                   endif
081500991026     C*
081600991026     C                   if        wrktip = *blanks
081700031008     C*
081800031008     C                   exsr      inztivlt
081900031008     C                   eval      vltsts = '2'
082000031008     C                   eval      vltmsg =
082100031008     C                               %trim(sysname)
082200031008     C                             + '/'
082300031008     C                             + %trim(psjobname)
082400031008     C                             + '/'
082500031008     C                             + %trim(psuser)
082600031008     C                             + '/'
082700031008     C                             + %trim(%editc(psjobnbr:'Z'))
082800031008     C                             + ' Parametro tipo file non passato.'
082900031008     C                   exsr      wrttivlt
083000120105     C                   eval      wrkesito = 'T'
083100031008     C*
083200991026     C                   exsr      uscita
083300991026     C                   endif
083400991026     C*
083500991026     C                   clear                   wrkesito
083600991026     C*
083700991026     C                   endsr
083800991111
083900000706
084000000706
084100991111      /TITLE Reperisco informazioni membro.
084200991111     C     rtvmbrd       begsr
084300991111     C* Pulisco e imposto la DS degli errori.
084400000124     C                   clear                   qusec
084500991111     C                   eval      qusbprv = %size(qusec)
084600991111     C*
084700991111     C                   call      'QUSRMBRD'
084800991111     C     qusm0200      parm                    mbrd01
084900991111     C                   parm                    mbrd02
085000991111     C                   parm                    mbrd03
085100991111     C                   parm                    mbrd04
085200991111     C                   parm                    mbrd05
085300991111     C                   parm                    mbrd06
085400991111     C                   parm                    qusec
085500991111     C*
085600991111     C                   endsr
085700031008
085800031008
085900031008
086000031008
086100031008      /TITLE Compongo il testo e spedisco una e-m@ail
086200031008     C     sndeml        begsr
086300031008     C*
086400031008     C* Inizializzo variabili
086500031008     C                   movel     *blanks       wrkEml          253
086600031008     C                   movel     *blanks       wrkEmlMsg      5000
086700031008     C                   movel     *blanks       wrkEmlOgg        44
086800031008     C* Valorizzo i campi della e-m@ail
086900120305     C                   eval      wrkEml='cedalert@brt.it'
087000031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
087100031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
087200031008     C                             'SU AS/400'                         +':/N'+
087300031008     C                             ' '                                 +':/N'+
087400031008     C                             'RIFERIMENTI'                       +':/N'+
087500031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
087600031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
087700031008     C                             'Cliente:'+vltksc                   +':/N'+
087800031008     C                             'TipoFile:'+vlttip                  +':/N'+
087900031008     C                             ' '                                 +':/N'+
088000031008     C                             'MESSAGGIO'                         +':/N'+
088100031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
088200031008     C*
088300031008     C                   call(e)   'TIS701C'
088400031008     C                   parm                    wrkEml
088500031008     C                   parm                    wrkEmlOgg
088600031008     C                   parm                    wrkEmlMsg
088700031008     C*
088800031008     C                   endsr
088900990910
089000000706
089100000706
089200990908      /TITLE Operazioni iniziali.
089300990908     C     *inzsr        begsr
089400990908     C*
089500990908     C     *entry        plist
089600991026     C     wrkksc        parm                    prmksc
089700991026     C     wrkksu        parm      wrkksu        prmksu
089800991026     C     wrktip        parm                    prmtip
089900991026     C                   parm      wrkesito      prmesito
090000000623     C                   parm                    prmprog
090100000623     C                   parm                    prmsrv
090200000628     C                   parm                    prmope
090300000713     C                   parm                    prmnfp
090400070301     C                   parm                    wprmout          10
090500120105     C                   parm                    wprmsrc           1
090600070301     C*
090700070301     C* Gestisco il ricevimento parametri
090800070301     C                   eval      prmout = *blanks
090900120105     C                   eval      prmsrc = *blanks
091000120105     C                   select
091100120105     C                   when      %parms = 9
091200070301     C                   eval      prmout = wprmout
091300120105     C                   when      %parms = 10
091400120105     C                   eval      prmout = wprmout
091500120105     C                   eval      prmsrc = wprmsrc
091600120105     C                   endsl
091700991104     C*
091800991105     C     k02vss02      klist
091900991105     C                   kfld                    vssksu
092000991104     C                   kfld                    vssisv
092100991026     C*
092200991026     C     k03vir02      klist
092300991026     C                   kfld                    virksc
092400991026     C                   kfld                    virtip
092500991026     C                   kfld                    virdti
092600991026     C*
092700991026     C     k02vir02      klist
092800991026     C                   kfld                    virksc
092900991026     C                   kfld                    virtip
093000000706     C*
093100000706     C     k04vir02      klist
093200000706     C                   kfld                    vreksc
093300000706     C                   kfld                    vretfl
093400000706     C                   kfld                    vredti
093500990920     C*
093600990920     C     k01vtf01      klist
093700990920     C                   kfld                    vtftip
093800000622     C*
093900000622     C     keytbe        klist
094000000622     C                   kfld                    tbecod
094100000622     C                   kfld                    tbeke1
094200000712     C*
094300000712     C     k01vlt        klist
094400000712     C                   kfld                    vltprg
094500000712     C                   kfld                    vltksc
094600031008     C*
094700031008     C     k08vlt        klist
094800031008     C                   kfld                    wrkfit
094900031008     C                   kfld                    wrkmbt
095000000706     C*
095100991104     C* Reperisco nome sistema.
095200991104     C                   eval      qusbprv = %size(qusec)
095300991104     C                   call      'QWCRNETA'
095400991104     C                   parm                    rneta01
095500991104     C                   parm                    rneta02
095600991104     C                   parm                    rneta03
095700991104     C                   parm                    rneta04
095800991104     C                   parm                    qusec
095900991104     C                   eval      nait = rneta01c
096000990908     C*
096100990908     C                   endsr
096200991026** ctdata mc
096300991026ABCDEFGHILMN
