000100010925      *PARMS DYNUSRPRF(*OWNER)
000200991026      /TITLE Download via Internet: decisore traduzione ASCII.
000300990908     H dftactgrp(*yes)
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
001700991026     Ftivlt00f  o    e             disk
001800000712     Ftivlt07l  uf   e           k disk    RENAME(tivlt000:tivlt700)
001900031008     Ftivlt08l  if   e           k disk    RENAME(tivlt000:tivlt800)
002000031008     F                                     PREFIX(w_)
002100991105     Ftivss02l  if   e           k disk
002200990920     Ftivtf01l  if   e           k disk
002300991026     Ftivir02l  if   e           k disk
002400000707     Ftivre01l  if   e           k disk
002500000622     Ftntbe01l  if   e           k disk
002600000627     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002700000627     F                                     PREFIX(f_)
002800000627
002900020212     D disv          e ds
003000030526     D dvir01        e ds
003100991027     D psds           sds
003200991027     D  psjobname            244    253
003300991027     D  psuser               254    263
003400991027     D  psjobnbr             264    269s 0
003500991026     D prmksc          s                   like(virksc)
003600991026     D prmksu          s                   like(virksc)
003700991026     D prmtip          s                   like(virtip)
003800991026     D prmesito        s              1
003900000623     D prmprog         s                   like(vltprg)
004000000623     D prmsrv          s                   like(vltisv)
004100000628     D prmope          s              1
004200000713     D prmnfp          s                   like(vltfl2)
004300991026     D wrkksc          s                   like(prmksc)
004400991026     D wrkksu          s                   like(prmksu)
004500991026     D wrktip          s                   like(prmtip)
004600991026     D wrkesito        s                   like(prmesito)
004700991026     D wrkoggiiso      s               d
004800991026     D wrkoggi         s              8  0
004900991026     D wrkora          s              6  0
005000991026     D wrkmese         s              2  0
005100000623     D wrkprg          s              8  0 INZ(*zeros)
005200000711     D wrkvarie        s              1    INZ('0')
005300000712     D wrkmsg          s             80    INZ(*blanks)
005400000622     D dwlprg          s             10    INZ(*all'0')
005500000914     D vssok           s               n   INZ(*off)
005600000711
005700991026     D                 ds                  inz
005800991026     D wrkday                  1      2s 0
005900991026     D wrkdaychr                      2    overlay(wrkday)
006000991026     D wrkpgt          s                   like(vtfpgt)
006100991026     D wrkppt          s                   like(vtfppt)
006200000711     D wrkpgi          s                   like(vtfpgi)
006300000711     D wrkppi          s                   like(vtfppi)
006400991026     D wrkfid          s                   like(vtffid)
006500991026     D wrkmbd          s                   like(vltmbd)
006600991026     D wrkfis          s                   like(vtffis)
006700991026     D wrkmbs          s                   like(vltmbs)
006800991026     D wrkfit          s                   like(vtffit)
006900991026     D wrkmbt          s                   like(vltmbd)
007000991026     D prmfid          s                   like(wrkfid)
007100991026     D prmmbd          s                   like(wrkmbd)
007200991026     D prmfis          s                   like(wrkfis)
007300991026     D prmmbs          s                   like(wrkmbs)
007400991026     D prmfit          s                   like(wrkfit)
007500991027     D prmmbt          s                   like(wrkmbt)
007600991026     D prmpgt          s                   like(wrkpgt)
007700991026     D prmppt          s                   like(wrkppt)
007800991026     D mc              s              1    dim(12) ctdata perrcd(12)
007900991104
008000991111     D mbrd01          s            266
008100991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
008200991111     D mbrd03          s              8    inz('MBRD0200')
008300991111     D mbrd04          ds
008400991111     D mbrd04a                       10                                         File
008500991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
008600991111     D mbrd05          s             10                                         Membro
008700991125     D mbrd06          s              1    inz('0')
008800991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
008900991104     D rneta01         ds
009000991104     D  rneta01a                      8b 0
009100991104     D  rneta01b                      8b 0
009200991104     D  rneta01c                     24
009300991104     D rneta02         s              8b 0 inz(%size(rneta01))
009400991104     D rneta03         s              8b 0 inz(1)
009500991104     D rneta04         s             10    inz('SYSNAME')
009600991104     D nait            ds
009700991104     D  nait01                       10
009800991104     D  nait02                        1
009900991104     D  nait03                        1
010000991104     D  nait04                        8b 0
010100991104     D  sysname                       8
010200991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
010300991104
010400000622
010500000905     D*-------------------
010600000905     D* DS di ridefinizione file di log x passaggio recordset SQL
010700000905     D*-------------------
010800000905     D  dsvlt        E DS                  extname(tivlt00f)
010900000905     D                                     prefix(s_)
011000000905
011100000905
011200000905
011300000622      /TITLE *** MAIN ***
011400000622     C*
011500000906     C                   if        prmnfp  > '01' and
011600000906     C                             prmprog > *blanks
011700000905     C                   exsr      rtvdatora
011800000905     C                   else
011900000905     C                   time                    wrkoggiiso
012000000905     C                   time                    wrkora
012100000905     C     *iso          move      wrkoggiiso    wrkoggi
012200000905     C                   endif
012300030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
012400000328     C* Inizializzo variabili "wrk".
012500000328     C                   exsr      inzwrk
012600991026     C* Controllo parametri ricevuti.
012700991026     C                   exsr      chkprm
012800000626     C* Reperisco informazioni file.
012900000706     C                   exsr      rtvleg
013000000706     C* Alla fine...
013100000706     C                   exsr      uscita
013200000626
013300000626
013400000905
013500000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
013600000905     C*
013700000905     C     rtvdatora     begsr
013800000905     C*
013900000905     C/EXEC SQL
014000000905     C+ declare C1 cursor for select *
014100000905     C+ from tivlt00f where
014200000905     C+ vltprg = :prmprog
014300000905     C+ for fetch only
014400000905     C/END-EXEC
014500000905     C
014600000905     C/EXEC SQL
014700000905     C+ open C1
014800000905     C/END-EXEC
014900000905     C
015000000905     C/EXEC SQL
015100000905     C+ Fetch C1 into :dsvlt
015200000905     C/END-EXEC
015300000905     C*
015400000906     C                   if        sqlcod = *zeros     and
015500000906     C                             s_vltprg  = prmprog and
015600000906     C                             s_vltdat  > *zeros  and
015700000905     C                             s_vltora  > *zeros
015800000905     C                   z-add     s_vltdat      wrkoggi
015900000905     C                   z-add     s_vltora      wrkora
016000000905     C                   else
016100000905     C                   time                    wrkoggiiso
016200000905     C                   time                    wrkora
016300000905     C     *iso          move      wrkoggiiso    wrkoggi
016400000905     C                   endif
016500000905     C
016600000905     C/EXEC SQL
016700000905     C+ close C1
016800000905     C/END-EXEC
016900000905     C
017000000905     C*
017100000905     C                   endsr
017200000905
017300000905
017400000706
017500000706      /TITLE Determino eventuali legami cliente/tipi file
017600000706     C     rtvleg        begsr
017700000706     C*
017800000706     C                   eval      virksc = wrkksu
017900000706     C                   eval      virtip = wrktip
018000000706     C                   eval      virdti = wrkoggi
018100000707     C                   eval      vtftip = wrktip
018200000706     C* Reperisco informazioni file.
018300000706     C                   exsr      rtvtip
018400000706     C*
018500000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
018600000713     C                   if        wrkesito = '0'     and
018700000713     C                             prmprog  = *blanks and
018800000713     C                             prmope   = *blanks
018900000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
019000000711     C                   movel     '1'           wrkvarie
019100000706     C*
019200000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
019300000707     C     k03vir02      setll     tivre01l
019400000707     C                   if        %found(tivre01l)
019500000707     C     k03vir02      reade     tivre01l
019600000706     C                   dow       not %eof
019700000706     C                   eval      virksc = wrkksu
019800000706     C                   eval      virtip = vretfl
019900000706     C                   eval      virdti = wrkoggi
020000000707     C                   eval      vtftip = vretfl
020100000712     C                   eval      wrktip = vretfl
020200000706     C* Reperisco informazioni file.
020300000706     C                   exsr      rtvtip
020400000712     C                   eval      virtip = vretip
020500000707     C     k03vir02      reade     tivre01l
020600000706     C                   enddo
020700000706     C                   endif
020800000706     C*
020900000706     C                   endif
021000031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
021100031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
021200031008     C                   if        chkPRG <> '1'
021300030512     C                   if        chkFTP <> '1'
021400000711     C                   exsr      exepgm
021500030512     C                   endif
021600031008     C                   endif
021700000706     C*
021800000706     C                   endsr
021900000706
022000000706
022100000706
022200000706      /TITLE Reperisco tipo file.
022300000706     C     rtvtip        begsr
022400000706     C* Reperisco il tipo file.
022500000706     C     k01vtf01      chain     tivtf01l
022600000706     C* Se il tipo file non esiste
022700000706     C* oppure non è valido per il download.
022800000706     C                   if        not %found
022900000706     C                             or vtfud <> 'D'
023000031008     C*
023100031008     C                   exsr      inztivlt
023200031008     C                   eval      vltsts = '2'
023300031008     C                   eval      vltmsg =
023400031008     C                               %trim(sysname)
023500031008     C                             + '/'
023600031008     C                             + %trim(psjobname)
023700031008     C                             + '/'
023800031008     C                             + %trim(psuser)
023900031008     C                             + '/'
024000031008     C                             + %trim(%editc(psjobnbr:'Z'))
024100031008     C                             + ' Tipo file inesistente.'
024200031008     C                   exsr      wrttivlt
024300031008     C                   eval      wrkesito = 'B'
024400031008     C*
024500000721     C                   else
024600000706     C* Reperisco il tipo file del cliente.
024700000706     C     k03vir02      setgt     tivir02l
024800000706     C     k02vir02      readpe    tivir02l
024900000706     C* Tipo file non previsto per il cliente
025000000706     C* oppure scaduto.
025100000731     C                   if        %eof
025200000706     C                             or virdtf < wrkoggi
025300000706     C                             or virdti > wrkoggi
025400000706     C*
025500000731     C                   exsr      inztivltp
025600000706     C*
025700000706     C                   eval      vltsts = '2'
025800000706     C                   eval      vltmsg =
025900000706     C                               %trim(sysname)
026000000706     C                             + '/'
026100000706     C                             + %trim(psjobname)
026200000706     C                             + '/'
026300000706     C                             + %trim(psuser)
026400000706     C                             + '/'
026500000706     C                             + %trim(%editc(psjobnbr:'Z'))
026600000706     C                             + ' Tipo file non previsto per il cliente.'
026700000706     C                   exsr      wrttivlt
026800000706     C                   eval      wrkesito = 'C'
026900000706     C*
027000000706     C                   else
027100000706     C*
027200000706     C* Imposto i default.
027300000706     C                   if        virpgt <> *blanks
027400000706     C                   eval      wrkpgt = virpgt
027500000706     C                   eval      wrkppt = virppt
027600000706     C                   else
027700000706     C                   eval      wrkpgt = vtfpgt
027800000706     C                   eval      wrkppt = vtfppt
027900000706     C                   endif
028000000712     C*
028100000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
028200000711     C                   if        wrkvarie = '0'                               <===================
028300000711     C                   if        virpgi <> *blanks                            <===================
028400000711     C                   eval      wrkpgi = virpgi                              <===================
028500000711     C                   eval      wrkppi = virppi                              <===================
028600000711     C                   else                                                   <===================
028700000711     C                   eval      wrkpgi = vtfpgi                              <===================
028800000711     C                   eval      wrkppi = vtfppi                              <===================
028900000711     C                   endif                                                  <===================
029000000711     C                   endif                                                  <===================
029100000706     C* File da tradurre.
029200000706     C                   if        virfid <> *blanks
029300000706     C                   eval      wrkfid = virfid
029400000706     C                   else
029500000706     C                   eval      wrkfid = vtffid
029600000706     C                   endif
029700000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
029800000706     C* File storico.
029900000706     C                   if        virfis <> *blanks
030000000706     C                   eval      wrkfis = virfis
030100000706     C                   else
030200000706     C                   eval      wrkfis = vtffis
030300000706     C                   endif
030400000706     C                   extrct    wrkoggiiso:*m wrkmese
030500000706     C                   extrct    wrkoggiiso:*d wrkday
030600000706     C                   eval      wrkmbs =
030700000706     C                             mc(wrkmese)
030800000706     C                             + wrkdaychr
030900000706     C                             + %subst(wrkksu:2:7)
031000030519     C*
031100030519     C                   eval      vssisv = virfi1
031200030519     C* Valorizzo subito il progressivo applicazione.
031300030519     C                   exsr      calprog
031400000706     C* File tradotto.
031500000706     C                   if        virfit <> *blanks
031600000706     C                   eval      wrkfit = virfit
031700000706     C                   else
031800000706     C                   eval      wrkfit = vtffit
031900000706     C                   endif
032000030526     C* Tipo nome membro tradotto
032100030526     C                   eval      dvir01 = virpth
032200030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
032300030526     C                   if        §VIR01TM = 'P'
032400030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
032500030526 xxx C***                eval      wrkmbs = dwlprg
032600030519     C                   else
032700030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
032800030519     C                   endif
032900000706     C* Controllo abilitazione al servizio.
033000000706     C                   exsr      chkvss
033100000706     C                   endif
033200000706     C                   endif
033300000706     C*
033400000706     C                   endsr
033500000706
033600000711
033700000711
033800000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
033900000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
034000000711     C*
034100000711     C     exepgm        begsr
034200000711     C* Lancio il pgm in variabile con relativi parametri
034300000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
034400000711    C                   if        wrkpgi <> *blanks
034500000712     C* Chiamo il pgm post traduzione.
034600000712     C                   call(e)   'TIS7T9C1'
034700000712     C                   parm      wrkfid        prmfid
034800000712     C                   parm      wrkmbd        prmmbd
034900000712     C                   parm      wrkfis        prmfis
035000000712     C                   parm      wrkmbs        prmmbs
035100031106     C                   parm      wrkfit        prmfit
035200031106     C                   parm      wrkmbt        prmmbt
035300000712     C                   parm                    wrkpgi
035400000712     C                   parm                    wrkppi
035500000712     C                   parm      *blanks       prmesito
035600000712     C* Se la call è finita in errore
035700000712     C* o il lancio del PGM ha avuto degli errori.
035800000712     C                   if        %error
035900000712     C                             or prmesito <> '0'
036000000712     C     k01vlt        setll     tivlt07l
036100000712     C                   if        %found(tivlt07l)
036200000712     C     k01vlt        reade     tivlt07l
036300000712     C                   dow       not %eof
036400000712     C                   if        vltsqz = *zeros and
036500000712     C                             (vltsts = '0' or vltsts = '1')
036600000712     C                   eval      vltsts = '2'
036700000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
036800000712     C                   eval      vltmsg =
036900000712     C                               %trim(sysname)
037000000712     C                             + '/'
037100000712     C                             + %trim(psjobname)
037200000712     C                             + '/'
037300000712     C                             + %trim(psuser)
037400000712     C                             + '/'
037500000712     C                             + %trim(%editc(psjobnbr:'Z'))
037600000712     C                             + %trim(wrkmsg)
037700000712     C                   update    tivlt700
037800000712     C                   endif
037900000712     C     k01vlt        reade     tivlt07l
038000000712     C                   enddo
038100000712     C                   endif
038200000712     C                   endif
038300000712     C*
038400000711     C                   endif
038500000711     C*
038600000711     C                   endsr
038700000711
038800000711
038900000626
039000000626      /TITLE Valorizzazione Progressivo Applicazione
039100000626     C     calprog       begsr
039200000626     C*
039300000706     C                   if        dwlprg = *all'0'
039400000627     C                   read(e)   tis7prgf
039500000627     C                   if        not %error
039600000627     C                   eval      dwlprg = f_tis7prgf
039700000627     C*
039800000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
039900000719     C                   if        prmsrv <> *blanks
040000000719     C                   eval      vssisv = prmsrv
040100000719     C                   endif
040200000626     C*
040300000719     C                   if        prmprog = *blanks
040400000626     C                   move(p)   dwlprg        wrkprg
040500000626     C                   add       1             wrkprg
040600000626     C                   move(p)   wrkprg        dwlprg
040700000731     C                   movel     vssisv        dwlprg
040800000626     C                   else
040900000626     C                   eval      dwlprg = prmprog
041000000626     C                   endif
041100000627     C*
041200000627     C                   eval      f_tis7prgf = dwlprg
041300000627     C                   update    tis7prg0
041400000627     C                   endif
041500000706     C                   endif
041600000626     C*
041700000626     C                   endsr
041800991104
041900000706
042000000706
042100000706
042200991104      /TITLE Controllo abilitazione servizio.
042300991104     C     chkvss        begsr
042400991104     C*
042500000914     C                   eval      vssok = *off
042600000914     C*
042700991105     C                   eval      vssksu = wrkksu
042800000914     C     k02vss02      setll     tivss02l
042900000914     C     k02vss02      reade     tivss02l
043000000914     C                   dow       not %eof
043100000914     C                   if        vssdde <= wrkoggi and
043200000914     C                             vssdsc >= wrkoggi and
043300000914     C                             vssvat <> 'A'     and
043400000914     C                             vssvat <> 'S'
043500000914     C                   eval      vssok = *on
043600000914     C                   leave
043700000914     C                   endif
043800000914     C     k02vss02      reade     tivss02l
043900000914     C                   enddo
044000000914     C*
044100000914     C                   if        vssok = *off
044200991104     C*
044300000622     C                   exsr      inztivlt
044400000329     C                   eval      vltsts = '2'
044500991104     C                   eval      vltmsg =
044600991104     C                               %trim(sysname)
044700991104     C                             + '/'
044800991104     C                             + %trim(psjobname)
044900991104     C                             + '/'
045000991104     C                             + %trim(psuser)
045100991104     C                             + '/'
045200991104     C                             + %trim(%editc(psjobnbr:'Z'))
045300991104     C                             + ' Cliente non abilitato al servizio.'
045400000622     C                   exsr      wrttivlt
045500000622     C                   eval      wrkesito = 'D'
045600031008     C*
045700000622     C                   else
045800031008     C*
045900031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
046000031008     C* del progressivo rispetto a file/membro e cliente
046100031008     C                   movel     '0'           chkPRG            1
046200031008     C     k08vlt        setll     tivlt08l
046300031008     C                   if        %equal(tivlt08l)
046400031008     C     k08vlt        reade     tivlt08l
046500031008     C                   dow       not %eof(tivlt08l)
046600031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
046700031008     C                   if        w_vltksc <> wrkksc
046800031008     C                   movel     '1'           chkPRG
046900031008     C                   endif
047000031008     C     k08vlt        reade     tivlt08l
047100031008     C                   enddo
047200031008     C                   endif
047300031008     C*
047400000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
047500031008     C                   if        chkPRG = '0'
047600000706     C                   exsr      traduci
047700031008     C                   else
047800031008     C*
047900031008     C                   exsr      inztivlt
048000031008     C                   eval      vltsts = '2'
048100031008     C                   eval      vltmsg =
048200031008     C                               %trim(sysname)
048300031008     C                             + '/'
048400031008     C                             + %trim(psjobname)
048500031008     C                             + '/'
048600031008     C                             + %trim(psuser)
048700031008     C                             + '/'
048800031008     C                             + %trim(%editc(psjobnbr:'Z'))
048900031008     C                             + ' Progressivo/membro NON univoco!!!'
049000031008     C                   exsr      wrttivlt
049100031008     C                   eval      wrkesito = 'F'
049200031008     C                   exsr      sndeml
049300031008     C                   exsr      uscita
049400031008     C                   endif
049500991104     C*
049600991104     C                   endif
049700991104     C*
049800991104     C                   endsr
049900000622
050000000622
050100000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
050200000622     C     rtvtia        begsr
050300000622     C*
050400000622     C                   eval      tbecod = 'ISV'
050500000731     C                   eval      tbeke1 = virfi1
050600000622     C     keytbe        chain     tntbe01l
050700000622     C                   if        %found(tntbe01l)
050800020212     C                   movel     tbeuni        disv
050900000622     C                   endif
051000000622     C*
051100000622     C                   endsr
051200000706
051300000706
051400000706
051500000706      /TITLE Traduzione.
051600000706     C     traduci       begsr
051700031105     C*
051800031105     C* Reperisco informazioni membro da tradurre.
051900031105     C                   eval      mbrd04a = wrkfid
052000031105     C                   eval      mbrd05  = wrkmbd
052100031105     C                   exsr      rtvmbrd
052200031105     C* Controllo se il membro da tradurre ha dei record.
052300031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
052400031105     C                   if        (qusei=*blanks and
052500031105     C                             qusnbrcr > 0) or prmnfp > '01'
052600030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
052700030512     C                   call      'TIS7T9R2'
052800031008     C                   parm                    wrkfit
052900031008     C                   parm                    wrkmbt
053000030512     C                   parm      '0'           chkFTP            1
053100031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
053200031105     C                   if        chkFTP = '0'
053300000706     C*
053400000706     C                   exsr      inztivlt
053500000706     C* Chiamo il pgm traduttore.
053600000706     C                   call(e)   'TIS7T9C'
053700000706     C                   parm      wrkfid        prmfid
053800000706     C                   parm      wrkmbd        prmmbd
053900000706     C                   parm      wrkfis        prmfis
054000000706     C                   parm      wrkmbs        prmmbs
054100000706     C                   parm      wrkfit        prmfit
054200000706     C                   parm      wrkmbt        prmmbt
054300000706     C                   parm      wrkpgt        prmpgt
054400000706     C                   parm      wrkppt        prmppt
054500000706     C                   parm      *blanks       prmesito
054600000706     C* Se la call è finita in errore
054700000706     C* o la traduzione ha avuto degli errori.
054800000706     C                   if        %error
054900000712     C                             or prmesito <> '0'
055000000706     C                   eval      vltsts = '2'
055100000706     C                   eval      vltmsg =
055200000706     C                               %trim(sysname)
055300000706     C                             + '/'
055400000706     C                             + %trim(psjobname)
055500000706     C                             + '/'
055600000706     C                             + %trim(psuser)
055700000706     C                             + '/'
055800000706     C                             + %trim(%editc(psjobnbr:'Z'))
055900000706     C                             + ' Errori nella traduzione.'
056000000706     C                   else
056100000706     C                   eval      wrkesito = '0'
056200000706     C                   endif
056300000706     C*
056400000706     C                   exsr      wrttivlt
056500031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
056600031105     C                   else
056700030512     C                   exsr      inztivlt
056800030512     C                   eval      vltsts = '2'
056900030512     C                   eval      vltmsg =
057000030512     C                               %trim(sysname)
057100030512     C                             + '/'
057200030512     C                             + %trim(psjobname)
057300030512     C                             + '/'
057400030512     C                             + %trim(psuser)
057500030512     C                             + '/'
057600030512     C                             + %trim(%editc(psjobnbr:'Z'))
057700030512     C                             + ' Cliente/TipoFile in uso da server FTP'
057800030512     C                   exsr      wrttivlt
057900030512     C                   eval      wrkesito = 'E'
058000030512     C                   endif
058100031105     C* NO record da tradurre
058200031105     C                   endif
058300000706     C*
058400000706     C                   endsr
058500000328
058600000706
058700000706
058800000328      /TITLE Inizializzo variabili "wrk".
058900000328     C     inzwrk        begsr
059000000328     C*
059100000328     C                   clear                   wrkfit
059200000328     C                   clear                   wrkmbt
059300000328     C                   clear                   wrkpgi
059400000328     C                   clear                   wrkppi
059500000328     C                   clear                   wrkfis
059600000328     C                   clear                   wrkmbs
059700000328     C*
059800000328     C                   endsr
059900991026
060000000706
060100000706
060200991026      /TITLE Inizializzo TIVLT00F.
060300991026     C     inztivlt      begsr
060400000804     C*
060500000804     C                   exsr      rtvtia
060600991026     C*
060700000328     C                   clear                   tivlt000
060800991026     C                   eval      vltksc = wrkksc
060900991026     C                   eval      vltksu = wrkksu
061000000721     C                   eval      vlttip = wrktip
061100991026     C                   eval      vltfld = wrkfit
061200991026     C                   eval      vltmbd = wrkmbt
061300991026     C                   eval      vltpgi = wrkpgi
061400991026     C                   eval      vltppi = wrkppi
061500020212     C                   if        §isvproc = 'F'
061600000804     C                   eval      vltsnd = 'D'
061700000804     C                   else
061800991026     C                   eval      vltsnd = vtfsnd
061900000804     C                   endif
062000991026     C                   eval      vltfls = wrkfis
062100991026     C                   eval      vltmbs = wrkmbs
062200991026     C                   eval      vltdat = wrkoggi
062300991115     C                   eval      vltora = wrkora
062400991026     C                   eval      vltsts = '0'
062500991027     C                   eval      vltmsg =
062600991104     C                               %trim(sysname)
062700991104     C                             + '/'
062800991104     C                             + %trim(psjobname)
062900991027     C                             + '/'
063000991027     C                             + %trim(psuser)
063100991027     C                             + '/'
063200991027     C                             + %trim(%editc(psjobnbr:'Z'))
063300000328     C                   z-add     *zeros        vltsqz
063400000731     C*
063500991026     C                   endsr
063600991026
063700000706
063800000731
063900000731      /TITLE Inizializzo parzialmente TIVLT00F.
064000000731     C     inztivltp     begsr
064100000731     C*
064200000731     C                   clear                   tivlt000
064300000731     C                   eval      vltksc = wrkksc
064400000731     C                   eval      vltksu = wrkksu
064500000731     C                   eval      vlttip = wrktip
064600000731     C                   eval      vltpgi = wrkpgi
064700000731     C                   eval      vltppi = wrkppi
064800000731     C                   eval      vltsnd = vtfsnd
064900000731     C                   eval      vltdat = wrkoggi
065000000731     C                   eval      vltora = wrkora
065100000731     C                   eval      vltsts = '0'
065200000731     C                   eval      vltmsg =
065300000731     C                               %trim(sysname)
065400000731     C                             + '/'
065500000731     C                             + %trim(psjobname)
065600000731     C                             + '/'
065700000731     C                             + %trim(psuser)
065800000731     C                             + '/'
065900000731     C                             + %trim(%editc(psjobnbr:'Z'))
066000000731     C                   z-add     *zeros        vltsqz
066100000731     C*
066200000731     C                   exsr      rtvtia
066300000731     C*
066400000731     C                   endsr
066500000731
066600000731
066700000706
066800991026      /TITLE Scrittura TIVLT00F.
066900000731     C     wrttivlt      begsr
067000991026     C*
067100000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
067200000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
067300000731     C                   eval      vltisv = virfi1
067400020212     C                   eval      vlttia = §isvproc
067500000622     C                   eval      vltprg = dwlprg
067600000713     C                   eval      vltfl2 = prmnfp
067700991026     C                   write     tivlt000
067800991026     C*
067900991026     C                   endsr
068000991026
068100000706
068200000706
068300991026      /TITLE Operazioni finali.
068400991026     C     uscita        begsr
068500991026     C*
068600000627     C                   seton                                        lr
068700991026     C*
068800991026     C                   endsr
068900991026
069000000706
069100000706
069200991026      /TITLE Controllo formale parametri ricevuti.
069300991026     C     chkprm        begsr
069400991026     C*
069500991104     C                   if        wrkksu = *blanks
069600031008     C*
069700031008     C                   exsr      inztivlt
069800031008     C                   eval      vltsts = '2'
069900031008     C                   eval      vltmsg =
070000031008     C                               %trim(sysname)
070100031008     C                             + '/'
070200031008     C                             + %trim(psjobname)
070300031008     C                             + '/'
070400031008     C                             + %trim(psuser)
070500031008     C                             + '/'
070600031008     C                             + %trim(%editc(psjobnbr:'Z'))
070700031008     C                             + ' Parametro codice cliente non passato.'
070800031008     C                   exsr      wrttivlt
070900031008     C                   eval      wrkesito = 'A'
071000031008     C*
071100991026     C                   exsr      uscita
071200991026     C                   endif
071300991026     C*
071400991026     C                   if        wrktip = *blanks
071500031008     C*
071600031008     C                   exsr      inztivlt
071700031008     C                   eval      vltsts = '2'
071800031008     C                   eval      vltmsg =
071900031008     C                               %trim(sysname)
072000031008     C                             + '/'
072100031008     C                             + %trim(psjobname)
072200031008     C                             + '/'
072300031008     C                             + %trim(psuser)
072400031008     C                             + '/'
072500031008     C                             + %trim(%editc(psjobnbr:'Z'))
072600031008     C                             + ' Parametro tipo file non passato.'
072700031008     C                   exsr      wrttivlt
072800031008     C                   eval      wrkesito = 'B'
072900031008     C*
073000991026     C                   exsr      uscita
073100991026     C                   endif
073200991026     C*
073300991026     C                   clear                   wrkesito
073400991026     C*
073500991026     C                   endsr
073600991111
073700000706
073800000706
073900991111      /TITLE Reperisco informazioni membro.
074000991111     C     rtvmbrd       begsr
074100991111     C* Pulisco e imposto la DS degli errori.
074200000124     C                   clear                   qusec
074300991111     C                   eval      qusbprv = %size(qusec)
074400991111     C*
074500991111     C                   call      'QUSRMBRD'
074600991111     C     qusm0200      parm                    mbrd01
074700991111     C                   parm                    mbrd02
074800991111     C                   parm                    mbrd03
074900991111     C                   parm                    mbrd04
075000991111     C                   parm                    mbrd05
075100991111     C                   parm                    mbrd06
075200991111     C                   parm                    qusec
075300991111     C*
075400991111     C                   endsr
075500031008
075600031008
075700031008
075800031008
075900031008      /TITLE Compongo il testo e spedisco una e-m@ail
076000031008     C     sndeml        begsr
076100031008     C*
076200031008     C* Inizializzo variabili
076300031008     C                   movel     *blanks       wrkEml          253
076400031008     C                   movel     *blanks       wrkEmlMsg      5000
076500031008     C                   movel     *blanks       wrkEmlOgg        44
076600031008     C* Valorizzo i campi della e-m@ail
076700120305     C                   eval      wrkEml='ced@brt.it'
076800031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
076900031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
077000031008     C                             'SU AS/400'                         +':/N'+
077100031008     C                             ' '                                 +':/N'+
077200031008     C                             'RIFERIMENTI'                       +':/N'+
077300031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
077400031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
077500031008     C                             'Cliente:'+vltksc                   +':/N'+
077600031008     C                             'TipoFile:'+vlttip                  +':/N'+
077700031008     C                             ' '                                 +':/N'+
077800031008     C                             'MESSAGGIO'                         +':/N'+
077900031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
078000031008     C*
078100031008     C                   call(e)   'TIS701C'
078200031008     C                   parm                    wrkEml
078300031008     C                   parm                    wrkEmlOgg
078400031008     C                   parm                    wrkEmlMsg
078500031008     C*
078600031008     C                   endsr
078700990910
078800000706
078900000706
079000990908      /TITLE Operazioni iniziali.
079100990908     C     *inzsr        begsr
079200990908     C*
079300990908     C     *entry        plist
079400991026     C     wrkksc        parm                    prmksc
079500991026     C     wrkksu        parm      wrkksu        prmksu
079600991026     C     wrktip        parm                    prmtip
079700991026     C                   parm      wrkesito      prmesito
079800000623     C                   parm                    prmprog
079900000623     C                   parm                    prmsrv
080000000628     C                   parm                    prmope
080100000713     C                   parm                    prmnfp
080200991104     C*
080300991105     C     k02vss02      klist
080400991105     C                   kfld                    vssksu
080500991104     C                   kfld                    vssisv
080600991026     C*
080700991026     C     k03vir02      klist
080800991026     C                   kfld                    virksc
080900991026     C                   kfld                    virtip
081000991026     C                   kfld                    virdti
081100991026     C*
081200991026     C     k02vir02      klist
081300991026     C                   kfld                    virksc
081400991026     C                   kfld                    virtip
081500000706     C*
081600000706     C     k04vir02      klist
081700000706     C                   kfld                    vreksc
081800000706     C                   kfld                    vretfl
081900000706     C                   kfld                    vredti
082000990920     C*
082100990920     C     k01vtf01      klist
082200990920     C                   kfld                    vtftip
082300000622     C*
082400000622     C     keytbe        klist
082500000622     C                   kfld                    tbecod
082600000622     C                   kfld                    tbeke1
082700000712     C*
082800000712     C     k01vlt        klist
082900000712     C                   kfld                    vltprg
083000000712     C                   kfld                    vltksc
083100031008     C*
083200031008     C     k08vlt        klist
083300031008     C                   kfld                    wrkfit
083400031008     C                   kfld                    wrkmbt
083500000706     C*
083600991104     C* Reperisco nome sistema.
083700991104     C                   eval      qusbprv = %size(qusec)
083800991104     C                   call      'QWCRNETA'
083900991104     C                   parm                    rneta01
084000991104     C                   parm                    rneta02
084100991104     C                   parm                    rneta03
084200991104     C                   parm                    rneta04
084300991104     C                   parm                    qusec
084400991104     C                   eval      nait = rneta01c
084500990908     C*
084600990908     C                   endsr
084700991026** ctdata mc
084800991026ABCDEFGHILMN
