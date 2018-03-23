000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060104      *  Leggo file reclami
000500051227      *-----------------------------------------------------------------*
000600060328     ftntud01l  uf a e           k disk
000700060328     ffiftd03l  if   e           k disk
000800060328     ffiapd01l  if   e           k disk
000900051227
001000060328     d contacolli      s                   like(tudqta)
001100060328     d contaritir      s                   like(tudqta)
001200060322     d conta           s                   like(tudqta)
001300051229     d dataalfa        s              6
001400051227     d dtaiso          s               d   inz
001500051229     d mult            s                   like(tudmult)
001600060328     d savddc          s                   like(fttddc) inz(0)
001700060328     d wpo             s                   like(tudfil) inz(0)
001800060328     d savpo           s                   like(tudfil) inz(0)
001900060328     d savstp          s                   like(ftdstp) inz(0)
002000060328     d ktip            s                   like(apdtip) inz('A')
002100060104     d keytud          ds                  likerec(tntud000:*key)
002200051229     d wrkgetlista     s           4096    varying
002300051227
002400051229     d dtup          e ds
002500060328     d fifttds       e ds                  extname(fiftt00f)
002600051227     d kpjba         e ds
002700051227     d tntu05ds      e ds
002800060104     dtntudtads      e ds
002900060328     D Dvpo          E DS                  INZ
003000060328     d Tibs02Ds      e ds
003100051227
003200051227      *-----------------------------------------------------------------*
003300060328      * Moltiplicatore calcolati:
003400060328      *    200 - colli ritirati > 200  (da DVPO §VPORITIRI)
003500051227      *-----------------------------------------------------------------*
003600051229
003700060104     c                   exsr      PREPASQL
003800060104     c
003900060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
004000060104     c                   if        t05mult=*blanks and t05fil=0
004100060104     c                   clear                   tntudtads
004200060104     c                   eval      §DTAazn = kcdaz
004300060104     c                   eval      §DTAdta = t05dtf
004400060104     c                   call      'TNTUDTAR'
004500060104     c                   parm                    tntudtads
004600060104     c                   endif
004700051229
004800051229     c                   Eval      *InLr = *On
004900051229
005000051229      *-----------------------------------------------------------------*
005100060104      * Lettura file fasi
005200051229      *-----------------------------------------------------------------*
005300060104     c     prepasql      begsr
005400051229
005500051229      *?Imposto la parte fissa dell'istruzione sql
005600051229     c                   eval      wrkgetlista =
005700060328     c                             'select fttddc, fttpdr, fttndc, -
005800060328     c                             ftttsr  from fiftt00f where -
005900060328     c                             fttddc between '+
006000051229     c                             %editc(t05dti:'X') + ' and ' +
006100051229     c                             %editc(t05dtf:'X')
006200060328     c                   eval      wrkgetlista = wrkgetlista +
006300060328     c                             ' and ftttsr=''R'''
006400060322      *?Ordinamento
006500060322     c                   eval      wrkgetlista = wrkgetlista +
006600060328     c                             ' order by fttddc, fttpdr'
006700051229      *?Solo lettura
006800051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
006900051229
007000051229     C/EXEC SQL
007100051229     C+ PREPARE s1 FROM :wrkgetlista
007200051229     C/END-EXEC
007300051229     C
007400051229     C/EXEC SQL
007500051229     C+ DECLARE A1 CURSOR FOR S1
007600051229     C/END-EXEC
007700051229     C
007800051229     C/EXEC SQL
007900051229     C+ OPEN a1
008000051229     C/END-EXEC
008100051229
008200051229     c                   clear                   conta
008300051229      *?Leggo il file
008400051229     c                   do        *hival
008500051229     C/EXEC SQL
008600060328     C+ FETCH NEXT FROM a1 INTO : fttddc, :fttpdr, :fttndc, :ftttsr
008700051229     C/END-EXEC
008800051229
008900051229     c                   select
009000051229
009100051229     c                   when      sqlcod = 100
009200060328     c                   exsr      aggiorna
009300051229     c                   leave
009400051229     c                   when      sqlcod < 0
009500051229     c                   seton                                        H1
009600051229     c                   exsr      srfine
009700051229     c                   other
009800060321     c
009900060321     c* Sommo e a cambio p.o. o data scrivo
010000060328     c                   EXSR      LeggiFTD
010100060104     c
010200051229     c                   endsl
010300051229
010400051229     c                   enddo
010500051229
010600051229     C/EXEC SQL
010700051229     C+ CLOSE a1
010800051229     C/END-EXEC
010900051229
011000051229     c                   endsr
011100060321      *-----------------------------------------------------------------*
011200060328      * Leggo file fiftd da fiftt escludendo colli ritirati se >200
011300060321      *-----------------------------------------------------------------*
011400060328     c     LeggiFTD      begsr
011500060328     c
011600060328     c* Escludo i padroncini con flag merce portata a magazzino='S'
011700060328     c     kapd          chain     fiapd01l
011800060328     c
011900060328     c                   if        (%found(fiapd01l) and apdfcm=' ') or
012000060328     c                             (not %found(fiapd01l))
012100060328     c
012200060328     c                   movel     fttpdr        wpo
012300060328     c
012400060328     c                   if        t05fil=0 or t05fil=wpo
012500060328     c
012600060328     c                   if        wpo<>savpo
012700060328     c                   if        savpo>0
012800060328     c                   exsr      Aggiorna
012900060328     c                   endif
013000060328     c                   clear                   contaritir
013100060328     c                   eval      savddc=fttddc
013200060328     c                   eval      savpo =wpo
013300060328     c                   endif
013400060328     c
013500060328     c* A cambio data memorizzo i colli
013600060328     c                   if        fttddc<>savddc
013700060328     c                   if        savddc>0
013800060328     c                   exsr      Aggiorna
013900060328     c                   endif
014000060328     c                   clear                   contaritir
014100060328     c                   eval      savddc=fttddc
014200060328     c                   eval      savpo =wpo
014300060328     c                   endif
014400060328     c
014500060328     c                   clear                   contacolli
014600060328     c     kftd          setll     fiftd03l
014700060328     c     kftd          reade     fiftd03l
014800060328     c                   dow       not %eof(fiftd03l)
014900060328     c* Per stesso stop conto i colli
015000060328     c                   if        ftdstp<>savstp
015100060328     c                   if        savstp>0
015200060328     c                   if        contacolli<§vporitiri
015300060328     c                   add       contacolli    contaritir
015400060328     c                   endif
015500060328     c                   endif
015600060328     c                   clear                   contacolli
015700060328     c                   eval      savstp=ftdstp
015800060328     c                   endif
015900060328     c
016000060328     c                   add       ftdncl        contacolli
016100060328     c     kftd          reade     fiftd03l
016200060328     c                   enddo
016300060328     c
016400060328     c                   if        savstp>0 and
016500060328     c                             contacolli<§vporitiri
016600060328     c                   add       contacolli    contaritir
016700060328     c                   endif
016800060328     c                   endif
016900060328     c                   endif
017000060321
017100060321     c                   endsr
017200060321      *-----------------------------------------------------------------*
017300060321      * Imposto cosa devo aggiornare
017400060321      *-----------------------------------------------------------------*
017500060321     c     aggiorna      begsr
017600060321
017700060321      /free
017800060321
017900060321       //?richeiste solo informative
018000060328       if contaritir> 0;
018100060328       mult = '200';
018200060328       conta = contaritir;
018300060322       exsr scrivitud;
018400060321       endif;
018500060321
018600060321      /end-free
018700060321
018800060321     c                   endsr
018900051227      *-----------------------------------------------------------------*
019000051228      * Scrivo/Aggiorno TNTUD
019100051227      *-----------------------------------------------------------------*
019200060321     c     SCrivitud     begsr
019300051229
019400051229      /free
019500051229
019600051229       //?imposto la data iso
019700060328       dataalfa = %subst(%char(savddc):3:6);
019800051229       dtaiso = %date(dataalfa:*ymd0);
019900051229
020000051229       //?imposto la chiave per tntud01l
020100051229       keytud.tudmult = mult;
020200060328       keytud.tudfil = savpo;
020300051229       keytud.tuddta = dtaiso;
020400051229
020500051229       //?aggancio tntud01l
020600051229       chain %kds(keytud:3) tntud01l;
020700051229
020800051229       //?trovo rcd aggiorno se è un ricalcolo
020900051229       if %found(tntud01l);
021000051229       tudqta = conta;
021100051229       update tntud000;
021200051229
021300051229       //?non trovo rcd scrivo
021400051229       else;
021500051229       clear tntud000;
021600051229       tudmult = mult;
021700051229       tuddta = dtaiso;
021800060328       tudfil = savpo;
021900051229       tudcli = 0;
022000051229       tudqta = conta;
022100051229       write tntud000;
022200051229       endif;
022300051229
022400051229      /end-free
022500051227
022600051229     c                   endsr
022700051229
022800051229      *-----------------------------------------------------------------*
022900051229      * Fine programmma con codice errore
023000051229      *-----------------------------------------------------------------*
023100051229     c     srfine        begsr
023200051229
023300051229     c                   eval      *InLr = *On
023400051229     c                   return
023500051229
023600051229     c                   endsr
023700051227
023800051227      *-----------------------------------------------------------------*
023900051227      * Routine iniziale
024000051227      *-----------------------------------------------------------------*
024100051227     c     *inzsr        begsr
024200051227
024300051227     c     *entry        plist
024400051227     c                   parm                    kpjba
024500051227     c                   eval      tntu05ds = kpjbu
024600060328     c
024700060328     c     kftd          Klist
024800060328     c                   Kfld                    fttpdr
024900060328     c                   Kfld                    ftttsr
025000060328     c                   Kfld                    fttndc
025100060328     c                   Kfld                    fttddc
025200060328     c     kapd          Klist
025300060328     c                   Kfld                    ktip
025400060328     c                   Kfld                    fttpdr
025500060328     c
025600060328     c                   Clear                   Tibs02Ds
025700060328     c                   Clear                   dvpo
025800060328     c                   Eval      T02tla = 'L'
025900060328     c                   Eval      T02Mod = 'C'
026000060328     c                   Eval      T02Sif = Knsif
026100060328     c                   Eval      T02Cod = 'VPO'
026200060328     c                   Movel(p)  'VPO'         T02Ke1
026300060328     c                   Call      'TIBS02R'
026400060328     c                   Parm                    Kpjba
026500060328     c                   Parm                    Tibs02Ds
026600060328     c                   If        T02Err = *Blanks
026700060328     c                   Movel     T02Uni        dvpo
026800060328     c                   EndIf
026900051227
027000051229     c                   endsr
