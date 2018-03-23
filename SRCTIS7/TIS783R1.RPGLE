000100970526     H*------------------------------------------------------------------------*
000200080910     H* Gestione batch oggetti in IFS
000300970526     H*------------------------------------------------------------------------*
000400130924     h decedit('0,') datedit(*dmy.) option(*nodebugio)
000500140804     h dftactgrp(*no) actgrp(*caller)
000600130924     h alwnull(*inputonly)
000700130924     h bnddir('UBBNDDIR')
000800130924
000900970526     F*------------------------------------------------------------------------*
001000970526     F* DATA BASE
001100970526     F*------------------------------------------------------------------------*
001200140721     Fwas7ifs0f UF   E             DISK    infds(was7ifsinf)
001300970526     D*------------------------------------------------------------------------*
001400140721
001500140721     D was7ifsinf      ds
001600140721     D  was7ifsrri           397    400i 0
001700140721     D was7ifsrrp      s             10s 0
001800140721
001900130924     F*------------------------------------------------------------------------*
002000130924     F* DS E VARIABILI
002100130924     F*------------------------------------------------------------------------*
002200130924     D dar8§pdf      e ds
002300130924     D wNamePDF        s            256a
002400130924     D mEsito          s              1A
002500130924     D pCheckOnly      s              1A
002600130924     D pPathOrig       s            120A
002700130924     D pFileName       s            256A
002800130924     D pPathCli        s                   like(§AR8PPDFP)
002900130924     D pNomePDF        s             61A
003000130924
003100130924
003200130924       //--------------------------------------------------------------
003300130924       // Definizione prototipi procedure.
003400130924       //--------------------------------------------------------------
003500130924
003600130924     D FNLSPDFR1       PR
003700130924     D                                     extpgm('FNLSPDFR1')
003800130924     D iPathOrig                    120A
003900130924     D iPathCli                            like(§AR8PPDFP)
004000130924     D iNomePDF                      61A
004100130924     D iCheckOnly                     1A
004200130924     D iFileName                    256A
004300130924     D iEsito                         1A
004400130924
004500970526     C*------------------------------------------------------------------------*
004600970526     C* MAIN LINES
004700970526     C*------------------------------------------------------------------------*
004800080910     C*
004900080910     C* Scorrimento tutto file x esecucione operazioni su oggetti in IFS
005000080910     C                   exsr      Elabora
005100080910     C* Fine *pgm
005200080910     C                   seton                                        LR
005300130924
005400130924
005500010226     C*------------------------------------------------------------------------*
005600080910     C* ELABORA
005700010226     C*------------------------------------------------------------------------*
005800080910     C     Elabora       BEGSR
005900010226     C*
006000100316     C     *start        setll     was7ifs0f
006100140721     C                   read(n)   was7ifs0f
006200010226     C*
006300100316     C                   dow       not %eof(was7ifs0f)
006400170411     C*
006500170411     C* Considero unicamente le entrate di tipo *blanks
006600170411     C                   if        ifsTIP = *blanks
006700081003     C*
006800140714     C                   setoff                                       54
006900130924     C                   seton                                        55
007000140714     C*
007100140714     C* Verifico l'opzione in ingresso in relazione al tipo opzione del record corrente
007200140714     C                   select
007300140714     C                   when      prmopz = *blanks
007400140714     C                   seton                                        54
007500140714     C                   when      prmopz = '01' AND
007600140714     C                             (ifsOPZ = 'RENA' OR
007700140714     C                              ifsOPZ = 'MOVE' OR
007800140714     C                              ifsOPZ = 'MORE' OR
007900160310     C                              ifsOPZ = 'COPY' OR
008000170926     C                              ifsOPZ = 'CORE' OR
008100140714     C                              ifsOPZ = 'REMO')
008200140714     C                   seton                                        54
008300140714     C                   when      prmopz = '02' AND
008400140714     C                             (ifsOPZ = 'DELE' OR
008500140714     C                              ifsOPZ = 'DELN' OR
008600171011     C                              ifsOPZ = 'MOVN' OR
008700140714     C                              ifsOPZ = 'RMDN')
008800140714     C                   seton                                        54
008900140714     C                   endsl
009000140714     C*
009100140721     C                   if        *in54
009200140721     C*
009300140721     C* Se record corrente da elaborare secondo i parametri di elaborazione in input
009400140721     C* rileggo per allocare medesimo record
009500140721     C                   eval      was7ifsrrp = was7ifsrri
009600140721     C     was7ifsrrp    chain     was7ifs0f
009700130924     C*
009800130924     C* Gestisco operazioni fallite
009900130924     C                   if        ifsSTS = '2'
010000130924     C                   if        ifsINS <
010100130924     C                             ((%dec(%timestamp())/1000000)-100000000)
010200130924     C                   delete    was7ifs0
010300130924     C                   endif
010400130924     C                   endif
010500130924     C*
010600130924     C* Gestisco operazioni da eseguire
010700081003     C                   if        ifsSTS = *blanks
010800130924     C*
010900090311     C                   clear                   par_OPZ           4
011000090312     C                   clear                   par_SPTH       2048
011100090311     C                   clear                   par_DPTH       2048
011200090312     C                   clear                   par_SFIL        255
011300090312     C                   clear                   par_DFIL        255
011400090311     C                   clear                   par_esito         1
011500090311     C                   clear                   par_msg        5000
011600090312     C*
011700090312     C                   eval      par_SPTH = %trim(ifsPTHS)
011800090312     C                   eval      par_DPTH = %trim(ifsPTHD)
011900090312     C                   eval      par_SFIL = %trim(ifsFILS)
012000090312     C                   eval      par_DFIL = %trim(ifsFILD)
012100130924     C*
012200130924     C* Verifico se il nome file sorgente contiene un carattere jolly
012300130924     C                   if        %scan('*':par_SFIL) > *zeros
012400130924     C                   eval      mEsito = *blanks
012500130924     C                   z-add     *zeros        wSafeExit         6 0
012600130924     C                   dow       mEsito <> '2' AND wSafeExit < 10000
012700130924     C                   exsr      sr_findPDF
012800130924     C                   if        mEsito <> '2'
012900130924     C                   eval      par_SFIL = pFileName
013000130924     C                   exsr      exeOPE
013100130924     C                   endif
013200130924     C                   add       1             wSafeExit
013300130924     C                   enddo
013400130924     C                   else
013500130924     C                   exsr      exeOPE
013600130924     C                   endif
013700130924     C*
013800130924     C* Incremento il numero d tentativi effettuati
013900130924     C                   eval      ifsTRYCUR = ifsTRYCUR + 1
014000130924     C*
014100130924     C* Gestione flaggatura stato record corrente
014200130924     C                   if        %error
014300130924     C                   eval      par_esito = '2'
014400130924     C                   endif
014500130924     C*
014600130924     C                   if        par_esito <> *blanks
014700130924     C                   if        ifsTRYCUR < ifsTRYTOT
014800130924     C                   else
014900130924     C                   eval      ifsSTS = par_esito
015000130924     C   55              exsr      sndeml
015100130924     C                   endif
015200130924     C                   update    was7ifs0
015300130924     C                   else
015400130924     C                   delete    was7ifs0
015500130924     C                   endif
015600130924     C************
015700130924     C                   endif
015800140714     C*
015900140714     C                   endif
016000170411     C                   endif
016100130924     C*
016200130924     C* Proseguo scorrendo il file sequenzialmente
016300140721     C                   read(n)   was7ifs0f
016400130924     C*
016500130924     C                   enddo
016600130924     C*
016700130924     C                   ENDSR
016800130924
016900130924
017000130924     C*------------------------------------------------------------------------*
017100130924     C* ESECUZIONE OPZIONI
017200130924     C*------------------------------------------------------------------------*
017300130924     C     EXEOPE        BEGSR
017400170411     C*
017500170411      /COPY GAITRASRC/SRCTIS7,TIS783XR
017600130924     C*
017700130924     C                   ENDSR
017800130924
017900130924
018000130924      /free
018100130924
018200130924       //-------------------------------------------------------------*
018300130924       //Richiamo driver "trova PDF"                                  *
018400130924       //-------------------------------------------------------------*
018500130924        Begsr sr_findPDF;
018600130924
018700130924            pFileName = *blanks;
018800130924
018900130924            pPathOrig  = %trim(ifsPTHS);
019000130924            pNomePDF   = %trim(ifsFILS);
019100130924            pPathCli   = *blanks;
019200130924            pCheckOnly = 'Y';
019300130924            mEsito     = '0';
019400130924
019500170405            if %scan('*':pNomePDF) > *zeros;
019600170405               pNomePDF = %scanrpl('*':'':pNomePDF);
019700170405            endif;
019800170405
019900130924            callP(e) FNLSPDFR1 (pPathOrig  :
020000130924                                pPathCli   :
020100130924                                pNomePDF   :
020200130924                                pCheckOnly :
020300130924                                pFileName  :
020400130924                                mEsito);
020500130924            // Verifico esito
020600130924            if %error;
020700130924               mEsito = '1';
020800130924            endif;
020900130924
021000130924        endsr;
021100130924
021200130924
021300130924      /end-free
021400130924
021500130924
021600090311     C*------------------------------------------------------------------------*
021700090311     C* INVIO MAIL
021800090311     C*------------------------------------------------------------------------*
021900090311     C     sndeml        begsr
022000090311     C*
022100090311     C* Inizializzo variabili
022200090311     C                   movel     *blanks       wrkEml          253
022300090311     C                   movel     *blanks       wrkEmlOgg        44
022400090311     C                   movel     *blanks       wrkEmlMsg      5000
022500090311     C* Valorizzo i campi della e-m@ail
022600120306     C                   eval      wrkEml    = 'cedalert@brt.it'
022700130924     C                   eval      wrkEmlOgg = 'VAS - WAS7IFS0F - ERROR!!!'
022800090311     C                   eval      wrkEmlMsg = par_msg
022900090311     C*
023000090311     C                   call(e)   'TIS701C'
023100090311     C                   parm                    wrkEml
023200090311     C                   parm                    wrkEmlOgg
023300090311     C                   parm                    wrkEmlMsg
023400090311     C*
023500090311     C                   endsr
023600130924
023700130924
023800970526     C*------------------------------------------------------------------------*
023900970526     C* *INZSR - OPERAZIONI INIZIALI
024000970526     C*------------------------------------------------------------------------*
024100970526     C     *INZSR        BEGSR
024200140714     C*
024300140714     C* Passaggio parametri
024400140714     C     *entry        plist
024500140714     C                   parm                    prmopz            2
024600991230     C*
024700991230     C                   ENDSR
