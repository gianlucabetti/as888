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
001200170411     Fwas7ifs2i UF   E           K DISK
001300140721
001400140721
001500130924     F*------------------------------------------------------------------------*
001600130924     F* DS E VARIABILI
001700130924     F*------------------------------------------------------------------------*
001800170411     D keyTIP          s                   like(ifsTIP) inz
001900130924
002000130924
002100970526     C*------------------------------------------------------------------------*
002200970526     C* MAIN LINES
002300970526     C*------------------------------------------------------------------------*
002400080910     C*
002500170411     C* Scorrimento tutto file x esecuzione operazioni su oggetti in IFS
002600080910     C                   exsr      Elabora
002700170411     C*
002800170411     C                   seton                                        rt
002900130924
003000130924
003100010226     C*------------------------------------------------------------------------*
003200080910     C* ELABORA
003300010226     C*------------------------------------------------------------------------*
003400080910     C     Elabora       BEGSR
003500170411     C*
003600170412     C                   eval      keyTIP = prmtip
003700170411     C     keyTIP        setll     was7ifs2i
003800170411     C     keyTIP        reade     was7ifs2i
003900010226     C*
004000170412     C                   dow       not %eof(was7ifs2i) AND not *inrt
004100170411     C                   seton                                        55
004200170412     C*
004300170412     C* Gestione ENDJOB.
004400170412     C                   shtdn                                        rt
004500170412     C                   if        not *inrt
004600170411     C*
004700170411     C* Considero unicamente le entrate del tipo richiesto
004800170411     C                   if        ifsTIP <> keyTIP
004900170411     C                   leave
005000170411     C                   else
005100130924     C*
005200130924     C* Gestisco operazioni fallite
005300130924     C                   if        ifsSTS = '2'
005400130924     C                   if        ifsINS <
005500130924     C                             ((%dec(%timestamp())/1000000)-100000000)
005600130924     C                   delete    was7ifs0
005700130924     C                   endif
005800130924     C                   endif
005900130924     C*
006000130924     C* Gestisco operazioni da eseguire
006100081003     C                   if        ifsSTS = *blanks
006200130924     C*
006300090311     C                   clear                   par_OPZ           4
006400090312     C                   clear                   par_SPTH       2048
006500090311     C                   clear                   par_DPTH       2048
006600090312     C                   clear                   par_SFIL        255
006700090312     C                   clear                   par_DFIL        255
006800090311     C                   clear                   par_esito         1
006900090311     C                   clear                   par_msg        5000
007000090312     C*
007100090312     C                   eval      par_SPTH = %trim(ifsPTHS)
007200090312     C                   eval      par_DPTH = %trim(ifsPTHD)
007300090312     C                   eval      par_SFIL = %trim(ifsFILS)
007400090312     C                   eval      par_DFIL = %trim(ifsFILD)
007500130924     C*
007600130924     C                   exsr      exeOPE
007700130924     C*
007800130924     C* Incremento il numero d tentativi effettuati
007900130924     C                   eval      ifsTRYCUR = ifsTRYCUR + 1
008000130924     C*
008100170411     C* Gestisco esito del operazioe richiesta
008200130924     C                   if        par_esito <> *blanks
008300130924     C                   if        ifsTRYCUR < ifsTRYTOT
008400130924     C                   else
008500130924     C                   eval      ifsSTS = par_esito
008600170411     C*
008700170411     C* Se esito non OK ed operazione prevede invio email di avviso => eseguo
008800130924     C   55              exsr      sndeml
008900130924     C                   endif
009000170411     C*
009100170411     C* Infine aggiorno il record operazione corrente
009200130924     C                   update    was7ifs0
009300130924     C                   else
009400130924     C                   delete    was7ifs0
009500130924     C                   endif
009600140714     C*
009700170411     C                   endif
009800170411     C                   endif
009900130924     C*
010000130924     C* Proseguo scorrendo il file sequenzialmente
010100170411     C     keyTIP        reade     was7ifs2i
010200170412     C*
010300170412     C                   endif
010400130924     C*
010500130924     C                   enddo
010600130924     C*
010700130924     C                   ENDSR
010800130924
010900130924
011000130924     C*------------------------------------------------------------------------*
011100130924     C* ESECUZIONE OPZIONI
011200130924     C*------------------------------------------------------------------------*
011300130924     C     EXEOPE        BEGSR
011400170411     C*
011500170411      /COPY GAITRASRC/SRCTIS7,TIS783XR
011600130924     C*
011700130924     C                   ENDSR
011800130924
011900130924
012000090311     C*------------------------------------------------------------------------*
012100090311     C* INVIO MAIL
012200090311     C*------------------------------------------------------------------------*
012300090311     C     sndeml        begsr
012400090311     C*
012500090311     C* Inizializzo variabili
012600090311     C                   movel     *blanks       wrkEml          253
012700090311     C                   movel     *blanks       wrkEmlOgg        44
012800090311     C                   movel     *blanks       wrkEmlMsg      5000
012900090311     C* Valorizzo i campi della e-m@ail
013000120306     C                   eval      wrkEml    = 'cedalert@brt.it'
013100130924     C                   eval      wrkEmlOgg = 'VAS - WAS7IFS0F - ERROR!!!'
013200090311     C                   eval      wrkEmlMsg = par_msg
013300090311     C*
013400090311     C                   call(e)   'TIS701C'
013500090311     C                   parm                    wrkEml
013600090311     C                   parm                    wrkEmlOgg
013700090311     C                   parm                    wrkEmlMsg
013800090311     C*
013900090311     C                   endsr
014000130924
014100130924
014200970526     C*------------------------------------------------------------------------*
014300970526     C* *INZSR - OPERAZIONI INIZIALI
014400970526     C*------------------------------------------------------------------------*
014500970526     C     *INZSR        BEGSR
014600140714     C*
014700140714     C* Passaggio parametri
014800140714     C     *entry        plist
014900170412     C                   parm                    prmtip           15
015000991230     C*
015100991230     C                   ENDSR
