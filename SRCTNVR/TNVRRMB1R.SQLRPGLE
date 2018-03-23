000400131106     Ftunnelf   if   e             DISK
000401131206     f                                     rename(telelog:telelogr)
000402131107     Fwfmbs10f  O    E             DISK
000700990908
000701131107     D wCount          s              6  0 INZ(*zeros)
000702131107     D n               s              6  0
000703131107     d TxtInOut        S           2048
000704131107     d ElencoChar      S            256
000705131107     d TipoElenco      S              1
000706131107     d CharSost        S              1
000707131107     d UpperCase       S              1
000708131107     d ChkNull         S              1    Inz('1')
000709131107     d CharNull        S              1
000710131107     d Esito           S              1
002200100607     D*------------------
002300100607     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
002400100607     D*------------------
002500110616     D InStringa       S          65535A   VARYING                              (stringa input)
002600100607     D InSepara        S             10A                                        (separatore)
002700100607     D InTypeOut       S              1A                                        (tipo output)
002800110617     D wSkParam        S          65535A                                        (output)
002900100607     D OutErrore       S              1A                                        (flag errore)
003000100607     D OutMsg          S             80A                                        (messaggio errore)
003100131107     D*SkSplitCSV_3    S             64    DIM(1024)
003101131107     D SkSplitCSV_4    S            128    DIM(512)
003102131106
003103131107
003104131107     C                   reset                   wCount
003105131107
005700131106     C                   EXSR      elabora                                      LETT/SCR. VAB/VAT
005800100607     C*
005900100607     C* Effettuo lancio XSPLIT solo x chiusura
006000100607     C                   CALL(e)   'XSPLIT2'
006100110616     C                   PARM      ''            InStringa
006200100607     C                   PARM                    InSepara
006300100607     C                   PARM                    InTypeOut
006400110616     C                   PARM      ''            wSkParam
006500100607     C                   PARM                    OutErrore
006600100607     C                   PARM                    OutMsg
006700000616     C*
006800010201     C                   seton                                        LR
006900990908
007000000801
016500110404     C*
016501131106     c     elabora       begsr
016502131106     c                   read      tunnelf
016504131106     c                   dow       not %eof(tunnelf)
016505131107
016506131107     C                   CLEAR                   WFMBS000
016507131206     c                   if        %subst(telelog:27:1)=']'
016508131107     C                   add       1             wCount
016509131107     C                   EVAL      §mbsPRG = wCount
016510131107     c                   endif
016511131107
016600110404     C* Verticalizzo le spunte ricevute
016700110404     C                   eval      InStringa =
016800131206     C                             %subst(telelog:27)
016900100607     C*
017000100607     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
017100110509     C                   CALL(e)   'XSPLIT2'
017200100607     C                   PARM                    InStringa
017300131107     C                   PARM      ';'           InSepara
017400131107     C                   PARM      '4'           InTypeOut
017500110616     C                   PARM      ''            wSkParam
017600100607     C                   PARM                    OutErrore
017700100607     C                   PARM                    OutMsg
017800131107     C                   MOVEA     wSkParam      SkSplitCSV_4
017900110509     C*
018000110509     C* Se errore in chiamata "splittatore" => marco il record corrente
018100110509     C                   if        %error
018500110509     C                   endif
018600100607     C*
018700100607     C* Valorizzo la schiera delle intestazioni colonne (elementi)
018800131107     C                   z-add     1             i                 4 0
018900131107     C                   dow       i <= %elem(SkSplitCSV_4)
019000131107     C                   if        SkSplitCSV_4(i) = *blanks
019100131107     C                   leave
019200131107     C                   else
019300131107     C                   if        %subst(SkSplitCSV_4(i):1:1)=']'
019301131107     c* cerco la presenza dell'ultimo carattere
019302131107     c*                  Eval      TxtInOut = %subst(SkSplitCSV_4(i):4)
019303131107     c*                  Call      'XCHKCHAR'
019304131107     c*                  Parm                    TxtInOut
019305131107     c*                  Parm                    ElencoChar
019306131107     c*                  Parm                    TipoElenco
019307131107     c*                  Parm                    CharSost
019308131107     c*                  Parm                    UpperCase
019309131107     c*                  Parm                    ChkNull
019310131107     c*                  Parm                    CharNull
019311131107     c*                  Parm                    Esito
019312131107     c*                  If        Esito = '1'
019314131107     c*                  eval      %subst(SkSplitCSV_4(i):4)=TxtInOut
019315131107     c*                  EndIf
019316131107     c                   eval      n=%checkr(' ':SkSplitCSV_4(i))
019317131107     c                   if        n>0 and %subst(SkSplitCSV_4(i):n)=X'03'
019318131107     c                   eval      %subst(SkSplitCSV_4(i):n)=' '
019319131107     c                   endif
019400131107     C                   eval      §mbsBRCD = %subst(SkSplitCSV_4(i):4)
019500131107     C                   write     WFMBS000
019600131107     C                   endif
019700131107     C                   endif
019800131107     C                   add       1             i
019900131107     C                   enddo
019901131106     c                   read      tunnelf
019902131106     c                   enddo
020000071003     C*
020100071003     C                   ENDSR
020200070411
020300110404
020400110404
021600070411     C***
021700070411
021800110404
021900990910
024000000613     C***
