000100021024     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200021024      * TRTR97R  *---------------------------------------------------*
000300021024      * - RICEZIONE ANAGRAFICA TESTI                                 *
000400021024      * - AGGIORNA IL FILE FLTXT00F RICEVUTO CANCELLANDO TUTTO IL    *
000500021024      *   TESTO E RISCRIVENDO SOLO I RECORD RICEVUTI NON ANNULLATI   *
000600021024      *--------------------------------------------------------------*
000700021024     fFntxt00r  uf   e             Disk    Usropn
000800021024     f                                     Rename(Fntxt000:FntxtR)
000900021024     f                                     Prefix(R_)
001000021024     fFntxt01l  uf a e           k Disk
001100021024     fCmric01l  uf   e           k Disk
001200021024
001300021024      *---------------------------------------------------------------*
001400021024      * Riepilogo indicatori                                          *
001500021024      *---------------------------------------------------------------*
001600021024      * 30 = Comodo
001700021024      * 99 = Errore su QCMDEXC
001800021024      *---------------------------------------------------------------*
001900021024
002000021024     d Cmd             s             60    Dim(5) Ctdata Perrcd(1)
002100021024
002200021024     d Comman          s             80
002300021024     d Dateu           s              8  0
002400021024     d Lenght          s             15  5 Inz(80)
002500021024     d RecTxt          s              7  0 Inz
002600021024     d Sav_TxtLet      s                   Like(TxtLet)
002700021024
002800021024     d DsFntxt       e ds                  Extname(Fntxt00f)
002900021024     d                                     Inz
003000021024     d DsFntxtR      e ds                  Extname(Fntxt00r)
003100021024     d                                     Prefix(R_)
003200021024     d                                     Inz
003300021024
003400021024     d Kpjba         e ds
003500021024     d  Libsys                92    101
003600021024     d  Legfil               483    492
003700021024     d  Mbrfil               493    502
003800021024
003900021024      *---------------------------------------------------------------*
004000021024      *    M A I N
004100021024      *---------------------------------------------------------------*
004200021024
004300021024      * Verifica l'esistenza del membro di ricezione
004400021024     c                   Exsr      Esimbr
004500021024
004600021024      * Operazioni di ricezione
004700021024If  1c                   If        Not *In99
004800021024     c                   Exsr      Ricmbr
004900021024
005000021024      * Disalloca il membro di ricezione
005100021024     c                   Exsr      Dismbr
005200021024     c                   EndIf
005300021024
005400021024     c                   SETON                                        LR
005500021024      *--------------------------------------------------------------------------------------------*
005600021024      * Verifica l'esistenza del membro di ricezione
005700021024      *--------------------------------------------------------------------------------------------*
005800021024
005900021024     c     Esimbr        Begsr
006000021024
006100021024      * controlla esistenza membro di ricezione, se non esiste chiude il programma
006200021024     c                   Clear                   Comman
006300021024     c                   Movel     Cmd(1)        Comman
006400021024     c                   Eval      %Subst(Comman:45:10) = Mbrfil
006500021024     c                   Call      'QCMDEXC'                            99
006600021024     c                   Parm                    Comman
006700021024     c                   Parm                    Lenght
006800021024
006900021024      * alloca il membro di ricezione, se non riesce chiude il programma
007000021024If  1c                   If        Not *in99
007100021024     c                   Clear                   Comman
007200021024     c                   Movel     Cmd(2)        Comman
007300021024     c                   Eval      %Subst(Comman:45:10) = Mbrfil
007400021024     c                   Call      'QCMDEXC'                            99
007500021024     c                   Parm                    Comman
007600021024     c                   Parm                    Lenght
007700021024
007800021024      * esegue sovrapposizione membro di ricezione, se non riesce chiude il programma
007900021024If  2c                   If        Not *in99
008000021024     c                   Clear                   Comman
008100021024     c                   Movel     Cmd(3)        Comman
008200021024     c                   Eval      %Subst(Comman:45:10) = Mbrfil
008300021024     c                   Call      'QCMDEXC'                            99
008400021024     c                   Parm                    Comman
008500021024     c                   Parm                    Lenght
008600021024    2c                   EndIf
008700021024    1c                   EndIf
008800021024
008900021024     c                   EndSr
009000021024
009100021024      *--------------------------------------------------------------------------------------------*
009200021024      * Operazioni di ricezione
009300021024      *--------------------------------------------------------------------------------------------*
009400021024
009500021024     c     Ricmbr        BegSr
009600021024
009700021024      * apre il file di ricezione
009800021024     c                   Open      Fntxt00r
009900021024
010000021024      * legge il file di ricezione
010100021024Do  1c                   Do        *Hival
010200021024     c                   Read      Fntxt00r                               30
010300021024      * Fine file
010400021024     c                   If        %Eof(Fntxt00r)
010500021024     c                   Leave
010600021024     c                   EndIf
010700021024
010800021024      * A cambio Tipo testo elimino quello gi� esistente
010900021024If  2c                   If        R_TxtLet <> Sav_TxtLet
011000021024     c     R_TxtLet      Setll     Fntxt01l
011100021024Do  3c                   Do        *Hival
011200021024     c     R_TxtLet      Reade     Fntxt01l
011300021024     c                   If        %Eof(Fntxt01l)
011400021024     c                   Leave
011500021024     c                   EndIf
011600021024     c                   Delete    Fntxt000
011700021024    3c                   EndDo
011800021024     c                   Eval      Sav_TxtLet = R_TxtLet
011900021024    2c                   EndIf
012000021024
012100021024      * scrive/aggiorna il record fisico e cancella il rcd di ricezione
012200021024     c                   Exsr      Scrpfm
012300021024
012400021024    1c                   EndDo
012500021024
012600021024      * chiude il file di ricezione
012700021024     c                   Close     Fntxt00r
012800021024
012900021024      * aggiorna l'archivio di controllo delle ricezioni
013000021024     c                   Exsr      Aggcmr
013100021024
013200021024     c                   EndSr
013300021024      *--------------------------------------------------------------------------------------------*
013400021024      * scrive il record fisico
013500021024      *--------------------------------------------------------------------------------------------*
013600021024
013700021024     c     Scrpfm        BegSr
013800021024
013900021024      * Elaboro solo i record non annullati
014000021024If  1c                   If        R_TxtAtb = *Blanks
014100021024     c                   Movel     DsFntxtr      DsFntxt
014200021024     c                   Movel     'R'           TxtFtr
014300021024     c                   Z-Add     Dateu         TxtDtr
014400021024     c                   Write     Fntxt000
014500021024    1c                   EndIf
014600021024
014700021024      * cancella il record di ricezione
014800021024     c                   Delete    Fntxtr
014900021024     c                   Add       1             RecTxt
015000021024
015100021024     c                   EndSr
015200021024
015300021024      *--------------------------------------------------------------------------------------------*
015400021024      * aggiorna l'archivio di controllo delle ricezioni
015500021024      *--------------------------------------------------------------------------------------------*
015600021024
015700021024     c     Aggcmr        BegSr
015800021024
015900021024     c     Legfil        Chain     Cmric01l                           30
016000021024If  1c                   If        Not *In30
016100021024     c                   Z-Add     RecTxt        Ricnr1
016200021024     c                   Movel     '1'           Ricfl2
016300021024     c                   Except    Updcmr
016400021024    1c                   EndIf
016500021024
016600021024     c                   EndSr
016700021024
016800021024      *--------------------------------------------------------------------------------------------*
016900021024      * disalloca il membro di ricezione
017000021024      *--------------------------------------------------------------------------------------------*
017100021024
017200021024     c     Dismbr        BegSr
017300021024
017400021024      * cancella la sovrascrittura del membro di ricezione
017500021024     c                   Clear                   Comman
017600021024     c                   Movel     Cmd(4)        Comman
017700021024     c                   Call      'QCMDEXC'                            99
017800021024     c                   Parm                    Comman
017900021024     c                   Parm                    Lenght
018000021024
018100021024      * disalloca il membro di ricezione
018200021024     c                   Clear                   Comman
018300021024     c                   Movel     Cmd(5)        Comman
018400021024     c                   Call      'QCMDEXC'                            99
018500021024     c                   Parm                    Comman
018600021024     c                   Parm                    Lenght
018700021024
018800021024     c                   EndSr
018900021024
019000021024      *--------------------------------------------------------------------------------------------*
019100021024      * operazioni iniziali
019200021024      *--------------------------------------------------------------------------------------------*
019300021024
019400021024     c     *Inzsr        BegSr
019500021024
019600021024      * ricevimento parametri
019700021024
019800021024     c     *Entry        Plist
019900021024     c                   Parm                    Kpjba
020000021024
020100021024      * Data del Giorno
020200021024     c                   Movel     *date         Dateu
020300021024
020400021024      * se il membro da ricevere non � stato dichiarato, assume il primo del file
020500021024     c                   If        MbrFil = *Blanks
020600021024     c                   Movel     '*FIRST'      MbrFil
020700021024     c                   EndIf
020800021024
020900021024     c                   EndSr
021000021024
021100021024      *--------------------------------------------------------------------------------------------*
021200021024      * Aggiornamento file
021300021024      *--------------------------------------------------------------------------------------------*
021400021024     oCmric000  e            Updcmr
021500021024     o                       Ricnr1
021600021024     o                       Ricfl2
021700021024
021800021024**   cmd - comandi
021900021024CHKOBJ OBJ(FNTXT00R) OBJTYPE(*FILE)     MBR(M234567890)
022000021024ALCOBJ OBJ((FNTXT00R *FILE *EXCL            M234567890))
022100021024OVRDBF FILE(FNTXT00R)                   MBR(M234567890)
022200021024DLTOVR FILE(FNTXT00R)
022300021024DLCOBJ OBJ((FNTXT00R *FILE *EXCL))
