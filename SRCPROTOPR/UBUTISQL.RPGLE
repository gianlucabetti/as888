000100071129     P*--------------------------------------------------
000200071129     P* Procedure name: CreaAlias
000300071129     P* Purpose:        Crea alias di un libreria/file/membro
000400071129     P* Returns:        Integer (SQLcode)
000500071129     P* Parameter:      pInAlias      => Nome alias da creare
000600071129     P* Parameter:      pInAliaslib   => Libreria alias da creare
000700071129     P* Parameter:      pInFile       => File del quale creare l'alias
000800071129     P* Parameter:      pInLib        => Libreria del file dui cui al parametro precedente
000900071129     P* Parameter:      pInMbr        => Membro del file di cui ai 2 parametri precedenti
001000071129     P* Parameter:      pOutEsito     => Esito (SQLcode)
001100071129     P*--------------------------------------------------
001200071129     D*
001300071129     D SQL_CreaAlias...
001400071129     D                 PR            10I 0
001500071129     D  piInAlias                    22A   CONST
001600071129     D  piInAliasLib                 10A   CONST
001700071129     D  piInFile                     10A   CONST
001800071129     D  piInLib                      10A   CONST
001900071129     D  piInMbr                      10A   CONST
002000071129     D  piOutEsito                   10I 0 OPTIONS(*NOPASS)
002100071129     D*
002200071129     P*--------------------------------------------------
002300071129     P* Procedure name: DropAlias
002400071129     P* Purpose:        Droppa alias
002500071129     P* Returns:        Integer (SQLcode)
002600071129     P* Parameter:      pInAlias      => Nome alias da droppare
002700071129     P* Parameter:      pInAliaslib   => Libreria alias da droppare
002800071129     P* Parameter:      pOutEsito     => Esito (SQLcode)
002900071129     P*--------------------------------------------------
003000071129     D SQL_DropAlias...
003100071129     D                 PR            10I 0
003200071129     D  piInAlias                    22A   CONST
003300071129     D  piInAliasLib                 10A   CONST
003400071129     D  piOutEsito                   10I 0 OPTIONS(*NOPASS)
