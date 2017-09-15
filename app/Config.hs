module Config
  ( connectInfo
  ) where

import Database.MySQL.Simple

connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost = "?",
                            connectPort = 3306,
                            connectUser = "?",
                        connectPassword = "?",
                        connectDatabase = "?",
                         connectOptions = [],
                            connectPath = "",
                             connectSSL = Nothing }
