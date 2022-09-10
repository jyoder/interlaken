module Path where

import Protolude

dashboardShow :: IsString a => a
dashboardShow = "/dashboard"

developmentHotReloadPing :: IsString a => a
developmentHotReloadPing = "/hot_reload"

loginCreate :: IsString a => a
loginCreate = "/logins"

loginNew :: IsString a => a
loginNew = "/logins/new"

signUpNew :: IsString a => a
signUpNew = "/sign_ups/new"

signUpCreate :: IsString a => a
signUpCreate = "/sign_ups"

signUpValidatePassword :: IsString a => a
signUpValidatePassword = "/sign_ups/validate_password"
