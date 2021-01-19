# -*- coding: utf-8 -*-
"""
Created on Wed Dec 16 15:37:12 2020

@author: Vishal Raul

IFEWs- Model-1

This code takes input variables and compute CN, MN, FN, GN, and nitrogen surplus in soil

Input variables:
 y_c   =  Corn yield (busheles/Acres) 
 y_s   =  Soy yield (busheles/Acres) 
 RCN_c = Rate of commercial N (Corn) kg/ha
 RCN_s = Rate of commercial N (Soy) kg/ha
 Hog   = Hogs/pigs population
 CAtt_B = Beef cattle population
 CAtt_M = Milk cattle population
 CAtt_O = Other cattle population
 
Output: Nitrogen surplus in soil (kg/ha) annual 
"""

import numpy as np
import openmdao.api as om

def IFEW(x):        
    
    class Agriculture(om.ExplicitComponent):
        """
        Agriculture: Computes FN, GN, CN, P_corn
        """
        def setup(self):
            # i/p 
            self.add_input('y_c', val= 0)            # Corn yield (busheles/Acres) 
            self.add_input('y_soy', val= 0)          # Soy yield (busheles/Acres) 
            self.add_input('RCN_corn', val= 0)       # rate of commercial N kg/ha
            self.add_input('RCN_soy', val= 0)        # rate of commercial N kg/ha
                    
            # o/p 
            self.add_output('P_corn', val=1.0)      # Corn production (busheles)
            self.add_output('P_soy', val=1.0)       # Soy production (busheles)
            self.add_output('CN', val=1.0)          # CN
            self.add_output('FN', val=1.0)          # FN
            self.add_output('GN', val=1.0)          # GN
            
                    
            # Finite difference all partials.
            self.declare_partials('*', '*', method='fd')
    
        def compute(self, inputs, outputs):
            """
            Evaluates P_corn, C_corn
            """                   
            #  1 bushels/acre = 67.25 kg/ha
    
            y_c = inputs['y_c']                 # corn yield bushels/acre
            y_soy = inputs['y_soy']             # corn yield bushels/acre
            
            RCN_corn = inputs['RCN_corn']       # kg/ha  # rate of commercial N /ha
            RCN_soy = inputs['RCN_soy']         # kg/ha  # rate of commercial N /ha
            
            A_corn  = 13500 * 1e3               # Acres planted (acres)  2019 data
            A_soy   = 9200 * 1e3                # Area planted (acres)   2019 data        
            A = (A_corn + A_soy)
                    
            outputs['P_corn'] = y_c * A_corn    # bushels 
            outputs['P_soy'] = y_soy * A_soy    # bushels 
                    
            # (1 bushels/acre = 67.25 kg/ha)
            outputs['FN'] = (81.1 *  (y_soy*67.25/1000) - 98.5)*A_soy / A                 # N kg
            
            outputs['GN'] = ((y_c*67.25)*(1.18/100) *A_corn + (y_soy*67.25)*(6.4/100)* A_soy  )/A   # N kg
            
            outputs['CN'] = (RCN_corn*A_corn + RCN_soy*A_soy )/ A
           
    
    class EtOH_Prod(om.ExplicitComponent):
        """
        ETOH production
        """
        def setup(self):
            
            # input
            self.add_input('P_corn_EtOH', val=1.0)       # corn production for EtOH (bushels)
            
            # output 
            self.add_output('P_EtOH', val=1.0)          # Ethanol production (mil/gal)
            self.add_output('WC_EtOH', val=1.0)
    
            # Finite difference all partials.
            self.declare_partials('*', '*', method='fd')
    
        def compute(self, inputs, outputs):
            EtOH_Dry = 2.7      # gal EtOH/bushels (Dry grind mills)
            WCR = 3             # gal water / gal of Ethanol
            
            outputs['P_EtOH'] = inputs['P_corn_EtOH']*EtOH_Dry  # gal od EtOH production                
            outputs['WC_EtOH'] = outputs['P_EtOH'] * WCR
            
            
    class Animal_Ag(om.ExplicitComponent):
        """
        Animal Agriculture: computes MN
        """
        def setup(self):
            #input
            self.add_input('Catt', val=np.array([1.0, 1.0, 1.0]))
            self.add_input('Hog', val=1.0)
            
            # output
            self.add_output('MN', val=1.0)
            
            # Finite difference all partials.
            self.declare_partials('*', '*', method='fd')
    
        def compute(self, inputs, outputs):
            # life cycle days
            lf_Beef = 365   
            lf_Milk = 365   
            lf_HS = 365     # Heifer/steer
            lf_Slught_Catt = 170     # 
            lf_Hog = 365
            
            # N kg /day per animal
            N_Beef = 0.15
            N_Milk = 0.204   
            N_HS = 0.1455     # Heifer/steer
            N_Slught_Catt = 0.104     # 
            N_HOG = 0.027
            
            A_corn  = 13500 * 1e3       # Acres planted (acres)  2019 data
            A_soy = 9200 * 1e3          # Area planted (acres)   2019 data        
            A = (A_corn + A_soy)
                    
            Catt_Beef = inputs['Catt'][0]
            Catt_Milk = inputs['Catt'][1]
            Catt_Othr = inputs['Catt'][2]
            
            Hog = inputs['Hog']
            
            Total_Catt_N = Catt_Beef*N_Beef*lf_Beef + Catt_Milk*N_Milk*lf_Milk + \
                           0.5*Catt_Othr*N_HS*lf_HS + 0.5*Catt_Othr*N_Slught_Catt*lf_Slught_Catt
            
            Total_Hog_N = Hog * N_HOG * lf_Hog
            
            outputs['MN'] = (Total_Catt_N + Total_Hog_N)/A  # N kg/ha
    
    
    class N_surplus(om.ExplicitComponent):
        """
        Nitrogen surplus in soil
        """
        def setup(self):
            # input 
            self.add_input('MN', val=1.0)
            self.add_input('FN', val=1.0)
            self.add_input('GN', val=1.0)
            self.add_input('CN', val=1.0)
            
            # output
            self.add_output('N_surplus', val=1.0)
    
            # Finite difference all partials.
            self.declare_partials('*', '*', method='fd')
    
        def compute(self, inputs, outputs):     
            outputs['N_surplus'] = (inputs['CN']  + inputs['MN'] + inputs['FN'] - inputs['GN'] )  # N kg/ha
    
    
            
    class Demand_Corn(om.ExplicitComponent):
        """
        Constraint 1 - used for future calculations
        """
        def setup(self):
            # input         
            self.add_input('P_corn', val=1.0)
            self.add_input('P_corn_EtOH', val=1.0)
            self.add_input('D_Corn', val=1.0)
            
            # output
            self.add_output('const1', val=1.0)
              
            # Finite difference all partials.
            self.declare_partials('*', '*', method='fd')
    
        def compute(self, inputs, outputs):
    
            outputs['const1'] =  inputs['D_Corn'] - (inputs['P_corn'] - inputs['P_corn_EtOH'] )
    
            
    class Demand_EtOH(om.ExplicitComponent):
        """
        Constraint 2 - used for future calculations
        """
        def setup(self):
            # input
            self.add_input('P_EtOH', val=1.0)
            self.add_input('D_EtOH', val=1.0)
            
            # output
            self.add_output('const2', val=1.0)
            
            # Finite difference all partials.
            self.declare_partials('*', '*', method='fd')
    
        def compute(self, inputs, outputs):
    
            outputs['const2'] =  inputs['D_EtOH'] - inputs['P_EtOH']
            
            
    class Demand_FP(om.ExplicitComponent):
        """
        Constraint 3 - used for future calculations
        """
        def setup(self):
            # input
            self.add_input('D_catt_meat', val=1.0)
            self.add_input('D_Hog', val=1.0)
            
            self.add_input('Catt', val=np.array([1.0, 1.0, 1.0]))
            self.add_input('Hog', val=1.0)
            
            # output 
            self.add_output('const3', val=1.0)
            self.add_output('const4', val=1.0)
            
            # Finite difference all partials.
            self.declare_partials('*', '*', method='fd')
    
        def compute(self, inputs, outputs):
    
            Catt_Beef = inputs['Catt'][0]
            Catt_Othr = inputs['Catt'][2]
            
            outputs['const3'] =  inputs['D_catt_meat']  - ( Catt_Beef + 0.5 * Catt_Othr)
            outputs['const4'] =  inputs['D_Hog'] - inputs['Hog']
            
            
            
    class SellarMDA(om.Group):
        """
        Group containing the Sellar MDA.
        """
        def setup(self):
            
            # Design variables
            indeps = self.add_subsystem('indeps', om.IndepVarComp(), promotes=['*'])
            
            indeps.add_output('w', np.ones(6))  
            
            indeps.add_output('y_c', 1)
            indeps.add_output('y_soy', 1)
            indeps.add_output('RCN_corn', 1)
            indeps.add_output('RCN_soy', 1)
            
            
            indeps.add_output('P_corn_EtOH', 10e6)        
            indeps.add_output('Catt', np.ones(3))
            indeps.add_output('Hog', 1)       
            
            indeps.add_output('D_EtOH', 1e6)            
            indeps.add_output('D_Corn', 20000)       
            indeps.add_output('D_catt_meat',10)
            indeps.add_output('D_Hog',10)
                    
            # Connections    
            self.add_subsystem('Agriculture', Agriculture(), promotes_inputs=['y_c','y_soy','RCN_corn'], promotes_outputs=['CN', 'GN', 'FN','P_corn'])
            self.add_subsystem('EtOH_Prod', EtOH_Prod(), promotes_inputs=['P_corn_EtOH'], promotes_outputs=['P_EtOH','WC_EtOH'])        
            self.add_subsystem('Animal_Ag', Animal_Ag(), promotes_inputs=['Catt','Hog'], promotes_outputs=['MN'])
            
            # Objective function
            self.add_subsystem('Obj', N_surplus(), promotes_inputs=['CN', 'GN', 'FN', 'MN'], promotes_outputs = ['N_surplus'])
    
    
            # Constraints function
            self.add_subsystem('con_Demand_Corn', Demand_Corn(), promotes_inputs=['D_Corn','P_corn','P_corn_EtOH'],promotes_outputs=['const1'])
            self.add_subsystem('con_Demand_EtOH', Demand_EtOH(), promotes_inputs=['P_EtOH','D_EtOH'], promotes_outputs=['const2'])
            self.add_subsystem('con_Demand_FP', Demand_FP(), promotes_inputs=['D_catt_meat', 'D_Hog', 'Catt','Hog'], promotes_outputs=['const3','const4'])
            
            
            
    prob = om.Problem()
    prob.model = SellarMDA()
        
    prob.model.add_objective('N_surplus')
    
    # Add constraint 
    prob.model.add_constraint('const1',  upper=0 )
    prob.model.add_constraint('const2',  upper=0)
    prob.model.add_constraint('const3',  upper=0)
    prob.model.add_constraint('const4',  upper=0)
        
    prob.setup()
    
    # weather parameter (used in future)
    # w16 = np.array([2020, 57, 75, 1.1, 1.3, 1.55])    
    # prob.set_val('indeps.w', w16)
        
    prob.set_val('indeps.D_EtOH', 4350*1e6)      # mil gal
    prob.set_val('indeps.D_Corn', 100*1e6)       # bushels
    prob.set_val('indeps.D_catt_meat', 1e5)      # bushels
    prob.set_val('indeps.D_Hog', 10e5)           # bushels
    
    prob.set_val('indeps.y_c', x[0])             # bushels/acre
    prob.set_val('indeps.y_soy', x[1])           # bushels/acre
    prob.set_val('indeps.RCN_corn', x[2])        # kg/ha
    prob.set_val('indeps.RCN_soy', x[3])         # kg/ha
    
    prob.set_val('indeps.Hog', x[4])  
    catt_p = np.array(x[5:])
    prob.set_val('indeps.Catt', catt_p ) 
    

    
    prob.run_model()
    
    print('\n Agriculture ----------------------------')
    print('y_c (bu/acre)=',prob['y_c'][0])
    print('P_corn (bu) =',prob['P_corn'][0])
    print('y_soy (bu/acre)=',prob['y_soy'][0])
    
    print('\n EtOH Production ----------------------------')
    print('P_EtOH (gal) =',prob['P_EtOH'][0])
    print('P_corn_EtOH (bu) =',prob['P_corn_EtOH'][0])
    print('WC_EtOH (gal) =',prob['WC_EtOH'][0])
    #
    print('\n Animal Ag ----------------------------')
    print('Catt (population)=',prob['Catt'])
    print('Hog (population)=',prob['Hog'][0])
        
    print('\n N_surplus ----------------------------')
    print('MN (kg/ha)=',prob['MN'][0])
    print('CN (kg/ha)=',prob['CN'][0])
    print('FN (kg/ha)=',prob['FN'][0])
    print('GN (kg/ha)=',prob['GN'][0])
    print('N_surplus (kg/ha)=',prob['N_surplus'][0])
    
    #
    print('\n Constraint (used for future calculations) ----------')
    print('const1  : con_Demand_Corn ',prob['const1'])
    print('const2  : con_Demand_EtOH',prob['const2'])
    print('const3  : con_Demand_FP_Catt',prob['const3'])
    print('const4  : con_Demand_FP_Hog',prob['const4'])
    
    return prob['N_surplus'][0]


if __name__ == '__main__':
    
    ## input variables
    ##           [ y_c , y_s, RCN_c, RCN_s, Hog   , CAtt_B , CAtt_M , CAtt_O]
    x = np.array([203 , 45 , 193  , 21   , 18*1e6, 773914 , 316411 , 1090324])
    
    N_surplus = IFEW(x)
    
    print('\n N_surplus (kg/ha)', N_surplus)