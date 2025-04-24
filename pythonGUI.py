import tkinter as tk
from tkinter import ttk, messagebox
from pyswip import Prolog

# Load Prolog
prolog = Prolog()
prolog.consult("DiagnosisProjectTest.pl")  # Update path if needed

# Create main window
root = tk.Tk()
root.title("Medical Diagnosis System")
root.geometry("500x600")
root.configure(bg="#f0f0f5")
current_patient_label = tk.Label(root, text="Current Patient: None", font=("Helvetica", 10), fg="blue")
current_patient_label.pack(pady=(0, 10))

# Variables
patient_var = tk.StringVar()
symptom_var = tk.StringVar()
symptom_list = []

# Functions
def set_patient():
    patient = patient_var.get().strip()
    if patient:
        list(prolog.query("retractall(current_patient(_))"))
        prolog.assertz(f"current_patient({patient})")
        messagebox.showinfo("Success", f"Patient set to '{patient}'")
        current_patient_label.config(text=f"Current Patient: {patient}")
    else:
        messagebox.showwarning("Input Error", "Please enter a patient name.")

def add_symptom():
    patient = patient_var.get().strip()
    symptom = symptom_var.get().strip()
    if not patient:
        messagebox.showwarning("Input Error", "Please set a patient name first.")
        return
    if symptom:
        prolog.assertz(f"symptom({patient}, {symptom})")
        symptom_list.append(symptom)
        update_symptom_display()
        symptom_var.set("")
    else:
        messagebox.showwarning("Input Error", "Please enter a symptom.")

def update_symptom_display():
    symptoms_text.configure(state='normal')
    symptoms_text.delete(1.0, tk.END)
    for s in symptom_list:
        symptoms_text.insert(tk.END, f"- {s}\n")
    symptoms_text.configure(state='disabled')

def show_diagnosis():
    patient = patient_var.get().strip()
    if not patient:
        messagebox.showwarning("Input Error", "Please enter a patient name.")
        return

    diagnosis = list(prolog.query(f"disease_score({patient}, Disease, Score)"))
    diagnosis_box.configure(state='normal')
    diagnosis_box.delete(1.0, tk.END)

    if not diagnosis:
        diagnosis_box.insert(tk.END, "No matching diagnosis found.")
    else:
        ranked = sorted(diagnosis, key=lambda d: d['Score'], reverse=True)
        diagnosis_box.insert(tk.END, "Possible Diagnoses:\n\n")
        for d in ranked:
            diagnosis_box.insert(tk.END, f"{d['Disease']} (Score: {d['Score']})\n")

    diagnosis_box.configure(state='disabled')

def clear_data():
    patient = patient_var.get().strip()
    if patient:
        list(prolog.query(f"retractall(symptom({patient}, _))"))
        symptom_list.clear()
        update_symptom_display()
        diagnosis_box.configure(state='normal')
        diagnosis_box.delete(1.0, tk.END)
        diagnosis_box.configure(state='disabled')
        messagebox.showinfo("Cleared", "Patient data cleared.")
    else:
        messagebox.showwarning("Input Error", "Please enter a patient name.")

# Layout
frame = ttk.Frame(root, padding=20)
frame.pack(expand=True, fill='both')


ttk.Label(frame, text="Patient Name:").pack(anchor='w')
ttk.Entry(frame, textvariable=patient_var, width=40).pack(pady=5)

ttk.Button(frame, text="Set Patient", command=set_patient).pack(pady=5)

ttk.Label(frame, text="Add Symptom:").pack(anchor='w', pady=(10, 0))
symptom_entry = ttk.Entry(frame, textvariable=symptom_var, width=40)
symptom_entry.pack(pady=5)

ttk.Button(frame, text="Add Symptom", command=add_symptom).pack(pady=5)

ttk.Label(frame, text="Symptoms Added:").pack(anchor='w', pady=(10, 0))
symptoms_text = tk.Text(frame, height=5, width=45, state='disabled', background='#fff', relief='sunken')
symptoms_text.pack(pady=5)

ttk.Button(frame, text="Show Diagnosis", command=show_diagnosis).pack(pady=(15, 5))

ttk.Label(frame, text="Diagnosis Result:").pack(anchor='w', pady=(10, 0))
diagnosis_box = tk.Text(frame, height=10, width=45, state='disabled', background='#fff', relief='sunken')
diagnosis_box.pack(pady=5)

ttk.Button(frame, text="Clear Patient Data", command=clear_data).pack(pady=(10, 5))
ttk.Button(frame, text="Exit", command=root.destroy).pack(pady=5)

# Run the GUI
root.mainloop()
